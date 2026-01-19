# services/chat_service.py
from __future__ import annotations

import json
import logging
import re
import uuid
import os
from typing import Any, Dict, List, Optional, Tuple

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core import chat_helpers as H
from services import store
from services import run_store

from core.intent_router import detect_intent as detect_intent_router

# ✅ si ya actualizaste intent_router.py con esta función, importala:
try:
    from core.intent_router import has_purchase_confirmation  # type: ignore
except Exception:
    has_purchase_confirmation = None  # fallback seguro

from core.lang import detect_language
from core.prompts import pick_system_prompt
from core.qa_risk_engine import build_risk_brief, build_negative_and_edge_cases

from services.execute_engine import handle_execute_mode  # engine general
from runner import execute_test, execute_heb_full_purchase  # runners

logger = logging.getLogger("vanya.chat_service")


# ============================================================
# SAFETY TOKEN (compra real)
# ============================================================
# El usuario debe escribir EXACTAMENTE este token en el prompt para permitir mode="purchase"
PURCHASE_CONFIRM_TOKEN = "CONFIRMAR_COMPRA"


def _prompt_has_purchase_token(prompt: str) -> bool:
    """
    Seguridad: compra real SOLO si el usuario incluye CONFIRMAR_COMPRA.
    Usa has_purchase_confirmation() si existe; si no, regex local.
    """
    p = prompt or ""
    if callable(has_purchase_confirmation):
        try:
            return bool(has_purchase_confirmation(p))  # type: ignore
        except Exception:
            pass

    return bool(re.search(rf"(^|\W){re.escape(PURCHASE_CONFIRM_TOKEN)}(\W|$)", p, flags=re.IGNORECASE))


# ============================================================
# OpenAI client
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Missing OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


# ============================================================
# Confidence helper (ADVISE / DOC / EXECUTE)
# ============================================================
def _confidence(mode: str, prompt: str, base_url: Optional[str]) -> Dict[str, Any]:
    score = 0.55
    if mode == "execute":
        score = 0.65 + (0.15 if base_url else -0.30)
    elif mode == "doc":
        score = 0.60

    if len(prompt or "") < 25:
        score -= 0.10

    score = max(0.10, min(0.95, score))
    label = "high" if score >= 0.80 else "medium" if score >= 0.55 else "low"
    return {"confidence_0_1": round(score, 2), "confidence_label": label}


# ============================================================
# JSON helpers
# ============================================================
def _safe_json_loads(s: str) -> Optional[Dict[str, Any]]:
    try:
        return json.loads(s)
    except Exception:
        return None


def _extract_json_object(raw: str) -> Optional[Dict[str, Any]]:
    if not raw:
        return None
    txt = raw.strip()

    obj = _safe_json_loads(txt)
    if isinstance(obj, dict):
        return obj

    start = txt.find("{")
    end = txt.rfind("}")
    if start == -1 or end == -1 or end <= start:
        return None

    snippet = txt[start : end + 1]
    obj = _safe_json_loads(snippet)
    return obj if isinstance(obj, dict) else None


# ============================================================
# Runner meta normalizer (para memoria)
# ============================================================
def _normalize_runner_meta(meta: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    meta = meta or {}

    runner = meta.get("runner") or {}
    if not isinstance(runner, dict):
        runner = {}

    evidence_url = (
        runner.get("evidence_url")
        or runner.get("screenshot_url")
        or runner.get("screenshotUrl")
        or meta.get("evidence_url")
        or meta.get("screenshot_url")
        or meta.get("evidenceUrl")
        or meta.get("screenshotUrl")
    )

    report_url = (
        runner.get("report_url")
        or runner.get("report_pdf_url")
        or runner.get("reportUrl")
        or meta.get("report_url")
        or meta.get("report_pdf_url")
        or meta.get("reportUrl")
    )

    status = (
        runner.get("status")
        or meta.get("runner_status")
        or meta.get("runnerStatus")
        or meta.get("status")
        or "unknown"
    )
    if isinstance(status, str):
        status = status.strip() or "unknown"

    duration_ms = (
        runner.get("duration_ms")
        or runner.get("durationMs")
        or meta.get("duration_ms")
        or meta.get("durationMs")
    )

    raw_runner = runner.get("raw") if isinstance(runner.get("raw"), dict) else runner

    meta["runner"] = {
        "status": status,
        "evidence_url": evidence_url,
        "report_url": report_url,
        "duration_ms": duration_ms,
        "raw": raw_runner,
        "pdf_error": meta.get("pdf_error"),
    }

    meta["evidence_url"] = evidence_url
    meta["report_url"] = report_url
    meta["runner_status"] = status
    meta["duration_ms"] = duration_ms

    return meta


# ============================================================
# DOC helpers (contrato executive/qa/artifact)
# ============================================================
def _normalize_doc_json(doc: Dict[str, Any], raw: Optional[str] = None) -> Dict[str, Any]:
    if not isinstance(doc, dict):
        doc = {}

    # v2
    if all(k in doc for k in ("executive", "qa", "artifact")):
        executive_text = str(doc.get("executive") or "").strip()
        qa_text = str(doc.get("qa") or "").strip()
        artifact_text = str(doc.get("artifact") or "").strip()

        return {
            "executive_view": {
                "title": "QA Artifact",
                "objective": executive_text,
                "top_risks": [],
                "matrix_summary": [],
            },
            "qa_view": {
                "sections": [
                    {"title": "QA analysis", "content": qa_text},
                    {"title": "QA artifact", "content": artifact_text},
                ]
            },
            "raw_v2": doc,
        }

    # v1 legacy
    if "executive_view" in doc or "qa_view" in doc:
        return doc

    # fallback
    return {
        "executive_view": {
            "title": "QA Artifact",
            "objective": "Model did not return valid JSON. Fallback shown.",
            "top_risks": [],
            "matrix_summary": [],
        },
        "qa_view": {
            "sections": [
                {
                    "title": "Model output",
                    "content": (raw or "")[:4000] if isinstance(raw, str) else "No content",
                }
            ],
        },
        "raw_fallback": doc,
    }


def _render_doc_answer_from_json(doc: Dict[str, Any]) -> str:
    ev = doc.get("executive_view", {}) if isinstance(doc, dict) else {}
    title = ev.get("title", "QA Artifact")
    objective = ev.get("objective", "")

    lines: List[str] = [f"## {title}"]
    if objective:
        lines.append(f"**Objective:** {objective}")

    risks = ev.get("top_risks") or []
    if risks:
        lines.append("\n### Top risks")
        for r in risks:
            if not isinstance(r, dict):
                continue
            pr = r.get("priority", "")
            rk = r.get("risk", "")
            im = r.get("impact", "")
            line = f"- **{pr}**: {rk}"
            if im:
                line += f" — {im}"
            lines.append(line)

    matrix = ev.get("matrix_summary") or []
    if matrix:
        lines.append("\n### Matrix summary")
        lines.append("| ID | Scenario | Expected | Priority |")
        lines.append("|---|---|---|---|")
        for row in matrix:
            if not isinstance(row, dict):
                continue
            lines.append(
                f"| {row.get('id','')} | {row.get('scenario','')} | "
                f"{row.get('expected','')} | {row.get('priority','')} |"
            )

    lines.append("\n> Tip: Use the Executive / QA tabs for full detail.")
    return "\n".join(lines).strip()


# ============================================================
# One-time intro
# ============================================================
def _should_introduce(history_msgs: List[Dict[str, Any]]) -> bool:
    for m in history_msgs or []:
        if (m.get("role") or "").strip() == "assistant":
            return False
    return True


# ============================================================
# MODE / PERSONA / PROMPT helpers
# ============================================================
def _detect_intent(prompt: str) -> str:
    """
    Decide el modo: execute | doc | chat (luego mapeamos chat->advise).

    Mantiene lo bueno:
    - HEB especial sin URL puede ser execute
    - DOC por keywords de artefactos
    - Fallback al intent_router
    """
    p = (prompt or "").strip().lower()
    if not p:
        return "chat"

    # 1) DOC (artefactos)
    doc_keywords = [
        "artefacto qa",
        "artefacto de qa",
        "matriz de pruebas",
        "matriz de test",
        "matriz de casos",
        "casos de prueba",
        "casos de test",
        "escenarios gherkin",
        "escenario gherkin",
        "feature gherkin",
        "documenta este flujo",
        "documenta el flujo",
        "documentación de pruebas",
        "documentacion de pruebas",
        "formato de casos",
        "plantilla de pruebas",
        "plantilla de casos",
        "riesgos",
        "invest",
        "checklist",
        "plan de pruebas",
        "estrategia de pruebas",
    ]
    if any(k in p for k in doc_keywords):
        return "doc"

    # 2) EXECUTE por verbos de acción
    web_verbs = [
        "ve a", "vete a", "abre ", "abrir ",
        "ejecuta", "ejecutar", "valida", "validar",
        "prueba ", "probar ",
        "haz click", "haz clic", "da click", "da clic", "click en",
        "login", "inicia sesión", "iniciar sesion",
        "corre ", "correr ", "lanza ", "lanzar ",
        "selecciona", "seleccionar",
        "llena", "llenar", "ingresa", "ingresar",
    ]
    has_web_verb = any(v in p for v in web_verbs)

    # URL / same / sitio (HEB)
    has_url = ("http://" in p) or ("https://" in p) or ("www." in p)
    has_same = any(x in p for x in ["la misma", "misma url", "mismo sitio", "mismo link", "same", "same url", "same site"])
    is_heb = ("heb" in p) or ("h-e-b" in p) or ("heb.com.mx" in p)

    # regla general
    if has_web_verb and (has_url or has_same):
        return "execute"

    # regla especial HEB (sin URL)
    if has_web_verb and is_heb:
        return "execute"

    # ✅ NUEVO: si piden ejecutar pero no dan URL (ej: "Ejecuta login"),
    # igual vamos a EXECUTE para que el engine pida la URL/target (o use "same")
    if has_web_verb and (not has_url) and (not has_same):
        return "execute"

    # fallback intent_router
    try:
        out = detect_intent_router(prompt)
        if isinstance(out, str):
            out_norm = out.strip().lower()
            if out_norm in ("execute", "doc", "chat"):
                # ojo: router puede exigir url; pero si él detecta execute, lo respetamos
                if out_norm == "execute":
                    return "execute"
                if out_norm == "doc":
                    return "doc"
    except Exception:
        pass

    return "chat"


def _persona(prompt: str, mode: str) -> str:
    if mode == "execute":
        return "automation"
    if mode == "doc":
        return "doc"
    return "lead"


def _system_prompt_for_mode(mode: str, lang: str, should_intro: bool) -> str:
    return pick_system_prompt(mode=mode, lang=lang, introduce=should_intro)


# ============================================================
# MEMORY (NO execute)
# ============================================================
_MEMORY_PATTERNS = [
    r"\b(última|ultima)\s+(prueba|ejecución|ejecucion|corrida|run)\b",
    r"\b(recu[ée]rdame|recuerda)\b",
    r"\b(qu[ée]\s+pas[óo]|qu[ée]\s+sal[ióio])\b",
    r"\b(resumen|summary)\b.*\b(prueba|ejecución|run)\b",
    r"\b(estatus|status|resultado)\b.*\b(prueba|run|ejecución)\b",
    r"\b(evidencia|evidence|reporte|report)\b",
    r"\b(qu[ée]\s+validamos|qu[ée]\s+se\s+valid[óo])\b",
    r"\b(last|previous)\s+(test|run|execution)\b",
    r"\b(what\s+happened|result|status)\b.*\b(test|run|execution)\b",
]


def _is_memory_query(prompt: str) -> bool:
    p = (prompt or "").strip().lower()
    if not p:
        return False

    # si hay ejecución explícita, no es memoria
    if any(w in p for w in ["ve a", "abre", "ejecuta", "valida", "haz clic", "da click", "login", "inicia sesión"]):
        return False

    for pat in _MEMORY_PATTERNS:
        if re.search(pat, p, flags=re.IGNORECASE):
            return True
    return False


def _summarize_last_execute(history_msgs: List[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    for m in reversed(history_msgs or []):
        if (m.get("role") or "").strip() != "assistant":
            continue

        meta = m.get("meta") or m.get("meta_json") or {}
        if isinstance(meta, str):
            meta = _safe_json_loads(meta) or {}
        if not isinstance(meta, dict):
            continue

        if (meta.get("mode") or "").strip() != "execute":
            continue

        meta = _normalize_runner_meta(meta)
        runner = meta.get("runner") or {}

        status = runner.get("status") or "unknown"
        if isinstance(status, str):
            status = status.strip() or "unknown"

        answer = (m.get("content") or "").strip() or "Summary: execution recorded."
        return {
            "answer": answer,
            "runner_status": status,
            "status_label": "PASSED"
            if str(status).lower() in ("passed", "ok", "success")
            else "FAILED"
            if str(status).lower() in ("failed", "error")
            else "UNKNOWN",
            "base_url": meta.get("base_url") or "",
            "evidence_url": runner.get("evidence_url"),
            "report_url": runner.get("report_url"),
            "duration_ms": runner.get("duration_ms"),
            "has_evidence": bool(runner.get("evidence_url")),
            "has_report": bool(runner.get("report_url")),
        }

    return None


# ============================================================
# HEB parsing helpers
# ============================================================
def _is_heb_prompt(prompt: str) -> bool:
    p = (prompt or "").lower()
    return ("heb" in p) or ("h-e-b" in p) or ("heb.com.mx" in p)


def _parse_products_from_prompt(prompt: str) -> Optional[List[str]]:
    """
    Intenta extraer productos del prompt. Soporta:
    - JSON embebido: {"products":["tomate","coca cola x2"], "mode":"cart"}
    - Texto: productos: tomate, coca cola x2
    - Lista simple: [tomate, coca cola]
    Retorna List[str] o None si no encuentra.
    """
    raw = (prompt or "").strip()
    if not raw:
        return None

    # 1) JSON embebido
    obj = _extract_json_object(raw)
    if isinstance(obj, dict):
        prods = obj.get("products")
        if isinstance(prods, list):
            out = []
            for x in prods:
                if isinstance(x, str) and x.strip():
                    out.append(x.strip())
            return out or None

    # 2) "productos:" / "products:"
    m = re.search(r"(productos|products)\s*:\s*(.+)$", raw, flags=re.IGNORECASE | re.MULTILINE)
    if m:
        tail = (m.group(2) or "").strip()
        # corta si hay otra sección
        tail = re.split(r"\n\s*(modo|mode|pago|payment|pickup)\s*:", tail, maxsplit=1, flags=re.IGNORECASE)[0].strip()
        # separa por coma
        items = [x.strip() for x in re.split(r",|;|\|", tail) if x.strip()]
        return items or None

    # 3) bracket list "[a, b]"
    m2 = re.search(r"\[([^\]]+)\]", raw)
    if m2:
        inner = m2.group(1)
        items = [x.strip().strip('"').strip("'") for x in inner.split(",") if x.strip()]
        return items or None

    return None


def _heb_mode_from_prompt(prompt: str) -> Tuple[str, List[str]]:
    """
    Decide mode HEB con seguridad.
    Retorna (mode, notices[])
    """
    p = (prompt or "").lower()
    notices: List[str] = []

    # Si el usuario dice explícitamente "solo carrito"
    if any(k in p for k in ["solo carrito", "solo agrega", "agrega al carrito", "solo agregar", "cart mode", "modo carrito"]):
        return "cart", notices

    # Si pide compra completa / comprar ahora / colocar orden
    wants_purchase = any(
        k in p
        for k in [
            "compra completa",
            "finaliza la compra",
            "termina la compra",
            "comprar ahora",
            "coloca la orden",
            "colocar orden",
            "realiza el pedido",
            "hacer el pedido",
            "place order",
            "order placed",
        ]
    )

    if wants_purchase:
        if _prompt_has_purchase_token(prompt):
            return "purchase", notices
        notices.append(
            f"🔒 Seguridad: pediste compra completa, pero NO incluiste el token {PURCHASE_CONFIRM_TOKEN}. "
            "Haré solo checkout (llego a Payment sin colocar la orden)."
        )
        return "checkout", notices

    # default: llegar a payment (demo sin riesgo)
    return "checkout", notices


# ============================================================
# HEB EXECUTE HELPER (seguro, demo-ready)
# ============================================================

def _handle_heb_execute(
    req: Any,
    session: Dict[str, Any],
    prompt: str,
    thread_id: str,
    persona: str,
) -> Dict[str, Any]:
    base_url = "https://www.heb.com.mx"

    # ---------------------------------------------------------
    # 1) Products: UI payload first, else parse from prompt
    # ---------------------------------------------------------
    products: Optional[List[str]] = None
    try:
        req_products = getattr(req, "products", None)
        if isinstance(req_products, list) and req_products:
            products = [str(x).strip() for x in req_products if str(x).strip()]
        else:
            products = _parse_products_from_prompt(prompt)
    except Exception:
        products = _parse_products_from_prompt(prompt)

    # ---------------------------------------------------------
    # 2) Mode + safety notices
    # ---------------------------------------------------------
    mode, notices = _heb_mode_from_prompt(prompt)

    # Helper: build evidence URL (/runs/{evidence_id})
    def _build_evidence_url(evidence_id: Optional[str]) -> Optional[str]:
        if not evidence_id:
            return None
        public_base = (os.getenv("PUBLIC_BASE_URL") or "").strip().rstrip("/")
        return f"{public_base}/runs/{evidence_id}" if public_base else f"/runs/{evidence_id}"

    # Helper: persist run best-effort (in-memory + supabase inside run_store)
    def _save_run_best_effort(payload: Dict[str, Any]) -> None:
        try:
            # import local to avoid cycles in some deployments
            from services import run_store
            run_store.save_run(payload)
        except Exception:
            logger.warning("Failed to save run in run_store (continuing)", exc_info=True)

    # ---------------------------------------------------------
    # 3) Execute runner
    # ---------------------------------------------------------
    try:
        runner_result = execute_heb_full_purchase(
            products=products,
            mode=mode,
            headless=True,
            viewport={"width": 1920, "height": 1080},
            timeout_s=180,
            expected="pass",
            pickup_mode="recoger_en_tienda",
            payment_mode="pago_al_recibir",  # demo sin tarjeta
        )
    except Exception as e:
        # Si el runner truena, igual generamos un evidence_id para poder guardar/consultar algo
        evidence_id = f"EV-heb-{uuid.uuid4().hex[:10]}"
        reason = f"Error ejecutando flujo HEB: {type(e).__name__}: {e}"

        evidence_url = _build_evidence_url(evidence_id)

        # Guardamos un "run" mínimo para que /runs/{evidence_id} muestre el error
        run_payload = {
            "ok": False,
            "status": "error",
            "expected": "pass",
            "outcome": "fail",
            "reason": reason,
            "evidence_id": evidence_id,
            "steps": [],
            "logs": [reason],
            "screenshot_b64": None,
            "duration_ms": None,
            "meta": {
                "base_url": base_url,
                "mode": mode,
                "products": products,
                "pickup_mode": "recoger_en_tienda",
                "payment_mode": "pago_al_recibir",
                "tags": ["heb", "execute", "error", mode],
                "evidence_url": evidence_url,
            },
        }
        _save_run_best_effort(run_payload)

        notice_block = ""
        if notices:
            notice_block = "\n\n" + "\n".join([f"- {n}" for n in notices])

        answer = (
            "Intenté ejecutar el flujo HEB pero ocurrió un error técnico.\n\n"
            f"- Detalle: {reason}\n"
            f"- Evidencia: {evidence_url}"
            f"{notice_block}"
        )

        meta = {
            "mode": "execute",
            "persona": persona,
            "base_url": base_url,
            "runner": {
                "status": "error",
                "evidence_url": evidence_url,
                "report_url": None,
                "duration_ms": None,
                "raw": {"error": reason, "mode": mode, "products": products, "evidence_id": evidence_id},
            },
            "evidence_id": evidence_id,
        }
        meta = _normalize_runner_meta(meta)

        try:
            store.add_message(thread_id, "assistant", answer, meta=meta)
        except Exception:
            logger.warning("Failed to persist HEB error answer", exc_info=True)

        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session.get("id"),
            "thread_id": thread_id,
            "answer": answer,
            "runner": meta.get("runner"),
            "evidence_id": evidence_id,
            "evidence_url": meta.get("evidence_url"),
            "report_url": meta.get("report_url"),
            "duration_ms": meta.get("duration_ms"),
            **_confidence("execute", prompt, base_url),
        }

    # ---------------------------------------------------------
    # 4) Normal result: save run + return evidence link
    # ---------------------------------------------------------
    status = str(runner_result.get("status") or "unknown").strip().lower()
    reason = str(runner_result.get("reason") or "Flujo HEB ejecutado.").strip()
    duration_ms = runner_result.get("duration_ms")

    evidence_id = str(runner_result.get("evidence_id") or "").strip() or None
    evidence_url = _build_evidence_url(evidence_id)

    # order number (si existe)
    order_number = None
    try:
        meta_out = runner_result.get("meta") or {}
        if isinstance(meta_out, dict):
            order_number = meta_out.get("order_number")
    except Exception:
        order_number = None

    # ✅ Asegura tags + guarda evidence_url en meta para que el /runs lo muestre también
    run_payload = dict(runner_result if isinstance(runner_result, dict) else {})
    meta_payload = run_payload.get("meta")
    if not isinstance(meta_payload, dict):
        meta_payload = {}
        run_payload["meta"] = meta_payload

    # tags para index (PR agent en el futuro) + debug
    tags = meta_payload.get("tags")
    if not isinstance(tags, list):
        tags = []
    base_tags = ["heb", "execute", mode]
    for t in base_tags:
        if t not in tags:
            tags.append(t)
    meta_payload["tags"] = tags

    # guarda evidence_url dentro del run payload
    if evidence_url:
        meta_payload["evidence_url"] = evidence_url

    # guarda también base_url/prompt (útil para auditoría)
    meta_payload.setdefault("base_url", base_url)
    meta_payload.setdefault("prompt", prompt)

    # Persistir en run_store (así /runs/{evidence_id} sirve)
    _save_run_best_effort(run_payload)

    # ---------------------------------------------------------
    # 5) Build human answer
    # ---------------------------------------------------------
    notice_block = ""
    if notices:
        notice_block = "\n\n" + "\n".join([f"- {n}" for n in notices])

    human_status = "PASÓ ✅" if status == "passed" else "FALLÓ ❌" if status == "failed" else "UNKNOWN"

    if mode == "cart":
        mode_line = "Modo: **CARRITO** (solo agregar productos)"
    elif mode == "checkout":
        mode_line = "Modo: **CHECKOUT** (llega a Payment, NO coloca orden)"
    else:
        mode_line = "Modo: **PURCHASE** (coloca orden real con pagar/recoger en tienda)"

    order_line = f"\n- **Orden:** {order_number}" if order_number else ""
    evid_line = f"\n- **Evidencia:** {evidence_url}" if evidence_url else ""

    answer = (
        "Ejecuté el flujo HEB (automatizado).\n\n"
        f"- Status del runner: **{status.upper()}** ({human_status})\n"
        f"- {mode_line}\n"
        f"- Detalle: {reason}\n"
        f"- Duración: {duration_ms} ms"
        f"{order_line}"
        f"{evid_line}"
        f"{notice_block}"
    )

    meta = {
        "mode": "execute",
        "persona": persona,
        "base_url": base_url,
        "runner": {
            "status": status,
            "evidence_url": evidence_url,
            "report_url": None,
            "duration_ms": duration_ms,
            "raw": runner_result,
        },
        "evidence_id": evidence_id,
    }
    meta = _normalize_runner_meta(meta)

    try:
        store.add_message(thread_id, "assistant", answer, meta=meta)
    except Exception:
        logger.warning("Failed to persist HEB execute answer", exc_info=True)

    return {
        "mode": "execute",
        "persona": persona,
        "session_id": session.get("id"),
        "thread_id": thread_id,
        "answer": answer,
        "runner": meta.get("runner"),
        "evidence_id": evidence_id,
        "evidence_url": meta.get("evidence_url"),
        "report_url": meta.get("report_url"),
        "duration_ms": meta.get("duration_ms"),
        **_confidence("execute", prompt, base_url),
    }


# ============================================================
# MAIN
# ============================================================
def handle_chat_run(req: Any) -> Dict[str, Any]:
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Empty prompt")

    # Session
    session_id, session = H.get_session(getattr(req, "session_id", None))
    session["id"] = session_id

    # Language
    try:
        lang = detect_language(prompt, session)
    except Exception:
        lang = "es"

    # Fix: si detecta "en" pero parece español
    p_lower = prompt.lower()
    spanish_markers = [
        " hola", "¿", "¡", "prueba", "pruebas",
        "ejecuta", "ejecutar", "sesión", "sesion",
        "flujo", "historia de usuario", "casos de prueba",
        "qué ", "que ", "valida", "validar",
    ]
    if (lang or "").lower().startswith("en") and any(m in p_lower for m in spanish_markers):
        lang = "es"

    session["lang"] = lang

    # Thread
    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    # Persist user message
    try:
        store.add_message(thread_id, "user", prompt, meta={"mode_hint": "input"})
    except Exception:
        logger.warning("Failed to persist user message (continuing)", exc_info=True)

    # History
    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    should_intro = _should_introduce(history_msgs)

    # MEMORY branch
    if _is_memory_query(prompt):
        mem = _summarize_last_execute(history_msgs)
        if not mem:
            answer = "No veo una ejecución previa en este chat todavía. Pídeme que ejecute una prueba primero."
            try:
                store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "memory": True})
            except Exception:
                pass
            return {
                "mode": "advise",
                "persona": _persona(prompt, "advise"),
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("advise", prompt, None),
            }

        answer = mem["answer"]
        try:
            store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "memory": True, **mem})
        except Exception:
            pass

        return {
            "mode": "advise",
            "persona": _persona(prompt, "advise"),
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "evidence_url": mem.get("evidence_url"),
            "report_url": mem.get("report_url"),
            "duration_ms": mem.get("duration_ms"),
            **_confidence("advise", prompt, mem.get("base_url")),
        }

    # Intent -> mode
    intent = _detect_intent(prompt)
    mode = "execute" if intent == "execute" else "doc" if intent == "doc" else "advise"
    persona = _persona(prompt, mode)

    # System prompt
    sys_prompt = _system_prompt_for_mode(mode=mode, lang=lang, should_intro=should_intro)
    messages: List[Dict[str, str]] = [{"role": "system", "content": sys_prompt}]

    # Context/history
    for m in history_msgs:
        role = (m.get("role") or "assistant").strip()
        content = (m.get("content") or "").strip()
        if content:
            messages.append({"role": role, "content": content})

    # EXECUTE
    if mode == "execute":
        # ✅ HEB especial: runner dedicado + safety token
        if _is_heb_prompt(prompt):
            return _handle_heb_execute(
                req=req,
                session=session,
                prompt=prompt,
                thread_id=thread_id,
                persona=persona,
            )

        # ✅ Todo lo demás usa engine normal (no rompemos tu flujo)
        return handle_execute_mode(
            req=req,
            session=session,
            prompt=prompt,
            thread_id=thread_id,
            persona=persona,
            messages=messages,
        )

    client = _client()

    # DOC (artefactos QA en JSON estricto)
    if mode == "doc":
        kwargs = {}
        # ✅ Si tu SDK soporta response_format, esto elimina el fallback casi al 100%
        try:
            kwargs["response_format"] = {"type": "json_object"}
        except Exception:
            pass

        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages + [{"role": "user", "content": "Return ONLY valid JSON. No additional text."}],
            temperature=settings.DOC_TEMPERATURE,
            max_tokens=settings.DOC_MAX_TOKENS,
            **kwargs,
        )
        raw = (resp.choices[0].message.content or "").strip()

        doc_json = _extract_json_object(raw)
        if not isinstance(doc_json, dict):
            doc_json = {}

        doc_json = _normalize_doc_json(doc_json, raw=raw)
        answer = _render_doc_answer_from_json(doc_json)

        try:
            store.add_message(
                thread_id,
                "assistant",
                answer,
                meta={"mode": "doc", "persona": persona, "doc_json": doc_json, "doc_schema": "v2"},
            )
        except Exception:
            logger.warning("Failed to persist doc answer (continuing)", exc_info=True)

        return {
            "mode": "doc",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "doc_json": doc_json,
            **_confidence("doc", prompt, None),
        }

    # ADVISE (riesgos / negativos / edge)
    if mode == "advise":
        try:
            risk = build_risk_brief(prompt)
            neg = build_negative_and_edge_cases(risk.get("domain", "general"))

            messages.append(
                {
                    "role": "user",
                    "content": (
                        "QA context (do not repeat verbatim; use it to prioritize):\n\n"
                        "Suggested risks:\n"
                        "P0:\n- "
                        + "\n- ".join((risk.get("p0") or [])[:3])
                        + "\nP1:\n- "
                        + "\n- ".join((risk.get("p1") or [])[:3])
                        + "\nP2:\n- "
                        + "\n- ".join((risk.get("p2") or [])[:3])
                        + "\n\nSuggested NEGATIVE cases:\n- "
                        + "\n- ".join((neg.get("negative") or [])[:8])
                        + "\n\nSuggested EDGE cases:\n- "
                        + "\n- ".join((neg.get("edge") or [])[:8])
                    ),
                }
            )
        except Exception:
            logger.warning("risk engine failed (continuing)", exc_info=True)

    resp = client.chat.completions.create(
        model=settings.OPENAI_MODEL,
        messages=messages,
        temperature=settings.ADV_TEMPERATURE,
        max_tokens=settings.ADV_MAX_TOKENS,
    )
    answer = (resp.choices[0].message.content or "").strip() or "¿Me das un poco más de contexto?"

    try:
        store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "persona": persona})
    except Exception:
        logger.warning("Failed to persist advise answer (continuing)", exc_info=True)

    base_url_hint = H.pick_base_url(req, session, prompt)

    return {
        "mode": "advise",
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        **_confidence("advise", prompt, base_url_hint),
    }
