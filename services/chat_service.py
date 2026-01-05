# services/chat_service.py
from __future__ import annotations

import os
import json
import logging
import re
import time
from typing import Any, Dict, List, Optional, Tuple

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core import chat_helpers as H
from services import store
from runner import execute_test

from core.intent_router import detect_intent
from core.lang import detect_language
from core.prompts import pick_system_prompt, language_style_header
from core.qa_risk_engine import build_risk_brief, build_negative_and_edge_cases

# Evidence + report (best-effort)
from services.cloudinary_service import upload_screenshot_b64 as cloud_upload_screenshot_b64
from services.cloudinary_service import upload_pdf_bytes as cloud_upload_pdf_bytes
from services.report_service import generate_pdf_report

logger = logging.getLogger("vanya.chat_service")


# ============================================================
# OpenAI client
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


# ============================================================
# Confidence helper (UI-friendly)
# ============================================================
def _confidence(mode: str, prompt: str, base_url: Optional[str]) -> Dict[str, Any]:
    score = 0.55
    if mode == "execute":
        score = 0.65 + (0.15 if base_url else -0.15)
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
    """
    Extrae un objeto JSON del texto:
    - Intenta parseo completo
    - Si falla, toma primer bloque { ... } best-effort
    """
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
# Runner meta normalizer (contract stable for UI)
# ============================================================
def _normalize_runner_meta(meta: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Contrato estable:
      meta["runner"]["status"]
      meta["runner"]["evidence_url"]
      meta["runner"]["report_url"]
      meta["runner"]["duration_ms"]
      meta["runner"]["raw"]  (runner original)
    """
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
    }

    # compat top-level (por si tu UI lo lee directo)
    meta["evidence_url"] = evidence_url
    meta["report_url"] = report_url
    meta["runner_status"] = status
    meta["duration_ms"] = duration_ms

    return meta


# ============================================================
# DOC renderer (para tu UI actual)
# ============================================================
def _render_doc_answer_from_json(doc: Dict[str, Any]) -> str:
    ev = doc.get("executive_view", {}) if isinstance(doc, dict) else {}
    title = ev.get("title", "Artefacto QA")
    objective = ev.get("objective", "")

    lines: List[str] = [f"## {title}"]
    if objective:
        lines.append(f"**Objetivo:** {objective}")

    risks = ev.get("top_risks") or []
    if risks:
        lines.append("\n### Riesgos principales")
        for r in risks:
            pr = r.get("priority", "")
            rk = r.get("risk", "")
            im = r.get("impact", "")
            line = f"- **{pr}**: {rk}"
            if im:
                line += f" — {im}"
            lines.append(line)

    matrix = ev.get("matrix_summary") or []
    if matrix:
        lines.append("\n### Matriz resumida")
        lines.append("| ID | Escenario | Resultado esperado | Prioridad |")
        lines.append("|---|---|---|---|")
        for row in matrix:
            lines.append(
                f"| {row.get('id','')} | {row.get('scenario','')} | {row.get('expected','')} | {row.get('priority','')} |"
            )

    lines.append("\n> Tip: Usa las pestañas Executive / QA para ver el detalle técnico.")
    return "\n".join(lines).strip()


# ============================================================
# One-time intro (recomendado: basado en historial del thread)
# ============================================================
def _should_introduce(history_msgs: List[Dict[str, Any]]) -> bool:
    for m in history_msgs or []:
        if (m.get("role") or "").strip() == "assistant":
            return False
    return True


def _intro_text() -> str:
    return "Hola, soy Vanya, tu Agente de QA inteligente.\n\n"


# ============================================================
# MODE / PERSONA / PROMPT helpers
# ============================================================
def _detect_intent(prompt: str) -> str:
    # detect_intent puede devolver: "execute" | "doc" | "chat" (o similar)
    try:
        out = detect_intent(prompt)
        if isinstance(out, str):
            return out.strip().lower()
    except Exception:
        pass
    return "chat"


def _persona(prompt: str, mode: str) -> str:
    # Solo etiqueta para UI/telemetría
    if mode == "execute":
        return "automation"
    if mode == "doc":
        return "doc"
    return "lead"


def _system_prompt_for_mode(mode: str, lang: str, introduced: bool) -> str:
    # pick_system_prompt ya trae tus prompts por modo/persona
    sys = pick_system_prompt(mode=mode)
    # Encabezado de estilo por idioma (una vez por chat/hilo)
    header = language_style_header(lang=lang, introduced=introduced)
    return f"{header}{sys}".strip()


# ============================================================
# MEMORY (NO execute)
# ============================================================
_MEMORY_PATTERNS = [
    # ES
    r"\b(última|ultima)\s+(prueba|ejecución|ejecucion|corrida|run)\b",
    r"\b(recu[ée]rdame|recuerda)\b",
    r"\b(qu[ée]\s+pas[óo]|qu[ée]\s+sal[ióio])\b",
    r"\b(resumen|summary)\b.*\b(prueba|ejecución|run)\b",
    r"\b(estatus|status|resultado)\b.*\b(prueba|run|ejecución)\b",
    r"\b(evidencia|evidence|reporte|report)\b",
    r"\b(qu[ée]\s+validamos|qu[ée]\s+se\s+valid[óo])\b",
    # EN
    r"\b(last|previous)\s+(test|run|execution)\b",
    r"\b(what\s+happened|result|status)\b.*\b(test|run|execution)\b",
]


def _is_memory_query(prompt: str) -> bool:
    p = (prompt or "").strip().lower()
    if not p:
        return False

    # Si suena a ejecución, NO es memoria
    if any(w in p for w in ["ve a", "abre", "ejecuta", "ejecutar", "valida", "haz clic", "da click", "login", "inicia sesión", "iniciar sesion"]):
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

        answer = (m.get("content") or "").strip() or "Resumen: ejecución registrada."
        return {
            "answer": answer,
            "runner_status": status,
            "status_label": "PASSED" if str(status).lower() in ("passed", "ok", "success") else "FAILED" if str(status).lower() in ("failed", "error") else "UNKNOWN",
            "base_url": meta.get("base_url") or "",
            "evidence_url": runner.get("evidence_url"),
            "report_url": runner.get("report_url"),
            "duration_ms": runner.get("duration_ms"),
            "has_evidence": bool(runner.get("evidence_url")),
            "has_report": bool(runner.get("report_url")),
        }

    return None


# ============================================================
# EXECUTE helpers
# ============================================================
def _looks_like_saucedemo(url: str) -> bool:
    return "saucedemo.com" in (url or "").lower()


def _strip_quotes(s: str) -> str:
    ss = (s or "").strip()
    if len(ss) >= 2 and ((ss[0] == ss[-1] == '"') or (ss[0] == ss[-1] == "'")):
        return ss[1:-1]
    return ss


def _make_png_data_url(b64_or_data_url: Optional[str]) -> Optional[str]:
    if not b64_or_data_url:
        return None
    s = str(b64_or_data_url).strip()
    if not s or s == "None":
        return None
    if s.startswith("data:image"):
        return s
    return f"data:image/png;base64,{s}"


def _ensure_has_assert(steps: List[Dict[str, Any]], base_url: str) -> List[Dict[str, Any]]:
    if not steps:
        return steps

    if any(str(s.get("action", "")).startswith("assert_") for s in steps):
        return steps

    if _looks_like_saucedemo(base_url):
        steps.append({"action": "assert_visible", "selector": "#user-name"})
        return steps

    # Generic fallback
    steps.append({"action": "assert_visible", "selector": "body"})
    return steps


def _parse_steps_from_prompt(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    p = (prompt or "").strip()
    low = p.lower()
    if not p:
        return None

    steps: List[Dict[str, Any]] = [
        {"action": "goto", "url": base_url},
        {"action": "wait_ms", "ms": 250},
    ]

    if "visibles" in low or "visible" in low:
        selectors = re.findall(r'(["\']?)(#[-\w]+|\.[-\w]+|\[[^\]]+\])\1', p)
        seen: List[str] = []
        for _, sel in selectors:
            if sel and sel not in seen:
                seen.append(sel)

        if _looks_like_saucedemo(base_url) and not seen:
            seen = ["#user-name", "#password", "#login-button"]

        for sel in seen:
            steps.append({"action": "assert_visible", "selector": sel})

        return _ensure_has_assert(steps, base_url)

    # fills (ES/EN)
    fill_patterns = [
        r'(?:llena|escribe|ingresa|teclea|fill|type)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])\s+(?:con|with)\s+(".*?"|\'.*?\')',
    ]
    for pat in fill_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            val = _strip_quotes(m.group(2))
            steps.append({"action": "assert_visible", "selector": sel})
            steps.append({"action": "fill", "selector": sel, "text": val})

    # clicks (ES/EN)
    click_patterns = [
        r'(?:haz\s+click\s+en|haz\s+clic\s+en|da\s+click\s+en|click)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])',
    ]
    for pat in click_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            steps.append({"action": "assert_visible", "selector": sel})
            steps.append({"action": "click", "selector": sel})

    # press Enter
    if re.search(r"\b(enter|intro|presiona\s+enter|presiona\s+intro)\b", low):
        steps.append({"action": "press", "key": "Enter"})

    # assert text contains
    text_patterns = [
        r'(?:valida|validar|verify|assert)\s+.*?(?:texto|text).*?(".*?"|\'.*?\')',
        r'(?:assert_text_contains)\s+(".*?"|\'.*?\')',
    ]
    found_text = None
    for pat in text_patterns:
        mm = re.search(pat, p, flags=re.IGNORECASE)
        if mm:
            found_text = _strip_quotes(mm.group(1))
            break
    if found_text:
        steps.append({"action": "wait_ms", "ms": 300})
        steps.append({"action": "assert_text_contains", "text": found_text})

    useful = [s for s in steps if s.get("action") not in ("goto", "wait_ms")]
    if not useful:
        return None

    return _ensure_has_assert(steps, base_url)


def _render_execute_answer(status: str, msg: str, evidence_url: Optional[str], report_url: Optional[str]) -> str:
    st = (status or "unknown").strip()
    prefix = "✅ Ejecutado" if st.lower() in ("passed", "ok", "success") else "⚠️ Ejecutado"
    parts = [f"{prefix} ({st})." + (f" {msg}" if msg else "")]
    if evidence_url:
        parts.append(f"📸 Evidencia: {evidence_url}")
    if report_url:
        parts.append(f"📄 Reporte: {report_url}")
    return "\n".join(parts).strip()


def _pull_evidence_fields(runner: Dict[str, Any]) -> Tuple[str, Optional[str]]:
    """
    FIX CRÍTICO:
    - Tu runner REAL manda screenshot en runner.raw.screenshot_b64 y evidence_id en runner.raw.evidence_id
    - También soporta top-level por compat.
    """
    raw = runner.get("raw") if isinstance(runner.get("raw"), dict) else {}
    evidence_id = str(runner.get("evidence_id") or raw.get("evidence_id") or "").strip()
    b64 = (
        runner.get("screenshot_b64")
        or runner.get("screenshotBase64")
        or runner.get("screenshotB64")
        or raw.get("screenshot_b64")
        or raw.get("screenshotBase64")
        or raw.get("screenshotB64")
    )
    return evidence_id, (str(b64) if b64 else None)


# ============================================================
# EXECUTE mode handler
# ============================================================
def _handle_execute_mode(
    req: Any,
    session: Dict[str, Any],
    prompt: str,
    thread_id: str,
    persona: str,
    messages: List[Dict[str, str]],
) -> Dict[str, Any]:
    """
    Flujo estable:
    base_url -> steps -> runner -> (upload evidence) -> (pdf report) -> save_run -> persist -> return
    """
    t0 = time.time()
    session_id = session.get("id")

    base_url = H.pick_base_url(req, session, prompt)

    if not base_url:
        answer = (
            "Para ejecutar necesito:\n"
            "- URL (o dime “la misma”)\n"
            "- Qué validar (botón / campo / texto esperado)\n"
            "- Credenciales (si aplica)"
        )
        store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona})
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            **_confidence("execute", prompt, None),
        }

    # 1) Parser determinístico
    steps = _parse_steps_from_prompt(prompt, base_url)

    # 2) Fallback LLM para steps
    if not steps:
        client = _client()
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages
            + [
                {
                    "role": "user",
                    "content": (
                        f"Base URL: {base_url}\n"
                        f"Generate Playwright steps to:\n{prompt}\n"
                        'Return ONLY valid JSON: {"steps": [...]}'
                    ),
                }
            ],
            temperature=settings.EXEC_TEMPERATURE,
            max_tokens=settings.EXEC_MAX_TOKENS,
        )
        raw = (resp.choices[0].message.content or "").strip()
        steps = H.extract_steps_from_text(raw)

    if not steps:
        answer = "No pude generar pasos ejecutables. Dime el botón/campo exacto y el texto esperado."
        store.add_message(
            thread_id,
            "assistant",
            answer,
            meta={"mode": "execute", "persona": persona, "base_url": base_url},
        )
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            **_confidence("execute", prompt, base_url),
        }

    steps = _ensure_has_assert(steps, base_url)

    # 3) Ejecutar runner
    runner_any: Any = {}
    runner: Dict[str, Any] = {}

    try:
        runner_any = execute_test(
            base_url=base_url,
            steps=steps,
            headless=bool(getattr(req, "headless", True)),
            timeout_s=settings.RUNNER_TIMEOUT_S,
        )
        runner = runner_any if isinstance(runner_any, dict) else {}
    except Exception as e:
        logger.exception("Runner execution failed")
        answer = (
            "No pude ejecutar la prueba.\n"
            "Verifica:\n"
            "- URL accesible\n"
            "- Credenciales correctas (si aplica)\n"
            "- Qué validación exacta quieres (texto/botón/elemento)\n"
            "Y reintenta."
        )
        store.add_message(
            thread_id,
            "assistant",
            answer,
            meta={
                "mode": "execute",
                "persona": persona,
                "error": f"{type(e).__name__}: {e}",
                "base_url": base_url,
                "steps": steps,
            },
        )
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "error": f"{type(e).__name__}: {e}",
            **_confidence("execute", prompt, base_url),
        }

    ok = bool(runner.get("ok", True))
    status = str(runner.get("status") or ("passed" if ok else "failed")).strip().lower() or ("passed" if ok else "failed")
    if status not in ("passed", "failed", "unknown"):
        status = "passed" if ok else "failed"

    msg = str(runner.get("message") or runner.get("detail") or runner.get("reason") or "").strip()

    # duration
    duration_ms = runner.get("duration_ms")
    try:
        duration_ms = int(duration_ms) if duration_ms is not None else int((time.time() - t0) * 1000)
    except Exception:
        duration_ms = int((time.time() - t0) * 1000)

    status_label = "PASSED" if status == "passed" else "FAILED" if status == "failed" else "UNKNOWN"

    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    screenshot_data_url: Optional[str] = None

    # 4) Evidence: raw.screenshot_b64 -> Cloudinary
    evidence_id, b64 = _pull_evidence_fields(runner)
    if b64:
        screenshot_data_url = _make_png_data_url(b64)

    try:
        if b64 and getattr(settings, "HAS_CLOUDINARY", False):
            # cloud_upload_screenshot_b64 returns dict -> extract secure_url/url
            res = cloud_upload_screenshot_b64(
                str(screenshot_data_url),  # NOTE: if your cloudinary_service expects data-url, pass screenshot_data_url instead
                evidence_id=(evidence_id or "EV-unknown"),
            )

            if isinstance(res, dict):
                evidence_url = (res.get("secure_url") or res.get("url") or "").strip() or None
            else:
                evidence_url = str(res).strip() or None

        # fallback: if runner already provides a URL
        if not evidence_url:
            raw = runner.get("raw", {}) or {}
            evidence_url = (
                runner.get("evidence_url")
                or runner.get("screenshot_url")
                or raw.get("evidence_url")
                or raw.get("screenshot_url")
            )

    except Exception:
        logger.exception("Evidence upload failed (continuing)")

    # 5) PDF report best-effort (never breaks)
    try:
        rep = generate_pdf_report(
            prompt=prompt,
            base_url=base_url,
            runner={**runner, "screenshot_data_url": screenshot_data_url} if screenshot_data_url else runner,
            steps=steps,
            evidence_id=evidence_id,
            meta={
                "thread_id": thread_id,
                "session_id": session_id,
                "headless": getattr(req, "headless", True),
            },
        )

        logger.info(f"[PDF] generate_pdf_report returned: {rep}")

        if not rep or not isinstance(rep, dict):
            logger.warning("[PDF] rep is empty or not a dict -> skipping upload")
        elif not rep.get("report_path"):
            logger.warning("[PDF] rep has no report_path -> skipping upload")
        else:
            rp = rep["report_path"]
            logger.info(f"[PDF] report_path={rp} exists={os.path.exists(rp)} size={(os.path.getsize(rp) if os.path.exists(rp) else 0)}")

        if getattr(settings, "HAS_CLOUDINARY", False) and rep and rep.get("report_path"):
            with open(rep["report_path"], "rb") as f:
                pdf_bytes = f.read()

            if pdf_bytes:
                res_pdf = cloud_upload_pdf_bytes(
                    pdf_bytes,
                    evidence_id=(evidence_id or "EV-unknown"),
                )
                logger.info(f"[PDF] cloudinary upload result keys: {list(res_pdf.keys()) if isinstance(res_pdf, dict) else type(res_pdf)}")
                logger.info(f"[PDF] cloudinary secure_url/url: {(res_pdf.get('secure_url') or res_pdf.get('url')) if isinstance(res_pdf, dict) else res_pdf}")

                if isinstance(res_pdf, dict):
                    report_url = (res_pdf.get("secure_url") or res_pdf.get("url") or "").strip() or None
                else:
                    report_url = str(res_pdf).strip() or None

    except Exception:
        logger.exception("PDF report generation/upload failed (continuing)")

    # 6) Guardar run best-effort (sin import cíclico)
    try:
        from services.run_store import save_run
        save_run(
            {
                **(runner if isinstance(runner, dict) else {}),
                "evidence_id": evidence_id,
                "base_url": base_url,
                "prompt": prompt,
                "steps": steps,
                "evidence_url": evidence_url,
                "report_url": report_url,
                "duration_ms": duration_ms,
                "thread_id": thread_id,
                "session_id": session_id,
                "mode": "execute",
                "status": status,
                "status_label": status_label,
            }
        )
    except Exception:
        logger.exception("save_run failed (continuing)")

    # 7) Respuesta final + meta estable
    answer = _render_execute_answer(status=status, msg=msg, evidence_url=evidence_url, report_url=report_url)

    # runner “raw” debe preservar el original completo para debug
    meta_runner = dict(runner) if isinstance(runner, dict) else {}
    meta_runner.setdefault("status", status)
    meta_runner.setdefault("duration_ms", duration_ms)
    # deja la captura data_url solo para reporte/UI si la necesitas
    if screenshot_data_url:
        meta_runner["screenshot_data_url"] = screenshot_data_url

    meta = {
        "mode": "execute",
        "persona": persona,
        "base_url": base_url,
        "steps": steps,
        "runner": {
            "status": status,
            "evidence_url": evidence_url,
            "report_url": report_url,
            "duration_ms": duration_ms,
            "raw": meta_runner,
        },
        "evidence_url": evidence_url,
        "report_url": report_url,
        "duration_ms": duration_ms,
        "status_label": status_label,
        "evidence_id": evidence_id,
    }

    meta = _normalize_runner_meta(meta)
    store.add_message(thread_id, "assistant", answer, meta=meta)

    return {
        "mode": "execute",
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        "runner": meta["runner"],
        "status_label": status_label,
        "evidence_id": evidence_id,
        "evidence_url": meta["runner"].get("evidence_url"),
        "report_url": meta["runner"].get("report_url"),
        "duration_ms": meta["runner"].get("duration_ms"),
        **_confidence("execute", prompt, base_url),
    }


# ============================================================
# MAIN
# ============================================================
def handle_chat_run(req: Any) -> Dict[str, Any]:
    """
    Producto: routing determinístico + respuestas seguras.
    - DOC: fuerza JSON estructurado (Executive/QA) y nunca truena.
    - EXECUTE: corre runner; si falla, mensaje útil (y error trazable).
    - ADVISE: respuesta QA lead.
    """
    # 0) Prompt siempre primero
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Prompt vacío")

    # 1) Session SIEMPRE antes de lang (FIX bug de 'session' no definido)
    session_id, session = H.get_session(getattr(req, "session_id", None))
    session["id"] = session_id

    # 2) Lang
    try:
        lang = detect_language(prompt, session)
    except Exception:
        lang = "es"
    session["lang"] = lang

    # 3) Thread
    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    # 4) Persist user message
    try:
        store.add_message(thread_id, "user", prompt, meta={"mode_hint": "input"})
    except Exception:
        logger.warning("Failed to persist user message (continuing)", exc_info=True)

    # 5) Load history
    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    introduced = not _should_introduce(history_msgs)

    # 6) MEMORY (solo resumen; no ejecuta)
    if _is_memory_query(prompt):
        mem = _summarize_last_execute(history_msgs)
        if not mem:
            answer = (
                "Aún no veo una ejecución previa en este chat. "
                "Pídeme que ejecute una prueba y luego te resumo el resultado."
            )
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

    # 7) Intent -> mode/persona
    intent = _detect_intent(prompt)  # execute | doc | chat
    mode = "execute" if intent == "execute" else "doc" if intent == "doc" else "advise"
    persona = _persona(prompt, mode)

    # 8) Build messages
    sys_prompt = _system_prompt_for_mode(mode=mode, lang=lang, introduced=introduced)
    messages: List[Dict[str, str]] = [{"role": "system", "content": sys_prompt}]

    # historial (solo contenido)
    for m in history_msgs:
        role = (m.get("role") or "assistant").strip()
        content = (m.get("content") or "").strip()
        if content:
            messages.append({"role": role, "content": content})

    # 9) EXECUTE
    if mode == "execute":
        return _handle_execute_mode(
            req=req,
            session=session,
            prompt=prompt,
            thread_id=thread_id,
            persona=persona,
            messages=messages,
        )

    # 10) DOC / ADVISE
    client = _client()

    if mode == "doc":
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages
            + [{"role": "user", "content": "Devuelve exclusivamente un JSON válido. No agregues texto adicional."}],
            temperature=settings.DOC_TEMPERATURE,
            max_tokens=settings.DOC_MAX_TOKENS,
        )
        raw = (resp.choices[0].message.content or "").strip()
        doc_json = _extract_json_object(raw)

        if not isinstance(doc_json, dict):
            doc_json = {
                "executive_view": {
                    "title": "Artefacto QA",
                    "objective": "El modelo no devolvió JSON válido. Se muestra salida de respaldo.",
                    "top_risks": [],
                    "matrix_summary": [],
                },
                "qa_view": {
                    "sections": [{"title": "Salida del modelo", "content": raw[:4000] if raw else "Sin contenido"}]
                },
            }

        answer = _render_doc_answer_from_json(doc_json)
        store.add_message(
            thread_id,
            "assistant",
            answer,
            meta={"mode": "doc", "persona": persona, "doc_json": doc_json, "doc_schema": "v1"},
        )
        return {
            "mode": "doc",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "doc_json": doc_json,
            **_confidence("doc", prompt, None),
        }

    # ADVISE
    try:
        risk = build_risk_brief(prompt)
        neg = build_negative_and_edge_cases(risk.get("domain", "general"))
        messages.append(
            {
                "role": "user",
                "content": (
                    "Contexto QA (no repitas literal, úsalo para priorizar):\n\n"
                    "Riesgos sugeridos:\n"
                    "P0:\n- " + "\n- ".join((risk.get("p0") or [])[:3]) +
                    "\nP1:\n- " + "\n- ".join((risk.get("p1") or [])[:3]) +
                    "\nP2:\n- " + "\n- ".join((risk.get("p2") or [])[:3]) +
                    "\n\nCasos NEGATIVOS sugeridos:\n- " + "\n- ".join((neg.get("negative") or [])[:8]) +
                    "\n\nEdge cases sugeridos:\n- " + "\n- ".join((neg.get("edge") or [])[:8])
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
    answer = (resp.choices[0].message.content or "").strip() or "¿Puedes darme más contexto?"

    store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "persona": persona})
    base_url_hint = H.pick_base_url(req, session, prompt)

    return {
        "mode": "advise",
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        **_confidence("advise", prompt, base_url_hint),
    }