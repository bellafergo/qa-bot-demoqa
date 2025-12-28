# services/chat_service.py
from __future__ import annotations

import json
import logging
import os
import re
import time
from typing import Any, Dict, List, Optional, Tuple

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core import chat_helpers as H
from services import store
from runner import execute_test

# Cloudinary (evidence + pdf)
from services.cloudinary_service import upload_screenshot_b64 as cloud_upload_screenshot_b64
from services.cloudinary_service import upload_pdf_bytes as cloud_upload_pdf_bytes
from services.report_service import generate_pdf_report

logger = logging.getLogger("vanya.chat_service")

def _safe_json_loads(s: str) -> Optional[Dict[str, Any]]:
    try:
        return json.loads(s)
    except Exception:
        return None


def _render_doc_answer_from_json(doc: Dict[str, Any]) -> str:
    """
    Genera un resumen legible (Markdown simple) a partir del doc_json
    para mantener compatibilidad con el UI actual.
    """
    ev = doc.get("executive_view", {})
    title = ev.get("title", "Artefacto QA")
    objective = ev.get("objective", "")

    lines: List[str] = []
    lines.append(f"## {title}")

    if objective:
        lines.append(f"**Objetivo:** {objective}")

    risks = ev.get("top_risks", [])
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
# SYSTEM PROMPTS (ligeros; el "modo" lo define el router principal)
# ============================================================
SYSTEM_PROMPT_LEAD = """Eres Vanya, QA Lead / SDET experta en Retail y E-commerce.
Tu objetivo es evitar defectos que afecten conversión, ingresos o experiencia.

Reglas:
- Señala riesgos CRÍTICOS en login, checkout, pagos, promociones, stock y performance.
- Prioriza acciones (P0 / P1 / P2).
- Pide solo la información mínima necesaria.
- Sé clara, directa y orientada a negocio.
"""

SYSTEM_PROMPT_AUTOMATION = """Eres Vanya, QA Automation / SDET en MODO EJECUCIÓN.
Tu misión es generar pasos robustos para ejecutar pruebas web.

Reglas:
- Devuelve pasos Playwright claros y estables.
- Usa selectores robustos (data-testid > id > role/text).
- Espera visibilidad antes de interactuar.
- No expliques: ejecuta (solo JSON de steps si se te pide).
"""


# ============================================================
# OpenAI client
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


def _persona(prompt: str, mode: str) -> str:
    """
    Mantiene tu lógica: por defecto lead, pero en execute usa automation
    (y permite override manual por prompt).
    """
    p = H.low(prompt)
    if "modo automation" in p or "modo automatizacion" in p:
        return "automation"
    if "modo lead" in p:
        return "lead"
    return "automation" if mode == "execute" else "lead"


def _system_prompt(persona: str) -> str:
    return SYSTEM_PROMPT_AUTOMATION if persona == "automation" else SYSTEM_PROMPT_LEAD


def _confidence(mode: str, prompt: str, base_url: Optional[str]) -> Dict[str, Any]:
    score = 0.55
    if mode == "execute":
        score = 0.65 + (0.15 if base_url else -0.15)
    elif mode == "doc":
        score = 0.60
    else:
        score = 0.55

    if len(prompt or "") < 25:
        score -= 0.10

    score = max(0.10, min(0.95, score))
    label = "high" if score >= 0.80 else "medium" if score >= 0.55 else "low"
    return {"confidence_0_1": round(score, 2), "confidence_label": label}


# ============================================================
# MEMORY (NO execute)
# ============================================================
def _is_memory_query(prompt: str) -> bool:
    p = H.low(prompt)
    keys = [
        "recuérdame", "recuerdame",
        "última prueba", "ultima prueba",
        "qué validamos", "que validamos",
        "resultado", "evidence", "evidencia",
        "qué pasó", "que paso",
        "resumen de la prueba", "summary de la prueba",
    ]
    return any(k in p for k in keys)


def _find_last_execute(history_msgs: List[Dict[str, Any]]) -> Optional[Tuple[int, Dict[str, Any]]]:
    for i in range(len(history_msgs) - 1, -1, -1):
        m = history_msgs[i] or {}
        if (m.get("role") or "").strip() != "assistant":
            continue
        meta = m.get("meta") or m.get("meta_json") or {}
        if (meta.get("mode") or "").strip() == "execute":
            return i, m
    return None


def _summarize_last_execute(history_msgs: List[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    hit = _find_last_execute(history_msgs)
    if not hit:
        return None

    idx, exec_msg = hit
    meta = exec_msg.get("meta") or exec_msg.get("meta_json") or {}

    user_prompt = ""
    for j in range(idx - 1, -1, -1):
        mj = history_msgs[j] or {}
        if (mj.get("role") or "").strip() == "user":
            user_prompt = (mj.get("content") or "").strip()
            if user_prompt:
                break

    base_url = (meta.get("base_url") or "").strip() or None
    status = (meta.get("runner_status") or "").strip() or "ok"
    duration_ms = meta.get("duration_ms")
    evidence_url = (meta.get("evidence_url") or "").strip() or None
    report_url = (meta.get("report_url") or "").strip() or None

    pieces: List[str] = []
    if user_prompt:
        pieces.append(f"**Última prueba:** {user_prompt}")
    if base_url:
        pieces.append(f"**URL:** {base_url}")
    pieces.append(f"**Resultado:** {status.upper()}")
    if isinstance(duration_ms, int):
        pieces.append(f"**Duración:** {duration_ms} ms")
    if evidence_url:
        pieces.append(f"**Evidence:** {evidence_url}")
    if report_url:
        pieces.append(f"**Reporte:** {report_url}")

    answer = "\n".join(pieces).strip()
    return {
        "answer": answer or "La última prueba fue ejecutada, pero no pude armar el resumen.",
        "base_url": base_url,
        "runner_status": status,
        "evidence_url": evidence_url,
        "report_url": report_url,
        "duration_ms": duration_ms,
    }


# ============================================================
# EXECUTION: deterministic parser (tu lógica, con mínimos fixes)
# ============================================================
def _looks_like_saucedemo(url: str) -> bool:
    return "saucedemo.com" in (url or "").lower()


def _strip_quotes(s: str) -> str:
    s = (s or "").strip()
    if (s.startswith('"') and s.endswith('"')) or (s.startswith("'") and s.endswith("'")):
        return s[1:-1].strip()
    return s


def _parse_steps_from_prompt(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    p = (prompt or "").strip()
    low = p.lower()

    steps: List[Dict[str, Any]] = [{"action": "goto", "url": base_url}, {"action": "wait_ms", "ms": 250}]

    # Visibilidad por lista de selectores
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
        return steps if len(steps) > 2 else None

    # Fill
    fill_patterns = [
        r'(?:llena|fill)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])\s+(?:con|with)\s+(".*?"|\'.*?\')',
    ]
    for pat in fill_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            val = _strip_quotes(m.group(2))
            steps.append({"action": "fill", "selector": sel, "text": val})

    # Click
    click_patterns = [
        r'(?:haz\s+click\s+en|haz\s+clic\s+en|click)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])',
    ]
    for pat in click_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            steps.append({"action": "click", "selector": sel})

    # Assert text
    text_patterns = [
        r'(?:valida|validar|verify|assert)\s+.*?(?:texto|text).*?(".*?"|\'.*?\')',
        r'(?:assert_text_contains)\s+(".*?"|\'.*?\')',
    ]
    found_text = None
    for pat in text_patterns:
        m = re.search(pat, p, flags=re.IGNORECASE)
        if m:
            found_text = _strip_quotes(m.group(1))
            break
    if found_text:
        steps.append({"action": "assert_text_contains", "text": found_text})

    useful = [s for s in steps if s["action"] not in ("goto", "wait_ms")]
    if not useful:
        return None
    return steps


# ============================================================
# Helpers: evidence data-url
# ============================================================
def _make_png_data_url(b64_or_data_url: Optional[str]) -> Optional[str]:
    if not b64_or_data_url:
        return None
    s = str(b64_or_data_url).strip()
    if not s:
        return None
    if s.startswith("data:image"):
        return s
    return f"data:image/png;base64,{s}"

# ============================================================
# MAIN
# ============================================================
def handle_chat_run(req: Any) -> Dict[str, Any]:
    """
    Producto: routing determinístico + respuestas seguras.
    - DOC: fuerza JSON estructurado (Executive/QA) y nunca truena.
    - EXECUTE: corre runner si aplica; si falla, mensaje útil (no solo "error").
    - ADVISE: respuesta normal QA lead.
    """
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Prompt vacío")

    # ---------- defaults seguros (evita UnboundLocalError) ----------
    answer: str = ""
    mode: str = "advise"
    persona: str = "lead"
    doc_json: Optional[dict] = None
    meta: Dict[str, Any] = {}

    # ---------- session ----------
    session_id, session = H.get_session(getattr(req, "session_id", None))

    # ---------- thread ----------
    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    # ---------- persist USER message (correcto: role=user) ----------
    try:
        store.add_message(
            thread_id,
            "user",
            prompt,
            meta={"mode_hint": "input"},
        )
    except Exception:
        logger.warning("Failed to persist user message (continuing)", exc_info=True)

    # ---------- load history ----------
    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    # ---------- decide mode determinísticamente ----------
    p = prompt.lower()

    DOC_TRIGGERS = (
        "matriz", "casos de prueba", "casos prueba", "gherkin", "escenarios",
        "checklist", "plan de pruebas", "test plan", "test cases", "matriz de pruebas",
    )
    EXEC_TRIGGERS = (
        "ve a ", "abre ", "navega", "haz click", "da click", "inicia sesión", "login",
        "valida", "verifica", "ejecuta", "ejecutar", "prueba ui", "playwright",
        "https://", "http://",
    )

    wants_doc = any(k in p for k in DOC_TRIGGERS)
    wants_execute = any(k in p for k in EXEC_TRIGGERS)

    # Regla de producto:
    # - Si pide artefacto -> DOC
    # - Si pide ejecutar UI -> EXECUTE
    # - Si viene mezclado, prioriza EXECUTE si hay URL + acción, si no DOC
    if wants_execute and ("http://" in p or "https://" in p or "saucedemo" in p or "valida" in p or "inicia sesión" in p):
        mode = "execute"
        persona = "automation"
    elif wants_doc:
        mode = "doc"
        persona = "doc"
    else:
        mode = "advise"
        persona = "lead"

    # ---------- build messages ----------
    # Nota: usa tu formato de mensajes existente (history + system + user)
    messages = H.build_messages(history_msgs, prompt, mode=mode, persona=persona)

    client = _client()

    # ============================================================
    # DOC (FORZADO) — siempre devuelve doc_json válido
    # ============================================================
    if mode == "doc":
        def _extract_json_object(raw: str) -> Optional[dict]:
            if not raw:
                return None
            raw = raw.strip()

            obj = _safe_json_loads(raw)
            if isinstance(obj, dict):
                return obj

            import re
            m = re.search(r"\{.*\}", raw, re.S)
            if not m:
                return None
            obj = _safe_json_loads(m.group(0))
            return obj if isinstance(obj, dict) else None

        try:
            resp = client.chat.completions.create(
                model=settings.OPENAI_MODEL,
                messages=messages,
                temperature=settings.DOC_TEMPERATURE,
                max_tokens=settings.DOC_MAX_TOKENS,
            )
            raw = (resp.choices[0].message.content or "").strip()
            doc_json = _extract_json_object(raw)

            if not isinstance(doc_json, dict):
                # fallback de producto (JSON válido siempre)
                doc_json = {
                    "executive_view": {
                        "title": "Artefacto QA",
                        "objective": "No se recibió JSON válido del modelo. Se entrega vista de respaldo.",
                        "top_risks": [],
                        "matrix_summary": [],
                    },
                    "qa_view": {
                        "sections": [
                            {
                                "title": "Salida del modelo (respaldo)",
                                "content": (raw[:4000] if raw else "Sin contenido"),
                            }
                        ]
                    },
                }

            answer = _render_doc_answer_from_json(doc_json)

            meta = {
                "mode": "doc",
                "persona": persona,
                "doc_json": doc_json,
                "doc_schema": "v1",
            }

            store.add_message(thread_id, "assistant", answer, meta=meta)

            return {
                "mode": "doc",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                "doc_json": doc_json,
                **_confidence("doc", prompt, None),
            }

        except Exception as e:
            logger.exception("DOC failure (safe fallback)")
            answer = "Ocurrió un problema generando el artefacto QA. Intenta de nuevo con más detalle (flujo/alcance) o reduce el número de casos."
            meta = {"mode": "doc", "persona": persona, "error": f"{type(e).__name__}: {str(e)}", "safe_fallback": True}
            try:
                store.add_message(thread_id, "assistant", answer, meta=meta)
            except Exception:
                pass
            return {
                "mode": "doc",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                "error": meta["error"],
                **_confidence("doc", prompt, None),
            }

    # ============================================================
    # EXECUTE — si falla, respuesta útil + runner si existe
    # ============================================================
    if mode == "execute":
        try:
            # tus helpers actuales deberían resolver base_url / pasos / runner
            # y devolver un dict con answer + meta/runner/evidence
            result = _handle_execute_mode(req=req, session=session, prompt=prompt, thread_id=thread_id, messages=messages)

            # garantiza que siempre exista answer para el frontend
            if isinstance(result, dict):
                if not (result.get("answer") or "").strip():
                    result["answer"] = "Ejecución completada. Revisa evidencia y resultado."
                return result

            # fallback raro
            answer = "No pude completar la ejecución. Reintenta indicando URL, credenciales y qué validar."
            store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona, "safe_fallback": True})
            return {
                "mode": "execute",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("execute", prompt, H.pick_base_url(req, session, prompt)),
            }

        except Exception as e:
            logger.exception("EXECUTE crashed (safe fallback)")
            answer = (
                "No pude ejecutar la prueba.\n"
                "Verifica:\n"
                "- URL accesible\n"
                "- Credenciales correctas (si aplica)\n"
                "- Qué validación exacta quieres (texto/botón/elemento)\n"
                "Y reintenta."
            )
            meta = {"mode": "execute", "persona": persona, "error": f"{type(e).__name__}: {str(e)}", "safe_fallback": True}
            try:
                store.add_message(thread_id, "assistant", answer, meta=meta)
            except Exception:
                pass
            return {
                "mode": "execute",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                "error": meta["error"],
                **_confidence("execute", prompt, H.pick_base_url(req, session, prompt)),
            }

    # ============================================================
    # ADVISE — QA lead normal, estable
    # ============================================================
    try:
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages,
            temperature=settings.ADV_TEMPERATURE,
            max_tokens=settings.ADV_MAX_TOKENS,
        )
        answer = (resp.choices[0].message.content or "").strip() or "¿Puedes darme más contexto?"

    except Exception as e:
        logger.exception("ADVISE failed (safe fallback)")
        answer = "Ocurrió un problema generando la respuesta. Intenta nuevamente."

    meta = {"mode": "advise", "persona": persona}
    try:
        store.add_message(thread_id, "assistant", answer, meta=meta)
    except Exception:
        logger.warning("Failed to persist assistant message (continuing)", exc_info=True)

    base_url_hint = H.pick_base_url(req, session, prompt)

    return {
        "mode": "advise",
        "persona": persona,
        "session_id": session_id,
        "thread_id": thread_id,
        "answer": answer,
        **_confidence("advise", prompt, base_url_hint),
    }