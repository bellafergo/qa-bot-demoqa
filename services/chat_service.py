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
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Prompt vacío")

    # ----------------------------
    # Session (in-memory)
    # ----------------------------
    session_id, session = H.get_session(getattr(req, "session_id", None))

    # ----------------------------
    # Thread (DB / store)
    # ----------------------------
    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    # ✅ Persist USER message (correct role)
    try:
        store.add_message(thread_id, "user", prompt, meta={"source": "chat"})
    except Exception:
        logger.exception("store.add_message failed (user prompt)")

    # ----------------------------
    # Load history
    # ----------------------------
    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    # ----------------------------
    # MEMORY (no execute)
    # ----------------------------
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
                logger.exception("store.add_message failed (memory empty)")
            return {
                "mode": "advise",
                "persona": _persona(prompt, "advise"),
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("advise", prompt, None),
            }

        answer = mem.get("answer") or "Resumen no disponible."
        try:
            store.add_message(
                thread_id,
                "assistant",
                answer,
                meta={
                    "mode": "advise",
                    "memory": True,
                    "base_url": mem.get("base_url"),
                    "runner_status": mem.get("runner_status"),
                    "evidence_url": mem.get("evidence_url"),
                    "report_url": mem.get("report_url"),
                    "duration_ms": mem.get("duration_ms"),
                },
            )
        except Exception:
            logger.exception("store.add_message failed (memory)")
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

    # ----------------------------
    # INTENT (execute vs doc vs advise)
    # ----------------------------
    wants_execute = bool(H.wants_execute(prompt, session))
    wants_doc = bool(H.wants_doc(prompt)) if not wants_execute else False  # doc no compite con execute

    mode = "execute" if wants_execute else "doc" if wants_doc else "advise"
    persona = _persona(prompt, mode)

    # ----------------------------
    # LLM messages
    # ----------------------------
    messages: List[Dict[str, str]] = [{"role": "system", "content": _system_prompt(persona)}]
    for m in history_msgs:
        role = (m.get("role") or "assistant").strip()
        content = (m.get("content") or "").strip()
        if content:
            messages.append({"role": role, "content": content})

    # ============================================================
    # EXECUTE (solo aquí se permite pedir URL)
    # ============================================================
    if mode == "execute":
        base_url = H.pick_base_url(req, session, prompt)

        if not base_url:
            answer = (
                "Para ejecutar necesito:\n"
                "- URL (o dime “la misma”)\n"
                "- Qué validar (botón / campo / texto esperado)\n"
                "- Credenciales (si aplica)"
            )
            try:
                store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona})
            except Exception:
                logger.exception("store.add_message failed (execute clarify)")

            return {
                "mode": "execute",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("execute", prompt, None),
            }

        # 0) Parser determinístico
        steps = _parse_steps_from_prompt(prompt, base_url)

        # 1) Fallback a LLM para steps
        if not steps:
            client = _client()
            resp = client.chat.completions.create(
                model=settings.OPENAI_MODEL,
                messages=messages
                + [
                    {
                        "role": "user",
                        "content": (
                            f"URL base: {base_url}\n"
                            f"Genera pasos Playwright para:\n{prompt}\n"
                            'Devuelve SOLO JSON con {"steps": [...]}'
                        ),
                    }
                ],
                temperature=settings.EXEC_TEMPERATURE,
                max_tokens=settings.EXEC_MAX_TOKENS,
            )
            raw = (resp.choices[0].message.content or "").strip()
            steps = H.extract_steps_from_text(raw) or []

        if not steps:
            answer = "No pude inferir pasos claros para ejecutar. Dime exactamente qué validar (campo/botón/texto)."
            try:
                store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona})
            except Exception:
                logger.exception("store.add_message failed (execute no steps)")
            return {
                "mode": "execute",
                "persona": persona,
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("execute", prompt, base_url),
            }

        # Ejecuta runner
        t0 = time.time()
        try:
            result = execute_test(
                base_url=base_url,
                steps=steps,
                headless=bool(getattr(req, "headless", True)),
                timeout_s=settings.RUNNER_TIMEOUT_S,
            )
        except Exception as e:
            result = {"status": "error", "error": f"{type(e).__name__}: {str(e)}"}

        duration_ms = int((time.time() - t0) * 1000)

        # Evidence / screenshot -> data url
        screenshot_b64 = (
            (result or {}).get("screenshot_b64")
            or (result or {}).get("screenshotBase64")
            or (result or {}).get("screenshotB64")
            or (result or {}).get("screenshot_base64")
        )
        screenshot_data_url = _make_png_data_url(screenshot_b64)

        # Cloudinary screenshot (si aplica)
        evidence_id = H.make_evidence_id()
        evidence_url = None
        cloud_public_id = None
        try:
            if screenshot_b64 and getattr(settings, "HAS_CLOUDINARY", False):
                up = cloud_upload_screenshot_b64(screenshot_b64, evidence_id=evidence_id, folder="vanya/evidence")
                evidence_url = up.get("secure_url") or up.get("url")
                cloud_public_id = up.get("public_id")
        except Exception:
            logger.warning("Cloudinary screenshot upload failed", exc_info=True)

        # PDF report (best effort)
        try:
            rep = generate_pdf_report(
                prompt=prompt,
                base_url=base_url,
                runner=result,
                steps=steps,
                evidence_id=evidence_id,
                meta={
                    "thread_id": thread_id,
                    "session_id": session_id,
                    "headless": bool(getattr(req, "headless", True)),
                },
            )
            pdf_path = rep.get("report_path")
            if pdf_path and os.path.exists(pdf_path) and getattr(settings, "HAS_CLOUDINARY", False):
                with open(pdf_path, "rb") as f:
                    pdf_bytes = f.read()
                uploaded_pdf = cloud_upload_pdf_bytes(pdf_bytes, evidence_id=evidence_id, folder="vanya/reports")
                report_url = uploaded_pdf.get("secure_url") or uploaded_pdf.get("url")
                result["report_url"] = report_url
                result["report_error"] = None
            else:
                result["report_url"] = None
                result["report_error"] = "PDF no disponible o Cloudinary no configurado"
        except Exception as e:
            result["report_url"] = None
            result["report_error"] = str(e)
            logger.warning("PDF report failed", exc_info=True)

        # Enriquecer runner para UI
        if isinstance(result, dict):
            result["duration_ms"] = result.get("duration_ms") or duration_ms
            result["screenshot_data_url"] = screenshot_data_url
            result["evidence_url"] = evidence_url

        # Render answer
        answer = (
            H.render_execute_answer(result, evidence_url=evidence_url)
            if hasattr(H, "render_execute_answer")
            else (f"✅ Ejecutado ({result.get('status')})." + (f"\nEvidence: {evidence_url}" if evidence_url else ""))
        )

        assistant_meta: Dict[str, Any] = {
            "mode": "execute",
            "persona": persona,
            "base_url": base_url,
            "duration_ms": duration_ms,
            "runner_status": result.get("status") or ("ok" if result.get("ok", True) else "error"),
            "evidence_id": evidence_id,
            "evidence_url": evidence_url,
            "cloudinary_public_id": cloud_public_id,
            "report_url": result.get("report_url"),
            "report_error": result.get("report_error"),
            "runner": {
                "status": result.get("status"),
                "error": result.get("error"),
                "evidence_id": evidence_id,
                "evidence_url": evidence_url,
                "screenshot_url": evidence_url,
                "screenshot_data_url": screenshot_data_url,
                "duration_ms": result.get("duration_ms") or duration_ms,
                "report_url": result.get("report_url"),
                "report_error": result.get("report_error"),
            },
            "steps": steps,
        }

        try:
            store.add_message(thread_id, "assistant", answer, meta=assistant_meta)
        except Exception:
            logger.exception("store.add_message failed (execute)")

        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "runner": result,
            "evidence_url": evidence_url,
            "report_url": result.get("report_url"),
            "report_error": result.get("report_error"),
            "duration_ms": duration_ms,
            **_confidence("execute", prompt, base_url),
        }

    # ============================================================
    # ADVISE / DOC (NUNCA debe romper el endpoint)
    # ============================================================
    client = _client()
    answer: str = ""
    doc_json: Optional[Dict[str, Any]] = None

    def _extract_json_object(raw: str) -> Optional[Dict[str, Any]]:
        if not raw:
            return None
        raw = raw.strip()
        obj = _safe_json_loads(raw)
        if isinstance(obj, dict):
            return obj
        m = re.search(r"\{.*\}", raw, re.S)
        if not m:
            return None
        return _safe_json_loads(m.group(0))

    try:
        if mode == "doc":
            resp = client.chat.completions.create(
                model=settings.OPENAI_MODEL,
                messages=messages,
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

            try:
                store.add_message(
                    thread_id,
                    "assistant",
                    answer,
                    meta={"mode": "doc", "persona": persona, "doc_json": doc_json, "doc_schema": "v1"},
                )
            except Exception:
                logger.exception("store.add_message failed (doc)")

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
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages,
            temperature=settings.ADV_TEMPERATURE,
            max_tokens=settings.ADV_MAX_TOKENS,
        )
        answer = (resp.choices[0].message.content or "").strip() or "¿Puedes darme más contexto?"

        try:
            store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "persona": persona})
        except Exception:
            logger.exception("store.add_message failed (advise)")

        base_url_hint = H.pick_base_url(req, session, prompt)

        return {
            "mode": "advise",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            **_confidence("advise", prompt, base_url_hint),
        }

    except Exception as e:
        logger.exception("DOC/ADVISE failure")
        answer = "Ocurrió un problema generando la respuesta. Intenta nuevamente o ajusta el alcance."
        err = f"{type(e).__name__}: {str(e)}"

        try:
            store.add_message(thread_id, "assistant", answer, meta={"mode": mode, "persona": persona, "error": err})
        except Exception:
            logger.exception("store.add_message failed (fallback)")

        return {
            "mode": mode,
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            "error": err,
        }