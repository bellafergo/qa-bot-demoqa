# services/chat_service.py
from __future__ import annotations

import json
import logging
import re
import time
from typing import Any, Dict, List, Optional

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core import chat_helpers as H
from services import store
from runner import execute_test

# Cloudinary (evidence + pdf) - usa tus servicios existentes
from services.cloudinary_service import upload_screenshot_b64 as cloud_upload_screenshot_b64
from services.cloudinary_service import upload_pdf_bytes as cloud_upload_pdf_bytes
from services.report_service import generate_pdf_report

logger = logging.getLogger("vanya.chat_service")


# ============================================================
# SYSTEM PROMPTS
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
- No expliques: ejecuta (siempre JSON en ejecución cuando se pida).
"""


# ============================================================
# OpenAI client
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


def _persona(prompt: str, mode: str) -> str:
    p = (prompt or "").lower()
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
# JSON helpers
# ============================================================
def _safe_json_loads(s: str) -> Optional[Dict[str, Any]]:
    try:
        return json.loads(s)
    except Exception:
        return None


def _extract_json_object(raw: str) -> Optional[Dict[str, Any]]:
    """
    Intenta extraer un objeto JSON dict del texto.
    - directo
    - rescate por regex { ... }
    """
    if not raw:
        return None
    txt = raw.strip()
    obj = _safe_json_loads(txt)
    if isinstance(obj, dict):
        return obj

    m = re.search(r"\{.*\}", txt, flags=re.S)
    if not m:
        return None
    obj = _safe_json_loads(m.group(0))
    return obj if isinstance(obj, dict) else None


# ============================================================
# DOC renderer (compat UI actual)
# ============================================================
def _render_doc_answer_from_json(doc: Dict[str, Any]) -> str:
    ev = doc.get("executive_view", {}) if isinstance(doc, dict) else {}
    title = ev.get("title", "Artefacto QA")
    objective = ev.get("objective", "")

    lines: List[str] = []
    lines.append(f"## {title}")
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
# MEMORY (NO execute)
# ============================================================
def _is_memory_query(prompt: str) -> bool:
    p = (prompt or "").lower()
    keys = [
        "recuérdame", "recuerdame", "última prueba", "ultima prueba",
        "qué validamos", "que validamos", "resultado", "evidence", "evidencia",
        "qué pasó", "que paso", "resumen de la prueba", "summary de la prueba",
    ]
    return any(k in p for k in keys)


def _summarize_last_execute(history_msgs: List[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    """
    Busca el último mensaje assistant con meta.mode == execute y arma un resumen.
    """
    for m in reversed(history_msgs or []):
        if (m.get("role") or "").strip() != "assistant":
            continue
        meta = m.get("meta") or m.get("meta_json") or {}
        if isinstance(meta, str):
            meta = _safe_json_loads(meta) or {}
        if not isinstance(meta, dict):
            meta = {}

        if meta.get("mode") != "execute":
            continue

        runner = meta.get("runner") or {}
        status = (runner.get("status") or "").strip() or meta.get("runner_status")
        base_url = meta.get("base_url")
        evidence_url = meta.get("evidence_url") or runner.get("screenshot_url") or runner.get("evidence_url")
        report_url = meta.get("report_url")
        duration_ms = meta.get("duration_ms") or runner.get("duration_ms")

        answer = m.get("content") or ""
        if not answer.strip():
            answer = "Resumen: ejecución registrada (sin mensaje)."

        return {
            "answer": answer,
            "runner_status": status,
            "base_url": base_url,
            "evidence_url": evidence_url,
            "report_url": report_url,
            "duration_ms": duration_ms,
        }

    return None


# ============================================================
# EXECUTE helpers
# ============================================================
def _looks_like_saucedemo(url: str) -> bool:
    s = (url or "").lower()
    return "saucedemo.com" in s


def _strip_quotes(s: str) -> str:
    ss = (s or "").strip()
    if len(ss) >= 2 and ((ss[0] == ss[-1] == '"') or (ss[0] == ss[-1] == "'")):
        return ss[1:-1]
    return ss


def _parse_steps_from_prompt(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    """
    Parser determinístico: si el usuario ya especificó fill/click/assert,
    generamos steps sin depender del LLM.
    (Basado en tu versión) :contentReference[oaicite:2]{index=2}
    """
    p = (prompt or "").strip()
    low = p.lower()

    steps: List[Dict[str, Any]] = [{"action": "goto", "url": base_url}, {"action": "wait_ms", "ms": 250}]

    # visibles
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
        return steps

    # fill
    fill_patterns = [
        r'(?:llena|fill)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])\s+(?:con|with)\s+(".*?"|\'.*?\')',
    ]
    for pat in fill_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            val = _strip_quotes(m.group(2))
            steps.append({"action": "fill", "selector": sel, "text": val})

    # click
    click_patterns = [
        r'(?:haz\s+click\s+en|haz\s+clic\s+en|click)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])',
    ]
    for pat in click_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            steps.append({"action": "click", "selector": sel})

    # assert text
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


def _make_png_data_url(b64_or_data_url: Optional[str]) -> Optional[str]:
    if not b64_or_data_url:
        return None
    s = str(b64_or_data_url).strip()
    if not s:
        return None
    if s.startswith("data:image"):
        return s
    return f"data:image/png;base64,{s}"


def _render_execute_answer(status: str, msg: str, evidence_url: Optional[str], report_url: Optional[str]) -> str:
    parts = [f"✅ Ejecutado ({status})." + (f" {msg}" if msg else "")]
    if evidence_url:
        parts.append(f"Evidence: {evidence_url}")
    if report_url:
        parts.append(f"Report: {report_url}")
    return "\n".join(parts).strip()


def _handle_execute_mode(
    req: Any,
    session: Dict[str, Any],
    prompt: str,
    thread_id: str,
    persona: str,
    messages: List[Dict[str, str]],
) -> Dict[str, Any]:
    base_url = H.pick_base_url(req, session, prompt)

    # Si falta URL, aquí sí pedimos (solo execute). :contentReference[oaicite:3]{index=3}
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
            "session_id": session.get("id"),
            "thread_id": thread_id,
            "answer": answer,
            **_confidence("execute", prompt, None),
        }

    # 0) parser determinístico
    steps = _parse_steps_from_prompt(prompt, base_url)

    # 1) fallback LLM para steps
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
                        "Devuelve SOLO JSON con {\"steps\": [...]}."
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
        store.add_message(thread_id, "assistant", answer, meta={"mode": "execute", "persona": persona, "base_url": base_url})
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session.get("id"),
            "thread_id": thread_id,
            "answer": answer,
            **_confidence("execute", prompt, base_url),
        }

    # 2) ejecutar runner
    t0 = time.time()
    runner: Dict[str, Any] = {}
    evidence_url = None
    report_url = None

    try:
        runner = execute_test(
            base_url=base_url,
            steps=steps,
            headless=bool(getattr(req, "headless", True)),
            timeout_s=settings.RUNNER_TIMEOUT_S,
        ) or {}
    except Exception as e:
        # runner crash controlado
        runner = {"ok": False, "status": "error", "message": f"{type(e).__name__}: {str(e)}"}

    duration_ms = int((time.time() - t0) * 1000)

    # evidencia
    screenshot_b64 = runner.get("screenshot_b64") or runner.get("screenshotBase64") or runner.get("screenshot_base64")
    screenshot_data_url = _make_png_data_url(screenshot_b64) if screenshot_b64 else None

    if screenshot_b64 and settings.HAS_CLOUDINARY:
        try:
            evidence_url = cloud_upload_screenshot_b64(screenshot_b64)
        except Exception:
            logger.exception("Cloudinary screenshot upload failed")

    # pdf report (si existe evidencia o runner)
    try:
        pdf_bytes = generate_pdf_report(
            prompt=prompt,
            base_url=base_url,
            steps=steps,
            runner=runner,
            duration_ms=duration_ms,
            evidence_url=evidence_url,
        )
        if pdf_bytes and settings.HAS_CLOUDINARY:
            report_url = cloud_upload_pdf_bytes(pdf_bytes, filename="report.pdf")
    except Exception:
        logger.exception("PDF report generation/upload failed")

    ok = bool(runner.get("ok", True))
    status = (runner.get("status") or ("ok" if ok else "fail")).strip()
    msg = (runner.get("message") or runner.get("detail") or "").strip()

    answer = _render_execute_answer(status=status, msg=msg, evidence_url=evidence_url, report_url=report_url)

    from app import save_run
    save_run(runner_result)

    meta = meta or {}
    meta["runner"] = runner_result

    meta = {
        "mode": "execute",
        "persona": persona,
        "base_url": base_url,
        "steps": steps,
        "runner": {**runner, "screenshot_data_url": screenshot_data_url},
        "evidence_url": evidence_url,
        "report_url": report_url,
        "duration_ms": duration_ms,
    }

    store.add_message(thread_id, "assistant", answer, meta=meta)

    return {
        "mode": "execute",
        "persona": persona,
        "thread_id": thread_id,
        "answer": answer,
        "runner": meta["runner"],
        "evidence_url": evidence_url,
        "report_url": report_url,
        "duration_ms": duration_ms,
        **_confidence("execute", prompt, base_url),
    }


# ============================================================
# MAIN
# ============================================================
def handle_chat_run(req: Any) -> Dict[str, Any]:
    """
    Producto: routing determinístico + respuestas seguras.
    - DOC: fuerza JSON estructurado (Executive/QA) y nunca truena.
    - EXECUTE: corre runner si aplica; si falla, mensaje útil (no solo "error").
    - ADVISE: respuesta QA lead.
    """
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Prompt vacío")

    # defaults seguros (evita UnboundLocalError) :contentReference[oaicite:4]{index=4}
    mode: str = "advise"
    persona: str = "lead"
    answer: str = ""
    doc_json: Optional[dict] = None

    # session
    session_id, session = H.get_session(getattr(req, "session_id", None))
    session["id"] = session_id  # para usarlo en execute

    # thread
    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    # persist USER message (correcto: role=user) :contentReference[oaicite:5]{index=5}
    try:
        store.add_message(thread_id, "user", prompt, meta={"mode_hint": "input"})
    except Exception:
        logger.warning("Failed to persist user message (continuing)", exc_info=True)

    # history
    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    # MEMORY (siempre advise) :contentReference[oaicite:6]{index=6}
    if _is_memory_query(prompt):
        mem = _summarize_last_execute(history_msgs)
        if not mem:
            answer = "Aún no veo una ejecución previa en este chat. Pídeme que ejecute una prueba y luego te resumo el resultado."
            store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "memory": True})
            return {
                "mode": "advise",
                "persona": _persona(prompt, "advise"),
                "session_id": session_id,
                "thread_id": thread_id,
                "answer": answer,
                **_confidence("advise", prompt, None),
            }

        answer = mem["answer"]
        store.add_message(thread_id, "assistant", answer, meta={"mode": "advise", "memory": True, **mem})
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

    # INTENT (execute tiene prioridad; doc no compite con execute) :contentReference[oaicite:7]{index=7}
    wants_execute = bool(H.wants_execute(prompt, session))
    wants_doc = bool(H.wants_doc(prompt)) if not wants_execute else False
    mode = "execute" if wants_execute else "doc" if wants_doc else "advise"
    persona = _persona(prompt, mode)

    # build messages
    messages: List[Dict[str, str]] = [{"role": "system", "content": _system_prompt(persona)}]
    for m in history_msgs:
        role = (m.get("role") or "assistant").strip()
        content = (m.get("content") or "").strip()
        if content:
            messages.append({"role": role, "content": content})

    # EXECUTE
    if mode == "execute":
        try:
            result = _handle_execute_mode(
                req=req,
                session=session,
                prompt=prompt,
                thread_id=thread_id,
                persona=persona,
                messages=messages,
            )
            if isinstance(result, dict) and (result.get("answer") or "").strip():
                result["session_id"] = session_id
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

    # DOC / ADVISE (nunca debe romper endpoint)
    try:
        client = _client()

        # DOC
        if mode == "doc":
            resp = client.chat.completions.create(
                model=settings.OPENAI_MODEL,
                messages=messages + [
                    {"role": "user", "content": "Devuelve exclusivamente un JSON válido. No agregues texto adicional."}
                ],
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
                        "sections": [
                            {"title": "Salida del modelo", "content": raw[:4000] if raw else "Sin contenido"}
                        ]
                    },
                }

            answer = _render_doc_answer_from_json(doc_json)
            store.add_message(thread_id, "assistant", answer, meta={"mode": "doc", "persona": persona, "doc_json": doc_json, "doc_schema": "v1"})

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

    except Exception as e:
        # fallback final: nunca 500
        msg = "Ocurrió un problema generando la respuesta. Intenta nuevamente o ajusta el alcance."
        logger.exception("DOC/ADVISE failure")
        try:
            store.add_message(thread_id, "assistant", msg, meta={"mode": mode, "persona": persona, "error": str(e)})
        except Exception:
            pass
        return {
            "mode": mode,
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": msg,
            "error": f"{type(e).__name__}: {str(e)}",
        }