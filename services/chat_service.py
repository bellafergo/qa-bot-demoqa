# services/chat_service.py
from __future__ import annotations

import json
import logging
import re
from typing import Any, Dict, List, Optional

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core import chat_helpers as H
from services import store

from core.intent_router import detect_intent as detect_intent_router
from core.lang import detect_language
from core.prompts import pick_system_prompt
from core.qa_risk_engine import build_risk_brief, build_negative_and_edge_cases

from services.execute_engine import handle_execute_mode  # nuevo engine

logger = logging.getLogger("vanya.chat_service")


# ============================================================
# OpenAI client
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Missing OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


# ============================================================
# Confidence helper (ADVISE / DOC)
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
# DOC helpers (nuevo contrato executive/qa/artifact)
# ============================================================
def _normalize_doc_json(doc: Dict[str, Any], raw: Optional[str] = None) -> Dict[str, Any]:
    """
    Normaliza la salida de DOC a un esquema estable para la UI.

    Soporta:
    - v2 (nuevo): {"executive": "...", "qa": "...", "artifact": "..."}
    - v1 (legacy): con claves "executive_view"/"qa_view"
    - Fallback cuando el modelo devuelve algo raro.
    """
    if not isinstance(doc, dict):
        doc = {}

    # 🆕 v2: contrato nuevo del prompt
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

    # Fallback genérico
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
    Decide el modo: execute | doc | chat (que luego mapeamos a advise).
    Regla:
    - Web + verbo de acción => execute
    - Palabras de artefacto / matriz / Gherkin => doc
    - Sino, usamos el intent_router como apoyo, pero nunca forzamos doc/execute
      si no hay señales claras.
    """
    p = (prompt or "").lower()

    if not p:
        return "chat"

    # 1) EXECUTE forzado: URL + verbo de acción
    has_url = ("http://" in p) or ("https://" in p) or ("www." in p)
    has_web_verb = any(
        kw in p
        for kw in [
            "ve a",
            "vete a",
            "abre ",
            "abrir ",
            "ejecuta",
            "ejecutar",
            "valida",
            "validar",
            "prueba ",
            "probar ",
            "haz click",
            "haz clic",
            "da click",
            "da clic",
            "click en",
            "login",
            "inicia sesión",
            "iniciar sesion",
        ]
    )

    if has_url and has_web_verb:
        return "execute"

    # 2) DOC forzado: cuando claramente piden artefacto / matriz / casos
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
    ]
    if any(kw in p for kw in doc_keywords):
        return "doc"

    # 3) Fallback: usamos el intent_router como sugerencia,
    #    pero SOLO confiamos en execute, y en doc solo si también hay keywords.
    try:
        out = detect_intent_router(prompt)
        if isinstance(out, str):
            out_norm = out.strip().lower()
            # El router puede forzar execute
            if out_norm == "execute":
                return "execute"
            # Para doc, exigimos doble condición: router + keywords
            if out_norm == "doc" and any(kw in p for kw in doc_keywords):
                return "doc"
    except Exception:
        pass

    # 4) Default: chat => lo mapeamos a ADVISE
    return "chat"


def _persona(prompt: str, mode: str) -> str:
    if mode == "execute":
        return "automation"
    if mode == "doc":
        return "doc"
    return "lead"


def _system_prompt_for_mode(mode: str, lang: str, should_intro: bool) -> str:
    """
    mode: advise | doc | execute | clarify
    lang: es | en
    should_intro: si debe presentarse en este turno
    """
    m = (mode or "").lower().strip()

    # EXECUTE: system prompt sin header extra para no contaminar tool-calls
    if m == "execute":
        return pick_system_prompt(mode="execute", lang=lang, introduce=False).strip()

    # ADVISE / DOC / CLARIFY ya incluyen STYLE + intro según parámetros
    return pick_system_prompt(mode=m, lang=lang, introduce=should_intro).strip()


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

    if any(
        w in p
        for w in [
            "ve a",
            "abre",
            "ejecuta",
            "ejecutar",
            "valida",
            "haz clic",
            "da click",
            "login",
            "inicia sesión",
            "iniciar sesion",
        ]
    ):
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
            answer = (
                "I don't see a previous execution in this chat yet. "
                "Ask me to run a test first."
            )
            try:
                store.add_message(
                    thread_id, "assistant", answer, meta={"mode": "advise", "memory": True}
                )
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
            store.add_message(
                thread_id,
                "assistant",
                answer,
                meta={"mode": "advise", "memory": True, **mem},
            )
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
        return handle_execute_mode(
            req=req,
            session=session,
            prompt=prompt,
            thread_id=thread_id,
            persona=persona,
            messages=messages,
        )

    client = _client()

    # DOC
    if mode == "doc":
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages
            + [
                {
                    "role": "user",
                    "content": (
                        "Generate the QA artifact for the previous request.\n"
                        "Return ONLY one valid JSON object with keys: executive, qa, artifact.\n"
                        "No additional text, no markdown, no comments."
                    ),
                }
            ],
            temperature=settings.DOC_TEMPERATURE,
            max_tokens=settings.DOC_MAX_TOKENS,
        )
        raw = (resp.choices[0].message.content or "").strip()

        doc_raw = _extract_json_object(raw)
        doc_json = _normalize_doc_json(doc_raw or {}, raw)

        answer = _render_doc_answer_from_json(doc_json)
        store.add_message(
            thread_id,
            "assistant",
            answer,
            meta={"mode": "doc", "persona": persona, "doc_json": doc_json, "doc_schema": "v2"},
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
    answer = (resp.choices[0].message.content or "").strip() or "Can you share a bit more context?"

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
