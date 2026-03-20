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

from core.lang import detect_language
from core.prompts import pick_system_prompt
from core.qa_risk_engine import build_risk_brief, build_negative_and_edge_cases

from services.execute_engine import handle_execute_mode  # engine general

logger = logging.getLogger("vanya.chat_service")


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

def _repair_json_only(client: OpenAI, raw: str) -> str:
    """
    Último recurso: pide al modelo reparar y devolver SOLO JSON válido.
    (No usa markdown, no usa backticks, no agrega texto extra.)
    """
    try:
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=[
                {
                    "role": "system",
                    "content": (
                        "You are a JSON repair tool. "
                        "Return ONLY a valid JSON object. No markdown. No backticks. No explanations."
                    ),
                },
                {
                    "role": "user",
                    "content": (
                        "Fix this into ONE valid JSON object that matches the expected schema.\n\n"
                        f"RAW:\n{raw}"
                    ),
                },
            ],
            temperature=0,
            max_tokens=settings.DOC_MAX_TOKENS,
        )
        return (resp.choices[0].message.content or "").strip()
    except Exception:
        return raw


def _doc_contract_hint() -> str:
    return (
        "Return ONLY valid JSON (no markdown, no fences). "
        "Use this schema:\n\n"
        "{\n"
        '  "executive_view": {\n'
        '    "title": "string",\n'
        '    "objective": "string",\n'
        '    "top_risks": [{"priority":"P0|P1|P2","risk":"string","impact":"string"}],\n'
        '    "matrix_summary": [{"id":"string","scenario":"string","expected":"string","priority":"P0|P1|P2"}],\n'
        '    "recommended_suites": [{"tag":"string","reason":"string"}]\n'
        "  },\n"
        '  "qa_view": {"sections": [{"title":"string","content":"string"}]}\n'
        "}\n"
    )


def _doc_call_json(client: OpenAI, messages: List[Dict[str, str]]) -> Tuple[str, Dict[str, Any]]:
    """
    1) Intenta JSON "estricto" con response_format si el modelo lo soporta.
    2) Si falla parseo, hace 1 retry pidiendo reparar a JSON.
    """
    # intento 1
    try:
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages + [{"role": "user", "content": _doc_contract_hint()}],
            temperature=0.2,
            max_tokens=settings.DOC_MAX_TOKENS,
            # 🔒 Forzar JSON si está disponible en tu modelo
            response_format={"type": "json_object"},
        )
        raw = (resp.choices[0].message.content or "").strip()
        obj = _extract_json_object(raw) or {}
        if isinstance(obj, dict) and obj:
            return raw, obj
    except TypeError:
        # response_format no soportado por ese modelo/SDK
        pass
    except Exception:
        pass

    # intento 1 (sin response_format)
    resp = client.chat.completions.create(
        model=settings.OPENAI_MODEL,
        messages=messages + [{"role": "user", "content": _doc_contract_hint()}],
        temperature=0.2,
        max_tokens=settings.DOC_MAX_TOKENS,
    )
    raw = (resp.choices[0].message.content or "").strip()
    obj = _extract_json_object(raw)
    if isinstance(obj, dict) and obj:
        return raw, obj

    # intento 2: repair
    repair = client.chat.completions.create(
        model=settings.OPENAI_MODEL,
        messages=messages
        + [
            {
                "role": "user",
                "content": (
                    "You returned invalid JSON. "
                    "Repair the following into a SINGLE valid JSON object that matches the schema. "
                    "Output ONLY JSON.\n\n"
                    f"RAW:\n{raw[:6000]}"
                ),
            }
        ],
        temperature=0.0,
        max_tokens=settings.DOC_MAX_TOKENS,
    )
    raw2 = (repair.choices[0].message.content or "").strip()
    obj2 = _extract_json_object(raw2) or {}
    return raw2, obj2 if isinstance(obj2, dict) else {}


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
    if not isinstance(ev, dict):
        ev = {}

    title = (ev.get("title") or "QA Artifact").strip() or "QA Artifact"
    objective = (ev.get("objective") or "").strip()

    lines: List[str] = [f"## {title}"]
    if objective:
        lines.append(f"**Objective:** {objective}")

    risks = ev.get("top_risks") or []
    if isinstance(risks, list) and risks:
        lines.append("\n### Top risks")
        for r in risks:
            if not isinstance(r, dict):
                continue
            pr = (r.get("priority") or "").strip()
            rk = (r.get("risk") or "").strip()
            im = (r.get("impact") or "").strip()

            if not rk:
                continue

            line = f"- **{pr or 'P?'}**: {rk}"
            if im:
                line += f" — {im}"
            lines.append(line)

    matrix = ev.get("matrix_summary") or []
    if isinstance(matrix, list) and matrix:
        lines.append("\n### Matrix summary")
        lines.append("| ID | Scenario | Expected | Priority |")
        lines.append("|---|---|---|---|")
        for row in matrix:
            if not isinstance(row, dict):
                continue
            tc_id = (row.get("id") or "").strip()
            scn = (row.get("scenario") or "").strip()
            exp = (row.get("expected") or "").strip()
            pri = (row.get("priority") or "").strip()

            if not scn:
                continue

            lines.append(f"| {tc_id} | {scn} | {exp} | {pri} |")

    # ✅ suites recomendadas para PR analysis
    suites = ev.get("recommended_suites") or []
    if isinstance(suites, list) and suites:
        lines.append("\n### Recommended suites")
        for s in suites:
            if not isinstance(s, dict):
                continue
            tag = (s.get("tag") or "").strip()
            rsn = (s.get("reason") or "").strip()
            if tag:
                lines.append(f"- **{tag}**: {rsn}")

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

    # URL / same
    has_url = ("http://" in p) or ("https://" in p) or ("www." in p)
    has_same = any(x in p for x in ["la misma", "misma url", "mismo sitio", "mismo link", "same", "same url", "same site"])

    # regla general
    if has_web_verb and (has_url or has_same):
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
# MAIN
# ============================================================
def handle_chat_run(
    req: Any,
    correlation_id: Optional[str] = None,
    client_id: Optional[str] = None,
    workspace_id: Optional[str] = None,
) -> Dict[str, Any]:
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
        return handle_execute_mode(
            req=req,
            session=session,
            prompt=prompt,
            thread_id=thread_id,
            persona=persona,
            messages=messages,
            correlation_id=correlation_id,
            client_id=client_id,
            workspace_id=workspace_id,
        )

    client = _client()

    # DOC (artefactos QA en JSON estricto)
    if mode == "doc":
        raw, doc_json = _doc_call_json(client, messages)

        # Normaliza al contrato que tu UI ya entiende
        doc_json = _normalize_doc_json(doc_json, raw=raw)
        answer = _render_doc_answer_from_json(doc_json)

        try:
            store.add_message(
                thread_id,
                "assistant",
                answer,
                meta={"mode": "doc", "persona": persona, "doc_json": doc_json, "doc_schema": "strict_v1"},
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
