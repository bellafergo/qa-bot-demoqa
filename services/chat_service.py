# services/chat_service.py
import json
import traceback
from typing import Any, Dict

from fastapi import HTTPException
from openai import OpenAI

from core.settings import settings
from core.state import cleanup_sessions

from core.prompts import SYSTEM_PROMPT, SYSTEM_PROMPT_DOC, SYSTEM_PROMPT_EXECUTE
from core.tools import QA_TOOL, QA_DOC_TOOL

from services.store import create_thread, add_message, update_thread_title

from core.chat_helpers import (
    _norm,
    _get_session,
    _push_history,
    _wants_doc,
    _wants_execute_explicit,
    _wants_execute_followup,
    _doc_requested_parts,
    _infer_domain,
    _extract_user_story,
    _make_title_from_prompt,
    _cache_get,
    _cache_set,
    _trim_doc,
    _fallback_minimal_doc,
    _render_doc_answer,
    _pick_base_url,
    _parse_tool_args,
    _run_runner,
    _render_execute_answer,
)


def _get_client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Falta OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


def handle_chat_run(req) -> Dict[str, Any]:
    """
    Modo:
    - DOC si detecta artefactos
    - EXECUTE si detecta intención UI y hay URL/last_url
    - ADVISE en otro caso

    Persistencia:
    - Supabase-first + SQLite fallback via services/store.py
    """
    try:
        cleanup_sessions()
        sid, session = _get_session(getattr(req, "session_id", None))

        prompt = _norm(getattr(req, "prompt", ""))
        if not prompt:
            raise HTTPException(status_code=400, detail="prompt vacío")

        client = _get_client()

        # ------------------------------------------------------------
        # Thread + persistencia user msg (Store)
        # ------------------------------------------------------------
        active_thread_id = (getattr(req, "thread_id", None) or "").strip()
        if not active_thread_id:
            t = create_thread("New chat")
            active_thread_id = t["id"]

        add_message(active_thread_id, "user", prompt)

        # título auto (no pasa nada si ya tenía)
        update_thread_title(active_thread_id, _make_title_from_prompt(prompt))

        # ------------------------------------------------------------
        # Intent routing
        # ------------------------------------------------------------
        wants_doc = _wants_doc(prompt)
        wants_execute = _wants_execute_explicit(prompt, session) or _wants_execute_followup(prompt, session)

        # ============================================================
        # PRIORIDAD 1: DOC MODE
        # ============================================================
        if wants_doc and not wants_execute:
            requested = _doc_requested_parts(prompt)
            domain = _infer_domain(prompt) or settings.DOC_DEFAULT_DOMAIN
            domain = domain if domain in ("retail", "pos", "web") else "web"

            story = _extract_user_story(prompt) or prompt
            context = ""

            cache_key = f"{domain}:{story}:{json.dumps(requested, sort_keys=True, ensure_ascii=False)}"
            doc_data = _cache_get(cache_key)

            if not doc_data:
                messages = [
                    {"role": "system", "content": SYSTEM_PROMPT_DOC},
                    {
                        "role": "user",
                        "content": (
                            "Genera artefactos QA SOLO documentación.\n"
                            "Devuelve EXCLUSIVAMENTE un tool-call a generate_qa_artifacts.\n\n"
                            f"DOMAIN: {domain}\n"
                            f"REQUESTED: {json.dumps(requested, ensure_ascii=False)}\n"
                            f"HISTORIA:\n{story}\n\n"
                            f"CONTEXTO:\n{context}\n\n"
                            "Reglas:\n"
                            "- Incluye patterns.recommended_smoke / recommended_regression / recommended_edges.\n"
                            "- Si faltan datos, agrega assumptions y questions_to_clarify.\n"
                        ),
                    },
                ]

                resp = client.chat.completions.create(
                    model=settings.OPENAI_MODEL,
                    messages=messages,
                    tools=[QA_DOC_TOOL],
                    tool_choice={"type": "function", "function": {"name": "generate_qa_artifacts"}},
                    temperature=settings.DOC_TEMPERATURE,
                    max_tokens=settings.DOC_MAX_TOKENS,
                )

                tool_calls = getattr(resp.choices[0].message, "tool_calls", None) or []
                if not tool_calls:
                    doc_data = _fallback_minimal_doc(requested, domain, context, story)
                else:
                    parsed = _parse_tool_args(tool_calls[0].function.arguments)
                    doc_data = parsed or _fallback_minimal_doc(requested, domain, context, story)

                doc_data = _trim_doc(doc_data)
                _cache_set(cache_key, doc_data)

            answer = _render_doc_answer(doc_data)

            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)

            add_message(active_thread_id, "assistant", answer, meta={"mode": "doc", "doc": doc_data})

            return {
                "mode": "doc",
                "session_id": sid,
                "thread_id": active_thread_id,
                "answer": answer,
                "doc": doc_data,
            }

        # ============================================================
        # PRIORIDAD 2: ADVISE MODE
        # ============================================================
        if not wants_execute:
            messages = [{"role": "system", "content": SYSTEM_PROMPT}]
            messages.extend(session["history"][-settings.MAX_HISTORY_MSGS :])
            messages.append({"role": "user", "content": prompt})

            resp = client.chat.completions.create(
                model=settings.OPENAI_MODEL,
                messages=messages,
                temperature=settings.ADV_TEMPERATURE,
                max_tokens=settings.ADV_MAX_TOKENS,
            )

            answer = (resp.choices[0].message.content or "").strip() or "OK"

            _push_history(session, "user", prompt)
            _push_history(session, "assistant", answer)

            add_message(active_thread_id, "assistant", answer, meta={"mode": "advise"})

            return {"mode": "advise", "session_id": sid, "thread_id": active_thread_id, "answer": answer}

        # ============================================================
        # PRIORIDAD 3: EXECUTE MODE
        # ============================================================
        base_url = _pick_base_url(req, session, prompt)
        if not base_url:
            need = (
                "Para ejecutar necesito la URL (o dime “la misma” si quieres usar la última) y qué validar exactamente.\n"
                "Faltan datos para ejecutar:\n"
                "- URL (o di “la misma”)\n"
                "- Qué validar (botón/campo/texto esperado)\n"
                "- Credenciales (si aplica)\n"
            )
            _push_history(session, "user", prompt)
            _push_history(session, "assistant", need)
            add_message(active_thread_id, "assistant", need, meta={"mode": "execute", "runner": {"status": "need_info"}})
            return {"mode": "need_info", "session_id": sid, "thread_id": active_thread_id, "answer": need}

        messages = [{"role": "system", "content": SYSTEM_PROMPT_EXECUTE}]
        messages.extend(session["history"][-max(3, min(settings.MAX_HISTORY_MSGS, 6)) :])
        messages.append(
            {
                "role": "user",
                "content": (
                    "Genera steps Playwright para validar en la web.\n"
                    f"BASE_URL: {base_url}\n"
                    f"REQUEST:\n{prompt}\n\n"
                    "Reglas:\n"
                    "- Devuelve SOLO tool-call run_qa_test.\n"
                    "- Usa selectores robustos: data-testid/#id/name; si no hay usa text.\n"
                ),
            }
        )

        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages,
            tools=[QA_TOOL],
            tool_choice={"type": "function", "function": {"name": "run_qa_test"}},
            temperature=settings.EXEC_TEMPERATURE,
            max_tokens=settings.EXEC_MAX_TOKENS,
        )

        tool_calls = getattr(resp.choices[0].message, "tool_calls", None) or []
        if not tool_calls:
            raise HTTPException(status_code=500, detail="El modelo no devolvió tool-call run_qa_test")

        steps_payload = _parse_tool_args(tool_calls[0].function.arguments) or {}
        steps = steps_payload.get("steps") or steps_payload.get("actions") or steps_payload
        if not steps:
            raise HTTPException(status_code=500, detail="Tool-call sin steps")

        result = _run_runner(steps=steps, base_url=base_url, headless=bool(getattr(req, "headless", True)))
        answer = _render_execute_answer(result)

        _push_history(session, "user", prompt)
        _push_history(session, "assistant", answer)

        add_message(active_thread_id, "assistant", answer, meta={"mode": "execute", "runner": result})

        return {"mode": "execute", "session_id": sid, "thread_id": active_thread_id, "answer": answer, "runner": result}

    except HTTPException:
        raise
    except Exception as e:
        print("CHAT_RUN ERROR:", repr(e))
        print(traceback.format_exc())
        raise HTTPException(status_code=500, detail=f"{type(e).__name__}: {e}")