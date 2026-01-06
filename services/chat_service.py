# services/chat_service.py
from __future__ import annotations

import os
import io
import json
import logging
import re
import time
import uuid
from typing import Any, Dict, List, Optional, Tuple

from fastapi import HTTPException
from openai import OpenAI

from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter

from core.settings import settings
from core import chat_helpers as H
from services import store
from runner import execute_test

from core.intent_router import detect_intent as detect_intent_router
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
        raise HTTPException(status_code=500, detail="Missing OPENAI_API_KEY")
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
# DOC renderer (para tu UI actual)
# ============================================================
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
                f"| {row.get('id','')} | {row.get('scenario','')} | {row.get('expected','')} | {row.get('priority','')} |"
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
    try:
        out = detect_intent_router(prompt)
        if isinstance(out, str):
            return out.strip().lower()
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
    sys = pick_system_prompt(mode=mode)
    header = language_style_header(lang=lang, introduced=should_intro)
    return f"{header}{sys}".strip()


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

    fill_patterns = [
        r'(?:llena|escribe|ingresa|teclea|fill|type)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])\s+(?:con|with)\s+(".*?"|\'.*?\')',
    ]
    for pat in fill_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            val = _strip_quotes(m.group(2))
            steps.append({"action": "assert_visible", "selector": sel})
            steps.append({"action": "fill", "selector": sel, "text": val})

    click_patterns = [
        r'(?:haz\s+click\s+en|haz\s+clic\s+en|da\s+click\s+en|click)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\])',
    ]
    for pat in click_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            steps.append({"action": "assert_visible", "selector": sel})
            steps.append({"action": "click", "selector": sel})

    if re.search(r"\b(enter|intro|presiona\s+enter|presiona\s+intro)\b", low):
        steps.append({"action": "press", "key": "Enter"})

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


def _render_execute_answer(
    status: str,
    msg: str,
    evidence_url: Optional[str],
    report_url: Optional[str],
    evidence_id: str,
    pdf_error: Optional[str] = None,
) -> str:
    st = (status or "unknown").strip()
    prefix = "✅ Executed" if st.lower() in ("passed", "ok", "success") else "⚠️ Executed"
    parts = [f"{prefix} ({st})." + (f" {msg}" if msg else "")]
    if evidence_id:
        parts.insert(1, f"· evid: {evidence_id}")
    if evidence_url:
        parts.append(f"📸 Evidence: {evidence_url}")
    if report_url:
        parts.append(f"📄 Report: {report_url}")
    elif pdf_error:
        parts.append(f"📄 Report: not available ({pdf_error})")
    return "\n".join(parts).strip()


def _pull_evidence_fields(runner: Dict[str, Any]) -> Tuple[str, Optional[str]]:
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


def _build_fallback_pdf_bytes(
    *,
    prompt: str,
    base_url: str,
    status: str,
    duration_ms: int,
    evidence_id: str,
    evidence_url: Optional[str],
    steps: List[Dict[str, Any]],
    runner: Dict[str, Any],
) -> bytes:
    """
    Always-works PDF generator (no filesystem). Keeps deploys stable on Render.
    """
    buf = io.BytesIO()
    c = canvas.Canvas(buf, pagesize=letter)
    w, h = letter

    y = h - 50
    c.setFont("Helvetica-Bold", 14)
    c.drawString(50, y, "Vanya QA Run Report")
    y -= 24

    c.setFont("Helvetica", 10)
    c.drawString(50, y, f"Base URL: {base_url}")
    y -= 14
    c.drawString(50, y, f"Status: {status}   Duration: {duration_ms} ms   Evidence ID: {evidence_id}")
    y -= 14
    c.drawString(50, y, f"Prompt: {prompt[:1200]}")
    y -= 18

    if evidence_url:
        c.drawString(50, y, f"Evidence URL: {evidence_url}")
        y -= 18

    c.setFont("Helvetica-Bold", 11)
    c.drawString(50, y, "Steps")
    y -= 14
    c.setFont("Helvetica", 9)
    for i, s in enumerate(steps[:80], start=1):
        line = f"{i}. {s.get('action')} " + (
            " ".join(
                [
                    f"{k}={s.get(k)}"
                    for k in ("url", "selector", "text", "key", "ms")
                    if s.get(k) is not None
                ]
            )
        )
        c.drawString(55, y, line[:120])
        y -= 11
        if y < 80:
            c.showPage()
            y = h - 50
            c.setFont("Helvetica", 9)

    c.showPage()
    c.save()
    return buf.getvalue()


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
    t0 = time.time()
    session_id = session.get("id")

    base_url = H.pick_base_url(req, session, prompt)

    if not base_url:
        answer = (
            "To execute I need:\n"
            "- URL (or say “same”)\n"
            "- What to validate (button / field / expected text)\n"
            "- Credentials (if applicable)"
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

    # 1) Deterministic parser
    steps = _parse_steps_from_prompt(prompt, base_url)

    # 2) LLM fallback for steps
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
        answer = "I couldn't generate executable steps. Tell me the exact element (selector) or expected text."
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

    # 3) Execute runner
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
            "I couldn't run the test.\n"
            "Check:\n"
            "- URL accessible\n"
            "- Credentials (if any)\n"
            "- What exactly to validate (text/button/element)\n"
            "Then retry."
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
    status = str(runner.get("status") or ("passed" if ok else "failed")).strip().lower() or (
        "passed" if ok else "failed"
    )
    if status not in ("passed", "failed", "unknown"):
        status = "passed" if ok else "failed"

    msg = str(runner.get("message") or runner.get("detail") or runner.get("reason") or "").strip()

    duration_ms = runner.get("duration_ms")
    try:
        duration_ms = int(duration_ms) if duration_ms is not None else int((time.time() - t0) * 1000)
    except Exception:
        duration_ms = int((time.time() - t0) * 1000)

    status_label = "PASSED" if status == "passed" else "FAILED" if status == "failed" else "UNKNOWN"

    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    screenshot_data_url: Optional[str] = None

    # 4) Evidence: runner.raw.screenshot_b64 -> Cloudinary
    evidence_id, b64 = _pull_evidence_fields(runner)
    if not evidence_id:
        evidence_id = f"EV-{uuid.uuid4().hex[:10]}"

    if b64:
        screenshot_data_url = _make_png_data_url(b64)

    try:
        if screenshot_data_url and getattr(settings, "HAS_CLOUDINARY", False):
            res = cloud_upload_screenshot_b64(
                str(screenshot_data_url),
                evidence_id=evidence_id,
            )
            if isinstance(res, dict):
                evidence_url = (res.get("secure_url") or res.get("url") or "").strip() or None
            else:
                evidence_url = str(res).strip() or None

        if not evidence_url:
            raw0 = runner.get("raw", {}) if isinstance(runner.get("raw"), dict) else {}
            evidence_url = (
                runner.get("evidence_url")
                or runner.get("screenshot_url")
                or raw0.get("evidence_url")
                or raw0.get("screenshot_url")
            )
    except Exception:
        logger.exception("Evidence upload failed (continuing)")

    # 5) PDF report: always build bytes; upload best-effort; NEVER silent
    pdf_error: Optional[str] = None
    try:
        pdf_bytes: Optional[bytes] = None

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

        # If it returned a file path, read it (filesystem can be flaky on PaaS)
        if isinstance(rep, dict) and rep.get("report_path"):
            rp = str(rep.get("report_path") or "").strip()
            if rp:
                if os.path.exists(rp):
                    try:
                        with open(rp, "rb") as f:
                            pdf_bytes = f.read()
                    except Exception as e:
                        pdf_error = f"Could not read report_path: {type(e).__name__}: {e}"
                else:
                    pdf_error = f"report_path does not exist: {rp}"

        # Fallback: always generate a minimal PDF in memory (no filesystem)
        if not pdf_bytes:
            try:
                pdf_bytes = _build_fallback_pdf_bytes(
                    prompt=prompt,
                    base_url=base_url,
                    status=status,
                    duration_ms=duration_ms,
                    evidence_id=evidence_id,
                    evidence_url=evidence_url,
                    steps=steps,
                    runner=runner,
                )
            except Exception as e:
                pdf_error = f"Fallback PDF failed: {type(e).__name__}: {e}"
                pdf_bytes = None

        # Upload bytes to Cloudinary RAW
        if not getattr(settings, "HAS_CLOUDINARY", False):
            pdf_error = pdf_error or "HAS_CLOUDINARY is false"
        elif not pdf_bytes:
            pdf_error = pdf_error or "pdf_bytes empty"
        elif len(pdf_bytes) <= 800:
            pdf_error = pdf_error or f"pdf_bytes too small: {len(pdf_bytes)}"
        else:
            res_pdf = cloud_upload_pdf_bytes(pdf_bytes, evidence_id=evidence_id)

            if isinstance(res_pdf, dict):
                report_url = (res_pdf.get("secure_url") or res_pdf.get("url") or "").strip() or None
            else:
                report_url = str(res_pdf).strip() or None

            if not report_url:
                pdf_error = "Cloudinary upload returned no secure_url/url"

    except Exception as e:
        pdf_error = f"{type(e).__name__}: {e}"
        logger.exception("PDF report generation/upload failed")

    # 6) Save run best-effort (include pdf_error)
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
                "pdf_error": pdf_error,
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

    # 7) Final answer + stable meta
    answer = _render_execute_answer(
        status=status,
        msg=msg,
        evidence_url=evidence_url,
        report_url=report_url,
        evidence_id=evidence_id,
        pdf_error=pdf_error,
    )

    meta_runner = dict(runner) if isinstance(runner, dict) else {}
    meta_runner.setdefault("status", status)
    meta_runner.setdefault("duration_ms", duration_ms)
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
            "pdf_error": pdf_error,
        },
        "evidence_url": evidence_url,
        "report_url": report_url,
        "pdf_error": pdf_error,
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
    prompt = H.norm(getattr(req, "prompt", "") or "")
    if not prompt:
        raise HTTPException(status_code=400, detail="Empty prompt")

    session_id, session = H.get_session(getattr(req, "session_id", None))
    session["id"] = session_id

    try:
        lang = detect_language(prompt, session)
    except Exception:
        lang = "es"
    session["lang"] = lang

    thread_id = (getattr(req, "thread_id", None) or "").strip() or None
    if not thread_id:
        t = store.create_thread(title=(prompt[:60] or "New chat"))
        thread_id = t["id"]

    try:
        store.add_message(thread_id, "user", prompt, meta={"mode_hint": "input"})
    except Exception:
        logger.warning("Failed to persist user message (continuing)", exc_info=True)

    try:
        thread = store.get_thread(thread_id)
        history_msgs = (thread.get("messages") or [])[-settings.MAX_HISTORY_MSGS :]
    except Exception:
        logger.warning("Failed to load thread history (continuing)", exc_info=True)
        history_msgs = []

    should_intro = _should_introduce(history_msgs)

    if _is_memory_query(prompt):
        mem = _summarize_last_execute(history_msgs)
        if not mem:
            answer = "I don't see a previous execution in this chat yet. Ask me to run a test first."
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

    intent = _detect_intent(prompt)
    mode = "execute" if intent == "execute" else "doc" if intent == "doc" else "advise"
    persona = _persona(prompt, mode)

    sys_prompt = _system_prompt_for_mode(mode=mode, lang=lang, should_intro=should_intro)
    messages: List[Dict[str, str]] = [{"role": "system", "content": sys_prompt}]

    for m in history_msgs:
        role = (m.get("role") or "assistant").strip()
        content = (m.get("content") or "").strip()
        if content:
            messages.append({"role": role, "content": content})

    if mode == "execute":
        return _handle_execute_mode(
            req=req,
            session=session,
            prompt=prompt,
            thread_id=thread_id,
            persona=persona,
            messages=messages,
        )

    client = _client()

    if mode == "doc":
        resp = client.chat.completions.create(
            model=settings.OPENAI_MODEL,
            messages=messages + [{"role": "user", "content": "Return ONLY valid JSON. No additional text."}],
            temperature=settings.DOC_TEMPERATURE,
            max_tokens=settings.DOC_MAX_TOKENS,
        )
        raw = (resp.choices[0].message.content or "").strip()
        doc_json = _extract_json_object(raw)

        if not isinstance(doc_json, dict):
            doc_json = {
                "executive_view": {
                    "title": "QA Artifact",
                    "objective": "Model did not return valid JSON. Fallback shown.",
                    "top_risks": [],
                    "matrix_summary": [],
                },
                "qa_view": {"sections": [{"title": "Model output", "content": raw[:4000] if raw else "No content"}]},
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
                    "QA context (do not repeat verbatim; use it to prioritize):\n\n"
                    "Suggested risks:\n"
                    "P0:\n- " + "\n- ".join((risk.get("p0") or [])[:3]) +
                    "\nP1:\n- " + "\n- ".join((risk.get("p1") or [])[:3]) +
                    "\nP2:\n- " + "\n- ".join((risk.get("p2") or [])[:3]) +
                    "\n\nSuggested NEGATIVE cases:\n- " + "\n- ".join((neg.get("negative") or [])[:8]) +
                    "\n\nSuggested EDGE cases:\n- " + "\n- ".join((neg.get("edge") or [])[:8])
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
