# services/execute_engine.py
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

from core.login_intent_resolver import build_login_steps

from services.cloudinary_service import upload_screenshot_b64 as cloud_upload_screenshot_b64
from services.cloudinary_service import upload_pdf_bytes as cloud_upload_pdf_bytes
from services.report_service import generate_pdf_report

logger = logging.getLogger("vanya.execute_engine")


# ============================================================
# OpenAI client (solo EXECUTE fallback)
# ============================================================
def _client() -> OpenAI:
    if not settings.OPENAI_API_KEY:
        raise HTTPException(status_code=500, detail="Missing OPENAI_API_KEY")
    return OpenAI(api_key=settings.OPENAI_API_KEY)


# ============================================================
# Confidence helper (UI-friendly, para EXECUTE)
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
# Runner meta normalizer (contract estable para UI)
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
# Helpers EXECUTE
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

    # si ya hay asserts, no tocar
    if any(str(s.get("action", "")).startswith("assert_") for s in steps):
        return steps

    # Detecta si el flujo intenta "login" (por pasos)
    did_fill_user = any(
        s.get("action") == "fill"
        and str(s.get("selector") or "") in ("#user-name", "input[name='user-name']")
        for s in steps
    )
    did_fill_pass = any(
        s.get("action") == "fill"
        and str(s.get("selector") or "") in ("#password", "input[name='password']")
        for s in steps
    )
    did_click_login = any(
        s.get("action") == "click"
        and str(s.get("selector") or "") in ("#login-button", "button[type='submit']")
        for s in steps
    )
    looks_like_login_flow = did_click_login or (did_fill_user and did_fill_pass)

    # SauceDemo: asserts de "login exitoso" vs solo "pantalla existe"
    if _looks_like_saucedemo(base_url):
        if looks_like_login_flow:
            # ✅ éxito real: no error + URL inventory
            steps.append({"action": "wait_ms", "ms": 450})
            steps.append({"action": "assert_not_visible", "selector": "[data-test='error']"})
            steps.append({"action": "assert_url_contains", "value": "inventory.html"})
            return steps

        # si NO es login flow, al menos valida que el form existe (neutral)
        steps.append({"action": "assert_visible", "selector": "#user-name"})
        return steps

    # Default genérico: deja un assert mínimo
    steps.append({"action": "assert_visible", "selector": "body"})
    return steps


def _parse_steps_from_prompt(prompt: str, base_url: str) -> Optional[List[Dict[str, Any]]]:
    p = (prompt or "").strip()
    low = p.lower()
    if not p:
        return None

    # -----------------------------------------
    # 0) Login intent resolver (SauceDemo)
    # -----------------------------------------
    if _looks_like_saucedemo(base_url) and any(
        k in low
        for k in [
            "login",
            "inicia sesión",
            "iniciar sesion",
            "usuario",
            "username",
            "password",
            "contraseña",
            "contrasena",
        ]
    ):
        negative = any(
            k in low
            for k in [
                "no existe",
                "invalido",
                "inválido",
                "incorrecto",
                "debe fallar",
                "should fail",
                "invalid",
                "wrong",
            ]
        )

        u = None
        pw = None

        m_u = re.search(
            r'(?:username|usuario)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)', p, flags=re.IGNORECASE
        )
        if m_u:
            u = _strip_quotes(m_u.group(1))

        m_p = re.search(
            r'(?:password|pass|contraseñ?a)\s*[:=]?\s*(".*?"|\'.*?\'|\S+)',
            p,
            flags=re.IGNORECASE,
        )
        if m_p:
            pw = _strip_quotes(m_p.group(1))

        steps: List[Dict[str, Any]] = [
            {"action": "goto", "url": base_url},
            {"action": "wait_ms", "ms": 250},
            {"action": "assert_visible", "selector": "#user-name"},
            {"action": "assert_visible", "selector": "#password"},
            {"action": "assert_visible", "selector": "#login-button"},
        ]

        if u is not None:
            steps.append({"action": "fill", "selector": "#user-name", "value": u})
        if pw is not None:
            steps.append({"action": "fill", "selector": "#password", "value": pw})

        if (u is not None) or (pw is not None) or any(
            k in low for k in ["haz click", "da click", "click", "submit", "entrar", "login"]
        ):
            steps.append({"action": "click", "selector": "#login-button"})
            steps.append({"action": "wait_ms", "ms": 450})

            if negative:
                steps.insert(0, {"expected": "fail"})
                steps.append({"action": "assert_visible", "selector": "[data-test='error']"})
            else:
                steps.append({"action": "assert_not_visible", "selector": "[data-test='error']"})
                steps.append({"action": "assert_url_contains", "value": "inventory.html"})

        return steps

    # -----------------------------------------
    # Base steps
    # -----------------------------------------
    steps: List[Dict[str, Any]] = [
        {"action": "goto", "url": base_url},
        {"action": "wait_ms", "ms": 250},
    ]

    # -----------------------------------------
    # Helpers para detectar selectores
    # -----------------------------------------
    def _extract_selectors_anywhere(text: str) -> List[str]:
        out: List[str] = []

        # 1) CSS típicos: #id, .class, [attr=...]
        for _, sel in re.findall(r'(["\']?)(#[-\w]+|\.[-\w]+|\[[^\]]+\])\1', text):
            if sel and sel not in out:
                out.append(sel)

        # 2) Tags simples citados: "h1"
        for m in re.finditer(r'(["\'])([a-zA-Z][a-zA-Z0-9_-]*)\1', text):
            sel = m.group(2)
            if sel and sel not in out:
                out.append(sel)

        # 3) Tags simples sin comillas cuando viene como "selector h1"
        m = re.search(
            r'(?:selector|elemento|element|tag)\s+([a-zA-Z][a-zA-Z0-9_-]*)',
            text,
            flags=re.IGNORECASE,
        )
        if m:
            sel = m.group(1)
            if sel and sel not in out:
                out.append(sel)

        return out

    # -----------------------------------------
    # Caso: "visibles"
    # -----------------------------------------
    if "visibles" in low or "visible" in low:
        seen = _extract_selectors_anywhere(p)

        if _looks_like_saucedemo(base_url) and not seen:
            seen = ["#user-name", "#password", "#login-button"]

        for sel in seen:
            steps.append({"action": "assert_visible", "selector": sel})

        return _ensure_has_assert(steps, base_url)

    # -----------------------------------------
    # Fill patterns
    # -----------------------------------------
    fill_patterns = [
        r'(?:llena|escribe|ingresa|teclea|fill|type)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\]|[a-zA-Z][a-zA-Z0-9_-]*)\s+(?:con|with)\s+(".*?"|\'.*?\')',
    ]
    for pat in fill_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            val = _strip_quotes(m.group(2))
            steps.append({"action": "assert_visible", "selector": sel})
            steps.append({"action": "fill", "selector": sel, "value": val})

    # -----------------------------------------
    # Click patterns
    # -----------------------------------------
    click_patterns = [
        r'(?:haz\s+click\s+en|haz\s+clic\s+en|da\s+click\s+en|click)\s+(".*?"|\'.*?\'|#[-\w]+|\.[-\w]+|\[[^\]]+\]|[a-zA-Z][a-zA-Z0-9_-]*)',
    ]
    for pat in click_patterns:
        for m in re.finditer(pat, p, flags=re.IGNORECASE):
            sel = _strip_quotes(m.group(1))
            steps.append({"action": "assert_visible", "selector": sel})
            steps.append({"action": "click", "selector": sel})

    # -----------------------------------------
    # Press
    # -----------------------------------------
    if re.search(r"\b(enter|intro|presiona\s+enter|presiona\s+intro)\b", low):
        last_sel = None
        for s in reversed(steps):
            if s.get("action") == "fill" and s.get("selector"):
                last_sel = s.get("selector")
                break
        if last_sel:
            steps.append({"action": "press", "selector": last_sel, "key": "Enter"})

    # -----------------------------------------
    # Assert text
    # -----------------------------------------
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
        steps.append(
            {"action": "assert_text_contains", "selector": "body", "text": found_text}
        )

    # -----------------------------------------
    # Assert selector existe
    # -----------------------------------------
    if re.search(r"(?:valida|validar|verify|assert).*(?:selector|elemento|element)", low):
        sels = _extract_selectors_anywhere(p)
        for sel in sels[:3]:
            steps.append({"action": "assert_visible", "selector": sel})

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
    evidence_id = str(
        runner.get("evidence_id") or raw.get("evidence_id") or ""
    ).strip()
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
    c.drawString(
        50,
        y,
        f"Status: {status}   Duration: {duration_ms} ms   Evidence ID: {evidence_id}",
    )
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
# EXECUTE mode handler (exportado)
# ============================================================
def handle_execute_mode(
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
        store.add_message(
            thread_id,
            "assistant",
            answer,
            meta={"mode": "execute", "persona": persona},
        )
        return {
            "mode": "execute",
            "persona": persona,
            "session_id": session_id,
            "thread_id": thread_id,
            "answer": answer,
            **_confidence("execute", prompt, None),
        }

    # 0) Login Intent Resolver (determinístico)
    steps = None
    try:
        steps = build_login_steps(base_url=base_url, prompt=prompt)
        if steps:
            logger.info("Login intent resolver applied (deterministic steps)")
    except Exception:
        logger.exception("build_login_steps failed (continuing with normal parser)")

    # 1) Parser determinístico
    if not steps:
        steps = _parse_steps_from_prompt(prompt, base_url)

    # 2) LLM fallback para steps
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
        answer = (
            "I couldn't generate executable steps. Tell me the exact element "
            "(selector) or expected text."
        )
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
        duration_ms = (
            int(duration_ms)
            if duration_ms is not None
            else int((time.time() - t0) * 1000)
        )
    except Exception:
        duration_ms = int((time.time() - t0) * 1000)

    status_label = "PASSED" if status == "passed" else "FAILED" if status == "failed" else "UNKNOWN"

    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    screenshot_data_url: Optional[str] = None

    # 4) Evidence
    evidence_id, b64 = _pull_evidence_fields(runner)
    if not evidence_id:
        evidence_id = f"EV-{uuid.uuid4().hex[:10]}"

    if b64:
        screenshot_data_url = _make_png_data_url(b64)

    try:
        if screenshot_data_url and getattr(settings, "HAS_CLOUDINARY", False):
            res = cloud_upload_screenshot_b64(str(screenshot_data_url), evidence_id=evidence_id)
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

    # 5) PDF
    pdf_error: Optional[str] = None
    pdf_bytes: Optional[bytes] = None

    try:
        rep = generate_pdf_report(
            prompt=prompt,
            base_url=base_url,
            runner={**runner, "screenshot_data_url": screenshot_data_url}
            if screenshot_data_url
            else runner,
            steps=steps,
            evidence_id=evidence_id,
            meta={
                "thread_id": thread_id,
                "session_id": session_id,
                "headless": getattr(req, "headless", True),
            },
        )

        if isinstance(rep, dict) and rep.get("report_path"):
            rp = str(rep.get("report_path") or "").strip()
            if rp and os.path.exists(rp):
                with open(rp, "rb") as f:
                    pdf_bytes = f.read()
    except Exception as e:
        pdf_error = f"report_service failed: {type(e).__name__}: {e}"
        logger.exception("report_service generate_pdf_report failed (will fallback)")

    if not pdf_bytes:
        try:
            pdf_bytes = _build_fallback_pdf_bytes(
                prompt=prompt,
                base_url=base_url,
                status=status,
                duration_ms=duration_ms,
                evidence_id=(evidence_id or "EV-unknown"),
                evidence_url=evidence_url,
                steps=steps,
                runner=runner,
            )
        except Exception as e:
            pdf_error = (pdf_error or "") + f" | fallback_pdf failed: {type(e).__name__}: {e}"
            logger.exception("fallback PDF generation failed")

    if not getattr(settings, "HAS_CLOUDINARY", False):
        pdf_error = pdf_error or "HAS_CLOUDINARY is false"
    elif not pdf_bytes:
        pdf_error = pdf_error or "pdf_bytes empty"
    elif len(pdf_bytes) <= 800:
        pdf_error = pdf_error or f"pdf_bytes too small: {len(pdf_bytes)}"
    else:
        try:
            res_pdf = cloud_upload_pdf_bytes(pdf_bytes, evidence_id=(evidence_id or "EV-unknown"))
            if isinstance(res_pdf, dict):
                report_url = (
                    (res_pdf.get("secure_url") or res_pdf.get("url") or "").strip() or None
                )
            else:
                report_url = str(res_pdf).strip() or None

            if not report_url:
                pdf_error = pdf_error or "Cloudinary upload returned no secure_url/url"
        except Exception as e:
            pdf_error = pdf_error or f"cloudinary upload failed: {type(e).__name__}: {e}"
            logger.exception("Cloudinary PDF upload failed")

    # 6) save_run
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

    # 7) respuesta final
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
