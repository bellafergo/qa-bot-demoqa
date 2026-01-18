from __future__ import annotations

import re
from typing import Any, Dict, Optional


# =========================
# Public API
# =========================

def classify_failure(error: Exception, signals: Dict[str, Any]) -> Dict[str, Any]:
    """
    Deterministic failure classifier for Playwright / UI E2E.
    - NO LLM calls (prod safe).
    - Uses error message + optional signals (action, selector, url, network_errors, http_status, dom_excerpt...).
    Returns a stable schema that you can store in Supabase.
    """

    msg = _safe_str(error)
    msg_l = msg.lower()

    action = _safe_str(signals.get("action")).lower()
    selector = _safe_str(signals.get("selector") or signals.get("target") or "")
    url = _safe_str(signals.get("url") or signals.get("current_url") or "")
    step_name = _safe_str(signals.get("step_name") or "")
    http_status = _maybe_int(signals.get("http_status"))
    network_errors = signals.get("network_errors") or []
    console_errors = signals.get("console_errors") or []
    dom_excerpt = _safe_str(signals.get("dom_excerpt") or signals.get("dom_snapshot") or "")

    # IMPORTANT: never leak PII in reason; keep excerpt minimal
    dom_excerpt = _redact_pii(dom_excerpt)[:1200]

    # --- 1) AUTH / PERMISSIONS ---
    # Typical auth issues:
    if any(k in msg_l for k in ["unauthorized", "forbidden", "status 401", "status 403"]) or http_status in (401, 403):
        return _out(
            category="AUTH",
            confidence=0.9,
            reason="Unauthorized/forbidden response (401/403) or permission issue.",
            retriable=False,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
        )

    # login redirect loops / session expired
    if "login" in url.lower() and any(k in msg_l for k in ["timeout", "navigation", "redirect"]):
        return _out(
            category="AUTH",
            confidence=0.75,
            reason="Likely auth/session issue (stuck on login/redirect).",
            retriable=True,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
        )

    # --- 2) UI CHANGED / LOCATOR NOT FOUND ---
    if _looks_like_locator_missing(msg_l):
        return _out(
            category="UI_CHANGED",
            confidence=0.85,
            reason="Locator not found or DOM changed (waiting for locator / strict mode / no nodes).",
            retriable=False,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
            evidence={"dom_excerpt": dom_excerpt} if dom_excerpt else None,
        )

    # --- 3) TIMEOUT / SLOW / FLAKY ---
    # Playwright timeouts
    if "timeout" in msg_l or "timed out" in msg_l or "timeouterror" in msg_l:
        # If network errors exist => more likely flaky/network
        if network_errors:
            return _out(
                category="NETWORK",
                confidence=0.8,
                reason="Timeout likely caused by network errors.",
                retriable=True,
                action=action,
                selector=selector,
                url=url,
                http_status=http_status,
                step_name=step_name,
                evidence={"network_errors": _short_list(network_errors, 10)},
            )

        # If action is goto / navigation => navigation/perf
        if action in ("goto", "navigate") or "navigation" in msg_l:
            return _out(
                category="NAVIGATION",
                confidence=0.75,
                reason="Navigation timeout (page load or redirects).",
                retriable=True,
                action=action,
                selector=selector,
                url=url,
                http_status=http_status,
                step_name=step_name,
            )

        # Otherwise flaky/perf
        return _out(
            category="FLAKY",
            confidence=0.7,
            reason="Generic timeout (could be slow UI, race, or intermittent).",
            retriable=True,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
        )

    # --- 4) ASSERTION / BUSINESS VALIDATION ---
    if _looks_like_assertion(msg_l):
        return _out(
            category="ASSERTION_FAILED",
            confidence=0.9,
            reason="Business/QA assertion failed (expected text/state not met).",
            retriable=False,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
        )

    # --- 5) JS / CONSOLE ERRORS ---
    if console_errors and any(_safe_str(x).strip() for x in console_errors):
        return _out(
            category="CLIENT_JS",
            confidence=0.7,
            reason="Console/JS errors detected during run.",
            retriable=True,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
            evidence={"console_errors": _short_list(console_errors, 10)},
        )

    # --- 6) SERVER 5XX ---
    if http_status and 500 <= http_status <= 599:
        return _out(
            category="SERVER",
            confidence=0.85,
            reason="Server error (5xx) detected.",
            retriable=True,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
        )

    # --- 7) CONFIG / ENV ---
    if any(k in msg_l for k in ["env", "missing", "keyerror", "no such file", "not defined"]) and (
        "supabase" in msg_l or "openai" in msg_l or "heb_" in msg_l or "api key" in msg_l
    ):
        return _out(
            category="ENV_CONFIG",
            confidence=0.8,
            reason="Environment/configuration error (missing env var / key / config).",
            retriable=False,
            action=action,
            selector=selector,
            url=url,
            http_status=http_status,
            step_name=step_name,
        )

    # --- 8) FALLBACK UNKNOWN ---
    return _out(
        category="UNKNOWN",
        confidence=0.45,
        reason="Could not classify deterministically. Inspect logs/evidence.",
        retriable=True,
        action=action,
        selector=selector,
        url=url,
        http_status=http_status,
        step_name=step_name,
        evidence={"message": msg[:800]},
    )


# =========================
# Helpers
# =========================

def _out(
    category: str,
    confidence: float,
    reason: str,
    retriable: bool,
    action: str,
    selector: str,
    url: str,
    http_status: Optional[int],
    step_name: str,
    evidence: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    return {
        "category": category,               # stable enum
        "confidence": round(float(confidence), 3),
        "retriable": bool(retriable),
        "reason": _safe_str(reason)[:600],
        "signals": {
            "action": action or None,
            "selector": selector or None,
            "url": url or None,
            "http_status": http_status,
            "step_name": step_name or None,
        },
        "evidence": evidence or None,
        "classifier_version": "1.0.0",
    }


def _safe_str(v: Any) -> str:
    try:
        if v is None:
            return ""
        return str(v)
    except Exception:
        return ""


def _maybe_int(v: Any) -> Optional[int]:
    try:
        if v is None or v == "":
            return None
        return int(v)
    except Exception:
        return None


def _looks_like_locator_missing(msg_l: str) -> bool:
    patterns = [
        "waiting for locator",
        "strict mode violation",
        "locator resolved to",
        "no node found",
        "element is not attached",
        "element is not visible",
        "target closed",
        "has-text",
        "cannot find",
        "not found",
    ]
    # We only treat as locator missing if it looks like element/selector context
    return ("locator" in msg_l and any(p in msg_l for p in patterns)) or (
        "waiting for selector" in msg_l
    )


def _looks_like_assertion(msg_l: str) -> bool:
    return any(k in msg_l for k in ["assert", "expected", "but received", "texto no encontrado", "falló", "failed assertion"])


def _short_list(items: Any, max_n: int) -> Any:
    if not isinstance(items, list):
        return items
    out = []
    for x in items[:max_n]:
        s = _safe_str(x).strip()
        if s:
            out.append(s[:300])
    return out


def _redact_pii(text: str) -> str:
    if not text:
        return ""
    # emails
    text = re.sub(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}", "[REDACTED_EMAIL]", text)
    # phone-like
    text = re.sub(r"\b(\+?\d[\d\s().-]{7,}\d)\b", "[REDACTED_PHONE]", text)
    return text
