# runners/api_evidence.py
"""
Structured, redacted HTTP evidence for API test runs (persisted on step dicts under ``evidence``).

Used by ``runners.api_runner`` only — keep sanitization logic in one place.
"""
from __future__ import annotations

import json
import re
import traceback
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

MAX_BODY_CHARS = 24_000
MAX_QUERY_VALUE_LEN = 2_048
MAX_HEADER_VALUE_LEN = 1_024
TRACE_MAX_CHARS = 1_200

_SENSITIVE_HEADER_NAMES = frozenset(
    {
        "authorization",
        "cookie",
        "set-cookie",
        "x-api-key",
        "api-key",
        "x-auth-token",
        "x-access-token",
        "proxy-authorization",
        "x-csrf-token",
    }
)

_SENSITIVE_QUERY_KEYS = re.compile(
    r"(?i)(^|_)(token|secret|password|apikey|api_key|auth|bearer|credential|access|refresh)($|_)"
)

_BEARER_RE = re.compile(r"(?i)(Bearer\s+)([\w\-\.~+/=]+)", re.MULTILINE)
_BASIC_RE = re.compile(r"(?i)(Basic\s+)([A-Za-z0-9+/=]+)")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def iso_timestamp_now() -> str:
    """Public alias for callers building ``request.timestamp``."""
    return _utc_iso()


def redact_plain_text(s: str) -> str:
    if not s:
        return s
    t = _BEARER_RE.sub(r"\1[REDACTED]", s)
    t = _BASIC_RE.sub(r"\1[REDACTED]", t)
    if len(t) > MAX_BODY_CHARS + 500:
        t = t[: MAX_BODY_CHARS + 500] + "…"
    return t


def sanitize_headers(headers: Any) -> Dict[str, str]:
    out: Dict[str, str] = {}
    if not isinstance(headers, dict):
        return out
    for k, v in headers.items():
        name = str(k)
        lk = name.lower()
        if lk in _SENSITIVE_HEADER_NAMES:
            out[name] = "[REDACTED]"
            continue
        vs = v if isinstance(v, str) else str(v)
        if len(vs) > MAX_HEADER_VALUE_LEN:
            vs = vs[:MAX_HEADER_VALUE_LEN] + "…"
        out[name] = redact_plain_text(vs)
    return out


def sanitize_query_params(params: Any) -> Any:
    if params is None:
        return None
    if isinstance(params, dict):
        out: Dict[str, Any] = {}
        for k, v in params.items():
            key = str(k)
            if _SENSITIVE_QUERY_KEYS.search(key):
                out[key] = "[REDACTED]"
            elif isinstance(v, str):
                sv = v if len(v) <= MAX_QUERY_VALUE_LEN else v[:MAX_QUERY_VALUE_LEN] + "…"
                out[key] = redact_plain_text(sv)
            elif isinstance(v, list):
                out[key] = [sanitize_query_params(x) if isinstance(x, dict) else redact_plain_text(str(x))[:512] for x in v[:50]]
            else:
                out[key] = v
        return out
    return params


def serialize_body_for_evidence(body: Any) -> Tuple[Optional[str], str]:
    """
    Return (text, content_kind) where content_kind is json|text|empty.
    """
    if body is None:
        return None, "empty"
    if isinstance(body, (dict, list)):
        try:
            return json.dumps(body, ensure_ascii=False, default=str), "json"
        except Exception:
            return str(body), "text"
    if isinstance(body, (bytes, bytearray)):
        try:
            s = bytes(body).decode("utf-8", errors="replace")
        except Exception:
            return None, "empty"
        return s, "text"
    s = str(body)
    return s, "text"


def truncate_text(text: str) -> Tuple[str, bool, int]:
    raw_len = len(text)
    if raw_len <= MAX_BODY_CHARS:
        return text, False, raw_len
    return text[:MAX_BODY_CHARS] + "\n… [truncated]", True, raw_len


def snapshot_request_evidence(
    *,
    method: str,
    url: str,
    headers: Dict[str, Any],
    params: Any,
    body: Any,
    started_at: str,
    duration_ms: Optional[int] = None,
) -> Dict[str, Any]:
    h = sanitize_headers(headers)
    pq = sanitize_query_params(params)
    body_text, kind = serialize_body_for_evidence(body)
    truncated = False
    original_size: Optional[int] = None
    if body_text is not None:
        body_text = redact_plain_text(body_text)
        body_text, truncated, original_size = truncate_text(body_text)
    ev: Dict[str, Any] = {
        "method": (method or "").upper(),
        "url": url,
        "headers": h,
        "query": pq,
        "timestamp": started_at,
    }
    if duration_ms is not None:
        ev["duration_ms"] = int(duration_ms)
    if body_text is not None:
        ev["body"] = body_text
        ev["body_kind"] = kind
    if truncated:
        ev["truncated"] = True
        ev["original_size"] = original_size
    return ev


def snapshot_response_evidence(
    resp: Any,
    *,
    duration_ms: int,
) -> Dict[str, Any]:
    try:
        text = resp.text
    except Exception:
        text = ""
    text = redact_plain_text(text)
    text, truncated, original_size = truncate_text(text)
    parsed: Any = None
    try:
        parsed = resp.json()
    except Exception:
        parsed = None
    body_field: Any = text
    if parsed is not None and isinstance(parsed, (dict, list)):
        try:
            pj = json.dumps(parsed, ensure_ascii=False, default=str)
            pj, t2, osz = truncate_text(pj)
            body_field = json.loads(pj) if not t2 else pj
            truncated = truncated or t2
            if t2:
                original_size = max(original_size or 0, osz)
        except Exception:
            body_field = text
    ev: Dict[str, Any] = {
        "status_code": int(resp.status_code),
        "headers": sanitize_headers(dict(resp.headers)),
        "duration_ms": int(duration_ms),
        "body": body_field,
    }
    if truncated:
        ev["truncated"] = True
        ev["original_size"] = original_size
    return ev


def pair_from_httpx_response(
    resp: Any,
    *,
    response_duration_ms: int,
    request_started_at: Optional[str] = None,
) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    """Build (request_evidence, response_evidence) from a completed response."""
    from urllib.parse import parse_qs, urlparse

    req = resp.request
    started = request_started_at or _utc_iso()
    body = None
    try:
        c = req.content
        if c:
            body = c.decode("utf-8", errors="replace")
    except Exception:
        body = None
    params = None
    try:
        q = parse_qs(urlparse(str(req.url)).query)
        if q:
            flat = {k: (v[0] if len(v) == 1 else v) for k, v in q.items()}
            params = sanitize_query_params(flat)
    except Exception:
        params = None
    req_ev = snapshot_request_evidence(
        method=str(req.method),
        url=str(req.url),
        headers=dict(req.headers),
        params=params,
        body=body,
        started_at=started,
        duration_ms=None,
    )
    resp_ev = snapshot_response_evidence(resp, duration_ms=response_duration_ms)
    return req_ev, resp_ev


def classify_failure(
    exc: BaseException,
    *,
    action: str,
    expected: Any = None,
    actual: Any = None,
) -> Dict[str, Any]:
    """Single ``failure`` object for step ``evidence``."""
    msg = str(exc).strip() or type(exc).__name__
    tb = "".join(traceback.format_exception_only(type(exc), exc)).strip()
    if len(tb) > TRACE_MAX_CHARS:
        tb = tb[:TRACE_MAX_CHARS] + "…"

    exc_mod = type(exc).__module__ or ""
    exc_name = type(exc).__name__
    httpx_timeout = exc_mod.startswith("httpx") and exc_name.endswith("Timeout")
    httpx_connect = exc_mod.startswith("httpx") and exc_name in (
        "ConnectError",
        "NetworkError",
        "RemoteProtocolError",
        "ReadError",
        "WriteError",
    )

    if isinstance(exc, AssertionError):
        ftype = "assertion_failed"
    elif isinstance(exc, TimeoutError) or httpx_timeout:
        ftype = "timeout"
    elif httpx_connect:
        ftype = "connection_error"
    elif exc_mod.startswith("httpx") and exc_name == "HTTPStatusError":
        ftype = "http_error"
    elif isinstance(exc, (json.JSONDecodeError, ValueError)) and "assert_json" in action:
        ftype = "schema_validation_error"
    elif "JSON" in exc_name or "json" in msg.lower():
        ftype = "schema_validation_error"
    else:
        ftype = "error"

    out: Dict[str, Any] = {
        "type": ftype,
        "message": msg,
        "traceback_short": tb,
    }
    if expected is not None:
        out["expected"] = expected
    if actual is not None:
        out["actual"] = actual
    return out


def build_step_evidence(
    *,
    request: Optional[Dict[str, Any]] = None,
    response: Optional[Dict[str, Any]] = None,
    failure: Dict[str, Any],
) -> Dict[str, Any]:
    ev: Dict[str, Any] = {"failure": failure}
    if request is not None:
        ev["request"] = request
    if response is not None:
        ev["response"] = response
    return ev
