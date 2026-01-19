# runners/common.py
from __future__ import annotations

import time
from typing import Any, Dict, Optional
from urllib.parse import urlparse


def _now_ms() -> int:
    return int(time.time() * 1000)


def _as_int(v: Any, default: int) -> int:
    try:
        return int(v)
    except Exception:
        return default


def _safe_str(v: Any) -> str:
    try:
        return "" if v is None else str(v)
    except Exception:
        return ""


def _normalize_action(step: Dict[str, Any]) -> str:
    a = (step.get("action") or step.get("type") or "").strip().lower()
    # compat aliases
    if a in ("wait_for_selector", "wait_for"):
        return "assert_visible"
    return a


def _pick_timeout_ms(step: Dict[str, Any], default_ms: int) -> int:
    return _as_int(step.get("timeout_ms"), default_ms)


def _selector_from_step(step: Dict[str, Any]) -> str:
    sel = step.get("selector") or step.get("target") or step.get("loc") or ""
    return _safe_str(sel).strip()


def _url_from_step(step: Dict[str, Any], base_url: Optional[str]) -> str:
    url = (step.get("url") or step.get("href") or "").strip()
    if url:
        return url

    path = (step.get("path") or "").strip()
    if path and base_url:
        if base_url.endswith("/") and path.startswith("/"):
            return base_url[:-1] + path
        if (not base_url.endswith("/")) and (not path.startswith("/")):
            return base_url + "/" + path
        return base_url + path

    return base_url or ""


def _norm_expected(v: Any) -> str:
    """
    expected: "pass" | "fail"
    Acepta variantes: true/false, passed/failed, ok/fail, etc.
    """
    s = _safe_str(v).strip().lower()
    if s in ("", "none", "null"):
        return "pass"

    if s in ("pass", "passed", "ok", "true", "success"):
        return "pass"
    if s in ("fail", "failed", "false", "error", "negative"):
        return "fail"

    return "pass"


def _final_status(expected: str, outcome: str, had_error: bool) -> str:
    """
    status final:
      - si hubo error técnico (Playwright/runner) => "error"
      - si expected == outcome => "passed"
      - si expected != outcome => "failed"
    """
    if had_error:
        return "error"
    return "passed" if expected == outcome else "failed"


def _domain_from_url(u: str) -> str:
    try:
        return urlparse(u).netloc or "unknown"
    except Exception:
        return "unknown"
