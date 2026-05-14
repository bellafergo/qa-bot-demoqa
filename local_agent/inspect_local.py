"""Run browser inspection on the agent machine (Phase 4C)."""
from __future__ import annotations

from typing import Any, Dict, Tuple

from models.browser_inspection_models import BrowserInspectionResult

from runners.browser_inspector_runner import run_browser_inspection
from services.browser_inspector_service import normalize_raw_runner_output


def run_local_browser_inspection(
    *,
    url: str,
    timeout_ms: int,
    headless: bool,
) -> Tuple[BrowserInspectionResult, Dict[str, Any]]:
    """Single navigation + inventory; no SSRF cloud validation (caller validated ``url``)."""
    raw = run_browser_inspection(url=url, timeout_ms=int(timeout_ms), headless=bool(headless))
    result = normalize_raw_runner_output(
        request_url=url,
        raw=raw,
        validated_navigation_url=url,
        upload_hosted_screenshot=False,
    )
    return result, raw
