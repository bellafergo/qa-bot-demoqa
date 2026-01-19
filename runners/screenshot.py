# runners/screenshot.py
from __future__ import annotations

import base64
from typing import List, Optional, Tuple


def _b64_png(img_bytes: bytes) -> str:
    """Encode bytes to base64 string (kept name for compat)."""
    return base64.b64encode(img_bytes).decode("utf-8")


def take_screenshot_robust(page) -> Tuple[Optional[str], List[str]]:
    """
    Screenshot optimizado en JPG con timeouts/reintentos propios.
    No depende del timeout global del contexto.
    """
    logs: List[str] = []
    b64: Optional[str] = None

    NAV_STABILIZE_MS = 800
    SHOT_TIMEOUT_MS = 8000
    RETRIES = 2

    try:
        page.wait_for_timeout(NAV_STABILIZE_MS)
    except Exception:
        pass

    try:
        page.wait_for_load_state("domcontentloaded", timeout=3000)
    except Exception:
        pass

    last_err: Optional[Exception] = None
    for attempt in range(RETRIES + 1):
        try:
            jpg = page.screenshot(
                type="jpeg",
                quality=60,
                full_page=False,
                timeout=SHOT_TIMEOUT_MS,
            )
            b64 = _b64_png(jpg)
            logs.append(f"Screenshot JPG OK [attempt {attempt + 1}]")
            return b64, logs
        except Exception as e:
            last_err = e
            logs.append(f"Screenshot JPG failed [attempt {attempt + 1}]: {type(e).__name__}: {e}")
            try:
                page.wait_for_timeout(500)
            except Exception:
                pass

    logs.append(
        f"Screenshot final failed: {type(last_err).__name__}: {last_err}"
        if last_err
        else "Screenshot final failed"
    )
    return None, logs
