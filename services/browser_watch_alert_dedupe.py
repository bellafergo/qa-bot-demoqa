# services/browser_watch_alert_dedupe.py
"""
In-process dedupe for browser watch Slack alerts (Phase 3D MVP).

NOT distributed-safe across workers — see TODO for Redis / shared store.

Env:
  VANYA_BROWSER_ALERT_DEDUPE_MINUTES — default 60 (min 1, max 1440).
"""
from __future__ import annotations

import hashlib
import logging
import os
import threading
import time
from typing import List, Optional, Tuple

logger = logging.getLogger("vanya.browser_watch_alert_dedupe")

_lock = threading.Lock()
_last_sent_mono: dict[str, float] = {}


def dedupe_window_seconds() -> float:
    raw = (os.getenv("VANYA_BROWSER_ALERT_DEDUPE_MINUTES") or "60").strip()
    try:
        minutes = max(1, min(int(raw), 24 * 60))
    except Exception:
        minutes = 60
    return float(minutes * 60)


def _fingerprint(
    *,
    watch_id: str,
    change_level: str,
    regression_signals: List[str],
    summary: str,
    visual_change_level: str,
    visual_similarity_score: Optional[float],
) -> str:
    sig = ",".join(sorted(str(x) for x in (regression_signals or [])))[:2000]
    sim = f"{visual_similarity_score:.4f}" if visual_similarity_score is not None else ""
    blob = "|".join(
        [
            (watch_id or "").strip(),
            (change_level or "").strip().lower(),
            (visual_change_level or "").strip().lower(),
            sim,
            sig,
            (summary or "")[:400],
        ]
    )
    return hashlib.sha256(blob.encode("utf-8", errors="replace")).hexdigest()


def _prune(now: float, window: float) -> None:
    """Drop entries older than 2× window to bound memory."""
    cutoff = now - max(window * 2.0, 60.0)
    dead = [k for k, t in _last_sent_mono.items() if t < cutoff]
    for k in dead:
        _last_sent_mono.pop(k, None)


def watch_alert_try_reserve_slot(
    *,
    watch_id: str,
    change_level: str,
    regression_signals: List[str],
    summary: str,
    visual_change_level: str,
    visual_similarity_score: Optional[float],
) -> Tuple[bool, str]:
    """
    If an equivalent alert was sent recently, return (False, reason).

    Otherwise reserve the dedupe slot (monotonic timestamp) and return (True, "").
    """
    window = dedupe_window_seconds()
    key = _fingerprint(
        watch_id=watch_id,
        change_level=change_level,
        regression_signals=regression_signals,
        summary=summary,
        visual_change_level=visual_change_level,
        visual_similarity_score=visual_similarity_score,
    )
    now = time.monotonic()
    with _lock:
        _prune(now, window)
        prev = _last_sent_mono.get(key)
        if prev is not None and (now - prev) < window:
            return False, "dedupe_equivalent_within_window"
        _last_sent_mono[key] = now
        return True, ""


def reset_watch_alert_dedupe_for_tests() -> None:
    """Test helper — clears in-memory dedupe state."""
    with _lock:
        _last_sent_mono.clear()
