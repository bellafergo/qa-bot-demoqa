# services/browser_watch_env.py
"""
Browser inspection watch — environment variables (Phase 3D).

New names (preferred):
  VANYA_BROWSER_WATCH_SCHEDULER, VANYA_BROWSER_WATCH_TICK_S, VANYA_BROWSER_WATCH_MAX_PER_TICK

Legacy (deprecated, still read as fallback):
  VANYA_BIO_WATCH_SCHEDULER, VANYA_BIO_WATCH_TICK_S, VANYA_BIO_WATCH_MAX_PER_TICK

TODO(Phase 3E+): remove VANYA_BIO_WATCH_* fallback once all deployments use VANYA_BROWSER_WATCH_*.
"""
from __future__ import annotations

import os
from typing import Optional

# Legacy prefix — do not use in new code; kept for backward compatibility only.
_LEGACY_SCHEDULER = "VANYA_BIO_WATCH_SCHEDULER"
_LEGACY_TICK = "VANYA_BIO_WATCH_TICK_S"
_LEGACY_MAX = "VANYA_BIO_WATCH_MAX_PER_TICK"

_NEW_SCHEDULER = "VANYA_BROWSER_WATCH_SCHEDULER"
_NEW_TICK = "VANYA_BROWSER_WATCH_TICK_S"
_NEW_MAX = "VANYA_BROWSER_WATCH_MAX_PER_TICK"


def getenv_str_priority(new_key: str, legacy_key: str) -> Optional[str]:
    """If ``new_key`` is set in the environment (even empty), use it; else ``legacy_key``."""
    if new_key in os.environ:
        return os.environ.get(new_key)
    if legacy_key in os.environ:
        return os.environ.get(legacy_key)
    return None


def browser_watch_scheduler_enabled() -> bool:
    raw = (getenv_str_priority(_NEW_SCHEDULER, _LEGACY_SCHEDULER) or "1").strip().lower()
    return raw not in ("0", "false", "no", "off")


def browser_watch_tick_seconds() -> int:
    raw = (getenv_str_priority(_NEW_TICK, _LEGACY_TICK) or "120").strip()
    try:
        return max(30, min(int(raw), 3600))
    except Exception:
        return 120


def browser_watch_max_per_tick() -> int:
    raw = (getenv_str_priority(_NEW_MAX, _LEGACY_MAX) or "5").strip()
    try:
        return max(1, min(int(raw), 25))
    except Exception:
        return 5
