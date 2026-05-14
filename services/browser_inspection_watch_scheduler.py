# services/browser_inspection_watch_scheduler.py
"""
Single-process background ticker for browser inspection watches (Phase 3C MVP).

Not distributed-safe across multiple workers — see TODO for Redis/cron later.
"""
from __future__ import annotations

import logging
import threading
from typing import Optional

logger = logging.getLogger("vanya.browser_inspection_watch_scheduler")

_started = False
_worker: Optional[threading.Thread] = None
_stop = threading.Event()
_tick_lock = threading.Lock()


def _tick_loop() -> None:
    from services.browser_watch_env import browser_watch_max_per_tick, browser_watch_tick_seconds

    sleep_s = browser_watch_tick_seconds()
    max_per_tick = browser_watch_max_per_tick()
    while not _stop.wait(sleep_s):
        if not _tick_lock.acquire(blocking=False):
            logger.debug("browser_inspection_watch_scheduler: skip overlapping tick")
            continue
        try:
            from services.browser_inspection_watch_service import tick_due_watches

            n = tick_due_watches(max_per_tick=max_per_tick)
            if n:
                logger.info("browser_inspection_watch_scheduler: ran %s watch tick(s)", n)
        except Exception:
            logger.exception("browser_inspection_watch_scheduler: tick batch failed")
        finally:
            _tick_lock.release()


def ensure_watch_scheduler_started() -> None:
    """Idempotent — starts a daemon thread when enabled via env (default on)."""
    global _started, _worker
    from services.browser_watch_env import browser_watch_scheduler_enabled

    if not browser_watch_scheduler_enabled():
        logger.info(
            "browser_inspection_watch_scheduler: disabled via "
            "VANYA_BROWSER_WATCH_SCHEDULER (or legacy VANYA_BIO_WATCH_SCHEDULER)"
        )
        return
    if _started:
        return
    _started = True
    _worker = threading.Thread(target=_tick_loop, name="vanya-bio-watch-scheduler", daemon=True)
    _worker.start()
    logger.info("browser_inspection_watch_scheduler: daemon thread started")
