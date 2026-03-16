# services/queue.py
from __future__ import annotations

import logging
import os
import threading
from typing import Any, Callable

logger = logging.getLogger("vanya")

REDIS_URL = (os.getenv("REDIS_URL") or "").strip()
_ENV = (os.getenv("ENV") or os.getenv("ENVIRONMENT") or "dev").strip().lower()
_IS_PROD = _ENV in ("prod", "production")

# ── Redis-backed queue (production) ──────────────────────────────────────────

if REDIS_URL:
    from redis import Redis
    from rq import Queue as _RQQueue

    logger.info("queue: Redis mode — %s", REDIS_URL)

    def get_redis() -> Redis:
        return Redis.from_url(REDIS_URL)

    def get_queue(name: str = "vanya") -> _RQQueue:
        # 30 min per job
        return _RQQueue(name, connection=get_redis(), default_timeout=60 * 30)

# ── In-memory fallback queue (no Redis configured) ───────────────────────────

else:
    if _IS_PROD:
        raise RuntimeError(
            "REDIS_URL is required in production environments. "
            "LocalQueue is not suitable for multi-worker deployments. "
            "Set the REDIS_URL environment variable before starting the server."
        )

    logger.warning(
        "queue: REDIS_URL not set — using in-memory queue (jobs run in background threads). "
        "Not suitable for production multi-worker deployments."
    )

    class _LocalJob:
        """Minimal job handle returned by _LocalQueue.enqueue()."""
        def __init__(self, job_id: str) -> None:
            self.id = job_id

    class _LocalQueue:
        """
        Thin synchronous-in-thread queue that mirrors the rq.Queue.enqueue() contract.
        Runs each job in a daemon thread so the HTTP response returns immediately.
        """

        def __init__(self, name: str) -> None:
            self.name = name

        def enqueue(
            self,
            fn: Callable,
            *args: Any,
            job_id: str | None = None,
            **kwargs: Any,
        ) -> _LocalJob:
            jid = job_id or "local-job"

            def _run() -> None:
                try:
                    fn(*args, **kwargs)
                except Exception as exc:
                    logger.exception("local queue: job %s failed — %s", jid, exc)

            t = threading.Thread(target=_run, daemon=True, name=f"vanya-job-{jid}")
            t.start()
            logger.debug("local queue: enqueued job %s in thread %s", jid, t.name)
            return _LocalJob(jid)

    def get_queue(name: str = "vanya") -> _LocalQueue:  # type: ignore[misc]
        return _LocalQueue(name)
