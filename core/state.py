# core/state.py
import threading
import time
from typing import Any, Dict, List, Optional

from core.settings import settings

# ============================================================
# Global lock — shared by all in-memory state containers.
# RLock (reentrant) so cleanup_sessions can acquire it while
# _LockedDict methods also acquire it from the same thread.
# ============================================================
_STATE_LOCK = threading.RLock()


class _LockedList(list):
    """
    list subclass that acquires _STATE_LOCK on every mutation.

    Used for LRU-order lists (DOC_CACHE_ORDER, RUNS_ORDER) that are
    appended/removed/popped from concurrent request handlers.

    Note: compound operations like "check membership then append" are still
    not fully atomic — callers that need that guarantee should hold
    _STATE_LOCK explicitly around the whole sequence.
    """

    def append(self, item: Any) -> None:
        with _STATE_LOCK:
            super().append(item)

    def remove(self, item: Any) -> None:
        with _STATE_LOCK:
            super().remove(item)

    def pop(self, index: int = -1) -> Any:  # type: ignore[override]
        with _STATE_LOCK:
            return super().pop(index)

    def insert(self, index: int, item: Any) -> None:
        with _STATE_LOCK:
            super().insert(index, item)

    def clear(self) -> None:
        with _STATE_LOCK:
            super().clear()


class _LockedDict(dict):
    """
    dict subclass that acquires _STATE_LOCK on every mutation.

    Python's GIL makes individual dict reads atomic at the C level,
    so we only lock write paths.  For multi-step read-then-write
    sequences callers should hold _STATE_LOCK explicitly:

        with _STATE_LOCK:
            if key not in SESSIONS:
                SESSIONS[key] = new_value
    """

    def __setitem__(self, key: Any, value: Any) -> None:
        with _STATE_LOCK:
            super().__setitem__(key, value)

    def __delitem__(self, key: Any) -> None:
        with _STATE_LOCK:
            super().__delitem__(key)

    def pop(self, *args: Any) -> Any:  # type: ignore[override]
        with _STATE_LOCK:
            return super().pop(*args)

    def update(self, *args: Any, **kwargs: Any) -> None:  # type: ignore[override]
        with _STATE_LOCK:
            super().update(*args, **kwargs)

    def clear(self) -> None:
        with _STATE_LOCK:
            super().clear()


# ============================================================
# In-memory state
# ============================================================

# Session memory (solo para continuidad rápida: last_url, last_seen, etc.)
SESSIONS: Dict[str, Dict[str, Any]] = _LockedDict()

# Doc cache (LRU)
DOC_CACHE: Dict[str, Dict[str, Any]] = _LockedDict()
DOC_CACHE_ORDER: List[str] = _LockedList()  # mutated by concurrent request handlers

# Runs cache (para reportes automáticos /runs si no hay tabla runs)
RUNS: Dict[str, Dict[str, Any]] = _LockedDict()
RUNS_ORDER: List[str] = _LockedList()  # not imported externally; _LockedList for consistency


# ============================================================
# Time helpers
# ============================================================
def now_ts() -> int:
    return int(time.time())


# ============================================================
# Session cleanup
# ============================================================
def cleanup_sessions(ttl_s: Optional[int] = None, max_sessions: Optional[int] = None) -> int:
    """
    Limpia sesiones viejas (TTL) y opcionalmente limita la cantidad de sesiones vivas.

    Returns: número de sesiones eliminadas.
    Holds _STATE_LOCK for the entire operation to avoid races during cleanup.
    """
    ttl = int(ttl_s if ttl_s is not None else getattr(settings, "SESSION_TTL_S", 3600))
    t = now_ts()

    with _STATE_LOCK:
        # 1) TTL cleanup — list() snapshot so we can mutate while iterating
        dead: List[str] = []
        for sid, s in list(SESSIONS.items()):
            last_seen = int(s.get("last_seen", t))
            if t - last_seen > ttl:
                dead.append(sid)

        for sid in dead:
            # Use dict.pop directly: _STATE_LOCK is reentrant, so this is safe.
            SESSIONS.pop(sid, None)

        removed = len(dead)

        # 2) Hard cap (por si algún bug crea demasiadas sesiones)
        cap = max_sessions
        if cap is None:
            cap = getattr(settings, "MAX_SESSIONS_IN_MEMORY", None)

        if isinstance(cap, int) and cap > 0 and len(SESSIONS) > cap:
            # elimina las más viejas por last_seen
            items = sorted(SESSIONS.items(), key=lambda kv: int(kv[1].get("last_seen", 0)))
            overflow = len(SESSIONS) - cap
            for i in range(overflow):
                sid = items[i][0]
                SESSIONS.pop(sid, None)
            removed += overflow

    return removed


__all__ = [
    "SESSIONS",
    "DOC_CACHE",
    "DOC_CACHE_ORDER",
    "RUNS",
    "RUNS_ORDER",
    "_STATE_LOCK",
    "_LockedDict",
    "_LockedList",
    "now_ts",
    "cleanup_sessions",
]
