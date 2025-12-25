# core/state.py
import time
from typing import Any, Dict, List, Optional

from core.settings import settings

# ============================================================
# In-memory state
# ============================================================

# Session memory (solo para continuidad rápida: last_url, last_seen, etc.)
SESSIONS: Dict[str, Dict[str, Any]] = {}

# Doc cache (LRU)
DOC_CACHE: Dict[str, Dict[str, Any]] = {}
DOC_CACHE_ORDER: List[str] = []

# Runs cache (para reportes automáticos /runs si no hay tabla runs)
RUNS: Dict[str, Dict[str, Any]] = {}
RUNS_ORDER: List[str] = []


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

    Returns: número de sesiones eliminadas
    """
    ttl = int(ttl_s if ttl_s is not None else getattr(settings, "SESSION_TTL_S", 3600))
    t = now_ts()

    # 1) TTL cleanup
    dead: List[str] = []
    for sid, s in list(SESSIONS.items()):
        last_seen = int(s.get("last_seen", t))
        if t - last_seen > ttl:
            dead.append(sid)

    for sid in dead:
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
    "now_ts",
    "cleanup_sessions",
]