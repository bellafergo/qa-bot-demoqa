# core/state.py
import time
from typing import Any, Dict, List

from core.settings import settings

# Session memory
SESSIONS: Dict[str, Dict[str, Any]] = {}

# Doc cache
DOC_CACHE: Dict[str, Dict[str, Any]] = {}
DOC_CACHE_ORDER: List[str] = []


def _now() -> int:
    return int(time.time())


def cleanup_sessions():
    """
    Limpia sesiones viejas (TTL).
    """
    t = _now()
    dead = []
    for sid, s in list(SESSIONS.items()):
        last_seen = int(s.get("last_seen", t))
        if t - last_seen > settings.SESSION_TTL_S:
            dead.append(sid)
    for sid in dead:
        SESSIONS.pop(sid, None)