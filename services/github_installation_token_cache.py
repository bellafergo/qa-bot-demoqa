# services/github_installation_token_cache.py
"""
In-memory cache for GitHub App installation access tokens.

Tokens are short-lived (~1h). Never logged. Not persisted to disk.
"""
from __future__ import annotations

import threading
import time
from dataclasses import dataclass
from typing import Dict, Optional


@dataclass
class _CacheEntry:
    token: str
    expires_at: float  # unix epoch


class InstallationTokenCache:
    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._entries: Dict[str, _CacheEntry] = {}

    def get(self, installation_id: str) -> Optional[str]:
        key = str(installation_id or "").strip()
        if not key:
            return None
        now = time.time()
        with self._lock:
            entry = self._entries.get(key)
            if not entry:
                return None
            # Refresh 5 minutes before expiry
            if entry.expires_at - 300 <= now:
                self._entries.pop(key, None)
                return None
            return entry.token

    def set(self, installation_id: str, token: str, expires_at_iso: str) -> None:
        key = str(installation_id or "").strip()
        tok = (token or "").strip()
        if not key or not tok:
            return
        expires = _parse_expires(expires_at_iso)
        with self._lock:
            self._entries[key] = _CacheEntry(token=tok, expires_at=expires)

    def invalidate(self, installation_id: str) -> None:
        key = str(installation_id or "").strip()
        with self._lock:
            self._entries.pop(key, None)


def _parse_expires(iso: str) -> float:
    if not iso:
        return time.time() + 3600
    try:
        from datetime import datetime, timezone
        ts = iso.replace("Z", "+00:00")
        dt = datetime.fromisoformat(ts)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return dt.timestamp()
    except Exception:
        return time.time() + 3600


installation_token_cache = InstallationTokenCache()
