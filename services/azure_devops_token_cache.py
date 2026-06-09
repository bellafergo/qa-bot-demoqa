# services/azure_devops_token_cache.py
"""In-memory access-token cache for Azure DevOps OAuth (refresh token persisted in project settings)."""
from __future__ import annotations

import threading
import time
from dataclasses import dataclass
from typing import Dict, Optional


@dataclass
class _CachedToken:
    access_token: str
    expires_at: float


class AzureDevOpsTokenCache:
    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._tokens: Dict[str, _CachedToken] = {}

    def get(self, project_id: str) -> Optional[str]:
        pid = (project_id or "").strip().lower()
        with self._lock:
            entry = self._tokens.get(pid)
            if not entry:
                return None
            if entry.expires_at <= time.time() + 30:
                self._tokens.pop(pid, None)
                return None
            return entry.access_token

    def set(self, project_id: str, access_token: str, *, expires_in: int) -> None:
        pid = (project_id or "").strip().lower()
        ttl = max(60, int(expires_in or 3600))
        with self._lock:
            self._tokens[pid] = _CachedToken(
                access_token=str(access_token or ""),
                expires_at=time.time() + ttl,
            )

    def invalidate(self, project_id: str) -> None:
        pid = (project_id or "").strip().lower()
        with self._lock:
            self._tokens.pop(pid, None)


azure_devops_token_cache = AzureDevOpsTokenCache()
