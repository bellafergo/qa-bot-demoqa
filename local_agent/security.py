"""Token-safe logging helpers (never log full secrets)."""
from __future__ import annotations

import hashlib


def token_log_label(token: str) -> str:
    t = (token or "").strip()
    if not t:
        return "(empty)"
    fp = hashlib.sha256(t.encode("utf-8")).hexdigest()[:12]
    if len(t) <= 4:
        return f"sha256:{fp}…(redacted)"
    return f"sha256:{fp}…{t[-4:]}"


def redact_headers_for_log(headers: dict) -> dict:
    out = dict(headers)
    auth = out.get("Authorization") or out.get("authorization")
    if isinstance(auth, str) and auth.lower().startswith("bearer "):
        raw = auth.split(" ", 1)[1].strip()
        out["Authorization"] = f"Bearer <redacted:{token_log_label(raw)}>"
    return out
