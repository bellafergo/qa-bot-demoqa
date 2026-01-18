from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Any, Dict, Optional

# Local fallback (dev)
BASE = Path("evidence/domain_memory")

# Supabase toggle
USE_SUPABASE_MEMORY = os.getenv("USE_SUPABASE_MEMORY", "1").strip() != "0"


def _has_supabase() -> bool:
    if not USE_SUPABASE_MEMORY:
        return False
    return bool(os.getenv("SUPABASE_URL") and os.getenv("SUPABASE_SERVICE_ROLE_KEY"))


def _sb():
    # Lazy import to avoid hard dependency failures locally
    from services.supabase_store import supabase_client
    return supabase_client()


def load_memory(domain: str) -> Dict[str, Any]:
    domain = (domain or "").strip().lower()
    if not domain:
        return {}

    # Prefer Supabase in prod
    if _has_supabase():
        try:
            sb = _sb()
            res = sb.table("domain_memory").select("data").eq("domain", domain).limit(1).execute()
            if getattr(res, "data", None):
                row = res.data[0]
                data = row.get("data") or {}
                return data if isinstance(data, dict) else {}
            return {}
        except Exception:
            # fall back local
            return _load_memory_local(domain)

    return _load_memory_local(domain)


def save_memory(domain: str, patch: Dict[str, Any]) -> None:
    domain = (domain or "").strip().lower()
    if not domain or not isinstance(patch, dict) or not patch:
        return

    # Prefer Supabase in prod
    if _has_supabase():
        try:
            sb = _sb()

            current = load_memory(domain)
            merged = _deep_merge(current, patch)

            sb.table("domain_memory").upsert(
                {"domain": domain, "data": merged},
                on_conflict="domain",
            ).execute()
            return
        except Exception:
            # fall back local
            _save_memory_local(domain, patch)
            return

    _save_memory_local(domain, patch)


# -------------------------
# Local fallback helpers
# -------------------------

def _load_memory_local(domain: str) -> Dict[str, Any]:
    path = BASE / f"{domain}.json"
    if path.exists():
        try:
            return json.loads(path.read_text()) or {}
        except Exception:
            return {}
    return {}


def _save_memory_local(domain: str, patch: Dict[str, Any]) -> None:
    BASE.mkdir(parents=True, exist_ok=True)
    path = BASE / f"{domain}.json"
    data = _load_memory_local(domain)
    merged = _deep_merge(data, patch)
    path.write_text(json.dumps(merged, indent=2, ensure_ascii=False))


def _deep_merge(a: Dict[str, Any], b: Dict[str, Any]) -> Dict[str, Any]:
    """Merge dict b into a (recursive). Returns new dict."""
    out: Dict[str, Any] = dict(a or {})
    for k, v in (b or {}).items():
        if isinstance(v, dict) and isinstance(out.get(k), dict):
            out[k] = _deep_merge(out[k], v)
        else:
            out[k] = v
    return out
