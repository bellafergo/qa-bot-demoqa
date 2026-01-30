# services/supabase_store.py
from __future__ import annotations

from typing import Any, Dict, List, Optional, Callable, TypeVar
import os
import logging
from datetime import datetime, timezone

logger = logging.getLogger("vanya.supabase")

# ============================================================
# Env
# ============================================================
SUPABASE_URL = (os.getenv("SUPABASE_URL") or "").strip()

# IMPORTANT:
# - SUPABASE_SERVICE_ROLE_KEY debe ser la "Secret key" (service_role) de Supabase (server-side).
# - NO uses la "Publishable key" aquí.
SUPABASE_SERVICE_ROLE_KEY = (os.getenv("SUPABASE_SERVICE_ROLE_KEY") or "").strip()

# Modo estricto: si está en 1, el backend debe usar Supabase sí o sí (sin fallback a SQLite).
SUPABASE_STRICT = (os.getenv("SUPABASE_STRICT", "0").strip() == "1")

# Optional: ayuda a diagnosticar en logs (sin exponer secretos)
SUPABASE_DEBUG = (os.getenv("SUPABASE_DEBUG", "0").strip() == "1")

# Singleton client
_supabase = None


# ============================================================
# Helpers
# ============================================================
def _utcnow_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _is_configured() -> bool:
    return bool(SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY)


def _mask(s: str, keep: int = 4) -> str:
    s = s or ""
    if len(s) <= keep:
        return "*" * len(s)
    return ("*" * (len(s) - keep)) + s[-keep:]


def _log_supabase_config() -> None:
    """Log status at import time (NO secrets)."""
    has_url = bool(SUPABASE_URL)
    has_key = bool(SUPABASE_SERVICE_ROLE_KEY)

    if SUPABASE_DEBUG:
        logger.info(
            "Supabase env: URL=%s KEY=%s STRICT=%s",
            "SET" if has_url else "MISSING",
            f"SET({_mask(SUPABASE_SERVICE_ROLE_KEY)})" if has_key else "MISSING",
            "1" if SUPABASE_STRICT else "0",
        )
    else:
        logger.info(
            "Supabase env: URL=%s KEY=%s STRICT=%s",
            "SET" if has_url else "MISSING",
            "SET" if has_key else "MISSING",
            "1" if SUPABASE_STRICT else "0",
        )

    if SUPABASE_STRICT and (not has_url or not has_key):
        logger.error(
            "FATAL: SUPABASE_STRICT=1 but Supabase credentials are missing. "
            "Set SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY (Secret key) in Render."
        )


_log_supabase_config()


T = TypeVar("T")


def _safe_execute(fn: Callable[[], T], default: T) -> T:
    try:
        return fn()
    except Exception as e:
        # Mejor diagnóstico en logs sin romper flujo
        logger.exception("Supabase operation failed: %s", e)
        return default


def _clip(s: str, n: int) -> str:
    s = (s or "").strip()
    return s if len(s) <= n else s[:n].rstrip() + "…"


def _extract_error(e: Exception) -> str:
    msg = str(e) or e.__class__.__name__
    return _clip(msg, 300)


# ============================================================
# Client
# ============================================================
def supabase_client():
    """
    Retorna el cliente Supabase (singleton) o None.
    Requiere SUPABASE_URL + SUPABASE_SERVICE_ROLE_KEY (Secret key).
    """
    global _supabase
    if _supabase is not None:
        return _supabase

    if not _is_configured():
        logger.warning("Supabase not configured (missing SUPABASE_URL or SUPABASE_SERVICE_ROLE_KEY).")
        return None

    try:
        from supabase import create_client  # type: ignore
        _supabase = create_client(SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY)
        return _supabase
    except Exception as e:
        logger.exception("Failed to create Supabase client: %s", _extract_error(e))
        return None


def check_supabase_health() -> Dict[str, Any]:
    """
    Prueba rápida para saber si:
      - está configurado
      - puede crear cliente
      - puede hacer una query mínima

    Returns:
      {"ok": bool, "configured": bool, "client": bool, "reachable": bool, "error": str|None}
    """
    configured = _is_configured()
    if not configured:
        return {
            "ok": False,
            "configured": False,
            "client": False,
            "reachable": False,
            "error": "Missing SUPABASE_URL or SUPABASE_SERVICE_ROLE_KEY",
        }

    sb = supabase_client()
    if sb is None:
        return {
            "ok": False,
            "configured": True,
            "client": False,
            "reachable": False,
            "error": "Could not create Supabase client (check URL/key and supabase package).",
        }

    try:
        # Query mínima: si falla aquí, casi siempre es KEY incorrecta o RLS/policies
        sb.table("threads").select("id").limit(1).execute()
        return {
            "ok": True,
            "configured": True,
            "client": True,
            "reachable": True,
            "error": None,
        }
    except Exception as e:
        return {
            "ok": False,
            "configured": True,
            "client": True,
            "reachable": False,
            "error": _extract_error(e),
        }


# ============================================================
# THREADS
# ============================================================
def sb_create_thread(title: str = "New chat") -> Optional[Dict[str, Any]]:
    sb = supabase_client()
    if not sb:
        return None

    payload = {
        "title": (title or "New chat").strip() or "New chat",
        "updated_at": _utcnow_iso(),
    }

    def _op():
        res = sb.table("threads").insert(payload).execute()
        data = getattr(res, "data", None) or []
        if data:
            row = data[0] or {}
            return {
                "id": row.get("id"),
                "title": row.get("title"),
                "updated_at": row.get("updated_at"),
            }
        return None

    return _safe_execute(_op, None)


def sb_get_thread(thread_id: str) -> Optional[Dict[str, Any]]:
    sb = supabase_client()
    if not sb or not thread_id:
        return None

    def _op():
        res = (
            sb.table("threads")
            .select("id,title,updated_at,created_at")
            .eq("id", thread_id)
            .limit(1)
            .execute()
        )
        data = getattr(res, "data", None) or []
        if data:
            row = data[0] or {}
            return {
                "id": row.get("id"),
                "title": row.get("title") or "New chat",
                "updated_at": row.get("updated_at") or row.get("created_at"),
            }
        return None

    return _safe_execute(_op, None)


def sb_touch_thread(thread_id: str) -> None:
    sb = supabase_client()
    if not sb or not thread_id:
        return

    def _op():
        sb.table("threads").update({"updated_at": _utcnow_iso()}).eq("id", thread_id).execute()
        return None

    _safe_execute(_op, None)


def sb_update_thread_title(thread_id: str, title: str) -> None:
    sb = supabase_client()
    if not sb or not thread_id:
        return

    clean = (title or "").strip()
    if not clean:
        return

    def _op():
        sb.table("threads").update({"title": clean, "updated_at": _utcnow_iso()}).eq("id", thread_id).execute()
        return None

    _safe_execute(_op, None)


def sb_delete_thread(thread_id: str) -> bool:
    sb = supabase_client()
    if not sb or not thread_id:
        return False

    def _op():
        # Orden: children primero
        sb.table("messages").delete().eq("thread_id", thread_id).execute()
        sb.table("runs").delete().eq("thread_id", thread_id).execute()
        sb.table("threads").delete().eq("id", thread_id).execute()
        return True

    return bool(_safe_execute(_op, False))


def sb_list_threads(limit: int = 80) -> List[Dict[str, Any]]:
    sb = supabase_client()
    if not sb:
        return []

    limit = max(1, min(int(limit or 80), 200))

    def _op():
        res = (
            sb.table("threads")
            .select("id,title,updated_at,created_at")
            .order("updated_at", desc=True)
            .limit(limit)
            .execute()
        )
        threads = list(getattr(res, "data", None) or [])
        if not threads:
            return []

        ids = [t.get("id") for t in threads if t.get("id")]
        if not ids:
            return []

        # Último mensaje por thread para preview
        res_m = (
            sb.table("messages")
            .select("thread_id,content,created_at")
            .in_("thread_id", ids)
            .order("created_at", desc=True)
            .limit(limit * 4)
            .execute()
        )
        messages = list(getattr(res_m, "data", None) or [])

        last_by_thread: Dict[str, Dict[str, Any]] = {}
        for m in messages:
            tid = m.get("thread_id")
            if tid and tid not in last_by_thread:
                last_by_thread[tid] = m

        out: List[Dict[str, Any]] = []
        for t in threads:
            tid = t.get("id")
            last_msg = last_by_thread.get(tid) if tid else None
            out.append(
                {
                    "id": tid,
                    "title": (t.get("title") or "").strip() or "New chat",
                    "updated_at": t.get("updated_at") or t.get("created_at"),
                    "preview": _clip(str(last_msg.get("content")) if last_msg else "", 60),
                }
            )
        return out

    return list(_safe_execute(_op, []))


# ============================================================
# MESSAGES
# ============================================================
def sb_add_message(
    thread_id: str,
    role: str,
    content: str,
    meta: Optional[Dict[str, Any]] = None,
) -> Optional[str]:
    sb = supabase_client()
    if not sb or not thread_id:
        return None

    payload = {
        "thread_id": thread_id,
        "role": (role or "assistant").strip() or "assistant",
        "content": content or "",
        "meta": meta or {},
    }

    def _op():
        res = sb.table("messages").insert(payload).execute()
        sb_touch_thread(thread_id)
        data = getattr(res, "data", None) or []
        if data:
            return (data[0] or {}).get("id")
        return None

    return _safe_execute(_op, None)


def sb_list_messages(thread_id: str, limit: int = 50) -> List[Dict[str, Any]]:
    sb = supabase_client()
    if not sb or not thread_id:
        return []

    limit = max(1, min(int(limit or 50), 200))

    def _op():
        res = (
            sb.table("messages")
            .select("id,thread_id,role,content,meta,created_at")
            .eq("thread_id", thread_id)
            .order("created_at", desc=False)
            .limit(limit)
            .execute()
        )
        return list(getattr(res, "data", None) or [])

    return list(_safe_execute(_op, []))


# Alias usado por store.py
sb_get_messages = sb_list_messages


# ============================================================
# RUNS
# ============================================================
def sb_add_run(thread_id: str, status: str, runner_meta: Dict[str, Any]) -> Optional[str]:
    sb = supabase_client()
    if not sb or not thread_id:
        return None

    payload = {
        "thread_id": thread_id,
        "status": (status or "unknown").strip() or "unknown",
        "runner": runner_meta or {},
    }

    def _op():
        res = sb.table("runs").insert(payload).execute()
        sb_touch_thread(thread_id)
        data = getattr(res, "data", None) or []
        if data:
            return (data[0] or {}).get("id")
        return None

    return _safe_execute(_op, None)
