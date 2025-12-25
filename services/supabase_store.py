# services/supabase_store.py
from __future__ import annotations

from typing import Any, Dict, List, Optional
import os
import logging
from datetime import datetime, timezone

logger = logging.getLogger("vanya.supabase")

SUPABASE_URL = (os.getenv("SUPABASE_URL") or "").strip()
SUPABASE_SERVICE_ROLE_KEY = (os.getenv("SUPABASE_SERVICE_ROLE_KEY") or "").strip()

# Singleton
_supabase = None


# ============================================================
# Helpers
# ============================================================
def _utcnow_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _is_configured() -> bool:
    return bool(SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY)


def supabase_client():
    """
    Regresa el cliente de Supabase (singleton).
    SERVICE_ROLE_KEY = solo server-side.
    """
    global _supabase
    if _supabase is not None:
        return _supabase

    if not _is_configured():
        logger.warning("Supabase no configurado")
        return None

    try:
        from supabase import create_client  # type: ignore
        _supabase = create_client(SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY)
        return _supabase
    except Exception:
        logger.exception("No se pudo crear cliente Supabase")
        return None


def _safe_execute(fn, default):
    try:
        return fn()
    except Exception:
        logger.exception("Supabase operation failed")
        return default


def _clip(s: str, n: int) -> str:
    s = (s or "").strip()
    return s if len(s) <= n else s[:n].rstrip() + "…"


# ============================================================
# THREADS
# ============================================================
def sb_create_thread(title: str = "New chat") -> Optional[Dict[str, Any]]:
    sb = supabase_client()
    if not sb:
        return None

    payload = {
        "title": (title or "New chat").strip(),
        "updated_at": _utcnow_iso(),
    }

    def _op():
        res = sb.table("threads").insert(payload).execute()
        if getattr(res, "data", None):
            row = res.data[0]
            return {
                "id": row.get("id"),
                "title": row.get("title"),
                "updated_at": row.get("updated_at"),
            }
        return None

    return _safe_execute(_op, None)


def sb_touch_thread(thread_id: str) -> None:
    sb = supabase_client()
    if not sb or not thread_id:
        return

    def _op():
        sb.table("threads").update(
            {"updated_at": _utcnow_iso()}
        ).eq("id", thread_id).execute()

    _safe_execute(_op, None)


def sb_update_thread_title(thread_id: str, title: str) -> None:
    sb = supabase_client()
    if not sb or not thread_id:
        return

    clean = (title or "").strip()
    if not clean:
        return

    def _op():
        sb.table("threads").update(
            {"title": clean, "updated_at": _utcnow_iso()}
        ).eq("id", thread_id).execute()

    _safe_execute(_op, None)


def sb_delete_thread(thread_id: str) -> bool:
    sb = supabase_client()
    if not sb or not thread_id:
        return False

    def _op():
        sb.table("messages").delete().eq("thread_id", thread_id).execute()
        sb.table("runs").delete().eq("thread_id", thread_id).execute()
        sb.table("threads").delete().eq("id", thread_id).execute()
        return True

    return bool(_safe_execute(_op, False))


def sb_list_threads(limit: int = 80) -> List[Dict[str, Any]]:
    sb = supabase_client()
    if not sb:
        return []

    limit = max(1, min(limit, 200))

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

        ids = [t["id"] for t in threads if t.get("id")]

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
            last_msg = last_by_thread.get(tid)
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
        "role": (role or "assistant").strip(),
        "content": content or "",
        "meta": meta or {},
    }

    def _op():
        res = sb.table("messages").insert(payload).execute()
        sb_touch_thread(thread_id)
        if getattr(res, "data", None):
            return res.data[0].get("id")
        return None

    return _safe_execute(_op, None)


def sb_list_messages(thread_id: str, limit: int = 50) -> List[Dict[str, Any]]:
    sb = supabase_client()
    if not sb or not thread_id:
        return []

    limit = max(1, min(limit, 200))

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
        "status": (status or "unknown").strip(),
        "runner": runner_meta or {},
    }

    def _op():
        res = sb.table("runs").insert(payload).execute()
        sb_touch_thread(thread_id)
        if getattr(res, "data", None):
            return res.data[0].get("id")
        return None

    return _safe_execute(_op, None)