# services/store.py
import os
import logging
from typing import Any, Dict, List, Optional

from sqlalchemy.orm import Session
from db import SessionLocal, Thread, Message, utcnow

logger = logging.getLogger("vanya.store")

# Supabase store (si existe)
try:
    from services.supabase_store import (
        supabase_client,
        sb_create_thread,
        sb_add_message,
        sb_touch_thread,
        sb_update_thread_title,
        sb_get_thread,          # opcional
        sb_list_threads,        # opcional
        sb_get_messages,        # opcional
        sb_delete_thread,       # opcional
    )
except Exception:
    supabase_client = None
    sb_create_thread = None
    sb_add_message = None
    sb_touch_thread = None
    sb_update_thread_title = None
    sb_get_thread = None
    sb_list_threads = None
    sb_get_messages = None
    sb_delete_thread = None


# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
_SUPABASE_STRICT = (os.getenv("SUPABASE_STRICT", "0").strip() == "1")

# cache: evita llamar supabase_client() por request
_SUPABASE_OK_CACHED: Optional[bool] = None


def _iso(x):
    if not x:
        return None
    if hasattr(x, "tzinfo"):
        if x.tzinfo is None:
            x = x.replace(tzinfo=utcnow().tzinfo)
        return x.astimezone(utcnow().tzinfo).isoformat()
    return str(x)


def _has_supabase() -> bool:
    """
    True si Supabase está disponible (y el cliente responde).
    Cacheado para no evaluarlo en cada request.
    """
    global _SUPABASE_OK_CACHED
    if _SUPABASE_OK_CACHED is not None:
        return _SUPABASE_OK_CACHED

    ok = False
    try:
        ok = bool(supabase_client and supabase_client())
    except Exception:
        ok = False

    _SUPABASE_OK_CACHED = ok
    if ok:
        logger.info("Supabase enabled (cache=true)")
    else:
        logger.info("Supabase disabled or unreachable (cache=true)")

    return ok


def _supabase_or_raise(op: str):
    """
    Si strict mode está habilitado y supabase no está listo -> error.
    """
    if _SUPABASE_STRICT and not _has_supabase():
        raise RuntimeError(f"SUPABASE_STRICT=1 y Supabase no está disponible ({op})")


def _safe_touch_supabase(thread_id: str):
    try:
        if sb_touch_thread:
            sb_touch_thread(thread_id)
    except Exception:
        logger.warning("Supabase touch failed", exc_info=True)
        if _SUPABASE_STRICT:
            raise


# ============================================================
# STORE API (lo que usa tu app)
# ============================================================
def create_thread(title: str = "New chat") -> Dict[str, Any]:
    title = (title or "New chat").strip() or "New chat"

    _supabase_or_raise("create_thread")

    # ✅ Supabase-first
    if _has_supabase() and sb_create_thread:
        thread_id = sb_create_thread(title)
        _safe_touch_supabase(thread_id)
        return {"id": thread_id, "title": title, "updated_at": None}

    # ✅ SQLite fallback
    db: Session = SessionLocal()
    try:
        t = Thread(title=title)
        db.add(t)
        db.commit()
        db.refresh(t)
        return {"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)}
    finally:
        db.close()


def list_threads() -> List[Dict[str, Any]]:
    _supabase_or_raise("list_threads")

    # ✅ Supabase-first
    if _has_supabase() and sb_list_threads:
        return sb_list_threads()

    # ✅ SQLite fallback
    db: Session = SessionLocal()
    try:
        threads = db.query(Thread).order_by(Thread.updated_at.desc()).all()
        return [{"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)} for t in threads]
    finally:
        db.close()


def get_thread(thread_id: str) -> Dict[str, Any]:
    if not thread_id:
        raise ValueError("thread_id vacío")

    _supabase_or_raise("get_thread")

    # ✅ Supabase-first
    if _has_supabase() and sb_get_thread and sb_get_messages:
        t = sb_get_thread(thread_id)
        if not t:
            raise KeyError("Thread not found")
        msgs = sb_get_messages(thread_id) or []
        return {
            "id": t.get("id"),
            "title": t.get("title"),
            "updated_at": t.get("updated_at"),
            "messages": msgs,
        }

    # ✅ SQLite fallback
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise KeyError("Thread not found")

        msgs = (
            db.query(Message)
            .filter(Message.thread_id == thread_id)
            .order_by(Message.created_at.asc())
            .all()
        )
        return {
            "id": t.id,
            "title": t.title,
            "updated_at": _iso(t.updated_at),
            "messages": [
                {
                    "id": getattr(m, "id", None),
                    "role": m.role,
                    "content": m.content,
                    "created_at": _iso(m.created_at),
                    "meta": getattr(m, "meta_json", None),
                }
                for m in msgs
            ],
        }
    finally:
        db.close()


def delete_thread(thread_id: str) -> Dict[str, Any]:
    if not thread_id:
        raise ValueError("thread_id vacío")

    _supabase_or_raise("delete_thread")

    # ✅ Supabase-first
    if _has_supabase() and sb_delete_thread:
        sb_delete_thread(thread_id)
        return {"ok": True}

    # ✅ SQLite fallback
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            raise KeyError("Thread not found")

        deleted_msgs = (
            db.query(Message)
            .filter(Message.thread_id == thread_id)
            .delete(synchronize_session=False)
        )
        db.delete(t)
        db.commit()
        return {"ok": True, "deleted_messages": int(deleted_msgs or 0)}
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()


def touch_thread(thread_id: str) -> None:
    if not thread_id:
        return

    _supabase_or_raise("touch_thread")

    # ✅ Supabase-first
    if _has_supabase() and sb_touch_thread:
        _safe_touch_supabase(thread_id)
        return

    # ✅ SQLite fallback
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if t:
            t.updated_at = utcnow()
            db.add(t)
            db.commit()
    finally:
        db.close()


def update_thread_title(thread_id: str, title: str) -> None:
    if not thread_id:
        return
    title = (title or "").strip()
    if not title:
        return

    _supabase_or_raise("update_thread_title")

    # ✅ Supabase-first
    if _has_supabase() and sb_update_thread_title:
        sb_update_thread_title(thread_id, title)
        _safe_touch_supabase(thread_id)
        return

    # ✅ SQLite fallback
    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if t:
            t.title = title
            t.updated_at = utcnow()
            db.add(t)
            db.commit()
    finally:
        db.close()


def add_message(thread_id: str, role: str, content: str, meta: Optional[dict] = None) -> None:
    if not thread_id:
        return
    role = (role or "assistant").strip() or "assistant"
    content = content or ""

    _supabase_or_raise("add_message")

    # ✅ Supabase-first
    if _has_supabase() and sb_add_message:
        sb_add_message(thread_id=thread_id, role=role, content=content, meta=meta or {})
        _safe_touch_supabase(thread_id)
        return

    # ✅ SQLite fallback
    db: Session = SessionLocal()
    try:
        m = Message(thread_id=thread_id, role=role, content=content)
        if meta is not None and hasattr(m, "meta_json"):
            m.meta_json = meta  # type: ignore
        db.add(m)

        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if t:
            t.updated_at = utcnow()
            db.add(t)

        db.commit()
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()