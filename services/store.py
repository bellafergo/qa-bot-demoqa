# services/store.py
"""
Thread & Message storage layer.

Simplified architecture (2025-01):
- PRIMARY: SQLAlchemy via DATABASE_URL (Postgres on Supabase)
- All CRUD goes through SQLAlchemy - no Supabase REST fallback complexity

This ensures reliable persistence when DATABASE_URL is properly configured.
"""
import logging
from typing import Any, Dict, List, Optional

from sqlalchemy.orm import Session

from db import SessionLocal, Thread, Message, utcnow

logger = logging.getLogger("vanya.store")


# ============================================================
# Helpers
# ============================================================
def _iso(x):
    if not x:
        return None
    if hasattr(x, "tzinfo"):
        if x.tzinfo is None:
            x = x.replace(tzinfo=utcnow().tzinfo)
        return x.astimezone(utcnow().tzinfo).isoformat()
    return str(x)




# ============================================================
# STORE API
# ============================================================
def create_thread(title: str = "New chat") -> Dict[str, Any]:
    """Create a new thread in Postgres."""
    title = (title or "New chat").strip() or "New chat"

    db: Session = SessionLocal()
    try:
        t = Thread(title=title)
        db.add(t)
        db.commit()
        db.refresh(t)
        logger.info("Created thread: %s", t.id)
        return {"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)}
    finally:
        db.close()


def list_threads() -> List[Dict[str, Any]]:
    """List all threads ordered by most recently updated."""
    db: Session = SessionLocal()
    try:
        threads = db.query(Thread).order_by(Thread.updated_at.desc()).all()
        return [{"id": t.id, "title": t.title, "updated_at": _iso(t.updated_at)} for t in threads]
    finally:
        db.close()


def get_thread(thread_id: str) -> Dict[str, Any]:
    """
    Devuelve:
      {
        id, title, updated_at,
        messages: [{id, role, content, created_at, meta}]
      }
    Raises KeyError if thread doesn't exist.
    """
    if not thread_id:
        raise ValueError("thread_id vacío")

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


def get_or_create_thread(thread_id: str) -> Dict[str, Any]:
    """
    Get thread if exists, otherwise create a new one with this ID.
    This prevents "Thread not found" errors from stale frontend state.

    Returns same structure as get_thread.
    """
    if not thread_id:
        raise ValueError("thread_id vacío")

    db: Session = SessionLocal()
    try:
        t = db.query(Thread).filter(Thread.id == thread_id).first()

        if not t:
            # Thread doesn't exist - create it with the requested ID
            logger.info("Thread %s not found, auto-creating", thread_id)
            t = Thread(id=thread_id, title="New chat")
            db.add(t)
            db.commit()
            db.refresh(t)

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


def thread_exists(thread_id: str) -> bool:
    """Check if a thread exists without loading all data."""
    if not thread_id:
        return False
    db: Session = SessionLocal()
    try:
        return db.query(Thread.id).filter(Thread.id == thread_id).first() is not None
    finally:
        db.close()


def delete_thread(thread_id: str) -> Dict[str, Any]:
    """Delete a thread and all its messages."""
    if not thread_id:
        raise ValueError("thread_id vacío")

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
        logger.info("Deleted thread %s with %d messages", thread_id, deleted_msgs or 0)
        return {"ok": True, "deleted_messages": int(deleted_msgs or 0)}
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()


def touch_thread(thread_id: str) -> None:
    """Update thread's updated_at timestamp."""
    if not thread_id:
        return

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
    """Update thread title."""
    if not thread_id:
        return
    title = (title or "").strip()
    if not title:
        return

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
    """Add a message to a thread. Auto-creates thread if it doesn't exist."""
    if not thread_id:
        return

    role = (role or "assistant").strip() or "assistant"
    content = content or ""

    db: Session = SessionLocal()
    try:
        # Ensure thread exists (auto-create if needed)
        t = db.query(Thread).filter(Thread.id == thread_id).first()
        if not t:
            logger.info("Thread %s not found for add_message, auto-creating", thread_id)
            t = Thread(id=thread_id, title="New chat")
            db.add(t)

        m = Message(thread_id=thread_id, role=role, content=content)
        if meta is not None and hasattr(m, "meta_json"):
            m.meta_json = meta  # type: ignore
        db.add(m)

        t.updated_at = utcnow()
        db.add(t)

        db.commit()
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()


# ============================================================
# PRO: thread memory helper
# ============================================================
def get_recent_messages(thread_id: str, limit: int = 12) -> List[Dict[str, Any]]:
    """
    Devuelve mensajes recientes en formato:
      [{role, content, meta?, created_at?}]
    """
    if not thread_id:
        return []

    limit = max(1, min(int(limit or 12), 30))

    db: Session = SessionLocal()
    try:
        msgs = (
            db.query(Message)
            .filter(Message.thread_id == thread_id)
            .order_by(Message.created_at.asc())
            .all()
        )
        msgs = msgs[-limit:]
        return [
            {
                "role": m.role,
                "content": m.content,
                "created_at": _iso(m.created_at),
                "meta": getattr(m, "meta_json", None),
            }
            for m in msgs
        ]
    finally:
        db.close()