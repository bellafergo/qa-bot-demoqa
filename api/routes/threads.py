# api/routes/threads.py
"""
Thread CRUD endpoints.

Flow:
- POST /threads → creates thread in Postgres, returns {id, title, updated_at}
- GET /threads → lists all threads
- GET /threads/{id} → returns thread with messages (auto-creates if missing)
- DELETE /threads/{id} → deletes thread and messages
"""
from typing import Optional
from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel

from services.store import (
    create_thread,
    list_threads,
    get_thread,
    get_or_create_thread,
    delete_thread,
    thread_exists,
)

router = APIRouter()


class CreateThreadRequest(BaseModel):
    title: Optional[str] = None


@router.post("/threads")
def api_create_thread(req: CreateThreadRequest = CreateThreadRequest()):
    """Create a new thread. Returns {id, title, updated_at}."""
    return create_thread(req.title or "New chat")


@router.get("/threads")
def api_list_threads():
    """List all threads ordered by most recently updated."""
    return list_threads()


@router.get("/threads/{thread_id}")
def api_get_thread(
    thread_id: str,
    auto_create: bool = Query(default=True, description="Auto-create thread if not found"),
):
    """
    Get thread with messages.

    By default (auto_create=true), creates the thread if it doesn't exist.
    This prevents 404 errors from stale frontend state.

    Set auto_create=false to get strict 404 behavior.
    """
    try:
        if auto_create:
            return get_or_create_thread(thread_id)
        else:
            return get_thread(thread_id)
    except KeyError:
        raise HTTPException(status_code=404, detail="Thread not found")


@router.head("/threads/{thread_id}")
def api_thread_exists(thread_id: str):
    """Check if thread exists (HEAD request, no body)."""
    if not thread_exists(thread_id):
        raise HTTPException(status_code=404, detail="Thread not found")
    return None


@router.delete("/threads/{thread_id}")
def api_delete_thread(thread_id: str):
    """Delete a thread and all its messages."""
    try:
        return delete_thread(thread_id)
    except KeyError:
        raise HTTPException(status_code=404, detail="Thread not found")