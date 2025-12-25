# api/routes/threads.py
from typing import Optional
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

from services.store import create_thread, list_threads, get_thread, delete_thread

router = APIRouter()

class CreateThreadRequest(BaseModel):
    title: Optional[str] = None

@router.post("/threads")
def api_create_thread(req: CreateThreadRequest = CreateThreadRequest()):
    return create_thread(req.title or "New chat")

@router.get("/threads")
def api_list_threads():
    return list_threads()

@router.get("/threads/{thread_id}")
def api_get_thread(thread_id: str):
    try:
        return get_thread(thread_id)
    except KeyError:
        raise HTTPException(status_code=404, detail="Thread not found")

@router.delete("/threads/{thread_id}")
def api_delete_thread(thread_id: str):
    try:
        return delete_thread(thread_id)
    except KeyError:
        raise HTTPException(status_code=404, detail="Thread not found")