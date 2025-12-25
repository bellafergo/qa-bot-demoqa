# api/routes/chat.py
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Optional

from services.chat_service import handle_chat_run

router = APIRouter()

class ChatRunRequest(BaseModel):
    prompt: str
    session_id: Optional[str] = None
    headless: bool = True
    base_url: Optional[str] = None
    thread_id: Optional[str] = None

@router.post("/chat_run")
def chat_run(req: ChatRunRequest):
    # Solo delega a service (y mantiene HTTPException)
    try:
        return handle_chat_run(req)
    except HTTPException:
        raise