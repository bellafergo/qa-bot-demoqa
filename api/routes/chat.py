# api/routes/chat.py
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Optional, Any, Dict

from services.chat_service import handle_chat_run

router = APIRouter()


class ChatRunRequest(BaseModel):
    prompt: str
    session_id: Optional[str] = None
    headless: bool = True
    base_url: Optional[str] = None
    thread_id: Optional[str] = None


def _ensure_data_url(obj: Any) -> None:
    """
    Agrega screenshot_data_url si existe screenshot_b64.
    Funciona incluso si viene como dict y aunque cambie el shape.
    """
    if not isinstance(obj, dict):
        return

    # Si ya existe, no lo toques
    if isinstance(obj.get("screenshot_data_url"), str) and obj["screenshot_data_url"].startswith("data:image/"):
        return

    b64 = obj.get("screenshot_b64") or obj.get("screenshotBase64") or obj.get("screenshotB64") or obj.get("screenshot_base64")
    if isinstance(b64, str) and b64.strip():
        if b64.startswith("data:image/"):
            obj["screenshot_data_url"] = b64
        else:
            obj["screenshot_data_url"] = "data:image/png;base64," + b64.strip()


@router.post("/chat_run")
def chat_run(req: ChatRunRequest) -> Dict[str, Any]:
    """
    Wrapper del service:
    - Mantiene el comportamiento actual
    - Garantiza screenshot_data_url para que el frontend renderice evidencia inline
    """
    try:
        result = handle_chat_run(req)

        if isinstance(result, dict):
            # runner top-level
            _ensure_data_url(result.get("runner"))

            # runner dentro de meta (por si lo mueves)
            meta = result.get("meta")
            if isinstance(meta, dict):
                _ensure_data_url(meta.get("runner"))

        return result

    except HTTPException:
        raise