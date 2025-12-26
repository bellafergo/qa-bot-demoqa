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


def _ensure_data_url(runner: Any) -> None:
    """
    Agrega runner.screenshot_data_url si existe screenshot_b64.
    Funciona incluso si runner viene como dict o si la estructura cambia ligeramente.
    """
    if not isinstance(runner, dict):
        return

    b64 = runner.get("screenshot_b64")
    if isinstance(b64, str) and b64.strip():
        # Evita duplicar si ya viene con prefijo
        if b64.startswith("data:image/"):
            runner["screenshot_data_url"] = b64
        else:
            runner["screenshot_data_url"] = "data:image/png;base64," + b64


@router.post("/chat_run")
def chat_run(req: ChatRunRequest) -> Dict[str, Any]:
    """
    Wrapper del service:
    - Mantiene el comportamiento actual
    - Garantiza que la evidencia se regrese como Data URL para que el frontend la pueda mostrar
    """
    try:
        result = handle_chat_run(req)

        # Post-proceso robusto: si el service regresó runner
        if isinstance(result, dict):
            runner = result.get("runner")
            _ensure_data_url(runner)

            # (Opcional) También intenta dentro de meta si algún día lo mueves ahí
            meta = result.get("meta")
            if isinstance(meta, dict) and isinstance(meta.get("runner"), dict):
                _ensure_data_url(meta["runner"])

        return result

    except HTTPException:
        raise