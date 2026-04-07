# api/routes/chat.py
import logging
from typing import Any, Dict, Optional

from fastapi import APIRouter, HTTPException, Request
from pydantic import BaseModel

from services.chat_service import handle_chat_run

logger = logging.getLogger("vanya")

router = APIRouter()


class ChatRunRequest(BaseModel):
    prompt: str
    session_id: Optional[str] = None
    headless: bool = True
    base_url: Optional[str] = None
    thread_id: Optional[str] = None
    project_id: Optional[str] = None


def _ensure_data_url(obj: Any) -> None:
    """
    Agrega screenshot_data_url si existe screenshot_b64 (u otros alias).
    """
    if not isinstance(obj, dict):
        return

    existing = obj.get("screenshot_data_url")
    if isinstance(existing, str) and existing.startswith("data:image/"):
        return

    b64 = (
        obj.get("screenshot_b64")
        or obj.get("screenshotBase64")
        or obj.get("screenshotB64")
        or obj.get("screenshot_base64")
    )

    if isinstance(b64, str) and b64.strip():
        s = b64.strip()
        obj["screenshot_data_url"] = s if s.startswith("data:image/") else "data:image/png;base64," + s


def _post_process_result(result: Any) -> None:
    if not isinstance(result, dict):
        return
    _ensure_data_url(result.get("runner"))
    meta = result.get("meta")
    if isinstance(meta, dict):
        _ensure_data_url(meta.get("runner"))


@router.post("/chat_run")
def chat_run(req: ChatRunRequest, request: Request) -> Dict[str, Any]:
    """
    Wrapper del service:
    - Garantiza screenshot_data_url
    - Nunca deja al frontend sin body usable
    - Propaga correlation_id para trazabilidad
    """
    correlation_id = getattr(request.state, "request_id", None)
    client_id = getattr(request.state, "client_id", None)
    workspace_id = getattr(request.state, "workspace_id", None)
    try:
        result = handle_chat_run(
            req,
            correlation_id=correlation_id,
            client_id=client_id,
            workspace_id=workspace_id,
        )
        _post_process_result(result)
        return result

    except HTTPException:
        raise

    except Exception as e:
        logger.exception("chat_run crashed")
        return {
            "mode": "error",
            "answer": "Ocurrió un error procesando tu solicitud. Intenta nuevamente.",
            "error": f"{type(e).__name__}: {str(e)}",
            "meta": {"safe_error": True},
        }