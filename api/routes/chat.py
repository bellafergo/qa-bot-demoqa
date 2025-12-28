# api/routes/chat.py
import logging
from typing import Any, Dict, Optional

from fastapi import APIRouter, HTTPException
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


def _ensure_data_url(obj: Any) -> None:
    """
    Agrega screenshot_data_url si existe screenshot_b64 (u otros alias).
    Funciona incluso si viene como dict y aunque cambie el shape.
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
        if s.startswith("data:image/"):
            obj["screenshot_data_url"] = s
        else:
            obj["screenshot_data_url"] = "data:image/png;base64," + s


def _post_process_result(result: Any) -> None:
    """
    Post-proceso robusto para evidencia:
    - runner top-level
    - runner dentro de meta
    """
    if not isinstance(result, dict):
        return

    _ensure_data_url(result.get("runner"))

    meta = result.get("meta")
    if isinstance(meta, dict):
        _ensure_data_url(meta.get("runner"))


@router.post("/chat_run")
def chat_run(req: ChatRunRequest) -> Dict[str, Any]:
    """
    Wrapper del service:
    - Mantiene el comportamiento actual
    - Garantiza screenshot_data_url para que el frontend renderice evidencia inline
    - Nunca deja que un error interno "opaque" la respuesta en frontend (evita Failed to fetch)
    """
    try:
        result = handle_chat_run(req)
        _post_process_result(result)
        return result

    except HTTPException:
        # ✅ Respeta errores intencionales (400/401/403/etc.)
        raise

    except Exception as e:
        # 🔒 Fallback final: nunca 500 sin cuerpo "usable"
        logger.exception("chat_run crashed")

        return {
            "mode": "error",
            "answer": "Ocurrió un error procesando tu solicitud. Intenta nuevamente.",
            "error": f"{type(e).__name__}: {str(e)}",
            "meta": {"safe_error": True},
        }