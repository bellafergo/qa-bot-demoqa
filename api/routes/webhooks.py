# api/routes/webhooks.py
from __future__ import annotations

import json
import logging
from typing import Any, Dict

from fastapi import APIRouter, Header, HTTPException, Request

from services.pr_agent import verify_github_signature, handle_pull_request_event

logger = logging.getLogger("vanya.webhooks")

router = APIRouter(prefix="/webhooks", tags=["webhooks"])


@router.post("/github")
async def github_webhook(
    request: Request,
    x_github_event: str = Header(default=""),
    x_hub_signature_256: str = Header(default=""),
):
    raw = await request.body()

    # 1) Verificación de firma (GitHub)
    if not verify_github_signature(raw, x_hub_signature_256):
        raise HTTPException(status_code=401, detail="Invalid signature")

    # 2) Solo procesamos pull_request (los demás 200 OK pero ignorados)
    if x_github_event != "pull_request":
        return {"ok": True, "ignored": True, "event": x_github_event}

    # 3) JSON payload
    try:
        payload: Dict[str, Any] = json.loads(raw.decode("utf-8"))
    except Exception:
        raise HTTPException(status_code=400, detail="Invalid JSON")

    # 4) PR Agent: comenta + dispara runs
    try:
        return handle_pull_request_event(payload)
    except HTTPException:
        raise
    except Exception as e:
        logger.exception("PR agent failed")
        # Importante: devolver 200 evita reintentos infinitos de GitHub
        return {"ok": False, "error": f"{type(e).__name__}: {e}"}
