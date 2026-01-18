# api/routes/webhooks.py
from __future__ import annotations

import json
import logging
from typing import Any, Dict

from fastapi import APIRouter, Header, HTTPException, Request

from services.pr_agent import (
    verify_github_signature,
    parse_github_pull_request_event,
    list_changed_files,
    select_suites,
    format_comment,
    post_pr_comment,
)

logger = logging.getLogger("vanya.webhooks")

router = APIRouter(prefix="/webhooks", tags=["webhooks"])


@router.post("/github")
async def github_webhook(
    request: Request,
    x_github_event: str = Header(default=""),
    x_hub_signature_256: str = Header(default=""),
):
    raw = await request.body()

    # 1) Verificación de firma (recomendado)
    if not verify_github_signature(raw, x_hub_signature_256):
        raise HTTPException(status_code=401, detail="Invalid signature")

    # 2) Solo procesamos PR events (por ahora)
    if x_github_event != "pull_request":
        return {"ok": True, "ignored": True, "event": x_github_event}

    try:
        payload: Dict[str, Any] = json.loads(raw.decode("utf-8"))
    except Exception:
        raise HTTPException(status_code=400, detail="Invalid JSON")

    action = (payload.get("action") or "").strip().lower()
    # Solo cuando se crea/actualiza (evita spam)
    if action not in {"opened", "synchronize", "reopened", "ready_for_review"}:
        return {"ok": True, "ignored": True, "action": action}

    ctx = parse_github_pull_request_event(payload)
    if not ctx:
        raise HTTPException(status_code=400, detail="Unsupported payload")

    # Skip drafts (opcional)
    pr = payload.get("pull_request") or {}
    if pr.get("draft") is True:
        return {"ok": True, "ignored": True, "reason": "draft_pr"}

    # 3) Baja archivos cambiados
    files = list_changed_files(ctx)

    # 4) Selección de suites (MVP rules-based)
    suites = select_suites(files)

    # 5) Comenta PR (por ahora solo recomendación; luego dispara runs)
    comment = format_comment(ctx, suites, run_links=[])

    try:
        post_pr_comment(ctx, comment)
    except Exception as e:
        logger.exception("Failed to post PR comment")
        raise HTTPException(status_code=500, detail=f"Failed to comment PR: {type(e).__name__}: {e}")

    return {"ok": True, "commented": True, "tags": suites.tags, "reason": suites.reason}
