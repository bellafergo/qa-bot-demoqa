# api/routes/drafts_routes.py
"""
Draft Review endpoints.

── Legacy (Explorer/AppMap) ──────────────────────────────────────────────────
POST /drafts/generate             { url }   -> list of draft test dicts
POST /drafts/generate-from-pages  { pages } -> list of draft test dicts (multi-page)
POST /drafts/approve              { drafts } -> { saved, skipped }

── Persistent Drafts ─────────────────────────────────────────────────────────
GET  /drafts                      ?status=  -> List[Draft]
POST /drafts                               -> Draft
PUT  /drafts/{draft_id}                    -> Draft
DELETE /drafts/{draft_id}                  -> { deleted: true }
POST /drafts/{draft_id}/approve            -> DraftApproveResponse
POST /drafts/{draft_id}/ai-suggest         -> AISuggestResponse
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel

from models.draft_models import (
    AISuggestResponse,
    Draft,
    DraftApproveResponse,
    DraftBatchCreate,
    DraftBatchResult,
    DraftCreate,
    DraftUpdate,
)
from services.db.draft_repository import draft_repo

logger = logging.getLogger("vanya.drafts")

router = APIRouter(prefix="/drafts", tags=["drafts"])


class GenerateRequest(BaseModel):
    url: str


class GenerateFromPagesRequest(BaseModel):
    pages: List[Dict[str, Any]]


class ApproveRequest(BaseModel):
    drafts: List[Dict[str, Any]]


@router.post("/generate")
def generate_drafts(req: GenerateRequest):
    from services.application_explorer import explore_page
    from services.suggested_tests import suggest_tests_from_inventory
    from services.test_draft_generator import generate_test_drafts

    inventory   = explore_page(req.url)
    suggestions = suggest_tests_from_inventory(inventory)
    drafts      = generate_test_drafts(inventory, suggestions)
    return {"drafts": drafts, "count": len(drafts)}


@router.post("/generate-from-pages")
def generate_drafts_from_pages(req: GenerateFromPagesRequest):
    from services.suggested_tests import suggest_tests_from_inventory
    from services.test_draft_generator import generate_test_drafts

    seen:   set  = set()
    all_drafts   = []

    for page in (req.pages or []):
        if not isinstance(page, dict):
            continue
        suggestions = suggest_tests_from_inventory(page)
        drafts      = generate_test_drafts(page, suggestions)
        for d in drafts:
            name = d.get("test_name", "")
            if name and name not in seen:
                seen.add(name)
                all_drafts.append(d)

    return {"drafts": all_drafts, "count": len(all_drafts)}


@router.post("/approve")
def approve_drafts_endpoint(req: ApproveRequest):
    from services.draft_approval_service import approve_drafts, persist_approved_drafts

    catalog_tests = approve_drafts(req.drafts)
    result        = persist_approved_drafts(catalog_tests)
    return result


# ── Persistent Drafts — CRUD ──────────────────────────────────────────────────

@router.get("", response_model=List[Draft])
def list_drafts(status: Optional[str] = Query(default=None)):
    """List all persistent drafts, optionally filtered by status."""
    return draft_repo.list_drafts(status=status)


@router.post("", response_model=Draft, status_code=201)
def create_draft(body: DraftCreate):
    """Create a new persistent draft."""
    return draft_repo.create_draft(body.model_dump())


@router.post("/batch", response_model=DraftBatchResult)
def create_drafts_batch(body: DraftBatchCreate):
    """
    Save multiple drafts in one call.
    Not atomic — each item is attempted independently.
    Returns per-item saved/error breakdown.
    """
    items = [d.model_dump() for d in body.drafts]
    result = draft_repo.create_drafts_batch(items)
    return DraftBatchResult(**result)


@router.put("/{draft_id}", response_model=Draft)
def update_draft(draft_id: str, body: DraftUpdate):
    """Update name, module, rationale, confidence, steps or assertions of a draft."""
    updated = draft_repo.update_draft(draft_id, body.model_dump(exclude_none=True))
    if not updated:
        raise HTTPException(status_code=404, detail=f"Draft {draft_id} not found")
    return updated


@router.delete("/{draft_id}")
def delete_draft(draft_id: str):
    """Permanently delete a draft."""
    if not draft_repo.delete_draft(draft_id):
        raise HTTPException(status_code=404, detail=f"Draft {draft_id} not found")
    return {"deleted": True, "draft_id": draft_id}


@router.post("/{draft_id}/approve", response_model=DraftApproveResponse)
def approve_persistent_draft(draft_id: str):
    """
    Approve a persistent draft — saves it as an active test in the catalog
    and marks the draft as approved.
    """
    draft = draft_repo.get_draft(draft_id)
    if not draft:
        raise HTTPException(status_code=404, detail=f"Draft {draft_id} not found")
    if draft.status == "approved":
        raise HTTPException(status_code=409, detail=f"Draft {draft_id} is already approved")

    from models.test_case import TestCaseCreate
    from services.test_catalog_service import catalog_service

    tc_id    = f"DRAFT-{draft_id}"
    tc_type  = "smoke" if draft.confidence == "low" else "regression"

    try:
        payload = TestCaseCreate(
            test_case_id = tc_id,
            name         = draft.name,
            module       = draft.module,
            type         = tc_type,
            priority     = draft.confidence,
            status       = "active",
            test_type    = "ui",
            steps        = draft.steps,
            assertions   = draft.assertions,
        )
        catalog_service.create_test_case(payload)
    except Exception as exc:
        raise HTTPException(status_code=409, detail=str(exc))

    draft_repo.set_status(draft_id, "approved")
    logger.info("drafts: approved draft %s → catalog test %s", draft_id, tc_id)
    return DraftApproveResponse(draft_id=draft_id, test_case_id=tc_id)


@router.post("/{draft_id}/ai-suggest", response_model=AISuggestResponse)
def ai_suggest_draft(draft_id: str):
    """
    Generate AI-powered improvement suggestions for a draft.

    Uses TestGenerationService (OpenAI-backed) with the draft's context as prompt.
    Returns suggested name, steps, assertions, and rationale improvements.
    Fails gracefully — always returns a valid AISuggestResponse.
    """
    draft = draft_repo.get_draft(draft_id)
    if not draft:
        raise HTTPException(status_code=404, detail=f"Draft {draft_id} not found")

    try:
        from models.test_generation_models import TestGenerationRequest
        from services.test_generation_service import generation_service

        req = TestGenerationRequest(
            title   = draft.name,
            module  = draft.module,
            prompt  = (
                f"Improve this QA test draft for the '{draft.module}' module.\n"
                f"Current name: {draft.name}\n"
                f"Current rationale: {draft.rationale or 'none'}\n"
                f"Current steps count: {len(draft.steps)}\n"
                "Suggest a better name, improved test steps, assertions, and rationale."
            ),
            source     = "draft_improvement",
            max_drafts = 1,
        )
        result = generation_service.generate(req)

        if not result.drafts:
            return AISuggestResponse(
                draft_id = draft_id,
                note     = "AI returned no suggestions for this draft",
            )

        d = result.drafts[0]
        steps      = [s.model_dump() if hasattr(s, "model_dump") else dict(s) for s in (d.steps or [])]
        assertions = [a.model_dump() if hasattr(a, "model_dump") else dict(a) for a in (d.assertions or [])]

        return AISuggestResponse(
            draft_id               = draft_id,
            suggested_name         = d.name or None,
            suggested_steps        = steps,
            suggested_assertions   = assertions,
            rationale_improvements = d.rationale or "",
            confidence             = d.confidence or "medium",
            note                   = "Generated via AI test generation service",
        )

    except Exception as exc:
        logger.warning("drafts: ai_suggest failed for %s — %s", draft_id, exc)
        return AISuggestResponse(
            draft_id = draft_id,
            note     = f"AI suggest unavailable: {type(exc).__name__}: {exc}",
        )
