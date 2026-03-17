# api/routes/drafts_routes.py
"""
Draft Review endpoints.

POST /drafts/generate   { url } -> list of draft test dicts
POST /drafts/approve    { drafts } -> { saved, skipped }
"""
from __future__ import annotations

from typing import Any, Dict, List

from fastapi import APIRouter
from pydantic import BaseModel

router = APIRouter(prefix="/drafts", tags=["drafts"])


class GenerateRequest(BaseModel):
    url: str


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


@router.post("/approve")
def approve_drafts_endpoint(req: ApproveRequest):
    from services.draft_approval_service import approve_drafts, persist_approved_drafts

    catalog_tests = approve_drafts(req.drafts)
    result        = persist_approved_drafts(catalog_tests)
    return result
