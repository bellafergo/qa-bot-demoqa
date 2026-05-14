# api/routes/browser_inspection_history_routes.py
"""
Read-only history for persisted browser inspections (Phase 3A).

Rows live in SQLite ``test_runs`` / optional Supabase ``qa_runs`` via ``persist_run_payload``,
with ``test_case_id == _browser_inspection`` and bounded ``meta`` summaries.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

from fastapi import APIRouter, HTTPException, Query

from models.browser_inspection_diff_models import BrowserInspectionDiffRequest
from services.browser_inspection_diff_service import compare_browser_inspections
from services.db.test_run_repository import test_run_repo

logger = logging.getLogger("vanya.browser_inspection_history")

router = APIRouter(prefix="/browser-inspections", tags=["browser-inspection"])


def _row_to_list_item(m: Any) -> Dict[str, Any]:
    meta = m.meta if isinstance(m.meta, dict) else {}
    bis = meta.get("browser_inspection_summary") if isinstance(meta.get("browser_inspection_summary"), dict) else {}
    return {
        "inspection_id": m.run_id,
        "persisted_run_id": m.run_id,
        "url": bis.get("url"),
        "final_url": bis.get("final_url"),
        "title": bis.get("title"),
        "status_code": bis.get("status_code"),
        "screenshot_url": bis.get("screenshot_url"),
        "inspection_succeeded": bis.get("inspection_succeeded"),
        "created_at": m.executed_at.isoformat() if hasattr(m.executed_at, "isoformat") else str(m.executed_at),
        "project_id": meta.get("project_id"),
        "page_type": (meta.get("app_map_summary") or {}).get("page_type") if isinstance(meta.get("app_map_summary"), dict) else None,
        "confidence": (meta.get("app_map_summary") or {}).get("confidence") if isinstance(meta.get("app_map_summary"), dict) else None,
    }


def _row_to_detail(m: Any) -> Dict[str, Any]:
    meta = m.meta if isinstance(m.meta, dict) else {}
    out: Dict[str, Any] = {
        "inspection_id": m.run_id,
        "persisted_run_id": m.run_id,
        "source": meta.get("source"),
        "execution_mode": meta.get("execution_mode"),
        "project_id": meta.get("project_id"),
        "status": m.status,
        "test_name": m.test_name,
        "created_at": m.executed_at.isoformat() if hasattr(m.executed_at, "isoformat") else str(m.executed_at),
        "evidence_url": m.evidence_url,
        "browser_inspection_summary": meta.get("browser_inspection_summary"),
        "app_map_summary": meta.get("app_map_summary"),
        "warnings": (meta.get("browser_inspection_summary") or {}).get("warnings")
        if isinstance(meta.get("browser_inspection_summary"), dict)
        else None,
    }
    return out


@router.get("")
def list_browser_inspections(
    project_id: Optional[str] = Query(default=None, max_length=256),
    limit: int = Query(default=50, ge=1, le=200),
    source: str = Query(default="browser_inspection", max_length=64),
):
    if source != "browser_inspection":
        raise HTTPException(status_code=400, detail="source must be browser_inspection")
    try:
        rows = test_run_repo.list_browser_inspection_runs(project_id=project_id, limit=limit)
        items: List[Dict[str, Any]] = [_row_to_list_item(r) for r in rows]
        return {"items": items, "count": len(items)}
    except Exception as exc:
        logger.exception("list_browser_inspections failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.post("/diff")
def diff_browser_inspections(body: BrowserInspectionDiffRequest):
    """
    Compare two persisted browser inspections (metadata only — no Playwright, no DOM).
    """
    try:
        return compare_browser_inspections(body).model_dump()
    except HTTPException:
        raise
    except Exception as exc:
        logger.exception("browser-inspections/diff failed")
        raise HTTPException(status_code=500, detail=str(exc)) from exc


@router.get("/{inspection_id}")
def get_browser_inspection(inspection_id: str):
    rid = (inspection_id or "").strip()
    if not rid:
        raise HTTPException(status_code=400, detail="inspection_id required")
    run = test_run_repo.get_run(rid)
    if run is None:
        raise HTTPException(status_code=404, detail="inspection not found")
    meta = run.meta if isinstance(run.meta, dict) else {}
    if run.test_case_id != "_browser_inspection" or meta.get("source") != "browser_inspection":
        raise HTTPException(status_code=404, detail="inspection not found")
    return _row_to_detail(run)
