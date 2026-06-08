# api/routes/evidence_routes.py
"""
Evidence Library API
====================

GET /evidences
    Lean list of all run evidence records (no blobs).
    Suitable for paginated Evidence Library UI consumption.

Architecture notes
------------------
- Delegates to run_history_service.list_runs() (SQLite, already lean).
- Projects only the fields needed for list views; NO screenshot_b64,
  NO dom snapshots, NO network data.
- Full evidence (HTML report with embedded screenshots) remains at
  GET /runs/{run_id}?format=json (canonical execution id).
"""
from __future__ import annotations

import logging
from typing import List, Optional

from fastapi import APIRouter, Query
from pydantic import BaseModel

from services.run_history_service import run_history_service

logger = logging.getLogger("vanya.evidence_routes")
router = APIRouter(prefix="/evidences", tags=["evidence"])


class EvidenceKpiSummary(BaseModel):
    total: int = 0
    screenshots: int = 0
    reports: int = 0
    logs_hint: int = 0
    passed: int = 0
    failed: int = 0
    last_at: Optional[str] = None


class EvidenceSummary(BaseModel):
    """Lean projection of a run record — only what the Evidence Library needs."""
    run_id:        str
    test_id:       Optional[str] = None
    test_name:     Optional[str] = None
    suite_name:    Optional[str] = None
    status:        str
    started_at:    Optional[str] = None
    duration_ms:   int = 0
    error_summary: Optional[str] = None
    evidence_url:  Optional[str] = None
    report_url:    Optional[str] = None
    meta:          Optional[dict] = None  # flaky/retry/quarantine metadata for Action Panel


def _runs_to_evidence_summaries(runs) -> List[EvidenceSummary]:
    result: List[EvidenceSummary] = []
    for r in runs:
        arts = r.artifacts or None
        meta_dict = r.meta.model_dump(exclude_none=True) if r.meta else None
        result.append(EvidenceSummary(
            run_id=r.run_id,
            test_id=r.test_id,
            test_name=r.test_name,
            suite_name=r.suite_name,
            status=r.status,
            started_at=r.started_at,
            duration_ms=int(r.duration_ms or 0),
            error_summary=r.error_summary,
            evidence_url=(arts.evidence_url if arts else None) or r.evidence_url,
            report_url=(arts.report_url if arts else None) or r.report_url,
            meta=meta_dict,
        ))
    return result


@router.get("/summary", response_model=EvidenceKpiSummary)
def evidence_kpi_summary(project_id: Optional[str] = Query(None)):
    """Lean KPI counts for Evidence Library header."""
    try:
        runs = run_history_service.list_runs(limit=200, project_id=project_id)
    except Exception:
        logger.exception("evidence_routes: summary list_runs failed")
        runs = []

    shots = reports = logs_hint = passed = failed = 0
    last_at: Optional[str] = None
    for r in runs:
        arts = r.artifacts
        ev_url = (arts.evidence_url if arts else None) or r.evidence_url
        rep_url = (arts.report_url if arts else None) or r.report_url
        if ev_url:
            shots += 1
        if rep_url:
            reports += 1
        if r.logs:
            logs_hint += 1
        st = str(r.status or "").lower()
        if st in ("pass", "passed", "completed"):
            passed += 1
        elif st in ("fail", "failed", "error"):
            failed += 1
        ts = r.started_at or r.finished_at
        if ts and (last_at is None or ts > last_at):
            last_at = ts

    return EvidenceKpiSummary(
        total=len(runs),
        screenshots=shots,
        reports=reports,
        logs_hint=logs_hint,
        passed=passed,
        failed=failed,
        last_at=last_at,
    )


@router.get("", response_model=List[EvidenceSummary])
def list_evidences(
    limit: int = Query(50, ge=1, le=200),
    test_case_id: Optional[str] = Query(None),
    project_id: Optional[str] = Query(None),
):
    """
    Return a lean list of run evidence records, most recent first.

    Parameters
    ----------
    limit        : max records to return (default 50, max 200)
    test_case_id : optional filter — only runs for this test case

    Each record includes evidence_url and report_url for direct linking;
    screenshot_b64 and all other large blobs are intentionally excluded.
    """
    try:
        runs = run_history_service.list_runs(
            test_case_id=test_case_id or None,
            project_id=project_id,
            limit=limit,
        )
    except Exception:
        logger.exception("evidence_routes: list_runs failed")
        runs = []

    return _runs_to_evidence_summaries(runs)
