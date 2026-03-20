# services/run_mapper.py
"""
Run Mapper
==========
Normalises any Vanya run representation to the CanonicalRun contract defined
in models/run_contract.py.

All API endpoints that return run objects should pass their data through
one of these functions before serialising the response.

Functions
---------
normalize_status(status)                → canonical status string
run_from_catalog_testrun(tr)            → CanonicalRun from TestRun model
suite_from_catalog_suite_result(sr)     → CanonicalSuiteResult from SuiteRunResult
run_from_orchestrator_job(job)          → CanonicalRun from OrchestratorJob
run_from_legacy_store(payload)          → CanonicalRun from run_store dict
normalize_run(raw)                      → CanonicalRun from any dict
normalize_run_list(runs)                → List[CanonicalRun]
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional

from models.run_contract import (
    CanonicalRun,
    CanonicalSuiteResult,
    RunArtifacts,
    RunMeta,
)

logger = logging.getLogger("vanya.run_mapper")


# ── Status normalisation ──────────────────────────────────────────────────────

_STATUS_MAP: Dict[str, str] = {
    # catalog / runner statuses → canonical
    "pass":      "passed",
    "passed":    "passed",
    "completed": "passed",    # orchestrator job completed (may still have partial failures)
    "fail":      "failed",
    "failed":    "failed",
    "error":     "error",     # preserve technical runner errors
    "partial":   "failed",    # orchestrator job with partial failures
    # lifecycle statuses
    "running":   "running",
    "queued":    "queued",
    "pending":   "queued",
    "planning":  "planning",
    "compiled":  "compiled",
    "canceled":  "canceled",
    "cancelled": "canceled",
    "aborted":   "canceled",
}


def normalize_status(status: str) -> str:
    """Map any raw status string to the canonical set."""
    return _STATUS_MAP.get(str(status or "").strip().lower(), "failed")


# ── Datetime helpers ──────────────────────────────────────────────────────────

def _to_iso(value: Any) -> Optional[str]:
    """
    Convert a datetime object, integer/float ms-epoch, or string to ISO-8601.
    Returns None for None / unhandled inputs.
    """
    if value is None:
        return None
    try:
        if hasattr(value, "isoformat"):
            return value.isoformat()
        if isinstance(value, (int, float)):
            from datetime import datetime, timezone
            return datetime.fromtimestamp(value / 1000, tz=timezone.utc).isoformat()
        return str(value)
    except Exception:
        return str(value)


# ── Catalog path ──────────────────────────────────────────────────────────────

def run_from_catalog_testrun(tr: Any) -> CanonicalRun:
    """
    Convert a models.test_run.TestRun instance to CanonicalRun.

    This is the primary mapping for runs produced by:
      catalog_service.run_test_case()
      catalog_service.run_suite()
    """
    raw_meta: Dict[str, Any] = dict(tr.meta) if tr.meta else {}
    raw_status = str(tr.status or "").lower()

    # Build error_summary from runner reason or first relevant log line
    error_summary: Optional[str] = None
    if raw_status in ("fail", "error", "failed"):
        reason = raw_meta.get("runner_reason") or raw_meta.get("reason")
        if reason:
            error_summary = str(reason)
        elif getattr(tr, "logs", None):
            bad = [l for l in tr.logs if "error" in l.lower() or "fail" in l.lower()]
            error_summary = (bad[0] if bad else tr.logs[-1])[:500]

    artifacts = RunArtifacts(
        screenshot_b64=raw_meta.get("screenshot_b64"),
        evidence_url=getattr(tr, "evidence_url", None),
        report_url=getattr(tr, "report_url", None),
    )

    meta = RunMeta(
        trigger_source=raw_meta.get("trigger_source") or "catalog",
        triggered_by=raw_meta.get("triggered_by"),
        environment=getattr(tr, "environment", None) or raw_meta.get("environment"),
        browser=raw_meta.get("browser"),
        base_url=raw_meta.get("base_url"),
        correlation_id=raw_meta.get("correlation_id"),
        client_id=raw_meta.get("client_id"),
        workspace_id=raw_meta.get("workspace_id"),
    )

    steps = list(getattr(tr, "steps_result", None) or [])
    return CanonicalRun(
        run_id=tr.run_id,
        job_id=raw_meta.get("job_id"),
        test_id=getattr(tr, "test_case_id", None),
        test_name=getattr(tr, "test_name", None),
        source=raw_meta.get("source", "catalog"),
        status=normalize_status(raw_status),
        started_at=_to_iso(getattr(tr, "executed_at", None)),
        finished_at=_to_iso(getattr(tr, "executed_at", None)),
        duration_ms=int(getattr(tr, "duration_ms", None) or 0),
        error_summary=error_summary,
        steps_count=len(steps),
        evidence_id=getattr(tr, "evidence_id", None),
        evidence_url=getattr(tr, "evidence_url", None),
        report_url=getattr(tr, "report_url", None),
        correlation_id=raw_meta.get("correlation_id"),
        steps=steps,
        artifacts=artifacts,
        meta=meta,
    )


def suite_from_catalog_suite_result(sr: Any) -> CanonicalSuiteResult:
    """Convert a models.test_run.SuiteRunResult to CanonicalSuiteResult."""
    return CanonicalSuiteResult(
        suite_run_id=sr.suite_run_id,
        started_at=_to_iso(getattr(sr, "started_at", None)),
        finished_at=_to_iso(getattr(sr, "finished_at", None)),
        environment=getattr(sr, "environment", "default"),
        total=int(sr.total or 0),
        passed=int(sr.passed or 0),
        failed=int(sr.failed or 0),
        errors=int(sr.errors or 0),
        duration_ms=int(sr.duration_ms or 0),
        runs=[run_from_catalog_testrun(r) for r in (sr.runs or [])],
        filter_applied=dict(sr.filter_applied or {}),
    )


# ── Orchestrator path ─────────────────────────────────────────────────────────

def run_from_orchestrator_job(job: Any) -> CanonicalRun:
    """
    Map an OrchestratorJob to a CanonicalRun (job-level summary).

    Individual test runs within the job are separate TestRun records in the
    catalog DB.  This function represents the job aggregate as a single entry.
    """
    total        = int(getattr(job, "total_count", 0) or 0)
    passed_count = int(getattr(job, "passed_count", 0) or 0)
    failed_count = int(getattr(job, "failed_count", 0) or 0)
    error_count  = int(getattr(job, "error_count", 0) or 0)

    summary = (
        f"{total} test(s) — "
        f"{passed_count} passed, {failed_count} failed, {error_count} errored"
        if total else None
    )

    return CanonicalRun(
        run_id=job.job_id,
        job_id=job.job_id,
        source="orchestrator",
        status=normalize_status(getattr(job, "status", "failed")),
        started_at=_to_iso(getattr(job, "started_at", None)),
        finished_at=_to_iso(getattr(job, "finished_at", None)),
        duration_ms=int(getattr(job, "duration_ms", 0) or 0),
        steps_count=0,
        summary=summary,
        error_summary=getattr(job, "error_message", None),
        meta=RunMeta(
            environment=getattr(job, "environment", None),
        ),
    )


# ── Legacy run_store path ─────────────────────────────────────────────────────

def run_from_legacy_store(payload: Dict[str, Any]) -> CanonicalRun:
    """
    Convert a run_store dict (produced by workers/jobs.py for async execute /
    chat path) to CanonicalRun.

    run_store payloads are keyed by evidence_id and contain arbitrary metadata.
    Fields are intentionally looked up in multiple locations to be resilient
    to the various shapes used across different callers.
    """
    raw_meta: Dict[str, Any] = payload.get("meta") or {}

    evidence_url = (
        payload.get("evidence_url")
        or raw_meta.get("evidence_url")
        or raw_meta.get("screenshot_url")
    )
    report_url   = payload.get("report_url")   or raw_meta.get("report_url")
    screenshot_b64 = payload.get("screenshot_b64") or raw_meta.get("screenshot_b64")

    artifacts = RunArtifacts(
        screenshot_b64=screenshot_b64,
        evidence_url=evidence_url,
        report_url=report_url,
    )

    meta = RunMeta(
        trigger_source=raw_meta.get("trigger_source") or "chat",
        triggered_by=raw_meta.get("triggered_by"),
        environment=raw_meta.get("environment"),
        browser=raw_meta.get("browser"),
        base_url=payload.get("base_url") or raw_meta.get("base_url"),
        correlation_id=raw_meta.get("correlation_id"),
        client_id=raw_meta.get("client_id"),
        workspace_id=raw_meta.get("workspace_id"),
    )

    run_id = payload.get("run_id") or payload.get("evidence_id") or "unknown"

    steps = list(payload.get("steps") or payload.get("steps_result") or [])
    return CanonicalRun(
        run_id=run_id,
        job_id=payload.get("job_id"),
        test_id=payload.get("test_case_id") or payload.get("test_id"),
        test_name=payload.get("test_name"),
        source=str(payload.get("source") or "chat"),
        status=normalize_status(str(payload.get("status") or "failed")),
        started_at=_to_iso(payload.get("started_at") or payload.get("created_at")),
        finished_at=_to_iso(payload.get("finished_at")),
        duration_ms=int(payload.get("duration_ms") or 0),
        error_summary=payload.get("error_message"),
        steps_count=len(steps),
        evidence_id=payload.get("evidence_id"),
        evidence_url=evidence_url,
        report_url=report_url,
        correlation_id=raw_meta.get("correlation_id"),
        steps=steps,
        artifacts=artifacts,
        meta=meta,
    )


# ── Generic dispatch ──────────────────────────────────────────────────────────

def normalize_run(raw: Dict[str, Any]) -> CanonicalRun:
    """
    Normalise any run dict to CanonicalRun.

    Dispatch heuristic:
      1. Already canonical  — has 'artifacts' key that is a dict
      2. TestRun-like       — has 'test_case_id' or 'steps_result'
      3. Legacy run_store   — everything else (evidence_id, kind, …)
    """
    if not isinstance(raw, dict):
        try:
            raw = dict(raw)
        except Exception:
            return CanonicalRun(run_id="unknown")

    # (1) Already canonical
    if isinstance(raw.get("artifacts"), dict):
        try:
            return CanonicalRun(**raw)
        except Exception:
            pass  # fall through if validation fails

    # (2) TestRun-like
    if "test_case_id" in raw or "steps_result" in raw:
        try:
            from models.test_run import TestRun
            tr = TestRun(**raw)
            return run_from_catalog_testrun(tr)
        except Exception as exc:
            logger.debug("normalize_run: TestRun parse failed (%s), using legacy path", exc)

    # (3) Legacy run_store dict
    return run_from_legacy_store(raw)


def normalize_run_list(runs: List[Any]) -> List[CanonicalRun]:
    """Normalise a heterogeneous list of run objects to CanonicalRun."""
    result: List[CanonicalRun] = []
    for r in runs:
        try:
            if hasattr(r, "model_dump"):          # Pydantic model (e.g. TestRun)
                result.append(run_from_catalog_testrun(r))
            elif isinstance(r, dict):
                result.append(normalize_run(r))
            else:
                result.append(normalize_run(dict(r)))
        except Exception as exc:
            logger.warning("normalize_run_list: skipped unmappable run: %s", exc)
    return result
