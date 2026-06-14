# services/project_initialize_service.py
"""
Orchestrate first-time / refresh setup for a catalog project.

Uses existing services only — no fake production data.
"""
from __future__ import annotations

import logging
from typing import List, Optional

from models.project_initialize_models import (
    ProjectInitializeRequest,
    ProjectInitializeResponse,
    ProjectInitStep,
)
from models.project_knowledge_models import ProjectKnowledgeRefreshRequest
from services.dashboard_service import dashboard_service
from services.project_knowledge_service import get_project_knowledge, refresh_project_knowledge

logger = logging.getLogger("vanya.project_initialize")


def _count_catalog(pid: str) -> tuple[int, int, int, List[str]]:
    from services.db.catalog_repository import catalog_repo

    cases = catalog_repo.list_test_cases(project_id=pid, status="active", limit=500) or []
    smoke_ids: List[str] = []
    critical_ids: List[str] = []
    for tc in cases:
        tid = (tc.test_case_id or "").strip()
        if not tid:
            continue
        if (tc.type or "").strip().lower() == "smoke":
            smoke_ids.append(tid)
        if (tc.priority or "").strip().lower() in ("critical", "high"):
            critical_ids.append(tid)
    return len(cases), len(smoke_ids), len(critical_ids), smoke_ids


def initialize_project(
    project_id: str,
    req: Optional[ProjectInitializeRequest] = None,
) -> ProjectInitializeResponse:
    pid = (project_id or "").strip()
    if not pid:
        raise ValueError("project_id is required")

    from services.db.project_repository import project_repo

    if project_repo.get_project(pid) is None:
        raise ValueError(f"Project not found: {pid}")

    opts = req or ProjectInitializeRequest()
    steps: List[ProjectInitStep] = []
    job_id: Optional[str] = None
    knowledge_updated = False
    catalog_total = smoke_n = critical_n = 0

    # ── 1. Catalog inventory ─────────────────────────────────────────────────
    try:
        catalog_total, smoke_n, critical_n, smoke_ids = _count_catalog(pid)
        steps.append(
            ProjectInitStep(
                step="catalog",
                status="ok",
                message=f"{catalog_total} active test(s) in catalog",
                details={
                    "total": catalog_total,
                    "smoke": smoke_n,
                    "critical_or_high": critical_n,
                },
            )
        )
    except Exception as exc:
        logger.exception("initialize: catalog scan failed project=%s", pid)
        smoke_ids = []
        steps.append(
            ProjectInitStep(
                step="catalog",
                status="failed",
                message=str(exc),
            )
        )

    # ── 2. System memory refresh ─────────────────────────────────────────────
    if opts.refresh_knowledge:
        try:
            refresh_project_knowledge(
                pid,
                ProjectKnowledgeRefreshRequest(mode="replace"),
            )
            knowledge_updated = True
            steps.append(
                ProjectInitStep(
                    step="knowledge",
                    status="ok",
                    message="System memory rebuilt from catalog, runs, and discovery",
                )
            )
        except Exception as exc:
            logger.exception("initialize: knowledge refresh failed project=%s", pid)
            steps.append(
                ProjectInitStep(
                    step="knowledge",
                    status="failed",
                    message=str(exc),
                )
            )
    else:
        steps.append(
            ProjectInitStep(
                step="knowledge",
                status="skipped",
                message="Knowledge refresh not requested",
            )
        )

    # ── 3. Smoke / critical async run (optional) ─────────────────────────────
    if opts.run_smoke:
        if catalog_total == 0:
            steps.append(
                ProjectInitStep(
                    step="smoke_run",
                    status="skipped",
                    message="No catalog tests — add tests before running smoke suite",
                )
            )
        else:
            try:
                from services.catalog_orchestrator import orchestrator_service

                if smoke_n > 0:
                    job = orchestrator_service.enqueue_suite(
                        type_="smoke",
                        project_id=pid,
                        limit=min(20, smoke_n),
                    )
                elif critical_n > 0:
                    job = orchestrator_service.enqueue_suite(
                        priority="critical",
                        project_id=pid,
                        limit=10,
                    )
                else:
                    job = orchestrator_service.enqueue_suite(
                        project_id=pid,
                        limit=min(10, catalog_total),
                    )
                job_id = getattr(job, "job_id", None)
                st = (getattr(job, "status", None) or "queued").lower()
                if st == "failed":
                    steps.append(
                        ProjectInitStep(
                            step="smoke_run",
                            status="failed",
                            message=getattr(job, "error_message", None) or "No matching tests to enqueue",
                            details={"job_id": job_id},
                        )
                    )
                else:
                    steps.append(
                        ProjectInitStep(
                            step="smoke_run",
                            status="ok",
                            message=f"Queued job {job_id} ({getattr(job, 'total_count', 0)} test(s))",
                            details={"job_id": job_id, "status": st},
                        )
                    )
            except Exception as exc:
                logger.exception("initialize: smoke enqueue failed project=%s", pid)
                steps.append(
                    ProjectInitStep(
                        step="smoke_run",
                        status="failed",
                        message=str(exc),
                    )
                )
    else:
        steps.append(
            ProjectInitStep(
                step="smoke_run",
                status="skipped",
                message="Smoke run not requested",
            )
        )

    # ── 4. Readiness snapshot ────────────────────────────────────────────────
    total_runs = 0
    try:
        summary = dashboard_service.get_summary(project_id=pid)
        total_runs = int(summary.total_runs or 0)
        mem = get_project_knowledge(pid)
        steps.append(
            ProjectInitStep(
                step="readiness",
                status="ok",
                message=f"Dashboard ready — {total_runs} run(s), memory={'yes' if mem else 'no'}",
                details={
                    "total_runs": total_runs,
                    "total_test_cases": summary.total_test_cases,
                    "pass_rate": summary.pass_rate,
                    "has_knowledge": mem is not None,
                },
            )
        )
    except Exception as exc:
        logger.exception("initialize: readiness snapshot failed project=%s", pid)
        steps.append(
            ProjectInitStep(
                step="readiness",
                status="partial",
                message=str(exc),
            )
        )

    # ── 4. Platform database assets (read-only bootstrap) ────────────────────
    try:
        from services.platform_asset_bootstrap_service import bootstrap_platform_assets

        boot = bootstrap_platform_assets(pid)
        steps.append(
            ProjectInitStep(
                step="platform_assets",
                status="ok",
                message=f"Platform assets: {len(boot.connections)} connection(s), {boot.probes_succeeded} probe(s) succeeded",
                details={
                    "agent_id": boot.agent_id,
                    "connections": len(boot.connections),
                    "probes_run": boot.probes_run,
                    "probes_succeeded": boot.probes_succeeded,
                },
            )
        )
    except Exception as exc:
        logger.exception("initialize: platform asset bootstrap failed project=%s", pid)
        steps.append(
            ProjectInitStep(
                step="platform_assets",
                status="partial",
                message=str(exc),
            )
        )

    failed = [s for s in steps if s.status == "failed"]
    ok = len(failed) == 0
    msg = (
        "Project initialized successfully"
        if ok
        else f"Completed with {len(failed)} failed step(s) — see steps for details"
    )

    return ProjectInitializeResponse(
        ok=ok,
        project_id=pid,
        steps=steps,
        catalog_tests=catalog_total,
        smoke_tests=smoke_n,
        critical_tests=critical_n,
        job_id=job_id,
        knowledge_updated=knowledge_updated,
        total_runs=total_runs,
        message=msg,
    )
