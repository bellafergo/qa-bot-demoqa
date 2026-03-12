# services/dashboard_service.py
"""
Dashboard Service
=================

Computes aggregated QA metrics from the persisted SQLite data.

All reads go through the repository layer — DB is the sole source of truth.
No in-memory stores are consulted.

Upgrade path: the repository methods use SQLAlchemy and will work with
a Postgres/Supabase DSN without any changes here.
"""
from __future__ import annotations

import logging
from typing import List

from models.dashboard_models import (
    DashboardSummary,
    DashboardModuleMetrics,
    RunStatusBreakdown,
    JobStatusBreakdown,
)
from models.test_run import TestRun
from models.orchestrator_job import OrchestratorJob
from services.db.catalog_repository import catalog_repo
from services.db.test_run_repository import test_run_repo
from services.db.orchestrator_job_repository import orch_job_repo

logger = logging.getLogger("vanya.dashboard")


def _pass_rate(pass_count: int, total: int) -> float:
    if total == 0:
        return 0.0
    return round(pass_count / total * 100, 2)


class DashboardService:

    # ── Summary ───────────────────────────────────────────────────────────────

    def get_summary(self) -> DashboardSummary:
        # Test cases
        tc_by_status = catalog_repo.count_by_status()
        active   = tc_by_status.get("active",   0)
        inactive = tc_by_status.get("inactive", 0)
        total_tc = sum(tc_by_status.values())

        tc_by_test_type = catalog_repo.count_by_test_type()
        total_ui_tests  = tc_by_test_type.get("ui",  0)
        total_api_tests = tc_by_test_type.get("api", 0)

        # Runs
        run_by_status = test_run_repo.count_by_status()
        pass_runs  = run_by_status.get("pass",  0)
        fail_runs  = run_by_status.get("fail",  0)
        error_runs = run_by_status.get("error", 0)
        total_runs = sum(run_by_status.values())
        last_run_at = test_run_repo.get_last_executed_at()

        # Jobs
        job_by_status = orch_job_repo.count_by_status()
        queued    = job_by_status.get("queued",    0)
        running   = job_by_status.get("running",   0)
        completed = job_by_status.get("completed", 0)
        partial   = job_by_status.get("partial",   0)
        failed    = job_by_status.get("failed",    0)
        total_jobs = sum(job_by_status.values())
        last_job_at = orch_job_repo.get_last_created_at()

        return DashboardSummary(
            total_test_cases    = total_tc,
            active_test_cases   = active,
            inactive_test_cases = inactive,
            total_ui_tests      = total_ui_tests,
            total_api_tests     = total_api_tests,
            total_runs          = total_runs,
            pass_runs           = pass_runs,
            fail_runs           = fail_runs,
            error_runs          = error_runs,
            pass_rate           = _pass_rate(pass_runs, total_runs),
            total_jobs          = total_jobs,
            queued_jobs         = queued,
            running_jobs        = running,
            completed_jobs      = completed,
            partial_jobs        = partial,
            failed_jobs         = failed,
            last_run_at         = last_run_at,
            last_job_at         = last_job_at,
        )

    # ── Recent records ────────────────────────────────────────────────────────

    def get_recent_runs(self, limit: int = 20) -> List[TestRun]:
        return test_run_repo.list_runs(limit=limit)

    def get_recent_jobs(self, limit: int = 20) -> List[OrchestratorJob]:
        return orch_job_repo.list_jobs(limit=limit)

    # ── By-module breakdown ───────────────────────────────────────────────────

    def get_by_module(self) -> List[DashboardModuleMetrics]:
        """
        Aggregate test-case count and run metrics per module.

        Algorithm:
          1. tc_by_module   = {module: tc_count}          (catalog DB)
          2. tc_to_module   = {test_case_id: module}      (catalog DB)
          3. runs_by_tc     = {test_case_id: {status: n}} (run DB)
          4. For each module, sum run stats across its test cases.
        """
        tc_by_module = catalog_repo.count_test_cases_by_module()   # {module: count}
        tc_to_module = {tc_id: mod for tc_id, mod in catalog_repo.all_modules()}
        runs_by_tc   = test_run_repo.count_runs_by_test_case()     # {tc_id: {status: n}}

        # Aggregate run stats per module
        module_runs: dict = {}  # {module: {status: count}}
        for tc_id, status_counts in runs_by_tc.items():
            mod = tc_to_module.get(tc_id, "unknown")
            agg = module_runs.setdefault(mod, {})
            for status, count in status_counts.items():
                agg[status] = agg.get(status, 0) + count

        # Build result for every module that has at least one test case
        result: List[DashboardModuleMetrics] = []
        for module, tc_count in sorted(tc_by_module.items()):
            run_stats = module_runs.get(module, {})
            pass_n  = run_stats.get("pass",  0)
            fail_n  = run_stats.get("fail",  0)
            error_n = run_stats.get("error", 0)
            total_r = pass_n + fail_n + error_n

            result.append(DashboardModuleMetrics(
                module          = module,
                test_case_count = tc_count,
                run_count       = total_r,
                pass_count      = pass_n,
                fail_count      = fail_n,
                error_count     = error_n,
                pass_rate       = _pass_rate(pass_n, total_r),
            ))

        return result

    # ── Status breakdowns ─────────────────────────────────────────────────────

    def get_run_status_breakdown(self) -> RunStatusBreakdown:
        by_status = test_run_repo.count_by_status()
        return RunStatusBreakdown(
            pass_count  = by_status.get("pass",  0),
            fail_count  = by_status.get("fail",  0),
            error_count = by_status.get("error", 0),
        )

    def get_job_status_breakdown(self) -> JobStatusBreakdown:
        by_status = orch_job_repo.count_by_status()
        return JobStatusBreakdown(
            queued    = by_status.get("queued",    0),
            running   = by_status.get("running",   0),
            completed = by_status.get("completed", 0),
            partial   = by_status.get("partial",   0),
            failed    = by_status.get("failed",    0),
        )


# Module-level singleton
dashboard_service = DashboardService()
