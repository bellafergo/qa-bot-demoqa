# tests/test_dashboard.py
"""
Tests for the QA Dashboard service and routes.

No browser, no network.
Uses the temporary SQLite DB set up by tests/conftest.py.
"""
from __future__ import annotations

import pytest

from models.test_case import TestCaseCreate
from models.test_run import TestRun
from models.orchestrator_job import OrchestratorJob
from models.dashboard_models import (
    DashboardSummary,
    DashboardModuleMetrics,
    RunStatusBreakdown,
    JobStatusBreakdown,
)
from services.dashboard_service import DashboardService
from services.test_catalog_service import TestCatalogService, _reset_for_testing
from services.db.catalog_repository import CatalogRepository
from services.db.test_run_repository import TestRunRepository
from services.db.orchestrator_job_repository import OrchestratorJobRepository


# ── Helpers ───────────────────────────────────────────────────────────────────

def _clean() -> DashboardService:
    """Wipe all tables and return a fresh DashboardService."""
    _reset_for_testing()
    OrchestratorJobRepository().clear_all()
    return DashboardService()


def _add_tc(tc_id: str, module: str = "demo", status: str = "active") -> None:
    svc = TestCatalogService()
    try:
        svc.create_test_case(TestCaseCreate(
            test_case_id=tc_id,
            name=f"Test {tc_id}",
            module=module,
            type="smoke",
            priority="medium",
            status=status,
            steps=[{"action": "goto", "value": "https://example.com"}],
            assertions=[],
        ))
    except ValueError:
        pass   # already exists — ok


def _add_run(tc_id: str, run_status: str = "pass") -> TestRun:
    run = TestRun(test_case_id=tc_id, environment="test", status=run_status)
    TestRunRepository().create_run(run)
    return run


def _add_job(status: str = "completed", tc_ids=None) -> OrchestratorJob:
    from datetime import datetime, timezone
    job = OrchestratorJob(
        job_type="single",
        test_case_ids=tc_ids or ["TC-DASH-001"],
        total_count=len(tc_ids or ["TC-DASH-001"]),
        status=status,
        environment="test",
        finished_at=datetime.now(timezone.utc) if status not in ("queued", "running") else None,
    )
    OrchestratorJobRepository().create_job(job)
    return job


# ── Summary on empty DB ───────────────────────────────────────────────────────

class TestSummaryEmpty:
    def test_all_zeros_when_empty(self):
        svc = _clean()
        s = svc.get_summary()
        assert isinstance(s, DashboardSummary)
        assert s.total_test_cases == 0
        assert s.total_runs == 0
        assert s.total_jobs == 0
        assert s.pass_rate == 0.0
        assert s.last_run_at is None
        assert s.last_job_at is None

    def test_pass_rate_zero_when_no_runs(self):
        svc = _clean()
        _add_tc("TC-DASH-E1")
        s = svc.get_summary()
        assert s.pass_rate == 0.0
        assert s.total_runs == 0


# ── Summary with data ─────────────────────────────────────────────────────────

class TestSummaryWithData:
    def setup_method(self):
        self.svc = _clean()

    def test_test_case_counts(self):
        _add_tc("TC-D-001", status="active")
        _add_tc("TC-D-002", status="active")
        _add_tc("TC-D-003", status="inactive")
        s = self.svc.get_summary()
        assert s.total_test_cases    == 3
        assert s.active_test_cases   == 2
        assert s.inactive_test_cases == 1

    def test_run_counts(self):
        _add_run("TC-X", "pass")
        _add_run("TC-X", "pass")
        _add_run("TC-X", "fail")
        _add_run("TC-X", "error")
        s = self.svc.get_summary()
        assert s.total_runs == 4
        assert s.pass_runs  == 2
        assert s.fail_runs  == 1
        assert s.error_runs == 1

    def test_pass_rate_computed(self):
        _add_run("TC-X", "pass")
        _add_run("TC-X", "pass")
        _add_run("TC-X", "pass")
        _add_run("TC-X", "fail")
        s = self.svc.get_summary()
        assert s.pass_rate == 75.0

    def test_pass_rate_100_percent(self):
        _add_run("TC-X", "pass")
        _add_run("TC-X", "pass")
        s = self.svc.get_summary()
        assert s.pass_rate == 100.0

    def test_job_counts(self):
        _add_job("completed")
        _add_job("completed")
        _add_job("partial")
        _add_job("failed")
        _add_job("queued")
        s = self.svc.get_summary()
        assert s.total_jobs     == 5
        assert s.completed_jobs == 2
        assert s.partial_jobs   == 1
        assert s.failed_jobs    == 1
        assert s.queued_jobs    == 1
        assert s.running_jobs   == 0

    def test_last_run_at_populated(self):
        _add_run("TC-Y", "pass")
        s = self.svc.get_summary()
        assert s.last_run_at is not None

    def test_last_job_at_populated(self):
        _add_job("completed")
        s = self.svc.get_summary()
        assert s.last_job_at is not None


# ── By-module ─────────────────────────────────────────────────────────────────

class TestByModule:
    def setup_method(self):
        self.svc = _clean()

    def test_empty_returns_empty_list(self):
        assert self.svc.get_by_module() == []

    def test_module_with_no_runs(self):
        _add_tc("TC-M-001", module="auth")
        result = self.svc.get_by_module()
        assert len(result) == 1
        m = result[0]
        assert m.module == "auth"
        assert m.test_case_count == 1
        assert m.run_count == 0
        assert m.pass_rate == 0.0

    def test_module_with_runs(self):
        _add_tc("TC-M-002", module="checkout")
        _add_run("TC-M-002", "pass")
        _add_run("TC-M-002", "pass")
        _add_run("TC-M-002", "fail")
        result = self.svc.get_by_module()
        m = next(x for x in result if x.module == "checkout")
        assert m.run_count  == 3
        assert m.pass_count == 2
        assert m.fail_count == 1
        assert m.pass_rate  == round(2 / 3 * 100, 2)

    def test_multiple_modules(self):
        _add_tc("TC-MA-001", module="auth")
        _add_tc("TC-MC-001", module="cart")
        _add_tc("TC-MC-002", module="cart")
        _add_run("TC-MA-001", "pass")
        _add_run("TC-MC-001", "fail")
        _add_run("TC-MC-002", "error")
        result = self.svc.get_by_module()
        modules = {m.module: m for m in result}
        assert "auth" in modules
        assert "cart" in modules
        assert modules["auth"].test_case_count == 1
        assert modules["cart"].test_case_count == 2
        assert modules["cart"].run_count       == 2

    def test_results_sorted_alphabetically(self):
        _add_tc("TC-Z", module="zzz")
        _add_tc("TC-A", module="aaa")
        _add_tc("TC-M", module="mmm")
        result = self.svc.get_by_module()
        names = [m.module for m in result]
        assert names == sorted(names)

    def test_runs_for_unknown_tc_not_included(self):
        """Runs whose test_case_id is not in the catalog are not reflected."""
        _add_tc("TC-KNOWN", module="known")
        _add_run("TC-UNKNOWN", "pass")   # this tc_id has no test case row
        result = self.svc.get_by_module()
        modules = {m.module for m in result}
        assert "known" in modules
        assert "unknown" not in modules


# ── Run status breakdown ──────────────────────────────────────────────────────

class TestRunStatusBreakdown:
    def setup_method(self):
        self.svc = _clean()

    def test_all_zero_when_empty(self):
        bd = self.svc.get_run_status_breakdown()
        assert isinstance(bd, RunStatusBreakdown)
        assert bd.pass_count  == 0
        assert bd.fail_count  == 0
        assert bd.error_count == 0

    def test_counts_correct(self):
        for _ in range(3):
            _add_run("TC-X", "pass")
        for _ in range(2):
            _add_run("TC-X", "fail")
        _add_run("TC-X", "error")
        bd = self.svc.get_run_status_breakdown()
        assert bd.pass_count  == 3
        assert bd.fail_count  == 2
        assert bd.error_count == 1


# ── Job status breakdown ──────────────────────────────────────────────────────

class TestJobStatusBreakdown:
    def setup_method(self):
        self.svc = _clean()

    def test_all_zero_when_empty(self):
        bd = self.svc.get_job_status_breakdown()
        assert isinstance(bd, JobStatusBreakdown)
        assert bd.queued    == 0
        assert bd.running   == 0
        assert bd.completed == 0
        assert bd.partial   == 0
        assert bd.failed    == 0

    def test_counts_correct(self):
        _add_job("completed")
        _add_job("completed")
        _add_job("partial")
        _add_job("failed")
        bd = self.svc.get_job_status_breakdown()
        assert bd.completed == 2
        assert bd.partial   == 1
        assert bd.failed    == 1
        assert bd.queued    == 0


# ── Recent runs ───────────────────────────────────────────────────────────────

class TestRecentRuns:
    def setup_method(self):
        self.svc = _clean()

    def test_empty_returns_empty(self):
        assert self.svc.get_recent_runs() == []

    def test_most_recent_first(self):
        r1 = _add_run("TC-ORD-1", "pass")
        r2 = _add_run("TC-ORD-2", "fail")
        r3 = _add_run("TC-ORD-3", "error")
        runs = self.svc.get_recent_runs(limit=10)
        run_ids = [r.run_id for r in runs]
        assert r3.run_id == run_ids[0]
        assert r1.run_id == run_ids[-1]

    def test_limit_respected(self):
        for i in range(10):
            _add_run(f"TC-LIM-{i}", "pass")
        runs = self.svc.get_recent_runs(limit=3)
        assert len(runs) == 3

    def test_returns_testrun_models(self):
        _add_run("TC-MODEL", "pass")
        runs = self.svc.get_recent_runs()
        assert all(isinstance(r, TestRun) for r in runs)


# ── Recent jobs ───────────────────────────────────────────────────────────────

class TestRecentJobs:
    def setup_method(self):
        self.svc = _clean()

    def test_empty_returns_empty(self):
        assert self.svc.get_recent_jobs() == []

    def test_most_recent_first(self):
        j1 = _add_job("completed")
        j2 = _add_job("partial")
        jobs = self.svc.get_recent_jobs()
        assert jobs[0].job_id == j2.job_id

    def test_limit_respected(self):
        for _ in range(10):
            _add_job("completed")
        jobs = self.svc.get_recent_jobs(limit=4)
        assert len(jobs) == 4

    def test_returns_orchestrator_job_models(self):
        _add_job("queued")
        jobs = self.svc.get_recent_jobs()
        assert all(isinstance(j, OrchestratorJob) for j in jobs)
