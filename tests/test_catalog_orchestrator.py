# tests/test_catalog_orchestrator.py
"""
Unit tests for CatalogOrchestratorService.

No browser, no network, no real runner.
The catalog_service.run_test_case() is patched wherever execution is tested.
"""
from __future__ import annotations

import threading
import time
from unittest.mock import MagicMock, patch

import pytest

import services.catalog_orchestrator as orch_module
from models.orchestrator_job import OrchestratorJob
from models.test_run import TestRun
from services.catalog_orchestrator import CatalogOrchestratorService
from services.test_catalog_service import TestCatalogService
import services.test_catalog_service as svc_module


# ── Helpers ───────────────────────────────────────────────────────────────────

def _fresh_orch() -> CatalogOrchestratorService:
    """Return a service with clean in-memory state."""
    orch_module._JOB_STORE.clear()
    orch_module._JOB_ORDER.clear()
    # Reset the queue (drain any stale items)
    while not orch_module._QUEUE.empty():
        try:
            orch_module._QUEUE.get_nowait()
        except Exception:
            break
    return CatalogOrchestratorService()


def _fresh_catalog() -> TestCatalogService:
    svc_module._CATALOG.clear()
    svc_module._RUN_STORE.clear()
    svc_module._RUN_ORDER.clear()
    return TestCatalogService()


def _seed_tc(svc: TestCatalogService, tc_id: str, status: str = "active") -> None:
    from models.test_case import TestCaseCreate
    payload = TestCaseCreate(
        test_case_id=tc_id,
        name=f"Test {tc_id}",
        module="demo",
        type="smoke",
        priority="medium",
        status=status,
        steps=[{"action": "goto", "value": "https://example.com"}],
        assertions=[],
    )
    svc.create_test_case(payload)


def _make_run(tc_id: str, status: str = "pass") -> TestRun:
    return TestRun(
        test_case_id=tc_id,
        environment="default",
        status=status,
        duration_ms=100,
    )


# ── OrchestratorJob model ─────────────────────────────────────────────────────

class TestOrchestratorJobModel:
    def test_defaults(self):
        job = OrchestratorJob(test_case_ids=["TC-A"], total_count=1)
        assert job.status == "queued"
        assert job.job_type == "single"
        assert job.completed_count == 0
        assert job.passed_count == 0
        assert job.failed_count == 0
        assert job.error_count == 0
        assert job.run_ids == []
        assert job.results == []
        assert job.started_at is None
        assert job.finished_at is None

    def test_duration_ms_none_until_finished(self):
        job = OrchestratorJob(test_case_ids=["TC-A"], total_count=1)
        assert job.duration_ms is None

    def test_duration_ms_computed(self):
        from datetime import datetime, timezone, timedelta
        job = OrchestratorJob(test_case_ids=["TC-A"], total_count=1)
        t0 = datetime.now(timezone.utc)
        job.started_at  = t0
        job.finished_at = t0 + timedelta(milliseconds=1500)
        assert job.duration_ms == 1500

    def test_job_id_is_uuid(self):
        job = OrchestratorJob(test_case_ids=["TC-A"], total_count=1)
        import uuid
        uuid.UUID(job.job_id)   # should not raise


# ── enqueue_single ────────────────────────────────────────────────────────────

class TestEnqueueSingle:
    def setup_method(self):
        self.catalog = _fresh_catalog()
        self.orch    = _fresh_orch()
        _seed_tc(self.catalog, "TC-S-001")

    def test_returns_queued_job(self):
        job = self.orch.enqueue_single("TC-S-001")
        assert isinstance(job, OrchestratorJob)
        assert job.status == "queued"
        assert job.job_type == "single"
        assert job.test_case_ids == ["TC-S-001"]
        assert job.total_count == 1

    def test_job_stored_immediately(self):
        job = self.orch.enqueue_single("TC-S-001")
        stored = self.orch.get_job(job.job_id)
        assert stored is not None
        assert stored.job_id == job.job_id

    def test_unknown_tc_raises(self):
        with pytest.raises(ValueError, match="not found"):
            self.orch.enqueue_single("TC-GHOST-999")

    def test_environment_stored(self):
        job = self.orch.enqueue_single("TC-S-001", environment="staging")
        assert job.environment == "staging"


# ── enqueue_suite ─────────────────────────────────────────────────────────────

class TestEnqueueSuite:
    def setup_method(self):
        self.catalog = _fresh_catalog()
        self.orch    = _fresh_orch()
        _seed_tc(self.catalog, "TC-SUITE-001")
        _seed_tc(self.catalog, "TC-SUITE-002")
        _seed_tc(self.catalog, "TC-SUITE-INV", status="inactive")

    def test_explicit_ids(self):
        job = self.orch.enqueue_suite(test_case_ids=["TC-SUITE-001", "TC-SUITE-002"])
        assert job.status == "queued"
        assert job.total_count == 2
        assert set(job.test_case_ids) == {"TC-SUITE-001", "TC-SUITE-002"}

    def test_inactive_ids_skipped(self):
        job = self.orch.enqueue_suite(
            test_case_ids=["TC-SUITE-001", "TC-SUITE-INV"]
        )
        assert "TC-SUITE-INV" not in job.test_case_ids
        assert job.total_count == 1

    def test_unknown_ids_skipped(self):
        job = self.orch.enqueue_suite(
            test_case_ids=["TC-SUITE-001", "TC-DOES-NOT-EXIST"]
        )
        assert "TC-DOES-NOT-EXIST" not in job.test_case_ids
        assert job.total_count == 1

    def test_empty_ids_gives_failed_job(self):
        job = self.orch.enqueue_suite(test_case_ids=[])
        assert job.status == "failed"
        assert job.error_message is not None

    def test_no_matching_filters_gives_failed_job(self):
        job = self.orch.enqueue_suite(module="nonexistent-module")
        assert job.status == "failed"

    def test_filter_by_module(self):
        # Both seeded cases use module="demo"
        job = self.orch.enqueue_suite(module="demo")
        assert job.status == "queued"
        assert job.total_count == 2

    def test_suite_job_type(self):
        job = self.orch.enqueue_suite(test_case_ids=["TC-SUITE-001"])
        assert job.job_type == "suite"


# ── Job store: list / get ──────────────────────────────────────────────────────

class TestJobStore:
    def setup_method(self):
        self.catalog = _fresh_catalog()
        self.orch    = _fresh_orch()
        _seed_tc(self.catalog, "TC-STORE-001")

    def test_list_empty(self):
        assert self.orch.list_jobs() == []

    def test_list_returns_most_recent_first(self):
        j1 = self.orch.enqueue_single("TC-STORE-001")
        j2 = self.orch.enqueue_single("TC-STORE-001")
        jobs = self.orch.list_jobs()
        assert jobs[0].job_id == j2.job_id   # newest first
        assert jobs[1].job_id == j1.job_id

    def test_get_unknown_returns_none(self):
        assert self.orch.get_job("does-not-exist") is None

    def test_list_limit(self):
        _seed_tc(self.catalog, "TC-STORE-002")
        for _ in range(5):
            self.orch.enqueue_single("TC-STORE-001")
        jobs = self.orch.list_jobs(limit=2)
        assert len(jobs) == 2


# ── Status transitions ────────────────────────────────────────────────────────

class TestStatusTransitions:
    """
    Tests for _run_job() using a mocked run_test_case to avoid real Playwright.
    We call _run_job directly and examine the job after it finishes.
    """

    def setup_method(self):
        self.catalog = _fresh_catalog()
        self.orch    = _fresh_orch()
        _seed_tc(self.catalog, "TC-T-001")
        _seed_tc(self.catalog, "TC-T-002")
        _seed_tc(self.catalog, "TC-T-003")

    def _run_direct(self, job: OrchestratorJob, side_effects):
        """Patch run_test_case and call _run_job synchronously."""
        with patch.object(
            svc_module.catalog_service.__class__,
            "run_test_case",
            side_effect=side_effects,
        ):
            orch_module._run_job(job)

    def test_all_pass_gives_completed(self):
        job = OrchestratorJob(
            job_type="single",
            test_case_ids=["TC-T-001"],
            total_count=1,
            environment="default",
        )
        orch_module._save_job(job)
        self._run_direct(job, [_make_run("TC-T-001", "pass")])
        assert job.status == "completed"
        assert job.passed_count == 1
        assert job.failed_count == 0

    def test_all_fail_gives_failed(self):
        job = OrchestratorJob(
            job_type="suite",
            test_case_ids=["TC-T-001", "TC-T-002"],
            total_count=2,
            environment="default",
        )
        orch_module._save_job(job)
        self._run_direct(job, [
            _make_run("TC-T-001", "fail"),
            _make_run("TC-T-002", "fail"),
        ])
        assert job.status == "failed"
        assert job.passed_count == 0
        assert job.failed_count == 2

    def test_mix_gives_partial(self):
        job = OrchestratorJob(
            job_type="suite",
            test_case_ids=["TC-T-001", "TC-T-002"],
            total_count=2,
            environment="default",
        )
        orch_module._save_job(job)
        self._run_direct(job, [
            _make_run("TC-T-001", "pass"),
            _make_run("TC-T-002", "fail"),
        ])
        assert job.status == "partial"
        assert job.passed_count == 1
        assert job.failed_count == 1

    def test_error_run_increments_error_count(self):
        job = OrchestratorJob(
            job_type="single",
            test_case_ids=["TC-T-001"],
            total_count=1,
            environment="default",
        )
        orch_module._save_job(job)
        self._run_direct(job, [_make_run("TC-T-001", "error")])
        assert job.error_count == 1
        assert job.status == "failed"   # 0 passed → failed

    def test_exception_in_run_captured(self):
        job = OrchestratorJob(
            job_type="single",
            test_case_ids=["TC-T-001"],
            total_count=1,
            environment="default",
        )
        orch_module._save_job(job)
        self._run_direct(job, [RuntimeError("boom")])
        assert job.error_count == 1
        assert job.completed_count == 1
        assert job.results[0]["status"] == "error"
        assert "boom" in job.results[0]["error"]

    def test_timestamps_set(self):
        job = OrchestratorJob(
            job_type="single",
            test_case_ids=["TC-T-001"],
            total_count=1,
            environment="default",
        )
        orch_module._save_job(job)
        self._run_direct(job, [_make_run("TC-T-001", "pass")])
        assert job.started_at is not None
        assert job.finished_at is not None
        assert job.finished_at >= job.started_at

    def test_counters_accurate_for_three_tests(self):
        job = OrchestratorJob(
            job_type="suite",
            test_case_ids=["TC-T-001", "TC-T-002", "TC-T-003"],
            total_count=3,
            environment="default",
        )
        orch_module._save_job(job)
        self._run_direct(job, [
            _make_run("TC-T-001", "pass"),
            _make_run("TC-T-002", "fail"),
            _make_run("TC-T-003", "error"),
        ])
        assert job.completed_count == 3
        assert job.passed_count   == 1
        assert job.failed_count   == 1
        assert job.error_count    == 1
        assert job.status == "partial"

    def test_run_ids_appended(self):
        job = OrchestratorJob(
            job_type="suite",
            test_case_ids=["TC-T-001", "TC-T-002"],
            total_count=2,
            environment="default",
        )
        orch_module._save_job(job)
        r1 = _make_run("TC-T-001", "pass")
        r2 = _make_run("TC-T-002", "pass")
        self._run_direct(job, [r1, r2])
        assert r1.run_id in job.run_ids
        assert r2.run_id in job.run_ids


# ── Concurrency limit (unit-level) ────────────────────────────────────────────

class TestConcurrencyLimit:
    """
    Verify that the semaphore actually bounds concurrent execution.
    We do this without spinning up the full worker loop.
    """

    def test_semaphore_value(self):
        # The semaphore internal counter should equal MAX_CONCURRENCY at rest
        # (assuming no jobs are running)
        sem = orch_module._SEM
        # Acquire all slots, then verify no more can be acquired without blocking
        slots = []
        acquired = 0
        while sem.acquire(blocking=False):
            slots.append(True)
            acquired += 1

        assert acquired == orch_module.MAX_CONCURRENCY

        # Release them all
        for _ in slots:
            sem.release()

    def test_max_concurrency_env_default(self):
        assert orch_module.MAX_CONCURRENCY >= 1
