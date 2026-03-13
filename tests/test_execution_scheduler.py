# tests/test_execution_scheduler.py
"""
Tests for the parallel execution scheduler (Block 13).

Coverage:
1.  Worker pool initializes with correct configuration
2.  Bounded concurrency is respected (per-type semaphores have correct capacity)
3.  Scheduling sorts critical tests before low-priority tests
4.  Scheduling sorts smoke tests before regression tests
5.  Mixed UI/API dispatch goes through correct semaphores
6.  Batch execution completes and updates job progress counters
7.  Retry policy retries on error status
8.  No retry on assertion failure (status=fail)
9.  Retry limit is respected (stops after EXECUTION_RETRY_LIMIT+1 attempts)
10. Execution status API returns expected fields
11. retry_failed_tests creates a new job for failed tests only
12. retry_failed_tests returns None when original job has no failures
13. Backward compatibility: enqueue_single still works
14. Backward compatibility: enqueue_suite still works
15. run-batch route returns 422 for empty test_case_ids
"""
from __future__ import annotations

import time
from typing import Any, Dict, List
from unittest.mock import MagicMock, patch

import pytest


# ── Shared reset helpers ───────────────────────────────────────────────────────

def _orch_reset():
    from services.catalog_orchestrator import _reset_for_testing
    _reset_for_testing()


def _cat_reset():
    try:
        from services.test_catalog_service import _reset_for_testing as _r
        _r()
    except Exception:
        pass


def _db_reset():
    """Wipe all catalog/run/job rows for a clean slate."""
    try:
        from services.db.catalog_repository import catalog_repo
        from services.db.test_run_repository import test_run_repo
        from services.db.orchestrator_job_repository import orch_job_repo
        from services.db.sqlite_db import get_session
        from services.db.catalog_repository import TestCaseRow
        from services.db.test_run_repository import TestRunRow
        from services.db.orchestrator_job_repository import OrchestratorJobRow
        with get_session() as s:
            s.query(TestRunRow).delete()
            s.query(OrchestratorJobRow).delete()
            s.query(TestCaseRow).delete()
    except Exception:
        pass


def _wait_job(job_id: str, timeout_s: float = 8.0):
    """Poll until job leaves queued/running state, then return it."""
    from services.catalog_orchestrator import _get_job
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        j = _get_job(job_id)
        if j and j.status not in ("queued", "running"):
            return j
        time.sleep(0.05)
    return _get_job(job_id)


def _make_fake_run(status: str = "pass"):
    run = MagicMock()
    run.run_id     = f"RUN-FAKE-{status}"
    run.status     = status
    run.duration_ms = 100
    return run


def _make_test_case(tc_id: str, priority: str = "medium", type_: str = "smoke",
                     test_type: str = "ui", status: str = "active"):
    from models.test_case import TestCaseCreate
    return TestCaseCreate(
        test_case_id = tc_id,
        name         = f"Test {tc_id}",
        module       = "test_module",
        type         = type_,
        priority     = priority,
        status       = status,
        test_type    = test_type,
        steps        = [{"action": "goto", "url": "https://example.com"}],
    )


# ── 1. Worker pool configuration ──────────────────────────────────────────────

class TestWorkerPoolConfig:

    def test_constants_exist_and_positive(self):
        from services.catalog_orchestrator import (
            EXECUTION_MAX_WORKERS,
            EXECUTION_MAX_UI_WORKERS,
            EXECUTION_MAX_API_WORKERS,
            EXECUTION_RETRY_LIMIT,
        )
        assert EXECUTION_MAX_WORKERS >= 1
        assert EXECUTION_MAX_UI_WORKERS >= 1
        assert EXECUTION_MAX_API_WORKERS >= 1
        assert EXECUTION_RETRY_LIMIT >= 0

    def test_semaphores_exist(self):
        import threading
        from services.catalog_orchestrator import _UI_SEM, _API_SEM
        assert isinstance(_UI_SEM, threading.Semaphore)
        assert isinstance(_API_SEM, threading.Semaphore)

    def test_ui_sem_capacity_matches_config(self):
        from services.catalog_orchestrator import _UI_SEM, EXECUTION_MAX_UI_WORKERS
        # Acquire all slots then verify we cannot acquire one more
        acquired = []
        for _ in range(EXECUTION_MAX_UI_WORKERS):
            ok = _UI_SEM.acquire(blocking=False)
            acquired.append(ok)
        exhausted = not _UI_SEM.acquire(blocking=False)
        # Release all
        for _ in acquired:
            _UI_SEM.release()
        assert all(acquired), "Should acquire all UI semaphore slots"
        assert exhausted, "UI semaphore should be exhausted when all slots taken"

    def test_stats_dict_initialized(self):
        from services.catalog_orchestrator import _STATS
        for key in ("active_workers", "running_ui", "running_api",
                    "queued_tasks", "completed_tasks", "retried_tasks"):
            assert key in _STATS


# ── 2. Scheduling ─────────────────────────────────────────────────────────────

class TestScheduling:

    def setup_method(self):
        _orch_reset()
        _cat_reset()
        _db_reset()

    def _create_tc(self, tc_id, priority="medium", type_="smoke"):
        from services.test_catalog_service import catalog_service
        tc = _make_test_case(tc_id, priority=priority, type_=type_)
        catalog_service.create_test_case(tc)
        return tc_id

    def test_critical_scheduled_before_low(self):
        from services.catalog_orchestrator import _schedule_tests
        id_critical = self._create_tc("SCHED-001", priority="critical", type_="smoke")
        id_low      = self._create_tc("SCHED-002", priority="low",      type_="regression")

        sorted_ids, notes = _schedule_tests([id_low, id_critical])

        assert sorted_ids[0] == id_critical
        assert sorted_ids[1] == id_low

    def test_smoke_before_regression_same_priority(self):
        from services.catalog_orchestrator import _schedule_tests
        id_regression = self._create_tc("SCHED-003", priority="high", type_="regression")
        id_smoke      = self._create_tc("SCHED-004", priority="high", type_="smoke")

        sorted_ids, _ = _schedule_tests([id_regression, id_smoke])

        assert sorted_ids[0] == id_smoke
        assert sorted_ids[1] == id_regression

    def test_scheduling_returns_non_empty_notes(self):
        from services.catalog_orchestrator import _schedule_tests
        id1 = self._create_tc("SCHED-005", priority="critical", type_="smoke")
        id2 = self._create_tc("SCHED-006", priority="medium",   type_="functional")

        _, notes = _schedule_tests([id1, id2])

        assert isinstance(notes, str)
        assert len(notes) > 0

    def test_scheduling_unknown_id_does_not_crash(self):
        from services.catalog_orchestrator import _schedule_tests
        sorted_ids, notes = _schedule_tests(["DOES-NOT-EXIST"])
        assert sorted_ids == ["DOES-NOT-EXIST"]

    def test_scheduling_empty_list(self):
        from services.catalog_orchestrator import _schedule_tests
        sorted_ids, notes = _schedule_tests([])
        assert sorted_ids == []


# ── 3. Mixed UI/API dispatch ──────────────────────────────────────────────────

class TestTestTypeDispatch:

    def test_get_test_type_ui(self):
        from services.catalog_orchestrator import _get_test_type
        _orch_reset(); _cat_reset(); _db_reset()
        from services.test_catalog_service import catalog_service
        tc = _make_test_case("DISP-001", test_type="ui")
        catalog_service.create_test_case(tc)
        assert _get_test_type("DISP-001") == "ui"

    def test_get_test_type_api(self):
        from services.catalog_orchestrator import _get_test_type
        _orch_reset(); _cat_reset(); _db_reset()
        from services.test_catalog_service import catalog_service
        tc = _make_test_case("DISP-002", test_type="api")
        catalog_service.create_test_case(tc)
        assert _get_test_type("DISP-002") == "api"

    def test_get_test_type_unknown_defaults_to_ui(self):
        from services.catalog_orchestrator import _get_test_type
        result = _get_test_type("TOTALLY-UNKNOWN-ID")
        assert result == "ui"


# ── 4. Batch execution with mocked runner ─────────────────────────────────────

class TestBatchExecution:

    def setup_method(self):
        _orch_reset()
        _cat_reset()
        _db_reset()
        from services.catalog_orchestrator import ensure_worker_started
        ensure_worker_started()

    def _create_active_tc(self, tc_id, priority="medium", type_="smoke"):
        from services.test_catalog_service import catalog_service
        tc = _make_test_case(tc_id, priority=priority, type_=type_, status="active")
        catalog_service.create_test_case(tc)
        return tc_id

    def test_batch_job_completes_all_pass(self):
        id1 = self._create_active_tc("BATCH-001")
        id2 = self._create_active_tc("BATCH-002")

        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            return_value=_make_fake_run("pass"),
        ):
            from services.catalog_orchestrator import orchestrator_service
            job = orchestrator_service.enqueue_suite(test_case_ids=[id1, id2])
            finished = _wait_job(job.job_id, timeout_s=10)

        assert finished is not None
        assert finished.status == "completed"
        assert finished.passed_count == 2
        assert finished.failed_count == 0
        assert finished.error_count  == 0
        assert finished.completed_count == 2

    def test_batch_job_partial_when_some_fail(self):
        id1 = self._create_active_tc("BATCH-003")
        id2 = self._create_active_tc("BATCH-004")

        call_count = [0]
        def _side_effect(*args, **kwargs):
            call_count[0] += 1
            if call_count[0] == 1:
                return _make_fake_run("pass")
            return _make_fake_run("fail")

        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            side_effect=_side_effect,
        ):
            from services.catalog_orchestrator import orchestrator_service
            job = orchestrator_service.enqueue_suite(test_case_ids=[id1, id2])
            finished = _wait_job(job.job_id, timeout_s=10)

        assert finished is not None
        assert finished.status == "partial"
        assert finished.passed_count == 1
        assert finished.failed_count == 1

    def test_job_has_scheduling_notes(self):
        id1 = self._create_active_tc("BATCH-005", priority="critical", type_="smoke")

        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            return_value=_make_fake_run("pass"),
        ):
            from services.catalog_orchestrator import orchestrator_service
            job = orchestrator_service.enqueue_suite(test_case_ids=[id1])
            finished = _wait_job(job.job_id, timeout_s=10)

        assert finished is not None
        assert finished.scheduling_notes is not None
        assert len(finished.scheduling_notes) > 0


# ── 5. Retry policy ───────────────────────────────────────────────────────────

class TestRetryPolicy:

    def setup_method(self):
        _orch_reset()
        _cat_reset()
        _db_reset()
        from services.catalog_orchestrator import ensure_worker_started
        ensure_worker_started()

    def _create_active_tc(self, tc_id):
        from services.test_catalog_service import catalog_service
        tc = _make_test_case(tc_id, status="active")
        catalog_service.create_test_case(tc)
        return tc_id

    def test_retry_on_error_status(self):
        """A test that returns error on first attempt is retried."""
        id1 = self._create_active_tc("RETRY-001")

        attempt_results = [_make_fake_run("error"), _make_fake_run("pass")]
        call_count = [0]

        def _side_effect(*args, **kwargs):
            result = attempt_results[min(call_count[0], len(attempt_results) - 1)]
            call_count[0] += 1
            return result

        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            side_effect=_side_effect,
        ):
            from services.catalog_orchestrator import _run_single_test
            from models.orchestrator_job import OrchestratorJob
            job = OrchestratorJob(test_case_ids=[id1], total_count=1, environment="test")

            with patch("services.catalog_orchestrator.EXECUTION_RETRY_LIMIT", 1):
                result = _run_single_test(job, id1, retry_limit=1)

        assert result["status"] == "pass"
        assert result["attempt"] == 2
        assert call_count[0] == 2

    def test_no_retry_on_assertion_failure(self):
        """A test that returns fail (assertion) is NOT retried."""
        id1 = self._create_active_tc("RETRY-002")
        call_count = [0]

        def _side_effect(*args, **kwargs):
            call_count[0] += 1
            return _make_fake_run("fail")

        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            side_effect=_side_effect,
        ):
            from services.catalog_orchestrator import _run_single_test
            from models.orchestrator_job import OrchestratorJob
            job = OrchestratorJob(test_case_ids=[id1], total_count=1, environment="test")
            result = _run_single_test(job, id1, retry_limit=1)

        assert result["status"] == "fail"
        assert result["attempt"] == 1
        assert call_count[0] == 1   # only one call, no retry

    def test_retry_limit_respected(self):
        """Retries stop after retry_limit attempts even if always error."""
        id1 = self._create_active_tc("RETRY-003")
        call_count = [0]

        def _always_error(*args, **kwargs):
            call_count[0] += 1
            return _make_fake_run("error")

        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            side_effect=_always_error,
        ):
            from services.catalog_orchestrator import _run_single_test
            from models.orchestrator_job import OrchestratorJob
            job = OrchestratorJob(test_case_ids=[id1], total_count=1, environment="test")
            result = _run_single_test(job, id1, retry_limit=2)

        assert result["status"] == "error"
        assert result["attempt"] == 3          # 3 total attempts (0, 1, 2)
        assert call_count[0] == 3

    def test_retry_on_runner_exception(self):
        """Runner exception is treated as retriable error."""
        id1 = self._create_active_tc("RETRY-004")
        call_count = [0]

        def _crash_then_pass(*args, **kwargs):
            call_count[0] += 1
            if call_count[0] == 1:
                raise RuntimeError("Connection timeout")
            return _make_fake_run("pass")

        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            side_effect=_crash_then_pass,
        ):
            from services.catalog_orchestrator import _run_single_test
            from models.orchestrator_job import OrchestratorJob
            job = OrchestratorJob(test_case_ids=[id1], total_count=1, environment="test")
            result = _run_single_test(job, id1, retry_limit=1)

        assert result["status"] == "pass"
        assert result["attempt"] == 2


# ── 6. Execution status ───────────────────────────────────────────────────────

class TestExecutionStatus:

    def setup_method(self):
        _orch_reset()

    def test_status_returns_all_expected_fields(self):
        from services.catalog_orchestrator import get_execution_status
        status = get_execution_status()

        expected_keys = {
            "active_jobs", "queued_jobs", "active_workers", "queue_depth",
            "max_workers", "max_ui_workers", "max_api_workers",
            "running_ui_workers", "running_api_workers",
            "queued_tasks", "running_tasks", "completed_tasks", "retried_tasks",
        }
        assert expected_keys <= set(status.keys())

    def test_status_values_are_non_negative(self):
        from services.catalog_orchestrator import get_execution_status
        status = get_execution_status()
        for k, v in status.items():
            assert isinstance(v, int), f"{k} should be int"
            assert v >= 0, f"{k} should be >= 0 (got {v})"

    def test_status_max_workers_matches_config(self):
        from services.catalog_orchestrator import get_execution_status, EXECUTION_MAX_WORKERS
        status = get_execution_status()
        assert status["max_workers"] == EXECUTION_MAX_WORKERS

    def test_status_route_returns_200(self):
        from fastapi.testclient import TestClient
        from app import app
        client = TestClient(app)
        resp = client.get("/execution/status")
        assert resp.status_code == 200
        data = resp.json()
        assert "active_workers" in data
        assert "max_workers" in data

    def test_health_route(self):
        from fastapi.testclient import TestClient
        from app import app
        client = TestClient(app)
        resp = client.get("/execution/health")
        assert resp.status_code == 200
        assert resp.json()["status"] == "ok"


# ── 7. retry_failed_tests ────────────────────────────────────────────────────

class TestRetryFailed:

    def setup_method(self):
        _orch_reset()
        _cat_reset()
        _db_reset()

    def test_retry_failed_creates_new_job(self):
        from services.catalog_orchestrator import _JOB_STORE, retry_failed_tests, _save_job
        from models.orchestrator_job import OrchestratorJob

        original = OrchestratorJob(
            job_type      = "suite",
            test_case_ids = ["TC-001", "TC-002"],
            total_count   = 2,
            status        = "partial",
            results       = [
                {"test_case_id": "TC-001", "status": "pass", "run_id": "R1"},
                {"test_case_id": "TC-002", "status": "fail", "run_id": "R2"},
            ],
        )
        _save_job(original)

        new_job = retry_failed_tests(original.job_id)

        assert new_job is not None
        assert new_job.job_id != original.job_id
        assert "TC-002" in new_job.test_case_ids
        assert "TC-001" not in new_job.test_case_ids
        assert new_job.total_count == 1
        assert new_job.status == "queued"

    def test_retry_failed_includes_error_status(self):
        from services.catalog_orchestrator import retry_failed_tests, _save_job
        from models.orchestrator_job import OrchestratorJob

        original = OrchestratorJob(
            job_type      = "suite",
            test_case_ids = ["TC-003", "TC-004"],
            total_count   = 2,
            status        = "failed",
            results       = [
                {"test_case_id": "TC-003", "status": "error", "run_id": None},
                {"test_case_id": "TC-004", "status": "error", "run_id": None},
            ],
        )
        _save_job(original)

        new_job = retry_failed_tests(original.job_id)
        assert new_job is not None
        assert len(new_job.test_case_ids) == 2

    def test_retry_failed_returns_none_when_no_failures(self):
        from services.catalog_orchestrator import retry_failed_tests, _save_job
        from models.orchestrator_job import OrchestratorJob

        original = OrchestratorJob(
            job_type      = "suite",
            test_case_ids = ["TC-005"],
            total_count   = 1,
            status        = "completed",
            results       = [
                {"test_case_id": "TC-005", "status": "pass", "run_id": "R5"},
            ],
        )
        _save_job(original)

        result = retry_failed_tests(original.job_id)
        assert result is None

    def test_retry_failed_returns_none_for_unknown_job(self):
        from services.catalog_orchestrator import retry_failed_tests
        result = retry_failed_tests("DOES-NOT-EXIST-JOB-ID")
        assert result is None


# ── 8. Backward compatibility ─────────────────────────────────────────────────

class TestBackwardCompatibility:

    def setup_method(self):
        _orch_reset()
        _cat_reset()
        _db_reset()
        from services.catalog_orchestrator import ensure_worker_started
        ensure_worker_started()

    def _create_active_tc(self, tc_id):
        from services.test_catalog_service import catalog_service
        tc = _make_test_case(tc_id, status="active")
        catalog_service.create_test_case(tc)
        return tc_id

    def test_enqueue_single_returns_queued_job(self):
        id1 = self._create_active_tc("BC-001")
        from services.catalog_orchestrator import orchestrator_service
        job = orchestrator_service.enqueue_single(id1)
        assert job.job_id
        assert job.status == "queued"
        assert job.test_case_ids == [id1]
        assert job.total_count == 1

    def test_enqueue_single_unknown_raises(self):
        from services.catalog_orchestrator import orchestrator_service
        with pytest.raises(ValueError, match="not found"):
            orchestrator_service.enqueue_single("DOES-NOT-EXIST")

    def test_enqueue_suite_returns_queued_job(self):
        id1 = self._create_active_tc("BC-002")
        id2 = self._create_active_tc("BC-003")
        from services.catalog_orchestrator import orchestrator_service
        job = orchestrator_service.enqueue_suite(test_case_ids=[id1, id2])
        assert job.status == "queued"
        assert job.total_count == 2

    def test_enqueue_suite_empty_returns_failed(self):
        from services.catalog_orchestrator import orchestrator_service
        job = orchestrator_service.enqueue_suite(test_case_ids=[])
        assert job.status == "failed"

    def test_single_job_completes_with_mocked_runner(self):
        id1 = self._create_active_tc("BC-004")
        with patch(
            "services.catalog_orchestrator.catalog_service.run_test_case",
            return_value=_make_fake_run("pass"),
        ):
            from services.catalog_orchestrator import orchestrator_service
            job = orchestrator_service.enqueue_single(id1)
            finished = _wait_job(job.job_id, timeout_s=10)

        assert finished is not None
        assert finished.status == "completed"
        assert finished.passed_count == 1


# ── 9. run-batch route ────────────────────────────────────────────────────────

class TestRunBatchRoute:

    def setup_method(self):
        _orch_reset()
        _cat_reset()
        _db_reset()

    def test_run_batch_returns_422_for_empty_ids(self):
        from fastapi.testclient import TestClient
        from app import app
        client = TestClient(app)
        resp = client.post("/execution/run-batch", json={"test_case_ids": []})
        assert resp.status_code == 422

    def test_run_batch_returns_202_for_valid_ids(self):
        from services.test_catalog_service import catalog_service
        tc = _make_test_case("ROUTE-001", status="active")
        catalog_service.create_test_case(tc)

        from fastapi.testclient import TestClient
        from app import app
        client = TestClient(app)
        resp = client.post("/execution/run-batch", json={"test_case_ids": ["ROUTE-001"]})
        assert resp.status_code == 202
        data = resp.json()
        assert data["ok"] is True
        assert "job_id" in data

    def test_retry_failed_route_returns_404_for_unknown_job(self):
        from fastapi.testclient import TestClient
        from app import app
        client = TestClient(app)
        resp = client.post(
            "/execution/retry-failed",
            json={"job_id": "TOTALLY-FAKE-JOB-ID"},
        )
        assert resp.status_code == 404
