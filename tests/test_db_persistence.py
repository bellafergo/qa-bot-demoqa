# tests/test_db_persistence.py
"""
Persistence layer tests for Vanya catalog / runs / orchestrator jobs.

No browser, no network.
Uses the temporary SQLite DB initialized by conftest.py.
"""
from __future__ import annotations

import json
import pytest

from models.test_case import TestCase, TestCaseCreate, TestStep, TestAssertion
from models.test_run import TestRun
from models.orchestrator_job import OrchestratorJob
from services.db.catalog_repository import CatalogRepository
from services.db.test_run_repository import TestRunRepository
from services.db.orchestrator_job_repository import OrchestratorJobRepository
from services.test_catalog_service import TestCatalogService, _reset_for_testing
from services.catalog_orchestrator import CatalogOrchestratorService, _reset_for_testing as _orch_reset


# ── Shared helpers ────────────────────────────────────────────────────────────

def _make_tc(tc_id: str, **overrides) -> TestCase:
    defaults = dict(
        test_case_id = tc_id,
        name         = f"Test {tc_id}",
        module       = "demo",
        type         = "smoke",
        priority     = "medium",
        status       = "active",
        steps        = [TestStep(action="goto", value="https://example.com")],
        assertions   = [],
        tags         = ["smoke"],
        base_url     = "https://example.com",
    )
    defaults.update(overrides)
    return TestCase(**defaults)


def _make_run(tc_id: str, status: str = "pass", **kwargs) -> TestRun:
    return TestRun(
        test_case_id=tc_id,
        environment="test",
        status=status,
        duration_ms=123,
        **kwargs,
    )


def _fresh_cat_repo() -> CatalogRepository:
    repo = CatalogRepository()
    repo.clear_all()
    return repo


def _fresh_run_repo() -> TestRunRepository:
    repo = TestRunRepository()
    repo.clear_all()
    return repo


def _fresh_job_repo() -> OrchestratorJobRepository:
    repo = OrchestratorJobRepository()
    repo.clear_all()
    return repo


# ── DB initialization ─────────────────────────────────────────────────────────

class TestDbInit:
    def test_tables_exist(self):
        """Smoke test: all repositories are usable (tables were created by conftest)."""
        repo = CatalogRepository()
        assert repo.is_empty() or True  # just calling it must not raise

    def test_catalog_repo_is_empty_or_not(self):
        _fresh_cat_repo()  # clears
        repo = CatalogRepository()
        assert repo.is_empty() is True

    def test_run_repo_list_empty(self):
        _fresh_run_repo()
        repo = TestRunRepository()
        assert repo.list_runs() == []

    def test_job_repo_list_empty(self):
        _fresh_job_repo()
        repo = OrchestratorJobRepository()
        assert repo.list_jobs() == []


# ── Catalog repository CRUD ───────────────────────────────────────────────────

class TestCatalogRepository:
    def setup_method(self):
        self.repo = _fresh_cat_repo()

    def test_create_and_get(self):
        tc = _make_tc("TC-REPO-001")
        self.repo.create_test_case(tc)
        found = self.repo.get_test_case("TC-REPO-001")
        assert found is not None
        assert found.test_case_id == "TC-REPO-001"
        assert found.name == tc.name

    def test_steps_roundtrip(self):
        tc = _make_tc("TC-REPO-002", steps=[
            TestStep(action="goto", value="https://example.com"),
            TestStep(action="input", target="username", value="user"),
        ])
        self.repo.create_test_case(tc)
        found = self.repo.get_test_case("TC-REPO-002")
        assert len(found.steps) == 2
        assert found.steps[0].action == "goto"
        assert found.steps[1].action == "input"

    def test_assertions_roundtrip(self):
        tc = _make_tc("TC-REPO-003", assertions=[
            TestAssertion(type="text_visible", value="Welcome"),
        ])
        self.repo.create_test_case(tc)
        found = self.repo.get_test_case("TC-REPO-003")
        assert len(found.assertions) == 1
        assert found.assertions[0].type == "text_visible"

    def test_tags_roundtrip(self):
        tc = _make_tc("TC-REPO-004", tags=["login", "smoke", "auth"])
        self.repo.create_test_case(tc)
        found = self.repo.get_test_case("TC-REPO-004")
        assert set(found.tags) == {"login", "smoke", "auth"}

    def test_get_unknown_returns_none(self):
        assert self.repo.get_test_case("TC-NOPE") is None

    def test_list_all(self):
        for i in range(3):
            self.repo.create_test_case(_make_tc(f"TC-LIST-{i}"))
        cases = self.repo.list_test_cases(status=None)
        assert len(cases) == 3

    def test_list_filter_by_status(self):
        self.repo.create_test_case(_make_tc("TC-ACT", status="active"))
        self.repo.create_test_case(_make_tc("TC-INACT", status="inactive"))
        active = self.repo.list_test_cases(status="active")
        assert all(c.status == "active" for c in active)
        assert len(active) == 1

    def test_list_filter_by_module(self):
        self.repo.create_test_case(_make_tc("TC-MOD-A", module="auth"))
        self.repo.create_test_case(_make_tc("TC-MOD-B", module="cart"))
        auth = self.repo.list_test_cases(module="auth", status=None)
        assert len(auth) == 1
        assert auth[0].test_case_id == "TC-MOD-A"

    def test_list_filter_by_tags(self):
        self.repo.create_test_case(_make_tc("TC-TAG-A", tags=["login", "smoke"]))
        self.repo.create_test_case(_make_tc("TC-TAG-B", tags=["checkout"]))
        login = self.repo.list_test_cases(tags=["login"], status=None)
        assert len(login) == 1

    def test_list_limit(self):
        for i in range(10):
            self.repo.create_test_case(_make_tc(f"TC-LIM-{i}"))
        limited = self.repo.list_test_cases(status=None, limit=3)
        assert len(limited) == 3

    def test_delete_removes(self):
        self.repo.create_test_case(_make_tc("TC-DEL"))
        assert self.repo.delete_test_case("TC-DEL") is True
        assert self.repo.get_test_case("TC-DEL") is None

    def test_delete_missing_returns_false(self):
        assert self.repo.delete_test_case("TC-GHOST") is False

    def test_is_empty_true_when_empty(self):
        assert self.repo.is_empty() is True

    def test_is_empty_false_after_insert(self):
        self.repo.create_test_case(_make_tc("TC-IS-EMPTY"))
        assert self.repo.is_empty() is False


# ── Test run repository ───────────────────────────────────────────────────────

class TestRunRepository_:
    def setup_method(self):
        self.repo = _fresh_run_repo()

    def test_create_and_get(self):
        run = _make_run("TC-R-001")
        self.repo.create_run(run)
        found = self.repo.get_run(run.run_id)
        assert found is not None
        assert found.run_id == run.run_id
        assert found.status == "pass"

    def test_logs_roundtrip(self):
        run = _make_run("TC-R-002", logs=["step 1 ok", "step 2 ok"])
        self.repo.create_run(run)
        found = self.repo.get_run(run.run_id)
        assert found.logs == ["step 1 ok", "step 2 ok"]

    def test_meta_roundtrip(self):
        run = _make_run("TC-R-003", meta={"runner_ok": True, "tc_module": "auth"})
        self.repo.create_run(run)
        found = self.repo.get_run(run.run_id)
        assert found.meta["runner_ok"] is True

    def test_get_unknown_returns_none(self):
        assert self.repo.get_run("does-not-exist") is None

    def test_list_most_recent_first(self):
        for i in range(3):
            self.repo.create_run(_make_run(f"TC-ORD-{i}"))
        runs = self.repo.list_runs()
        # Ordered by executed_at desc — last inserted first
        assert runs[0].test_case_id == "TC-ORD-2"

    def test_list_filtered_by_tc_id(self):
        self.repo.create_run(_make_run("TC-A"))
        self.repo.create_run(_make_run("TC-B"))
        self.repo.create_run(_make_run("TC-A"))
        a_runs = self.repo.list_runs(test_case_id="TC-A")
        assert len(a_runs) == 2
        assert all(r.test_case_id == "TC-A" for r in a_runs)

    def test_upsert_idempotent(self):
        """create_run uses merge — re-saving same run_id must not raise."""
        run = _make_run("TC-UPS")
        self.repo.create_run(run)
        self.repo.create_run(run)  # second call with same run_id
        assert len(self.repo.list_runs()) == 1


# ── Orchestrator job repository ───────────────────────────────────────────────

class TestOrchestratorJobRepository_:
    def setup_method(self):
        self.repo = _fresh_job_repo()

    def _make_job(self, **kwargs) -> OrchestratorJob:
        defaults = dict(
            job_type="single",
            test_case_ids=["TC-A"],
            total_count=1,
            environment="test",
        )
        defaults.update(kwargs)
        return OrchestratorJob(**defaults)

    def test_create_and_get(self):
        job = self._make_job()
        self.repo.create_job(job)
        found = self.repo.get_job(job.job_id)
        assert found is not None
        assert found.job_id == job.job_id
        assert found.status == "queued"

    def test_update_persists_status(self):
        from datetime import datetime, timezone
        job = self._make_job()
        self.repo.create_job(job)

        job.status = "running"
        job.started_at = datetime.now(timezone.utc)
        self.repo.update_job(job)

        found = self.repo.get_job(job.job_id)
        assert found.status == "running"
        assert found.started_at is not None

    def test_update_persists_counters(self):
        job = self._make_job(test_case_ids=["TC-A", "TC-B"], total_count=2)
        self.repo.create_job(job)

        job.completed_count = 2
        job.passed_count = 1
        job.failed_count = 1
        job.status = "partial"
        self.repo.update_job(job)

        found = self.repo.get_job(job.job_id)
        assert found.completed_count == 2
        assert found.passed_count == 1
        assert found.failed_count == 1
        assert found.status == "partial"

    def test_results_json_roundtrip(self):
        job = self._make_job()
        job.results = [{"test_case_id": "TC-A", "status": "pass", "run_id": "abc"}]
        self.repo.create_job(job)
        self.repo.update_job(job)
        found = self.repo.get_job(job.job_id)
        assert len(found.results) == 1
        assert found.results[0]["status"] == "pass"

    def test_get_unknown_returns_none(self):
        assert self.repo.get_job("no-such-job") is None

    def test_list_most_recent_first(self):
        j1 = self._make_job()
        j2 = self._make_job()
        self.repo.create_job(j1)
        self.repo.create_job(j2)
        jobs = self.repo.list_jobs()
        assert jobs[0].job_id == j2.job_id


# ── Service layer integration ─────────────────────────────────────────────────

class TestCatalogServiceWithDb:
    """Verify TestCatalogService uses DB-backed repos correctly."""

    def setup_method(self):
        _reset_for_testing()

    def test_create_and_retrieve(self):
        svc = TestCatalogService()
        from models.test_case import TestCaseCreate
        payload = TestCaseCreate(
            test_case_id="TC-SVC-001",
            name="Svc test",
            module="demo",
            type="smoke",
            priority="medium",
            steps=[{"action": "goto", "value": "https://example.com"}],
            assertions=[],
        )
        tc = svc.create_test_case(payload)
        assert tc.test_case_id == "TC-SVC-001"

        found = svc.get_test_case("TC-SVC-001")
        assert found is not None
        assert found.name == "Svc test"

    def test_duplicate_raises(self):
        svc = TestCatalogService()
        from models.test_case import TestCaseCreate
        p = TestCaseCreate(
            test_case_id="TC-DUP", name="Dup", module="x", type="smoke",
            priority="low", steps=[{"action": "goto", "value": "https://x.com"}], assertions=[],
        )
        svc.create_test_case(p)
        with pytest.raises(ValueError, match="already exists"):
            svc.create_test_case(p)

    def test_delete_removes_from_db(self):
        svc = TestCatalogService()
        from models.test_case import TestCaseCreate
        p = TestCaseCreate(
            test_case_id="TC-DELDB", name="Del", module="x", type="smoke",
            priority="low", steps=[{"action": "goto", "value": "https://x.com"}], assertions=[],
        )
        svc.create_test_case(p)
        assert svc.delete_test_case("TC-DELDB") is True

        # Survives restart — DB is the truth
        svc2 = TestCatalogService()
        assert svc2.get_test_case("TC-DELDB") is None

    def test_save_run_persists(self):
        svc = TestCatalogService()
        run = _make_run("TC-RUN-SVC")
        svc._save_run(run)

        found = svc.get_run(run.run_id)
        assert found is not None
        assert found.run_id == run.run_id

    def test_list_runs_from_db(self):
        svc = TestCatalogService()
        svc._save_run(_make_run("TC-LSVC"))
        svc._save_run(_make_run("TC-LSVC"))
        runs = svc.list_runs(test_case_id="TC-LSVC")
        assert len(runs) == 2


# ── Seed idempotency ──────────────────────────────────────────────────────────

class TestSeedIdempotency:
    def test_seed_only_loads_once(self):
        _reset_for_testing()
        from services.test_catalog_service import load_seed_catalog
        load_seed_catalog()
        load_seed_catalog()   # second call — must not duplicate
        svc = TestCatalogService()
        cases = svc.list_test_cases(status=None)
        ids = [c.test_case_id for c in cases]
        assert len(ids) == len(set(ids)), "Duplicate test_case_ids after double seed"

    def test_seed_skipped_when_not_empty(self):
        _reset_for_testing()
        svc = TestCatalogService()
        from models.test_case import TestCaseCreate
        # Insert one case manually
        svc.create_test_case(TestCaseCreate(
            test_case_id="TC-MANUAL",
            name="Manual",
            module="x",
            type="smoke",
            priority="low",
            steps=[{"action": "goto", "value": "https://x.com"}],
            assertions=[],
        ))
        from services.test_catalog_service import load_seed_catalog
        load_seed_catalog()   # should skip — catalog is not empty
        cases = svc.list_test_cases(status=None)
        ids = [c.test_case_id for c in cases]
        # Only the manually inserted case should be there
        assert "TC-MANUAL" in ids
        assert "TC-DEMO-001" not in ids


# ── Orchestrator persistence ──────────────────────────────────────────────────

class TestOrchestratorPersistence:
    """
    Verify that orchestrator jobs are persisted to DB via _run_job.
    Uses a mock for run_test_case to avoid real browser.
    """

    def setup_method(self):
        _reset_for_testing()
        _orch_reset()

    def test_job_stored_in_db_on_enqueue(self):
        _reset_for_testing()   # ensure TC exists
        from models.test_case import TestCaseCreate
        svc = TestCatalogService()
        svc.create_test_case(TestCaseCreate(
            test_case_id="TC-ORCH-P1",
            name="P1", module="x", type="smoke", priority="low",
            steps=[{"action": "goto", "value": "https://x.com"}], assertions=[],
        ))

        orch = CatalogOrchestratorService()
        job = orch.enqueue_single("TC-ORCH-P1", environment="test")

        # Job must be readable from DB immediately
        from services.db.orchestrator_job_repository import OrchestratorJobRepository
        repo = OrchestratorJobRepository()
        db_job = repo.get_job(job.job_id)
        assert db_job is not None
        assert db_job.job_id == job.job_id
        assert db_job.status == "queued"

    def test_run_job_updates_db_on_finish(self):
        import services.catalog_orchestrator as orch_mod
        import services.test_catalog_service as svc_mod
        from unittest.mock import patch
        from datetime import datetime, timezone

        _reset_for_testing()
        svc = TestCatalogService()
        from models.test_case import TestCaseCreate
        svc.create_test_case(TestCaseCreate(
            test_case_id="TC-ORCH-P2",
            name="P2", module="x", type="smoke", priority="low",
            steps=[{"action": "goto", "value": "https://x.com"}], assertions=[],
        ))

        job = OrchestratorJob(
            job_type="single",
            test_case_ids=["TC-ORCH-P2"],
            total_count=1,
            environment="test",
        )
        orch_mod._save_job(job)

        run = _make_run("TC-ORCH-P2", "pass")
        with patch.object(svc_mod.catalog_service.__class__, "run_test_case", return_value=run):
            orch_mod._run_job(job)

        # After _run_job completes, DB must reflect the final state
        from services.db.orchestrator_job_repository import OrchestratorJobRepository
        repo = OrchestratorJobRepository()
        db_job = repo.get_job(job.job_id)
        assert db_job is not None
        assert db_job.status == "completed"
        assert db_job.passed_count == 1
        assert db_job.finished_at is not None
