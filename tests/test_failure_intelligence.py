# tests/test_failure_intelligence.py
"""
Tests for Failure Intelligence (Block 14).

Coverage:
1.  Clustering groups selector failures together
2.  Clustering groups API failures together
3.  Clustering with no failures returns empty list
4.  Cluster filter by module
5.  Cluster filter by root_cause_category
6.  Flaky detection: alternating pass/fail → suspected_flaky=True
7.  Flaky detection: consistently passing → not flaky
8.  Flaky detection: consistently failing → not flaky
9.  Flaky detection: fewer than min_runs → not flaky
10. Flaky detection: flip_rate below threshold → not flaky
11. Regression detection: repeated failures flagged
12. Regression detection: single failure NOT flagged
13. Regression pattern has correct fields
14. Summary counts align with underlying data
15. Summary with empty history returns safe defaults
16. Health route
17. Summary route (200)
18. Clusters route (200)
19. Flaky-tests route (200)
20. Regressions route (200)
21. Dashboard summary includes failure intelligence fields
22. Existing tests still pass (non-regression guard)
"""
from __future__ import annotations

import time
from datetime import datetime, timezone
from typing import List
from unittest.mock import patch

import pytest


@pytest.fixture(autouse=True)
def _fi_tests_sqlite_history_only():
    """
    Failure Intelligence tests persist runs to SQLite only.

    When ``SUPABASE_*`` is set locally, ``run_history_service`` would otherwise
    read ``qa_runs`` and miss synthetic rows — force the SQLite read path here.

    Patch both ``qa_runs_read`` and ``run_history_service`` bindings: the latter
    keeps a module-level import of ``supabase_qa_runs_enabled`` that does not
    follow patches on ``qa_runs_read`` alone.
    """
    with (
        patch("services.qa_runs_read.supabase_qa_runs_enabled", return_value=False),
        patch("services.run_history_service.supabase_qa_runs_enabled", return_value=False),
    ):
        yield


# ── Shared DB / state helpers ─────────────────────────────────────────────────

def _db_reset():
    """Wipe all catalog/run/job rows for a clean slate."""
    try:
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


def _now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _make_run(tc_id: str, status: str, logs: List[str] = None, module: str = "test_module"):
    """Create and persist a synthetic TestRun."""
    from models.test_run import TestRun
    from services.db.test_run_repository import test_run_repo
    run = TestRun(
        test_case_id = tc_id,
        test_name    = f"Test {tc_id}",
        executed_at  = _now_iso(),
        environment  = "test",
        status       = status,
        logs         = logs or [],
        steps_result = [],
        meta         = {},
    )
    test_run_repo.create_run(run)
    return run


def _make_tc(tc_id: str, module: str = "test_module"):
    """Create and persist a synthetic TestCase in the catalog."""
    from models.test_case import TestCaseCreate
    from services.test_catalog_service import catalog_service
    try:
        catalog_service.create_test_case(TestCaseCreate(
            test_case_id = tc_id,
            name         = f"Test {tc_id}",
            module       = module,
            type         = "smoke",
            priority     = "medium",
            steps        = [{"action": "goto", "url": "https://example.com"}],
        ))
    except ValueError:
        pass  # already exists — ignore


# ── 1-5. Failure Clustering ───────────────────────────────────────────────────

class TestFailureClustering:

    def setup_method(self):
        _db_reset()

    def test_clusters_selector_failures_together(self):
        """Three runs with 'selector not found' logs in same module → one cluster."""
        from services.failure_intelligence_service import FailureIntelligenceService
        _make_tc("TC-SEL-001", module="checkout")
        for _ in range(3):
            _make_run("TC-SEL-001", "fail", logs=["selector not found in checkout page"])

        service = FailureIntelligenceService()
        clusters = service.get_clusters()

        sel_clusters = [c for c in clusters if c.root_cause_category == "selector_issue"]
        assert len(sel_clusters) >= 1
        biggest = max(sel_clusters, key=lambda c: c.total_failures)
        assert biggest.total_failures == 3
        assert biggest.module == "checkout"

    def test_clusters_api_failures_together(self):
        """Two runs with HTTP-500 logs → api_failure cluster."""
        from services.failure_intelligence_service import FailureIntelligenceService
        _make_tc("TC-API-001", module="payments")
        for _ in range(2):
            _make_run("TC-API-001", "error", logs=["500 Internal Server Error calling /api/pay"])

        service = FailureIntelligenceService()
        clusters = service.get_clusters()

        api_clusters = [c for c in clusters if c.root_cause_category == "api_failure"]
        assert len(api_clusters) >= 1
        assert api_clusters[0].total_failures == 2

    def test_no_failed_runs_returns_empty_clusters(self):
        """Only passing runs → no clusters."""
        from services.failure_intelligence_service import FailureIntelligenceService
        _make_tc("TC-PASS-001")
        for _ in range(5):
            _make_run("TC-PASS-001", "pass")

        service  = FailureIntelligenceService()
        clusters = service.get_clusters()
        assert clusters == []

    def test_cluster_filter_by_module(self):
        """Filter by module returns only matching cluster."""
        from services.failure_intelligence_service import FailureIntelligenceService
        _make_tc("TC-SEL-002", module="checkout")
        _make_tc("TC-API-002", module="payments")
        _make_run("TC-SEL-002", "fail", logs=["selector not found"])
        _make_run("TC-API-002", "error", logs=["500 Internal Server Error"])

        service = FailureIntelligenceService()
        clusters = service.get_clusters(module="checkout")
        assert all(c.module == "checkout" for c in clusters)

    def test_cluster_filter_by_rca_category(self):
        """Filter by root_cause_category keeps only matching clusters."""
        from services.failure_intelligence_service import FailureIntelligenceService
        _make_tc("TC-SEL-003", module="checkout")
        _make_tc("TC-API-003", module="payments")
        _make_run("TC-SEL-003", "fail", logs=["selector not found"])
        _make_run("TC-API-003", "error", logs=["500 Internal Server Error"])

        service = FailureIntelligenceService()
        clusters = service.get_clusters(root_cause_category="api_failure")
        assert all(c.root_cause_category == "api_failure" for c in clusters)

    def test_canonical_merge_auth_variants_single_cluster(self):
        """QA-03A: failures across AUTH/auth/Auth → one cluster with display label."""
        from services.failure_intelligence_service import FailureIntelligenceService

        _make_tc("TC-CAN-A", module="AUTH")
        _make_tc("TC-CAN-B", module="auth")
        _make_tc("TC-CAN-C", module="Auth")
        _make_run("TC-CAN-A", "fail", logs=["selector not found on login"])
        _make_run("TC-CAN-B", "fail", logs=["selector not found on login"])
        _make_run("TC-CAN-C", "fail", logs=["selector not found on login"])

        service = FailureIntelligenceService()
        clusters = service.get_clusters()
        auth_clusters = [c for c in clusters if c.module.lower() == "auth"]
        assert len(auth_clusters) == 1
        assert auth_clusters[0].module == "AUTH"
        assert auth_clusters[0].total_failures == 3


# ── 6-10. Flaky Detection ─────────────────────────────────────────────────────

class TestFlakyDetection:

    def setup_method(self):
        _db_reset()

    def test_alternating_pass_fail_detected_as_flaky(self):
        """P/F/P/F/P/F alternating pattern → suspected_flaky=True, flip_rate=1.0."""
        from services.failure_intelligence_service import FailureIntelligenceService

        tc_id = "TC-FLAKY-001"
        _make_tc(tc_id)
        for status in ["pass", "fail", "pass", "fail", "pass", "fail"]:
            _make_run(tc_id, status)
            time.sleep(0.001)  # ensure distinct executed_at ordering

        service = FailureIntelligenceService()
        signals = service.get_flaky_tests()

        match = next((s for s in signals if s.test_case_id == tc_id), None)
        assert match is not None
        assert match.suspected_flaky is True
        assert match.flip_rate >= 0.4
        assert match.pass_count >= 1
        assert match.fail_count >= 1

    def test_consistently_passing_not_flaky(self):
        """All-pass history → suspected_flaky=False."""
        from services.failure_intelligence_service import FailureIntelligenceService

        tc_id = "TC-STABLE-001"
        _make_tc(tc_id)
        for _ in range(6):
            _make_run(tc_id, "pass")

        service = FailureIntelligenceService()
        signals = service.get_flaky_tests()

        match = next((s for s in signals if s.test_case_id == tc_id), None)
        assert match is not None
        assert match.suspected_flaky is False
        assert match.flip_rate == 0.0

    def test_consistently_failing_not_flaky(self):
        """All-fail history → suspected_flaky=False (no balance between pass/fail)."""
        from services.failure_intelligence_service import FailureIntelligenceService

        tc_id = "TC-CONSFAIL-001"
        _make_tc(tc_id)
        for _ in range(5):
            _make_run(tc_id, "fail")

        service = FailureIntelligenceService()
        signals = service.get_flaky_tests()

        match = next((s for s in signals if s.test_case_id == tc_id), None)
        assert match is not None
        assert match.suspected_flaky is False  # no passes → not "flaky", just broken

    def test_fewer_than_min_runs_not_flaky(self):
        """Only 2 runs (< FLAKY_MIN_RUNS=4) → never suspected flaky."""
        from services.failure_intelligence_service import FailureIntelligenceService, FLAKY_MIN_RUNS

        tc_id = "TC-FEW-001"
        _make_tc(tc_id)
        _make_run(tc_id, "pass")
        _make_run(tc_id, "fail")

        service = FailureIntelligenceService()
        signals = service.get_flaky_tests(min_runs=FLAKY_MIN_RUNS)

        match = next((s for s in signals if s.test_case_id == tc_id), None)
        assert match is not None
        assert match.suspected_flaky is False

    def test_low_flip_rate_not_flaky(self):
        """P/P/P/F pattern → flip_rate = 1/3 ≈ 0.33 < threshold → not flaky."""
        from services.failure_intelligence_service import FailureIntelligenceService

        tc_id = "TC-LOWFLIP-001"
        _make_tc(tc_id)
        for status in ["fail", "pass", "pass", "pass", "pass"]:
            _make_run(tc_id, status)

        service = FailureIntelligenceService()
        signals = service.get_flaky_tests(flip_threshold=0.4)

        match = next((s for s in signals if s.test_case_id == tc_id), None)
        assert match is not None
        # flip_rate for [P,P,P,P,F] = 1 transition / 4 pairs = 0.25 < 0.4
        assert match.suspected_flaky is False


# ── 11-13. Regression Detection ───────────────────────────────────────────────

class TestRegressionDetection:

    def setup_method(self):
        _db_reset()

    def test_repeated_failures_flagged_as_regression(self):
        """Test with 3 recent failures appears in regressions list."""
        from services.failure_intelligence_service import FailureIntelligenceService

        tc_id = "TC-REG-001"
        _make_tc(tc_id, module="checkout")
        for _ in range(3):
            _make_run(tc_id, "fail", logs=["assertion failed on cart total"])

        service     = FailureIntelligenceService()
        regressions = service.get_regressions(min_failures=2)

        match = next((r for r in regressions if r.test_case_id == tc_id), None)
        assert match is not None
        assert match.repeated_failures == 3

    def test_single_failure_not_a_regression(self):
        """Test with only 1 failure does NOT appear (below min_failures=2)."""
        from services.failure_intelligence_service import FailureIntelligenceService

        tc_id = "TC-NOREG-001"
        _make_tc(tc_id)
        _make_run(tc_id, "fail", logs=["assertion error"])

        service     = FailureIntelligenceService()
        regressions = service.get_regressions(min_failures=2)

        match = next((r for r in regressions if r.test_case_id == tc_id), None)
        assert match is None

    def test_regression_pattern_fields(self):
        """RegressionPattern has the required fields correctly populated."""
        from services.failure_intelligence_service import FailureIntelligenceService

        tc_id = "TC-REG-002"
        _make_tc(tc_id, module="login")
        for _ in range(2):
            _make_run(tc_id, "error", logs=["selector not found"])

        service     = FailureIntelligenceService()
        regressions = service.get_regressions(min_failures=2)

        match = next((r for r in regressions if r.test_case_id == tc_id), None)
        assert match is not None
        assert match.pattern_id == f"REG-{tc_id}"
        assert match.module == "login"
        assert len(match.affected_runs) == 2
        assert isinstance(match.latest_root_cause, str)
        assert len(match.summary) > 0


# ── 14-15. Summary ────────────────────────────────────────────────────────────

class TestSummary:

    def setup_method(self):
        _db_reset()

    def test_summary_counts_align(self):
        """Summary fields are consistent with underlying data after populating runs."""
        from services.failure_intelligence_service import FailureIntelligenceService

        # Flaky test
        _make_tc("TC-SUM-FLAKY", module="cart")
        for status in ["pass", "fail", "pass", "fail", "pass", "fail"]:
            _make_run("TC-SUM-FLAKY", status)
            time.sleep(0.001)

        # Regression test
        _make_tc("TC-SUM-REG", module="checkout")
        for _ in range(3):
            _make_run("TC-SUM-REG", "fail", logs=["selector not found"])

        service = FailureIntelligenceService()
        summary = service.get_summary()

        assert summary.total_failed_runs >= 3
        assert summary.total_clusters >= 1
        assert summary.flaky_tests_count >= 1
        assert summary.recurrent_regressions_count >= 1
        assert isinstance(summary.top_failure_categories, dict)
        assert isinstance(summary.notes, str)
        assert len(summary.notes) > 0

    def test_summary_empty_history(self):
        """Empty DB → all counts are 0 and no crash."""
        from services.failure_intelligence_service import FailureIntelligenceService

        service = FailureIntelligenceService()
        summary = service.get_summary()

        assert summary.total_failed_runs == 0
        assert summary.total_clusters == 0
        assert summary.flaky_tests_count == 0
        assert summary.recurrent_regressions_count == 0
        assert summary.top_failure_categories == {}
        assert "No significant" in summary.notes


# ── 16-20. Routes ─────────────────────────────────────────────────────────────

class TestRoutes:

    def setup_method(self):
        _db_reset()

    def _client(self):
        from fastapi.testclient import TestClient
        from app import app
        return TestClient(app)

    def test_health_route(self):
        resp = self._client().get("/failure-intelligence/health")
        assert resp.status_code == 200
        assert resp.json()["status"] == "ok"

    def test_summary_route(self):
        resp = self._client().get("/failure-intelligence/summary")
        assert resp.status_code == 200
        data = resp.json()
        for key in ("total_failed_runs", "total_clusters", "flaky_tests_count",
                    "recurrent_regressions_count", "top_failure_categories", "notes"):
            assert key in data

    def test_clusters_route(self):
        resp = self._client().get("/failure-intelligence/clusters")
        assert resp.status_code == 200
        assert isinstance(resp.json(), list)

    def test_flaky_tests_route(self):
        resp = self._client().get("/failure-intelligence/flaky-tests")
        assert resp.status_code == 200
        assert isinstance(resp.json(), list)

    def test_regressions_route(self):
        resp = self._client().get("/failure-intelligence/regressions")
        assert resp.status_code == 200
        assert isinstance(resp.json(), list)


# ── 21. Dashboard extension ───────────────────────────────────────────────────

class TestDashboardExtension:

    def setup_method(self):
        _db_reset()

    def test_dashboard_summary_includes_fi_fields(self):
        """DashboardSummary model contains the three new failure intelligence fields."""
        from models.dashboard_models import DashboardSummary

        summary = DashboardSummary()
        assert hasattr(summary, "flaky_tests_count")
        assert hasattr(summary, "recurrent_regressions_count")
        assert hasattr(summary, "total_failure_clusters")
        assert summary.flaky_tests_count == 0
        assert summary.recurrent_regressions_count == 0
        assert summary.total_failure_clusters == 0

    def test_dashboard_route_includes_fi_fields(self):
        """GET /dashboard/summary response contains the new fi fields."""
        from fastapi.testclient import TestClient
        from app import app
        client = TestClient(app)
        resp = client.get("/dashboard/summary")
        assert resp.status_code == 200
        data = resp.json()
        assert "flaky_tests_count" in data
        assert "recurrent_regressions_count" in data
        assert "total_failure_clusters" in data


# ── 22. Non-regression guard ──────────────────────────────────────────────────

class TestNonRegression:
    """Smoke-test that core components still import and return expected types."""

    def test_rca_service_still_works(self):
        """RCA service is unmodified and can still analyze a synthetic run."""
        from models.test_run import TestRun
        from services.rca_service import rca_service

        run = TestRun(
            test_case_id = "TC-RCA-GUARD",
            status       = "fail",
            executed_at  = _now_iso(),
            environment  = "test",
            logs         = ["selector not found on login page"],
        )
        result = rca_service.analyze(run)
        assert result.root_cause_category == "selector_issue"

    def test_failure_intelligence_models_importable(self):
        from models.failure_intelligence_models import (
            FailureCluster,
            FlakyTestSignal,
            FailureIntelligenceSummary,
            RegressionPattern,
        )
        assert FailureCluster
        assert FlakyTestSignal
        assert FailureIntelligenceSummary
        assert RegressionPattern


# ── Run history facade + adapter (Supabase-shaped canonical) ─────────────────

class TestFailureIntelligenceRunHistoryIntegration:

    def test_get_clusters_invokes_run_history_list_runs(self):
        """Clusters load through ``run_history_service.list_runs`` (not repo directly)."""
        from services.failure_intelligence_service import FailureIntelligenceService
        from services.run_history_service import run_history_service

        _make_tc("TC-RH-TRACE", module="checkout")
        _make_run("TC-RH-TRACE", "fail", logs=["selector not found on page"])

        orig = run_history_service.list_runs
        calls = []

        def spy(**kwargs):
            calls.append(kwargs)
            return orig(**kwargs)

        with patch.object(run_history_service, "list_runs", side_effect=spy):
            FailureIntelligenceService().get_clusters(limit=50)

        assert len(calls) >= 1
        assert any("limit" in c for c in calls)

    def test_clusters_non_empty_when_sqlite_empty_but_history_returns_failures(self):
        """``qa_runs`` can carry failures while SQLite is empty — FI still clusters."""
        from services.failure_intelligence_service import FailureIntelligenceService
        from models.run_contract import CanonicalRun, RunArtifacts, RunMeta

        _db_reset()
        _make_tc("TC-SB-ONLY", module="payments")
        rid = "run-supabase-only-1"
        fake_row = CanonicalRun(
            run_id=rid,
            test_id="TC-SB-ONLY",
            test_name="remote",
            source="api",
            status="failed",
            started_at="2026-05-10T10:00:00+00:00",
            steps=[],
            logs=["500 Internal Server Error calling /api/pay"],
            artifacts=RunArtifacts(),
            meta=RunMeta(environment="staging"),
        )
        with patch("services.qa_runs_read.supabase_qa_runs_enabled", return_value=True):
            with patch(
                "services.run_history_service.run_history_service.list_runs",
                return_value=[fake_row],
            ):
                clusters = FailureIntelligenceService().get_clusters(limit=50)
        api_clusters = [c for c in clusters if c.root_cause_category == "api_failure"]
        assert len(api_clusters) >= 1
        assert rid in api_clusters[0].run_ids

    def test_flaky_normalizes_passed_failed_canonical_statuses(self):
        """Canonical ``passed`` / ``failed`` map to storage pass/fail for flip heuristics."""
        from services.failure_intelligence_service import (
            FailureIntelligenceService,
            FLAKY_MIN_RUNS,
        )
        from models.run_contract import CanonicalRun, RunArtifacts, RunMeta

        _db_reset()
        tc = "TC-CANON-STAT"
        _make_tc(tc)
        rows = []
        for i, st in enumerate(["passed", "failed", "passed", "failed", "passed", "failed"]):
            rows.append(
                CanonicalRun(
                    run_id=f"cr-{i}",
                    test_id=tc,
                    status=st,
                    started_at=f"2026-05-11T10:0{i}:00+00:00",
                    steps=[],
                    logs=[],
                    artifacts=RunArtifacts(),
                    meta=RunMeta(),
                )
            )
        with patch("services.qa_runs_read.supabase_qa_runs_enabled", return_value=True):

            def fake_list_runs(*, test_case_id=None, project_id=None, limit=100):
                if test_case_id in (None, tc):
                    return rows[:limit]
                return []

            with patch(
                "services.run_history_service.run_history_service.list_runs",
                side_effect=fake_list_runs,
            ):
                signals = FailureIntelligenceService().get_flaky_tests(min_runs=FLAKY_MIN_RUNS)
        match = next((s for s in signals if s.test_case_id == tc), None)
        assert match is not None
        assert match.suspected_flaky is True
        assert match.pass_count >= 1
        assert match.fail_count >= 1
