# tests/test_rca.py
"""
Tests for the Root Cause Analysis (RCA) service.

All tests use synthetic TestRun objects — no browser, no network.
The analyze_run_id tests persist a run to the SQLite DB via conftest.py.
"""
from __future__ import annotations

import pytest

from models.rca_models import RCAAnalysisResult, RCAEvidenceSignal
from models.test_run import TestRun
from services.rca_service import RCAService, rca_service
from services.test_catalog_service import _reset_for_testing
from services.catalog_orchestrator import _reset_for_testing as _orch_reset


# ── Helpers ───────────────────────────────────────────────────────────────────

def _svc() -> RCAService:
    return RCAService()


def _failed_run(logs=None, steps_result=None, meta=None, status="fail") -> TestRun:
    return TestRun(
        test_case_id = "TC-TEST-001",
        test_name    = "Test run",
        status       = status,
        logs         = logs or [],
        steps_result = steps_result or [],
        meta         = meta or {},
    )


def _step_err(action: str, error: str) -> dict:
    return {"action": action, "status": "failed", "error": error}


# ── Selector issue ────────────────────────────────────────────────────────────

class TestSelectorIssue:
    def test_locator_not_found_classified(self):
        run = _failed_run(logs=["[RESOLVE_FAIL] locator not found for selector: .login-btn"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "selector_issue"

    def test_element_not_visible_classified(self):
        run = _failed_run(steps_result=[_step_err("click", "element not visible: #submit")])
        result = _svc().analyze(run)
        assert result.root_cause_category == "selector_issue"

    def test_resolve_fail_log_classified(self):
        run = _failed_run(logs=["[RESOLVE_FAIL] action=click selector=.btn"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "selector_issue"

    def test_selector_issue_layer_is_ui(self):
        run = _failed_run(logs=["locator not found for .my-element"])
        result = _svc().analyze(run)
        assert result.impacted_layer == "ui"

    def test_selector_issue_has_evidence_signals(self):
        run = _failed_run(logs=["element not found: #login-form"])
        result = _svc().analyze(run)
        assert len(result.evidence_signals) >= 1

    def test_selector_issue_has_recommendation(self):
        run = _failed_run(logs=["locator not found: .nav-link"])
        result = _svc().analyze(run)
        assert "locator" in result.recommendation.lower() or "selector" in result.recommendation.lower()


# ── Timeout issue ─────────────────────────────────────────────────────────────

class TestTimeoutIssue:
    def test_timeout_in_log_classified(self):
        run = _failed_run(logs=["Timeout en step 3: fill — TimeoutError: waiting 15000ms"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "timeout_issue"

    def test_timed_out_string_classified(self):
        run = _failed_run(logs=["Operation timed out after 30s"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "timeout_issue"

    def test_navigation_timeout_classified(self):
        run = _failed_run(logs=["navigation timeout: page load exceeded 15000ms"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "timeout_issue"

    def test_timeout_has_recommendation(self):
        run = _failed_run(logs=["TimeoutError: waiting for element"])
        result = _svc().analyze(run)
        assert "timeout" in result.recommendation.lower() or "wait" in result.recommendation.lower()

    def test_timeout_confidence_is_medium_or_high(self):
        run = _failed_run(logs=["Timeout waiting for selector .btn"])
        result = _svc().analyze(run)
        assert result.confidence in ("medium", "high")


# ── Assertion issue ───────────────────────────────────────────────────────────

class TestAssertionIssue:
    def test_assertion_error_classified(self):
        run = _failed_run(logs=["Fallo en step 4: assert_text_contains — AssertionError: Texto no encontrado"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "assertion_issue"

    def test_text_not_found_classified(self):
        run = _failed_run(logs=["Expected contiene: 'Welcome' — not found on page"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "assertion_issue"

    def test_assert_url_fail_classified(self):
        run = _failed_run(logs=["assert_url_contains falló: '/dashboard' no está en '/login'"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "assertion_issue"

    def test_assertion_layer_is_ui(self):
        run = _failed_run(logs=["AssertionError: expected text not found"])
        result = _svc().analyze(run)
        assert result.impacted_layer == "ui"

    def test_assertion_step_error_classified(self):
        run = _failed_run(steps_result=[_step_err("assert_text_contains", "assertion failure: text not found")])
        result = _svc().analyze(run)
        assert result.root_cause_category == "assertion_issue"


# ── API failure ───────────────────────────────────────────────────────────────

class TestAPIFailure:
    def test_http_500_classified(self):
        run = _failed_run(logs=["Server returned status 500 Internal Server Error"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "api_failure"

    def test_connection_refused_classified(self):
        run = _failed_run(logs=["Error: connection refused at http://api:8080/users"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "api_failure"

    def test_http_502_classified(self):
        run = _failed_run(logs=["502 Bad Gateway received from upstream"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "api_failure"

    def test_net_err_classified(self):
        run = _failed_run(logs=["net::ERR_CONNECTION_REFUSED at https://api.example.com"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "api_failure"

    def test_api_failure_layer_is_api(self):
        run = _failed_run(logs=["HTTP 500 Internal Server Error"])
        result = _svc().analyze(run)
        assert result.impacted_layer == "api"

    def test_api_failure_has_recommendation(self):
        run = _failed_run(logs=["connection refused at backend service"])
        result = _svc().analyze(run)
        assert "backend" in result.recommendation.lower() or "api" in result.recommendation.lower()


# ── Auth issue ────────────────────────────────────────────────────────────────

class TestAuthIssue:
    def test_unauthorized_classified(self):
        run = _failed_run(logs=["Request failed: 401 Unauthorized"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "auth_issue"

    def test_forbidden_classified(self):
        run = _failed_run(logs=["403 Forbidden: access denied to /admin"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "auth_issue"

    def test_login_failed_classified(self):
        run = _failed_run(logs=["Login failed: invalid credentials"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "auth_issue"

    def test_token_expired_classified(self):
        run = _failed_run(logs=["JWT error: token expired at claim 'exp'"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "auth_issue"

    def test_auth_layer_is_auth(self):
        run = _failed_run(logs=["401 Unauthorized response"])
        result = _svc().analyze(run)
        assert result.impacted_layer == "auth"

    def test_auth_has_priority_over_api(self):
        # 401 is auth, not api_failure — auth should take priority
        run = _failed_run(logs=["401 Unauthorized", "connection refused"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "auth_issue"


# ── Data issue ────────────────────────────────────────────────────────────────

class TestDataIssue:
    def test_missing_test_data_classified(self):
        run = _failed_run(logs=["Error: missing test data for entity 'user'"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "data_issue"

    def test_no_records_found_classified(self):
        run = _failed_run(logs=["Query returned 0 rows: no records found"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "data_issue"

    def test_invalid_fixture_classified(self):
        run = _failed_run(logs=["Setup failed: invalid fixture 'users.json'"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "data_issue"

    def test_data_layer_is_data(self):
        run = _failed_run(logs=["missing test data for checkout flow"])
        result = _svc().analyze(run)
        assert result.impacted_layer == "data"


# ── Navigation issue ──────────────────────────────────────────────────────────

class TestNavigationIssue:
    def test_navigation_failed_classified(self):
        run = _failed_run(logs=["Navigation failed: page did not load within timeout"])
        result = _svc().analyze(run)
        assert result.root_cause_category in ("timeout_issue", "navigation_issue")

    def test_redirect_loop_classified(self):
        run = _failed_run(logs=["Detected redirect loop on /checkout"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "navigation_issue"

    def test_dns_error_classified(self):
        run = _failed_run(logs=["net::ERR_NAME_NOT_RESOLVED for https://staging.app.io"])
        result = _svc().analyze(run)
        assert result.root_cause_category in ("api_failure", "navigation_issue")


# ── Environment issue ─────────────────────────────────────────────────────────

class TestEnvironmentIssue:
    def test_service_unavailable_classified(self):
        run = _failed_run(logs=["Backend service unavailable: health check failed"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "environment_issue"

    def test_database_error_classified(self):
        run = _failed_run(logs=["database error: connection pool exhausted"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "environment_issue"


# ── Unknown fallback ──────────────────────────────────────────────────────────

class TestUnknownFallback:
    def test_empty_logs_returns_unknown(self):
        run = _failed_run()
        result = _svc().analyze(run)
        assert result.root_cause_category == "unknown"
        assert result.confidence == "low"

    def test_unrecognized_error_returns_unknown(self):
        run = _failed_run(logs=["some unrecognized random noise xyz123"])
        result = _svc().analyze(run)
        assert result.root_cause_category == "unknown"

    def test_unknown_recommendation_present(self):
        run = _failed_run()
        result = _svc().analyze(run)
        assert result.recommendation

    def test_unknown_layer_is_unknown(self):
        run = _failed_run()
        result = _svc().analyze(run)
        assert result.impacted_layer == "unknown"


# ── Passed run ────────────────────────────────────────────────────────────────

class TestPassedRun:
    def test_passed_run_returns_no_analysis(self):
        run = _failed_run(status="pass")
        result = _svc().analyze(run)
        assert result.root_cause_category == "unknown"
        assert result.evidence_signals == []

    def test_passed_run_confidence_is_high(self):
        run = _failed_run(status="pass")
        result = _svc().analyze(run)
        assert result.confidence == "high"

    def test_passed_run_recommendation_no_action(self):
        run = _failed_run(status="pass")
        result = _svc().analyze(run)
        assert "no action" in result.recommendation.lower()


# ── Result structure ──────────────────────────────────────────────────────────

class TestResultStructure:
    def test_result_is_rca_analysis_result(self):
        run = _failed_run(logs=["timeout waiting for element"])
        result = _svc().analyze(run)
        assert isinstance(result, RCAAnalysisResult)

    def test_run_id_preserved(self):
        run = _failed_run(logs=["timeout"])
        result = _svc().analyze(run)
        assert result.run_id == run.run_id

    def test_summary_is_non_empty(self):
        run = _failed_run(logs=["timeout"])
        result = _svc().analyze(run)
        assert result.summary

    def test_probable_cause_is_non_empty(self):
        run = _failed_run(logs=["locator not found"])
        result = _svc().analyze(run)
        assert result.probable_cause

    def test_evidence_signals_are_rca_signal_instances(self):
        run = _failed_run(logs=["element not visible: .submit-btn"])
        result = _svc().analyze(run)
        assert all(isinstance(s, RCAEvidenceSignal) for s in result.evidence_signals)

    def test_evidence_signal_has_source(self):
        run = _failed_run(logs=["Timeout in step 2"])
        result = _svc().analyze(run)
        assert all(s.source for s in result.evidence_signals)

    def test_evidence_signal_has_signal_type(self):
        run = _failed_run(logs=["AssertionError: text not found"])
        result = _svc().analyze(run)
        assert all(s.signal_type for s in result.evidence_signals)


# ── Confidence ────────────────────────────────────────────────────────────────

class TestConfidence:
    def test_multiple_signals_give_high_confidence(self):
        run = _failed_run(logs=[
            "Timeout en step 1: goto — TimeoutError",
            "Timeout en step 2: fill — TimeoutError",
            "Timeout en step 3: click — TimeoutError",
        ])
        result = _svc().analyze(run)
        assert result.confidence in ("medium", "high")

    def test_single_clear_signal_gives_medium_or_high(self):
        run = _failed_run(logs=["401 Unauthorized at /api/users"])
        result = _svc().analyze(run)
        assert result.confidence in ("medium", "high")

    def test_no_signals_gives_low_confidence(self):
        run = _failed_run()
        result = _svc().analyze(run)
        assert result.confidence == "low"


# ── Recommendations ───────────────────────────────────────────────────────────

class TestRecommendations:
    def test_selector_recommendation(self):
        run = _failed_run(logs=["selector not found: .login-btn"])
        result = _svc().analyze(run)
        assert result.recommendation
        assert len(result.recommendation) > 10

    def test_timeout_recommendation(self):
        run = _failed_run(logs=["TimeoutError waiting 15000ms for element"])
        result = _svc().analyze(run)
        assert "timeout" in result.recommendation.lower() or "wait" in result.recommendation.lower()

    def test_api_recommendation(self):
        run = _failed_run(logs=["HTTP 500 from backend"])
        result = _svc().analyze(run)
        assert "api" in result.recommendation.lower() or "backend" in result.recommendation.lower()

    def test_auth_recommendation(self):
        run = _failed_run(logs=["401 Unauthorized: token expired"])
        result = _svc().analyze(run)
        assert "credential" in result.recommendation.lower() or "auth" in result.recommendation.lower()

    def test_data_recommendation(self):
        run = _failed_run(logs=["missing test data for product fixture"])
        result = _svc().analyze(run)
        assert "data" in result.recommendation.lower()


# ── DB integration: analyze_run_id ───────────────────────────────────────────

class TestAnalyzeRunId:
    def setup_method(self):
        _reset_for_testing()
        _orch_reset()

    def test_analyze_existing_run(self):
        from services.db.test_run_repository import test_run_repo
        run = TestRun(
            test_case_id = "TC-DB-001",
            status       = "fail",
            logs         = ["Timeout en step 2: fill — TimeoutError: exceeded 15000ms"],
        )
        test_run_repo.create_run(run)

        result = rca_service.analyze_run_id(run.run_id)
        assert result.run_id == run.run_id
        assert result.root_cause_category == "timeout_issue"

    def test_analyze_missing_run_raises(self):
        with pytest.raises(ValueError, match="not found"):
            rca_service.analyze_run_id("nonexistent-run-id-xyz")

    def test_analyze_passed_run_from_db(self):
        from services.db.test_run_repository import test_run_repo
        run = TestRun(
            test_case_id = "TC-DB-002",
            status       = "pass",
            logs         = [],
        )
        test_run_repo.create_run(run)

        result = rca_service.analyze_run_id(run.run_id)
        assert result.root_cause_category == "unknown"
        assert result.evidence_signals == []

    def test_step_errors_drive_classification(self):
        from services.db.test_run_repository import test_run_repo
        run = TestRun(
            test_case_id  = "TC-DB-003",
            status        = "fail",
            steps_result  = [
                {"action": "click", "status": "failed",
                 "error": "locator not found: .checkout-btn"}
            ],
        )
        test_run_repo.create_run(run)

        result = rca_service.analyze_run_id(run.run_id)
        assert result.root_cause_category == "selector_issue"
