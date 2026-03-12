# tests/test_business_risk.py
"""
Tests for the Business Risk Scoring service.

All core tests use synthetic TestRun objects — no browser, no network.
DB-backed tests persist a run via conftest.py SQLite setup.
"""
from __future__ import annotations

import pytest

from models.business_risk_models import BusinessRiskResult
from models.test_run import TestRun
from services.business_risk_service import BusinessRiskService, _detect_flow, _infer_flow
from services.test_catalog_service import _reset_for_testing
from services.catalog_orchestrator import _reset_for_testing as _orch_reset


# ── Helpers ───────────────────────────────────────────────────────────────────

def _svc() -> BusinessRiskService:
    return BusinessRiskService()


def _run(module: str = "", name: str = "", status: str = "fail", logs=None) -> TestRun:
    return TestRun(
        test_case_id = "TC-RISK-001",
        test_name    = name,
        status       = status,
        meta         = {"tc_module": module} if module else {},
        logs         = logs or [],
    )


# ── Flow detection ─────────────────────────────────────────────────────────────

class TestFlowDetection:
    def test_checkout_module(self):
        assert _detect_flow("checkout-service") == "checkout"

    def test_cart_module(self):
        assert _detect_flow("cart") == "checkout"

    def test_payment_module(self):
        assert _detect_flow("payment-gateway") == "checkout"

    def test_order_module(self):
        assert _detect_flow("order-api") == "checkout"

    def test_login_module(self):
        assert _detect_flow("login") == "authentication"

    def test_auth_module(self):
        assert _detect_flow("auth-service") == "authentication"

    def test_session_module(self):
        assert _detect_flow("session-manager") == "authentication"

    def test_search_module(self):
        assert _detect_flow("search-api") == "product_discovery"

    def test_product_module(self):
        assert _detect_flow("product-catalog") == "product_discovery"

    def test_inventory_module(self):
        assert _detect_flow("inventory-service") == "product_discovery"

    def test_profile_module(self):
        assert _detect_flow("user-profile") == "user_profile"

    def test_settings_module(self):
        assert _detect_flow("settings-page") == "user_profile"

    def test_account_module(self):
        assert _detect_flow("account") == "user_profile"

    def test_admin_module(self):
        assert _detect_flow("admin-panel") == "internal"

    def test_unknown_returns_none(self):
        assert _detect_flow("totally-random-xyz") is None


# ── Checkout risk ──────────────────────────────────────────────────────────────

class TestCheckoutRisk:
    def test_checkout_module_is_critical(self):
        result = _svc().analyze(_run(module="checkout-service"))
        assert result.risk_level == "critical"

    def test_cart_module_is_critical(self):
        result = _svc().analyze(_run(module="cart"))
        assert result.risk_level == "critical"

    def test_payment_module_is_critical(self):
        result = _svc().analyze(_run(module="payment-gateway"))
        assert result.risk_level == "critical"

    def test_order_module_is_critical(self):
        result = _svc().analyze(_run(module="order-api"))
        assert result.risk_level == "critical"

    def test_checkout_flow_name(self):
        result = _svc().analyze(_run(module="checkout"))
        assert result.affected_business_flow == "checkout"

    def test_checkout_impact_mentions_purchases(self):
        result = _svc().analyze(_run(module="checkout"))
        assert "purchase" in result.impact_summary.lower()

    def test_checkout_recommendation_is_fix_immediately(self):
        result = _svc().analyze(_run(module="checkout"))
        assert "immediately" in result.priority_recommendation.lower()

    def test_checkout_from_test_name(self):
        result = _svc().analyze(_run(name="Checkout happy path completes order"))
        assert result.risk_level == "critical"


# ── Authentication risk ────────────────────────────────────────────────────────

class TestAuthenticationRisk:
    def test_login_module_is_high(self):
        result = _svc().analyze(_run(module="login-service"))
        assert result.risk_level == "high"

    def test_auth_module_is_high(self):
        result = _svc().analyze(_run(module="auth"))
        assert result.risk_level == "high"

    def test_auth_flow_name(self):
        result = _svc().analyze(_run(module="auth-service"))
        assert result.affected_business_flow == "authentication"

    def test_auth_impact_mentions_login(self):
        result = _svc().analyze(_run(module="login"))
        assert "log in" in result.impact_summary.lower() or "login" in result.impact_summary.lower()

    def test_auth_recommendation_is_fix_today(self):
        result = _svc().analyze(_run(module="auth"))
        assert "today" in result.priority_recommendation.lower()

    def test_auth_from_test_name(self):
        result = _svc().analyze(_run(name="Login valid user"))
        assert result.risk_level == "high"


# ── Medium risk flows ──────────────────────────────────────────────────────────

class TestMediumRisk:
    def test_search_module_is_medium(self):
        result = _svc().analyze(_run(module="search-service"))
        assert result.risk_level == "medium"

    def test_product_module_is_medium(self):
        result = _svc().analyze(_run(module="product-catalog"))
        assert result.risk_level == "medium"

    def test_profile_module_is_medium(self):
        result = _svc().analyze(_run(module="user-profile"))
        assert result.risk_level == "medium"

    def test_settings_module_is_medium(self):
        result = _svc().analyze(_run(module="settings"))
        assert result.risk_level == "medium"

    def test_product_discovery_flow_name(self):
        result = _svc().analyze(_run(module="search"))
        assert result.affected_business_flow == "product_discovery"

    def test_user_profile_flow_name(self):
        result = _svc().analyze(_run(module="profile"))
        assert result.affected_business_flow == "user_profile"

    def test_medium_recommendation_says_schedule(self):
        result = _svc().analyze(_run(module="search"))
        assert "soon" in result.priority_recommendation.lower() or "schedule" in result.priority_recommendation.lower()


# ── Low risk flows ─────────────────────────────────────────────────────────────

class TestLowRisk:
    def test_admin_module_is_low(self):
        result = _svc().analyze(_run(module="admin-panel"))
        assert result.risk_level == "low"

    def test_internal_module_is_low(self):
        result = _svc().analyze(_run(module="internal-tools"))
        assert result.risk_level == "low"

    def test_unknown_module_is_low(self):
        result = _svc().analyze(_run(module="xyz-unknown-service"))
        assert result.risk_level == "low"

    def test_low_recommendation_says_monitor(self):
        result = _svc().analyze(_run(module="admin"))
        assert "monitor" in result.priority_recommendation.lower()

    def test_internal_flow_name(self):
        result = _svc().analyze(_run(module="admin"))
        assert result.affected_business_flow == "internal"


# ── Module vs test name priority ──────────────────────────────────────────────

class TestSignalPriority:
    def test_module_takes_priority_over_name(self):
        # module=checkout should win even if name says admin
        result = _svc().analyze(_run(module="checkout", name="Admin console test"))
        assert result.risk_level == "critical"
        assert result.affected_business_flow == "checkout"

    def test_test_name_used_when_no_module(self):
        result = _svc().analyze(_run(module="", name="Login valid user test"))
        assert result.affected_business_flow == "authentication"
        assert result.risk_level == "high"

    def test_confidence_high_when_module_present(self):
        result = _svc().analyze(_run(module="checkout"))
        assert result.confidence == "high"

    def test_confidence_medium_when_name_only(self):
        result = _svc().analyze(_run(module="", name="Checkout happy path"))
        assert result.confidence == "medium"

    def test_confidence_low_when_no_signals(self):
        result = _svc().analyze(_run(module="", name=""))
        assert result.confidence == "low"


# ── Passed run ────────────────────────────────────────────────────────────────

class TestPassedRun:
    def test_passed_run_is_low_risk(self):
        result = _svc().analyze(_run(module="checkout", status="pass"))
        assert result.risk_level == "low"

    def test_passed_run_confidence_is_high(self):
        result = _svc().analyze(_run(module="checkout", status="pass"))
        assert result.confidence == "high"

    def test_passed_run_impact_no_business_impact(self):
        result = _svc().analyze(_run(status="pass"))
        assert "passed" in result.impact_summary.lower()


# ── Result structure ──────────────────────────────────────────────────────────

class TestResultStructure:
    def test_result_is_business_risk_result(self):
        result = _svc().analyze(_run(module="checkout"))
        assert isinstance(result, BusinessRiskResult)

    def test_run_id_preserved(self):
        run = _run(module="auth")
        result = _svc().analyze(run)
        assert result.run_id == run.run_id

    def test_impact_summary_is_non_empty(self):
        result = _svc().analyze(_run(module="checkout"))
        assert result.impact_summary

    def test_recommendation_is_non_empty(self):
        result = _svc().analyze(_run(module="auth"))
        assert result.priority_recommendation

    def test_rca_category_is_set(self):
        result = _svc().analyze(_run(module="checkout"))
        assert result.rca_category is not None

    def test_risk_level_valid_value(self):
        valid = {"critical", "high", "medium", "low"}
        result = _svc().analyze(_run(module="auth"))
        assert result.risk_level in valid

    def test_confidence_valid_value(self):
        valid = {"low", "medium", "high"}
        result = _svc().analyze(_run(module="search"))
        assert result.confidence in valid


# ── DB-backed tests ────────────────────────────────────────────────────────────

class TestDBIntegration:
    def setup_method(self):
        _reset_for_testing()
        _orch_reset()

    def test_analyze_run_id_checkout(self):
        from services.db.test_run_repository import test_run_repo
        from services.business_risk_service import business_risk_service
        run = TestRun(
            test_case_id = "TC-BR-001",
            test_name    = "Checkout happy path",
            status       = "fail",
            meta         = {"tc_module": "checkout-service"},
            logs         = ["TimeoutError: exceeded 15000ms"],
        )
        test_run_repo.create_run(run)
        result = business_risk_service.analyze_run_id(run.run_id)
        assert result.risk_level == "critical"
        assert result.affected_business_flow == "checkout"

    def test_analyze_run_id_not_found(self):
        from services.business_risk_service import business_risk_service
        with pytest.raises(ValueError, match="not found"):
            business_risk_service.analyze_run_id("nonexistent-xyz-000")

    def test_analyze_run_id_auth(self):
        from services.db.test_run_repository import test_run_repo
        from services.business_risk_service import business_risk_service
        run = TestRun(
            test_case_id = "TC-BR-002",
            test_name    = "Login valid user",
            status       = "fail",
            meta         = {"tc_module": "auth-service"},
            logs         = ["401 Unauthorized: invalid credentials"],
        )
        test_run_repo.create_run(run)
        result = business_risk_service.analyze_run_id(run.run_id)
        assert result.risk_level == "high"
        assert result.affected_business_flow == "authentication"
