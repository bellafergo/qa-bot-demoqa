# tests/test_pr_analysis.py
"""
Tests for the PR Impact Analysis service.

No browser, no network, no real GitHub API.
Uses the temporary SQLite DB from conftest.py.
"""
from __future__ import annotations

import pytest
from unittest.mock import patch, MagicMock

from models.pr_analysis_models import (
    PRAnalysisRequest,
    PRAnalysisResult,
    DraftTestSuggestion,
)
from models.test_case import TestCaseCreate
from services.pr_analysis_service import PRAnalysisService, _match_domain_keywords
from services.test_catalog_service import TestCatalogService, _reset_for_testing
from services.catalog_orchestrator import _reset_for_testing as _orch_reset


# ── Helpers ───────────────────────────────────────────────────────────────────

def _fresh_svc() -> PRAnalysisService:
    _reset_for_testing()
    _orch_reset()
    return PRAnalysisService()


def _add_tc(tc_id: str, module: str, tags=None, name: str = None,
            type_: str = "smoke", priority: str = "medium") -> None:
    svc = TestCatalogService()
    try:
        svc.create_test_case(TestCaseCreate(
            test_case_id=tc_id,
            name=name or f"Test {tc_id}",
            module=module,
            type=type_,
            priority=priority,
            steps=[{"action": "goto", "value": "https://example.com"}],
            assertions=[],
            tags=tags or [],
        ))
    except ValueError:
        pass


def _req(**kwargs) -> PRAnalysisRequest:
    return PRAnalysisRequest(**kwargs)


# ── Module inference from file paths ─────────────────────────────────────────

class TestModuleInference:
    def test_login_file_infers_auth(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["app/login.py"]))
        assert "auth" in result

    def test_auth_path_infers_auth(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["src/auth/views.py"]))
        assert "auth" in result

    def test_checkout_file_infers_checkout(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["services/checkout_service.py"]))
        assert "checkout" in result

    def test_cart_file_infers_checkout(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["components/cart.js"]))
        assert "checkout" in result

    def test_payment_file_infers_checkout(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["billing/payment_processor.py"]))
        assert "checkout" in result

    def test_migration_file_infers_database(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["db/migrations/0042_add_users.py"]))
        assert "database" in result

    def test_profile_file_infers_account(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["users/profile_view.py"]))
        assert "account" in result

    def test_form_file_infers_forms(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["components/ContactForm.jsx"]))
        assert "forms" in result

    def test_multiple_files_multiple_modules(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=[
            "auth/login.py",
            "checkout/cart.py",
        ]))
        assert "auth" in result
        assert "checkout" in result

    def test_empty_files_returns_empty(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req())
        assert result == []

    def test_unrelated_file_returns_nothing(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_files=["README.md"]))
        assert result == []


# ── Module inference from title/description ───────────────────────────────────

class TestTitleDescriptionInference:
    def test_login_in_title_infers_auth(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(title="Fix login redirect loop"))
        assert "auth" in result

    def test_checkout_in_description(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(
            description="Refactors the payment and checkout flow to handle SCA"
        ))
        assert "checkout" in result

    def test_title_and_description_combined(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(
            title="Update user profile page",
            description="Also fixes form validation on settings screen",
        ))
        assert "account" in result
        assert "forms" in result


# ── changed_modules override ──────────────────────────────────────────────────

class TestChangedModulesOverride:
    def test_explicit_module_included(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(changed_modules=["payments"]))
        assert "payments" in result

    def test_explicit_merged_with_inferred(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(
            changed_files=["auth/login.py"],
            changed_modules=["custom-module"],
        ))
        assert "auth" in result
        assert "custom-module" in result

    def test_no_duplicates_in_result(self):
        svc = _fresh_svc()
        result = svc._infer_modules(_req(
            changed_files=["auth/login.py"],
            changed_modules=["auth"],
        ))
        assert result.count("auth") == 1


# ── Risk scoring ──────────────────────────────────────────────────────────────

class TestRiskScoring:
    def test_auth_is_high_risk(self):
        svc = _fresh_svc()
        level, reasons = svc._score_risk(_req(changed_files=["auth/views.py"]), ["auth"])
        assert level == "high"
        assert any("auth" in r.lower() or "high-risk" in r.lower() for r in reasons)

    def test_database_migration_is_high_risk(self):
        svc = _fresh_svc()
        level, _ = svc._score_risk(_req(changed_files=["migrations/0001.py"]), ["database"])
        assert level == "high"

    def test_many_files_is_high_risk(self):
        svc = _fresh_svc()
        files = [f"file_{i}.py" for i in range(20)]
        level, reasons = svc._score_risk(_req(changed_files=files), [])
        assert level == "high"
        assert any("20" in r or "files" in r.lower() for r in reasons)

    def test_deletion_heavy_diff_is_high_risk(self):
        svc = _fresh_svc()
        diff = "\n".join(["- " + f"removed line {i}" for i in range(20)] +
                         ["+ added line"])
        level, _ = svc._score_risk(_req(diff_text=diff), [])
        assert level in ("medium", "high")

    def test_forms_change_is_medium_risk(self):
        svc = _fresh_svc()
        level, _ = svc._score_risk(_req(changed_files=["controllers/user_form.py"]), ["forms"])
        assert level in ("medium", "high")

    def test_docs_only_is_low_risk(self):
        svc = _fresh_svc()
        level, reasons = svc._score_risk(
            _req(changed_files=["README.md", "docs/usage.rst"]), []
        )
        assert level == "low"
        assert any("doc" in r.lower() or "low" in r.lower() for r in reasons)

    def test_empty_input_is_low_risk(self):
        svc = _fresh_svc()
        level, reasons = svc._score_risk(_req(), [])
        assert level == "low"
        assert len(reasons) >= 1

    def test_risk_reasons_always_populated(self):
        svc = _fresh_svc()
        _, reasons = svc._score_risk(_req(), [])
        assert isinstance(reasons, list)
        assert len(reasons) >= 1

    def test_checkout_in_title_is_high_risk(self):
        svc = _fresh_svc()
        level, _ = svc._score_risk(
            _req(title="Fix checkout payment flow"), ["checkout"]
        )
        assert level == "high"


# ── Test matching ─────────────────────────────────────────────────────────────

class TestCatalogMatching:
    def setup_method(self):
        _reset_for_testing()
        _orch_reset()

    def test_no_tests_no_match(self):
        svc = _fresh_svc()
        matched = svc._match_tests(["auth"], _req())
        assert matched == []

    def test_exact_module_match(self):
        _add_tc("TC-AUTH-001", module="auth-service", tags=["login"])
        svc = PRAnalysisService()
        matched = svc._match_tests(["auth"], _req())
        assert "TC-AUTH-001" in matched

    def test_tag_match(self):
        _add_tc("TC-TAG-001", module="generic", tags=["login", "smoke"])
        svc = PRAnalysisService()
        matched = svc._match_tests(["auth"], _req())
        assert "TC-TAG-001" in matched

    def test_name_keyword_match(self):
        _add_tc("TC-NAME-001", module="generic", name="Login valid user", tags=[])
        svc = PRAnalysisService()
        matched = svc._match_tests(["auth"], _req())
        assert "TC-NAME-001" in matched

    def test_no_module_overlap_no_match(self):
        _add_tc("TC-UNREL-001", module="unrelated-module", tags=["nothing"])
        svc = PRAnalysisService()
        matched = svc._match_tests(["auth"], _req())
        assert "TC-UNREL-001" not in matched

    def test_higher_relevance_comes_first(self):
        # Exact module match should outrank a name-only match
        _add_tc("TC-EXACT-001", module="auth",    tags=["login"], name="Auth test", type_="smoke", priority="critical")
        _add_tc("TC-WEAK-001",  module="generic", tags=[],        name="Login helper")
        svc = PRAnalysisService()
        matched = svc._match_tests(["auth"], _req())
        assert matched.index("TC-EXACT-001") < matched.index("TC-WEAK-001")

    def test_no_domains_returns_empty(self):
        _add_tc("TC-SOME-001", module="auth", tags=["login"])
        svc = PRAnalysisService()
        matched = svc._match_tests([], _req())
        assert matched == []

    def test_multiple_domains_union_results(self):
        _add_tc("TC-A-001", module="auth-service",     tags=["login"])
        _add_tc("TC-C-001", module="checkout-service", tags=["payment"])
        svc = PRAnalysisService()
        matched = svc._match_tests(["auth", "checkout"], _req())
        assert "TC-A-001" in matched
        assert "TC-C-001" in matched


# ── Draft test generation ─────────────────────────────────────────────────────

class TestDraftGeneration:
    def test_no_domains_no_drafts(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts([], _req())
        assert drafts == []

    def test_auth_generates_drafts(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts(["auth"], _req())
        assert len(drafts) >= 2
        names = [d.name for d in drafts]
        assert any("login" in n.lower() or "Login" in n for n in names)

    def test_checkout_generates_drafts(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts(["checkout"], _req())
        assert len(drafts) >= 1
        assert all(d.module == "checkout" for d in drafts)

    def test_drafts_have_steps_and_assertions(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts(["auth"], _req())
        for d in drafts:
            assert len(d.suggested_steps) >= 1
            assert len(d.suggested_assertions) >= 1

    def test_no_duplicate_draft_names(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts(["auth", "auth"], _req())
        names = [d.name for d in drafts]
        assert len(names) == len(set(names))

    def test_drafts_are_draft_suggestion_instances(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts(["auth"], _req())
        assert all(isinstance(d, DraftTestSuggestion) for d in drafts)

    def test_source_signal_enriched_with_file(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts(
            ["auth"],
            _req(changed_files=["src/auth/login_service.py"]),
        )
        assert any("login" in d.source_signal.lower() for d in drafts)

    def test_unknown_domain_returns_no_drafts(self):
        svc = _fresh_svc()
        drafts = svc._generate_drafts(["xyzzy-unknown"], _req())
        assert drafts == []


# ── auto_enqueue ──────────────────────────────────────────────────────────────

class TestAutoEnqueue:
    def setup_method(self):
        _reset_for_testing()
        _orch_reset()

    def test_enqueue_creates_job_when_matches_exist(self):
        _add_tc("TC-ENQ-001", module="auth-service", tags=["login"])
        svc = PRAnalysisService()
        result = svc.analyze(_req(
            changed_files=["auth/login.py"],
            auto_enqueue=True,
        ))
        assert result.orchestrator_job_id is not None
        assert len(result.matched_test_case_ids) > 0

    def test_no_job_when_no_matches(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(
            changed_files=["README.md"],
            auto_enqueue=True,
        ))
        assert result.orchestrator_job_id is None
        assert result.matched_tests_count == 0

    def test_no_enqueue_when_flag_false(self):
        _add_tc("TC-NOENC-001", module="auth-service", tags=["login"])
        svc = PRAnalysisService()
        result = svc.analyze(_req(
            changed_files=["auth/login.py"],
            auto_enqueue=False,
        ))
        assert result.orchestrator_job_id is None


# ── Full analyze pipeline ─────────────────────────────────────────────────────

class TestAnalyzePipeline:
    def setup_method(self):
        _reset_for_testing()
        _orch_reset()

    def test_returns_pr_analysis_result(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(changed_files=["auth/login.py"]))
        assert isinstance(result, PRAnalysisResult)

    def test_summary_is_non_empty_string(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(changed_files=["auth/login.py"]))
        assert isinstance(result.summary, str)
        assert len(result.summary) > 0

    def test_pr_id_preserved(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(pr_id="PR-1234"))
        assert result.pr_id == "PR-1234"

    def test_no_drafts_by_default(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(changed_files=["auth/login.py"]))
        assert result.suggested_new_tests == []

    def test_drafts_returned_when_flag_set(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(
            changed_files=["auth/login.py"],
            generate_draft_tests=True,
        ))
        assert len(result.suggested_new_tests) >= 1

    def test_confidence_high_when_files_and_modules_present(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(changed_files=["auth/login.py"]))
        assert result.confidence in ("medium", "high")

    def test_confidence_low_when_no_signals(self):
        svc = _fresh_svc()
        result = svc.analyze(_req())
        assert result.confidence == "low"

    def test_no_match_summary_is_informative(self):
        svc = _fresh_svc()
        result = svc.analyze(_req(changed_files=["auth/login.py"]))
        assert "no catalog" in result.summary.lower() or "found" in result.summary.lower()
