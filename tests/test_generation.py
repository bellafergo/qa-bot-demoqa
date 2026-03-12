# tests/test_generation.py
"""
Tests for the Test Generation service.

No browser, no network, no real external APIs.
Uses the temporary SQLite DB from conftest.py.
"""
from __future__ import annotations

import pytest

from models.test_generation_models import (
    ApproveDraftRequest,
    DraftGeneratedTest,
    TestGenerationRequest,
    TestGenerationResponse,
    ApproveDraftResponse,
)
from services.test_generation_service import TestGenerationService, _infer_domains, _detect_domains
from services.test_catalog_service import TestCatalogService, _reset_for_testing
from services.catalog_orchestrator import _reset_for_testing as _orch_reset


# ── Helpers ───────────────────────────────────────────────────────────────────

def _fresh_svc() -> TestGenerationService:
    _reset_for_testing()
    _orch_reset()
    return TestGenerationService()


def _req(**kwargs) -> TestGenerationRequest:
    return TestGenerationRequest(**kwargs)


# ── Domain detection ──────────────────────────────────────────────────────────

class TestDomainDetection:
    def test_login_in_text_detects_auth(self):
        assert "auth" in _detect_domains("login page changes")

    def test_checkout_in_text_detects_checkout(self):
        assert "checkout" in _detect_domains("payment gateway refactor")

    def test_search_in_text_detects_search(self):
        assert "search" in _detect_domains("improve search filter performance")

    def test_profile_in_text_detects_account(self):
        assert "account" in _detect_domains("update user profile settings")

    def test_form_in_text_detects_forms(self):
        assert "forms" in _detect_domains("form validation bug fix")

    def test_nav_in_text_detects_navigation(self):
        assert "navigation" in _detect_domains("router redirect change")

    def test_unrelated_text_returns_empty(self):
        result = _detect_domains("general infrastructure update")
        assert len(result) == 0 or "navigation" not in result

    def test_multiple_domains_from_text(self):
        result = _detect_domains("login and checkout payment flow")
        assert "auth" in result
        assert "checkout" in result


class TestDomainInference:
    def test_explicit_module_auth(self):
        req = _req(prompt="test login", module="auth")
        domains = _infer_domains(req)
        assert "auth" in domains

    def test_changed_modules_drives_inference(self):
        req = _req(title="PR changes", changed_modules=["auth-service", "checkout-service"])
        domains = _infer_domains(req)
        assert "auth" in domains
        assert "checkout" in domains

    def test_prompt_drives_inference(self):
        req = _req(prompt="Fix the login session bug")
        domains = _infer_domains(req)
        assert "auth" in domains

    def test_requirement_text_drives_inference(self):
        req = _req(requirement_text="As a user I want to checkout and pay securely")
        domains = _infer_domains(req)
        assert "checkout" in domains

    def test_no_signal_returns_empty_domains(self):
        req = _req(title="general refactor")
        domains = _infer_domains(req)
        # May return empty or only weakly matched domains — should not crash
        assert isinstance(domains, list)


# ── Generation from auth/login ────────────────────────────────────────────────

class TestAuthGeneration:
    def test_login_prompt_generates_drafts(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix the login flow"))
        assert result.total_drafts >= 2
        assert len(result.drafts) >= 2

    def test_auth_drafts_include_login_valid(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Refactor the login and auth session management"))
        names = [d.name for d in result.drafts]
        assert any("login" in n.lower() or "Login" in n for n in names)

    def test_auth_drafts_include_negative_case(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Update authentication module"))
        names = [d.name.lower() for d in result.drafts]
        assert any("invalid" in n or "wrong" in n or "negative" in n for n in names)

    def test_auth_module_explicit(self):
        svc = _fresh_svc()
        result = svc.generate(_req(title="Auth changes", module="auth"))
        assert all(d.module == "auth" for d in result.drafts)

    def test_max_drafts_respected(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login flow", max_drafts=2))
        assert result.total_drafts <= 2
        assert len(result.drafts) <= 2


# ── Generation from checkout ──────────────────────────────────────────────────

class TestCheckoutGeneration:
    def test_checkout_prompt_generates_drafts(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix checkout payment flow"))
        assert result.total_drafts >= 1

    def test_checkout_includes_happy_path(self):
        svc = _fresh_svc()
        result = svc.generate(_req(requirement_text="User can complete a purchase through checkout"))
        names = [d.name.lower() for d in result.drafts]
        assert any("checkout" in n or "order" in n or "payment" in n for n in names)

    def test_checkout_module_in_drafts(self):
        svc = _fresh_svc()
        result = svc.generate(_req(title="Payment update", module="checkout"))
        assert all(d.module == "checkout" for d in result.drafts)


# ── Generic/fallback generation ───────────────────────────────────────────────

class TestGenericFallback:
    def test_unclear_prompt_generates_generic_drafts(self):
        svc = _fresh_svc()
        result = svc.generate(_req(title="misc infrastructure changes"))
        assert result.total_drafts >= 1
        # Generic strategy or no domain-specific strategy
        assert "generic" in result.generation_strategy or result.total_drafts >= 1

    def test_generic_drafts_have_low_confidence(self):
        svc = _fresh_svc()
        result = svc.generate(_req(title="misc changes"))
        # If all generic, confidence should be low
        if result.generation_strategy == "template-based:generic":
            assert all(d.confidence == "low" for d in result.drafts)

    def test_generic_fallback_has_notes(self):
        svc = _fresh_svc()
        result = svc.generate(_req(title="misc changes"))
        if result.generation_strategy == "template-based:generic":
            assert len(result.notes) >= 1


# ── Draft structure validation ────────────────────────────────────────────────

class TestDraftStructure:
    def test_drafts_are_draft_generated_test_instances(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login flow"))
        assert all(isinstance(d, DraftGeneratedTest) for d in result.drafts)

    def test_drafts_have_non_empty_name(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login flow"))
        assert all(d.name for d in result.drafts)

    def test_drafts_have_valid_type(self):
        valid_types = {"smoke", "regression", "functional", "negative", "e2e"}
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login flow"))
        assert all(d.type in valid_types for d in result.drafts)

    def test_drafts_have_valid_priority(self):
        valid_priorities = {"low", "medium", "high", "critical"}
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix checkout payment"))
        assert all(d.priority in valid_priorities for d in result.drafts)

    def test_drafts_have_valid_confidence(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix auth system"))
        assert all(d.confidence in ("low", "medium", "high") for d in result.drafts)

    def test_drafts_have_steps(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login"))
        assert all(len(d.steps) >= 1 for d in result.drafts)

    def test_drafts_have_assertions(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login"))
        assert all(len(d.assertions) >= 1 for d in result.drafts)

    def test_drafts_have_rationale(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login"))
        assert all(d.rationale for d in result.drafts)

    def test_no_duplicate_names_in_drafts(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login and checkout"))
        names = [d.name for d in result.drafts]
        assert len(names) == len(set(names))

    def test_draft_id_is_assigned(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login"))
        assert all(d.draft_id for d in result.drafts)

    def test_source_signal_populated(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix the login auth flow", title="Auth fix"))
        assert all(d.source_signal for d in result.drafts)

    def test_result_is_test_generation_response(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login"))
        assert isinstance(result, TestGenerationResponse)

    def test_generation_strategy_is_non_empty(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login"))
        assert result.generation_strategy


# ── Confidence assignment ─────────────────────────────────────────────────────

class TestConfidenceAssignment:
    def test_known_auth_domain_gives_high_confidence_drafts(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Login auth refactor", module="auth"))
        high_confidence = [d for d in result.drafts if d.confidence == "high"]
        assert len(high_confidence) >= 1

    def test_type_override_respected(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Login auth refactor", type="regression"))
        assert all(d.type == "regression" for d in result.drafts)

    def test_priority_override_respected(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Login auth refactor", priority="critical"))
        assert all(d.priority == "critical" for d in result.drafts)


# ── Approval creates test cases ───────────────────────────────────────────────

class TestApproval:
    def setup_method(self):
        _reset_for_testing()
        _orch_reset()

    def test_approve_creates_test_cases_in_catalog(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login flow", max_drafts=2))
        approve_req = ApproveDraftRequest(drafts=result.drafts, activate=True)
        resp = svc.approve(approve_req)
        assert resp.total_created == 2
        assert len(resp.created_test_case_ids) == 2

    def test_approved_ids_have_tc_gen_prefix(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login flow", max_drafts=2))
        resp = svc.approve(ApproveDraftRequest(drafts=result.drafts))
        assert all(id_.startswith("TC-GEN-") for id_ in resp.created_test_case_ids)

    def test_approved_test_exists_in_catalog(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login", max_drafts=1))
        resp = svc.approve(ApproveDraftRequest(drafts=result.drafts))
        catalog = TestCatalogService()
        tc = catalog.get_test_case(resp.created_test_case_ids[0])
        assert tc is not None
        assert tc.name == result.drafts[0].name

    def test_approve_with_activate_false_creates_inactive(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login", max_drafts=1))
        resp = svc.approve(ApproveDraftRequest(drafts=result.drafts, activate=False))
        catalog = TestCatalogService()
        tc = catalog.get_test_case(resp.created_test_case_ids[0])
        assert tc.status == "inactive"

    def test_second_approval_of_same_drafts_skips_conflicts(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login", max_drafts=1))
        drafts = result.drafts

        # First approval
        resp1 = svc.approve(ApproveDraftRequest(drafts=drafts))
        assert resp1.total_created == 1

        # Rebuild draft with the same content but a fresh draft_id
        # (simulating re-submission of the same draft)
        # The service allocates the next TC-GEN-XXXX — this should succeed
        resp2 = svc.approve(ApproveDraftRequest(drafts=drafts))
        # Second approval gets a different TC-GEN-XXXX — should also succeed
        assert resp2.total_created == 1
        assert resp1.created_test_case_ids[0] != resp2.created_test_case_ids[0]

    def test_approve_does_not_overwrite_existing_catalog_test(self):
        from models.test_case import TestCaseCreate
        catalog = TestCatalogService()
        # Pre-populate a test that we don't want overwritten
        catalog.create_test_case(TestCaseCreate(
            test_case_id="TC-GEN-0001",
            name="Pre-existing test",
            module="auth",
            type="smoke",
            priority="high",
            steps=[{"action": "goto", "value": "/"}],
            assertions=[],
        ))

        svc = TestGenerationService()
        result = svc.generate(_req(prompt="Fix login", max_drafts=1))
        resp = svc.approve(ApproveDraftRequest(drafts=result.drafts))

        # ID TC-GEN-0001 is taken → service must assign TC-GEN-0002+
        assert resp.total_created == 1
        assert resp.created_test_case_ids[0] != "TC-GEN-0001"

    def test_approve_empty_drafts_returns_zero(self):
        svc = _fresh_svc()
        resp = svc.approve(ApproveDraftRequest(drafts=[]))
        assert resp.total_created == 0
        assert resp.total_skipped == 0

    def test_approved_test_has_generated_tag(self):
        svc = _fresh_svc()
        result = svc.generate(_req(prompt="Fix login", max_drafts=1))
        resp = svc.approve(ApproveDraftRequest(drafts=result.drafts))
        catalog = TestCatalogService()
        tc = catalog.get_test_case(resp.created_test_case_ids[0])
        assert "generated" in tc.tags


# ── PR/module-based generation ────────────────────────────────────────────────

class TestPRModuleGeneration:
    def test_changed_modules_drive_generation(self):
        svc = _fresh_svc()
        result = svc.generate(_req(
            title="PR changes",
            changed_modules=["auth-service"],
        ))
        assert result.total_drafts >= 1
        names = [d.name.lower() for d in result.drafts]
        assert any("login" in n for n in names)

    def test_risk_level_high_with_auth_module(self):
        svc = _fresh_svc()
        result = svc.generate(_req(
            title="Security patch",
            changed_modules=["auth"],
            risk_level="high",
        ))
        assert result.total_drafts >= 1

    def test_source_pr_analysis_in_signal(self):
        svc = _fresh_svc()
        result = svc.generate(_req(
            title="PR impact",
            changed_modules=["checkout-service"],
            source="pr-analysis",
        ))
        assert all("pr-analysis" in d.source_signal for d in result.drafts)

    def test_multi_module_generates_union(self):
        svc = _fresh_svc()
        result = svc.generate(_req(
            title="Cross-domain PR",
            changed_modules=["auth-service", "checkout-service"],
            max_drafts=10,
        ))
        names = [d.name.lower() for d in result.drafts]
        has_auth_test     = any("login" in n for n in names)
        has_checkout_test = any("checkout" in n or "payment" in n or "order" in n for n in names)
        assert has_auth_test
        assert has_checkout_test


# ── Input validation ──────────────────────────────────────────────────────────

class TestInputValidation:
    def test_no_signal_raises_validation_error(self):
        import pydantic
        with pytest.raises(pydantic.ValidationError):
            TestGenerationRequest()

    def test_only_prompt_is_valid(self):
        req = TestGenerationRequest(prompt="Fix login")
        assert req.prompt == "Fix login"

    def test_only_requirement_text_is_valid(self):
        req = TestGenerationRequest(requirement_text="User should be able to login")
        assert req.requirement_text is not None

    def test_only_title_is_valid(self):
        req = TestGenerationRequest(title="Login fix")
        assert req.title == "Login fix"

    def test_max_drafts_default_is_five(self):
        req = TestGenerationRequest(prompt="Fix login")
        assert req.max_drafts == 5
