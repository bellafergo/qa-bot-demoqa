# tests/test_block16_robustness.py
"""
Block 16 — Execution Robustness tests.

All tests are deterministic and require no browser, network, or DB.

Covers:
  - NormalizedStep schema (core.schemas)
  - Step validation layer (core.step_validator)
  - Natural language parser (core.nl_parser)
  - Enhanced step normalizer (core.step_normalizer.normalize_steps_full)
  - Selector healer new fallback types (services.selector_healer)
  - Test lifecycle management (models.test_case + services.lifecycle_service)
  - Orchestrator batch sharding + fair-queue (services.catalog_orchestrator)
  - Evidence capture (runners.evidence_capture)
"""
from __future__ import annotations

import pytest


# ══════════════════════════════════════════════════════════════════════════════
# 1. NormalizedStep schema
# ══════════════════════════════════════════════════════════════════════════════

class TestNormalizedStep:

    def test_import(self):
        from core.schemas import NormalizedStep  # noqa: F401

    def test_minimal_construction(self):
        from core.schemas import NormalizedStep
        ns = NormalizedStep(action="goto", url="https://example.com")
        assert ns.action == "goto"
        assert ns.url == "https://example.com"

    def test_defaults(self):
        from core.schemas import NormalizedStep
        ns = NormalizedStep(action="click")
        assert ns.timeout_ms == 10_000
        assert ns.fallbacks == []
        assert ns.metadata == {}
        assert ns.target is None
        assert ns.selector is None

    def test_with_target_spec(self):
        from core.schemas import NormalizedStep, TargetSpec
        ts = TargetSpec(primary="#login-btn")
        ns = NormalizedStep(action="click", target=ts)
        assert ns.target.primary == "#login-btn"

    def test_extra_fields_allowed(self):
        from core.schemas import NormalizedStep
        ns = NormalizedStep(action="fill", value="hello", custom_extra="x")
        assert ns.value == "hello"


# ══════════════════════════════════════════════════════════════════════════════
# 2. Step Validation
# ══════════════════════════════════════════════════════════════════════════════

class TestStepValidator:

    def _v(self, steps):
        from core.step_validator import validate_steps
        return validate_steps(steps)

    def test_valid_goto(self):
        res = self._v([{"action": "goto", "url": "https://example.com"}])
        assert res.valid
        assert res.errors == []
        assert res.validated_count == 1

    def test_valid_click_with_selector(self):
        res = self._v([{"action": "click", "selector": "#btn"}])
        assert res.valid

    def test_valid_click_with_target(self):
        res = self._v([{"action": "click", "target": {"primary": "#btn"}}])
        assert res.valid

    def test_valid_fill(self):
        res = self._v([{"action": "fill", "selector": "#email", "value": "a@b.com"}])
        assert res.valid

    def test_valid_press(self):
        res = self._v([{"action": "press", "selector": "#inp", "key": "Enter"}])
        assert res.valid

    def test_valid_wait_ms(self):
        res = self._v([{"action": "wait_ms", "ms": 500}])
        assert res.valid

    def test_missing_action(self):
        res = self._v([{"selector": "#x"}])
        assert not res.valid
        assert any(e.error_type == "missing_required_field" for e in res.errors)

    def test_invalid_action(self):
        res = self._v([{"action": "teleport", "selector": "#x"}])
        assert not res.valid
        assert any(e.error_type == "invalid_action" for e in res.errors)

    def test_missing_selector_for_click(self):
        res = self._v([{"action": "click"}])
        assert not res.valid
        assert any(e.error_type == "missing_selector" for e in res.errors)

    def test_missing_url_for_goto(self):
        res = self._v([{"action": "goto"}])
        assert not res.valid
        assert any(e.field == "url" for e in res.errors)

    def test_missing_key_for_press(self):
        res = self._v([{"action": "press", "selector": "#x"}])
        assert not res.valid
        assert any(e.field == "key" for e in res.errors)

    def test_malformed_target_not_dict(self):
        res = self._v([{"action": "click", "target": "not-a-dict"}])
        assert not res.valid
        assert any(e.error_type == "malformed_target" for e in res.errors)

    def test_wait_ms_without_ms_is_warning_not_error(self):
        res = self._v([{"action": "wait_ms"}])
        assert res.valid               # warnings don't block
        assert len(res.warnings) >= 1

    def test_step_index_in_errors(self):
        res = self._v([
            {"action": "goto", "url": "https://x.com"},
            {"action": "click"},   # missing selector
        ])
        assert not res.valid
        failing = [e for e in res.errors if e.error_type == "missing_selector"]
        assert failing[0].step_index == 1

    def test_non_dict_step(self):
        res = self._v(["not_a_dict"])
        assert not res.valid

    def test_empty_list_is_valid(self):
        res = self._v([])
        assert res.valid
        assert res.validated_count == 0

    def test_multiple_errors_collected(self):
        res = self._v([
            {"action": "click"},       # missing selector
            {"action": "goto"},        # missing url
            {"action": "bogus"},       # invalid action
        ])
        assert len(res.errors) >= 3


# ══════════════════════════════════════════════════════════════════════════════
# 3. Natural Language Parser
# ══════════════════════════════════════════════════════════════════════════════

class TestNLParser:

    def _p(self, prompt, context=None):
        from core.nl_parser import parse_natural_language
        return parse_natural_language(prompt, context)

    def test_import(self):
        from core.nl_parser import parse_natural_language, NLParseResult  # noqa

    def test_goto_url(self):
        r = self._p("Go to https://example.com/login")
        assert r.steps
        step = r.steps[0]
        assert step.action == "goto"
        assert "example.com" in (step.url or "")
        assert step.confidence >= 0.9

    def test_click_button(self):
        r = self._p("Click the Login button")
        assert r.steps
        step = r.steps[0]
        assert step.action == "click"
        assert step.target is not None
        assert step.confidence > 0.5

    def test_fill_field(self):
        r = self._p("Fill the email field with 'user@example.com'")
        step = next((s for s in r.steps if s.action == "fill"), None)
        assert step is not None
        assert step.value == "user@example.com"

    def test_assert_visible(self):
        r = self._p("Verify that the dashboard is visible")
        step = next((s for s in r.steps if s.action == "assert_visible"), None)
        assert step is not None

    def test_assert_not_visible(self):
        r = self._p("Check that the error message is not visible")
        step = next((s for s in r.steps if s.action == "assert_not_visible"), None)
        assert step is not None

    def test_assert_text(self):
        r = self._p("See 'Welcome back' is visible")
        step = next((s for s in r.steps if s.action == "assert_text_contains"), None)
        assert step is not None
        assert step.text == "Welcome back"

    def test_wait(self):
        r = self._p("Wait 500ms")
        step = next((s for s in r.steps if s.action == "wait_ms"), None)
        assert step is not None
        assert step.ms == 500

    def test_press_enter(self):
        r = self._p("Press Enter")
        step = next((s for s in r.steps if s.action == "press"), None)
        assert step is not None
        assert step.key == "Enter"

    def test_multi_step_prompt(self):
        r = self._p(
            "Go to https://example.com. Then click the Login button. "
            "Then fill the email field with 'a@b.com'. Then press Enter."
        )
        actions = [s.action for s in r.steps]
        assert "goto"  in actions
        assert "click" in actions
        assert "fill"  in actions
        assert "press" in actions

    def test_overall_confidence_range(self):
        r = self._p("Navigate to https://demo.com and click Submit")
        assert 0.0 <= r.overall_confidence <= 1.0

    def test_ambiguous_prompt_flagged(self):
        r = self._p("Do something to the thing")
        # Vague prompt — ambiguous or 0 steps
        assert r.ambiguous or len(r.steps) == 0

    def test_fallback_suggestions_generated(self):
        r = self._p("Click the Submit button")
        step = next((s for s in r.steps if s.action == "click"), None)
        assert step is not None
        assert len(step.fallback_suggestions) > 0

    def test_assert_url(self):
        r = self._p("URL should contain '/dashboard'")
        step = next((s for s in r.steps if s.action == "assert_url_contains"), None)
        assert step is not None

    def test_base_url_injected_in_relative_goto(self):
        r = self._p(
            "Go to /login",
            context={"base_url": "https://myapp.com"},
        )
        step = next((s for s in r.steps if s.action == "goto"), None)
        assert step is not None
        assert step.url == "https://myapp.com/login"

    def test_empty_prompt_returns_no_steps(self):
        r = self._p("")
        assert r.steps == []
        assert r.overall_confidence == 0.0


# ══════════════════════════════════════════════════════════════════════════════
# 4. Extended step normalizer (normalize_steps_full)
# ══════════════════════════════════════════════════════════════════════════════

class TestNormalizeStepsFull:

    def _n(self, steps, **kw):
        from core.step_normalizer import normalize_steps_full
        return normalize_steps_full(steps, **kw)

    def test_returns_normalized_step_objects(self):
        from core.schemas import NormalizedStep
        result = self._n([{"action": "goto", "url": "https://x.com"}])
        assert len(result) == 1
        assert isinstance(result[0], NormalizedStep)

    def test_goto_passthrough(self):
        result = self._n([{"action": "goto", "url": "https://x.com"}])
        assert result[0].action == "goto"
        assert result[0].url == "https://x.com"

    def test_click_with_selector_gets_target(self):
        result = self._n([{"action": "click", "selector": "#login-btn"}])
        ns = result[0]
        assert ns.target is not None
        assert ns.target.primary == "#login-btn"
        assert ns.selector == "#login-btn"   # preserved

    def test_id_selector_gets_css_fallback(self):
        result = self._n([{"action": "click", "selector": "#submit"}])
        fbs = result[0].target.fallbacks
        assert any(f.type == "css" for f in fbs)

    def test_aria_label_selector_gets_label_fallback(self):
        result = self._n([{"action": "click", "selector": "[aria-label='Close']"}])
        fbs = result[0].target.fallbacks
        types = {f.type for f in fbs}
        assert "label" in types or "text" in types

    def test_placeholder_selector_gets_placeholder_fallback(self):
        result = self._n([{"action": "fill", "selector": "[placeholder='Search']"}])
        fbs = result[0].target.fallbacks
        types = {f.type for f in fbs}
        assert "placeholder" in types

    def test_existing_target_dict_preserved(self):
        step = {
            "action": "click",
            "target": {"primary": "#btn", "fallbacks": [{"type": "text", "value": "Submit"}]},
        }
        result = self._n([step])
        ns = result[0]
        assert ns.target.primary == "#btn"
        assert len(ns.target.fallbacks) == 1

    def test_default_timeout_applied(self):
        result = self._n([{"action": "click", "selector": "#x"}], default_timeout_ms=5000)
        assert result[0].timeout_ms == 5000

    def test_no_selector_no_target_step_passes_through(self):
        result = self._n([{"action": "click"}])
        ns = result[0]
        assert ns.action == "click"
        assert ns.target is None

    def test_empty_list(self):
        assert self._n([]) == []


# ══════════════════════════════════════════════════════════════════════════════
# 5. Selector healer new fallback types (unit — no browser required)
# ══════════════════════════════════════════════════════════════════════════════

class TestSelectorHealerNewTypes:
    """
    Tests for partial_text and aria_heuristic fallback types.
    Uses a minimal mock page object — no Playwright dependency.
    """

    def _make_page(self, *, visible_selector=None, visible_text=None, visible_aria=None):
        """Create a lightweight page mock."""
        class FakeLoc:
            def __init__(self, ok=True):
                self._ok = ok
                self.first = self

            def wait_for(self, **kw):
                if not self._ok:
                    from playwright.sync_api import TimeoutError as PwTimeout
                    raise PwTimeout("not found")

        class FakePage:
            def __init__(self, visible_sel, visible_txt, visible_aria):
                self._vis_sel = visible_sel
                self._vis_txt = visible_txt
                self._vis_aria = visible_aria

            def locator(self, sel):
                ok = (self._vis_sel and sel == self._vis_sel) or \
                     (self._vis_aria and sel == self._vis_aria)
                return FakeLoc(ok)

            def get_by_text(self, text, *, exact=True):
                ok = (self._vis_txt is not None and
                      (text in self._vis_txt if not exact else text == self._vis_txt))
                return FakeLoc(ok)

        return FakePage(visible_selector, visible_text, visible_aria)

    def test_partial_text_success(self):
        from services.selector_healer import _try_fallback
        page = self._make_page(visible_text="Submit Order")
        loc, resolved = _try_fallback(page, "partial_text", "Submit", 1000)
        assert "partial_text=Submit" == resolved

    def test_partial_text_empty_value_raises(self):
        from services.selector_healer import _try_fallback
        page = self._make_page()
        with pytest.raises(ValueError, match="empty value"):
            _try_fallback(page, "partial_text", "", 1000)

    def test_aria_heuristic_aria_label_success(self):
        from services.selector_healer import _try_fallback
        page = self._make_page(visible_selector="[aria-label*='Close']")
        loc, resolved = _try_fallback(page, "aria_heuristic", "Close", 1000)
        assert "aria-label" in resolved

    def test_aria_heuristic_empty_value_raises(self):
        from services.selector_healer import _try_fallback
        page = self._make_page()
        with pytest.raises(ValueError, match="empty value"):
            _try_fallback(page, "aria_heuristic", "", 1000)

    def test_partial_text_in_supported_types(self):
        from services.selector_healer import _SUPPORTED_TYPES
        assert "partial_text" in _SUPPORTED_TYPES

    def test_aria_heuristic_in_supported_types(self):
        from services.selector_healer import _SUPPORTED_TYPES
        assert "aria_heuristic" in _SUPPORTED_TYPES


# ══════════════════════════════════════════════════════════════════════════════
# 6. Lifecycle management
# ══════════════════════════════════════════════════════════════════════════════

def _make_tc(**overrides):
    """Build a minimal TestCase dict for testing."""
    from models.test_case import TestCase, LifecycleState
    data = dict(
        test_case_id="TC-TEST-001",
        name="Test",
        module="auth",
        type="smoke",
        priority="medium",
        steps=[{"action": "goto", "url": "https://example.com"}],
        lifecycle_state=LifecycleState.active,
    )
    data.update(overrides)
    return TestCase(**data)


class TestLifecycleModel:

    def test_lifecycle_state_enum_values(self):
        from models.test_case import LifecycleState
        assert LifecycleState.draft.value      == "draft"
        assert LifecycleState.active.value     == "active"
        assert LifecycleState.deprecated.value == "deprecated"
        assert LifecycleState.archived.value   == "archived"

    def test_testcase_default_lifecycle(self):
        from models.test_case import LifecycleState
        tc = _make_tc()
        assert tc.lifecycle_state == LifecycleState.active

    def test_version_history_empty_by_default(self):
        tc = _make_tc()
        assert tc.version_history == []

    def test_testcase_version_info_model(self):
        from models.test_case import TestCaseVersionInfo
        vi = TestCaseVersionInfo(version=1, change_summary="initial")
        assert vi.version == 1
        assert vi.change_summary == "initial"


class TestLifecycleService:

    def test_active_to_deprecated(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc()
        lifecycle_service.transition(tc, LifecycleState.deprecated, "no longer needed")
        assert tc.lifecycle_state == LifecycleState.deprecated
        assert tc.status == "inactive"

    def test_active_to_archived(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc()
        lifecycle_service.transition(tc, LifecycleState.archived)
        assert tc.lifecycle_state == LifecycleState.archived

    def test_deprecated_to_active(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc(lifecycle_state=LifecycleState.deprecated)
        lifecycle_service.transition(tc, LifecycleState.active)
        assert tc.lifecycle_state == LifecycleState.active
        assert tc.status == "active"

    def test_archived_is_terminal(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc(lifecycle_state=LifecycleState.archived)
        with pytest.raises(ValueError, match="terminal"):
            lifecycle_service.transition(tc, LifecycleState.active)

    def test_invalid_transition_raises(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc(lifecycle_state=LifecycleState.draft)
        with pytest.raises(ValueError):
            lifecycle_service.transition(tc, LifecycleState.deprecated)

    def test_transition_appends_version_history(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc()
        lifecycle_service.transition(tc, LifecycleState.deprecated)
        assert len(tc.version_history) == 1
        assert tc.version_history[0].lifecycle_state == "active"

    def test_transition_increments_version(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc()
        original_version = tc.version
        lifecycle_service.transition(tc, LifecycleState.deprecated)
        assert tc.version == original_version + 1

    def test_idempotent_transition(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc()
        v_before = tc.version
        lifecycle_service.transition(tc, LifecycleState.active)  # same state
        assert tc.version == v_before   # no change
        assert len(tc.version_history) == 0

    def test_bump_version(self):
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc()
        v_before = tc.version
        lifecycle_service.bump_version(tc, change_summary="steps updated")
        assert tc.version == v_before + 1
        assert tc.version_history[0].change_summary == "steps updated"

    def test_is_runnable_active(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc()
        assert lifecycle_service.is_runnable(tc) is True

    def test_is_runnable_archived(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        tc = _make_tc(lifecycle_state=LifecycleState.archived)
        assert lifecycle_service.is_runnable(tc) is False

    def test_get_allowed_transitions(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        allowed = lifecycle_service.get_allowed_transitions(LifecycleState.active)
        assert LifecycleState.deprecated in allowed
        assert LifecycleState.archived in allowed

    def test_get_allowed_transitions_archived_empty(self):
        from models.test_case import LifecycleState
        from services.lifecycle_service import lifecycle_service
        allowed = lifecycle_service.get_allowed_transitions(LifecycleState.archived)
        assert allowed == []


# ══════════════════════════════════════════════════════════════════════════════
# 7. Orchestrator batch sharding + fair queue
# ══════════════════════════════════════════════════════════════════════════════

class TestOrchestratorSharding:
    """Tests for sharding and batch dispatch — no real execution needed."""

    def _reset(self):
        from services.catalog_orchestrator import _reset_for_testing
        _reset_for_testing()

    def test_shard_size_config_exists(self):
        from services.catalog_orchestrator import SHARD_SIZE
        assert isinstance(SHARD_SIZE, int)
        assert SHARD_SIZE >= 1

    def test_enqueue_with_sharding_two_shards(self):
        from services.catalog_orchestrator import orchestrator_service, _reset_for_testing
        _reset_for_testing()
        tc_ids = [f"TC-{i:03d}" for i in range(25)]
        jobs = orchestrator_service.enqueue_with_sharding(tc_ids, shard_size=10)
        assert len(jobs) == 3          # 10 + 10 + 5
        total = sum(j.total_count for j in jobs)
        assert total == 25

    def test_enqueue_with_sharding_job_type(self):
        from services.catalog_orchestrator import orchestrator_service, _reset_for_testing
        _reset_for_testing()
        jobs = orchestrator_service.enqueue_with_sharding(["A", "B", "C"], shard_size=2)
        assert all(j.job_type == "shard" for j in jobs)

    def test_enqueue_with_sharding_single_shard(self):
        from services.catalog_orchestrator import orchestrator_service, _reset_for_testing
        _reset_for_testing()
        jobs = orchestrator_service.enqueue_with_sharding(["A", "B"], shard_size=10)
        assert len(jobs) == 1
        assert jobs[0].total_count == 2

    def test_enqueue_batch_empty_returns_failed_job(self):
        from services.catalog_orchestrator import orchestrator_service, _reset_for_testing
        _reset_for_testing()
        job = orchestrator_service.enqueue_batch([])
        assert job.status == "failed"

    def test_enqueue_batch_sets_batch_type(self):
        from services.catalog_orchestrator import orchestrator_service, _reset_for_testing
        _reset_for_testing()
        job = orchestrator_service.enqueue_batch(["TC-001", "TC-002"])
        assert job.job_type == "batch"
        assert job.status == "queued"
        assert job.total_count == 2


class TestOrchestratorFairQueue:
    """Tests for fair FIFO ordering within same priority+type bucket."""

    def test_schedule_tests_preserves_fifo_within_bucket(self):
        """
        When tests have equal priority+type, earlier tests (lower index) must
        come out first after sorting.  We mock catalog_repo so no DB is needed.
        """
        import sys
        import types

        # Build a fake catalog_repo
        fake_repo = types.SimpleNamespace()

        def _get(tc_id):
            priority = "medium"
            typ = "smoke"
            return types.SimpleNamespace(priority=priority, type=typ)

        fake_repo.get_test_case = _get

        # Patch it into the module namespace temporarily
        from services import catalog_orchestrator as orch_mod
        from services.db import catalog_repository as cat_repo_mod
        original = cat_repo_mod.catalog_repo
        cat_repo_mod.catalog_repo = fake_repo

        try:
            sorted_ids, _ = orch_mod._schedule_tests(["TC-C", "TC-A", "TC-B"])
            # All same priority+type → FIFO: order must be C, A, B
            assert sorted_ids == ["TC-C", "TC-A", "TC-B"]
        finally:
            cat_repo_mod.catalog_repo = original


# ══════════════════════════════════════════════════════════════════════════════
# 8. Evidence capture
# ══════════════════════════════════════════════════════════════════════════════

class TestEvidenceCapture:

    def test_import(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig, EvidenceBundle  # noqa

    def test_default_config(self):
        from runners.evidence_capture import EvidenceConfig
        cfg = EvidenceConfig()
        assert cfg.screenshot_timeline is False
        assert cfg.video is False
        assert cfg.network_trace is False
        assert cfg.dom_snapshot is False
        assert cfg.action_trace is False

    def test_finalize_returns_bundle(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig
        cap = EvidenceCapture(run_id="run-1", config=EvidenceConfig())
        bundle = cap.finalize(metadata={"step_count": 3})
        assert bundle.run_id == "run-1"
        assert bundle.metadata["step_count"] == 3

    def test_screenshot_skipped_when_not_configured(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig
        cap = EvidenceCapture(run_id="run-2", config=EvidenceConfig(screenshot_timeline=False))
        result = cap.capture_step_screenshot(None, step_index=0, action="goto")
        assert result is None
        bundle = cap.finalize()
        assert bundle.screenshots == []

    def test_dom_snapshot_skipped_when_not_configured(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig
        cap = EvidenceCapture(run_id="run-3", config=EvidenceConfig(dom_snapshot=False))
        result = cap.capture_dom_snapshot(None, label="test")
        assert result is None

    def test_start_trace_with_none_context_does_not_raise(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig
        cap = EvidenceCapture(run_id="run-4", config=EvidenceConfig(action_trace=True))
        cap.start_trace(None)   # should not raise

    def test_stop_trace_without_start_returns_none(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig
        cap = EvidenceCapture(run_id="run-5", config=EvidenceConfig(action_trace=True))
        result = cap.stop_trace(None)
        assert result is None

    def test_bundle_to_dict(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig
        cap = EvidenceCapture(run_id="run-6", config=EvidenceConfig())
        bundle = cap.finalize()
        d = bundle.to_dict()
        assert d["run_id"] == "run-6"
        assert "screenshots" in d
        assert "network_events" in d
        assert "dom_snapshots" in d

    def test_make_evidence_config_from_dict(self):
        from runners.evidence_capture import make_evidence_config_from_dict
        cfg = make_evidence_config_from_dict({
            "screenshot_timeline": True,
            "dom_snapshot": True,
            "unknown_key": "ignored",
        })
        assert cfg.screenshot_timeline is True
        assert cfg.dom_snapshot is True
        assert cfg.video is False

    def test_metadata_counts_populated_on_finalize(self):
        from runners.evidence_capture import EvidenceCapture, EvidenceConfig
        cap = EvidenceCapture(run_id="run-7", config=EvidenceConfig())
        bundle = cap.finalize()
        assert "screenshot_count" in bundle.metadata
        assert bundle.metadata["screenshot_count"] == 0
