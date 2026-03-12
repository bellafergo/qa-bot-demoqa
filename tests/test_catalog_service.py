# tests/test_catalog_service.py
"""
Unit tests for the Test Catalog Service.

No browser, no network, no OpenAI.
Tests cover: CRUD, step normalization, assertion conversion, suite filtering.
"""
from __future__ import annotations

import pytest
from models.test_case import TestCaseCreate
from models.test_run import TestRun
from services.test_catalog_service import (
    TestCatalogService,
    _step_to_runner,
    _assertion_to_step,
    _build_runner_steps,
)
from models.test_case import TestCase, TestStep, TestAssertion


# ── Helpers ───────────────────────────────────────────────────────────────────

def _make_payload(**overrides) -> TestCaseCreate:
    base = {
        "test_case_id": f"TC-TEST-{id(overrides):04x}",
        "name": "Test case",
        "module": "demo",
        "type": "smoke",
        "priority": "medium",
        "steps": [{"action": "goto", "value": "https://example.com"}],
        "assertions": [],
    }
    base.update(overrides)
    return TestCaseCreate(**base)


def _fresh_service() -> TestCatalogService:
    """Return a new service instance with a clean DB state."""
    from services.test_catalog_service import _reset_for_testing
    _reset_for_testing()
    return TestCatalogService()


# ── Step normalizer ───────────────────────────────────────────────────────────

class TestStepToRunner:
    def test_goto_value_becomes_url(self):
        s = _step_to_runner({"action": "goto", "value": "https://example.com"})
        assert s["action"] == "goto"
        assert s["url"] == "https://example.com"
        assert "value" not in s

    def test_goto_target_becomes_url(self):
        s = _step_to_runner({"action": "goto", "target": "https://x.com"})
        assert s["url"] == "https://x.com"

    def test_input_becomes_fill(self):
        s = _step_to_runner({"action": "input", "target": "username", "value": "tom"})
        assert s["action"] == "fill"
        assert s["selector"] == "username"
        assert s["value"] == "tom"

    def test_type_alias_becomes_fill(self):
        s = _step_to_runner({"action": "type", "target": "#email", "value": "a@b.com"})
        assert s["action"] == "fill"

    def test_click_target_becomes_selector(self):
        s = _step_to_runner({"action": "click", "target": "login button"})
        assert s["action"] == "click"
        assert s["selector"] == "login button"

    def test_wait_ms_value_becomes_ms(self):
        s = _step_to_runner({"action": "wait_ms", "value": "750"})
        assert s["action"] == "wait_ms"
        assert s["ms"] == 750

    def test_wait_alias(self):
        s = _step_to_runner({"action": "wait", "ms": 300})
        assert s["action"] == "wait_ms"

    def test_assert_text_contains_value_becomes_text(self):
        s = _step_to_runner({"action": "assert_text_contains", "value": "Welcome"})
        assert s["action"] == "assert_text_contains"
        assert s["text"] == "Welcome"
        assert s["selector"] == "body"

    def test_assert_text_contains_target_becomes_selector(self):
        s = _step_to_runner({
            "action": "assert_text_contains",
            "target": ".flash",
            "value": "Success",
        })
        assert s["selector"] == ".flash"
        assert s["text"] == "Success"

    def test_assert_url_contains_value_preserved(self):
        s = _step_to_runner({"action": "assert_url_contains", "value": "/secure"})
        assert s["value"] == "/secure"

    def test_unknown_action_passes_through(self):
        s = _step_to_runner({"action": "screenshot"})
        assert s["action"] == "screenshot"

    def test_navigate_alias(self):
        s = _step_to_runner({"action": "navigate", "value": "https://y.com"})
        assert s["action"] == "goto"
        assert s["url"] == "https://y.com"


# ── Assertion converter ───────────────────────────────────────────────────────

class TestAssertionToStep:
    def test_text_visible(self):
        step = _assertion_to_step({"type": "text_visible", "value": "Welcome"})
        assert step is not None
        assert step["action"] == "assert_text_contains"
        assert step["text"] == "Welcome"
        assert step["selector"] == "body"

    def test_text_visible_custom_selector(self):
        step = _assertion_to_step({"type": "text_visible", "value": "Yo", "target": ".msg"})
        assert step["selector"] == ".msg"

    def test_url_contains(self):
        step = _assertion_to_step({"type": "url_contains", "value": "/dashboard"})
        assert step["action"] == "assert_url_contains"
        assert step["value"] == "/dashboard"

    def test_element_visible(self):
        step = _assertion_to_step({"type": "element_visible", "target": "#btn"})
        assert step["action"] == "assert_visible"
        assert step["selector"] == "#btn"

    def test_element_not_visible(self):
        step = _assertion_to_step({"type": "element_not_visible", "target": ".error"})
        assert step["action"] == "assert_not_visible"
        assert step["selector"] == ".error"

    def test_text_visible_without_value_returns_none(self):
        step = _assertion_to_step({"type": "text_visible"})
        assert step is None

    def test_element_visible_without_target_returns_none(self):
        step = _assertion_to_step({"type": "element_visible"})
        assert step is None

    def test_unknown_type_returns_none(self):
        step = _assertion_to_step({"type": "screenshot_matches", "value": "x"})
        assert step is None


# ── Runner steps builder ──────────────────────────────────────────────────────

class TestBuildRunnerSteps:
    def _tc(self, steps, assertions=None, base_url=None):
        return TestCase(
            test_case_id="TC-BUILD-001",
            name="Build test",
            module="demo",
            type="smoke",
            priority="low",
            steps=[TestStep(**s) for s in steps],
            assertions=[TestAssertion(**a) for a in (assertions or [])],
            base_url=base_url,
        )

    def test_prepends_goto_when_missing(self):
        tc = self._tc(
            [{"action": "input", "target": "email", "value": "x@y.com"}],
            base_url="https://example.com",
        )
        steps = _build_runner_steps(tc)
        assert steps[0]["action"] == "goto"
        assert steps[0]["url"] == "https://example.com"

    def test_does_not_double_goto(self):
        tc = self._tc([{"action": "goto", "value": "https://x.com"}])
        steps = _build_runner_steps(tc)
        gotos = [s for s in steps if s["action"] == "goto"]
        assert len(gotos) == 1

    def test_assertions_appended_after_steps(self):
        tc = self._tc(
            [{"action": "goto", "value": "https://x.com"}],
            assertions=[{"type": "text_visible", "value": "Hello"}],
        )
        steps = _build_runner_steps(tc)
        last = steps[-1]
        assert last["action"] == "assert_text_contains"
        assert last["text"] == "Hello"

    def test_base_url_override(self):
        tc = self._tc([{"action": "click", "target": "#btn"}], base_url="https://original.com")
        steps = _build_runner_steps(tc, base_url="https://override.com")
        assert steps[0]["url"] == "https://override.com"

    def test_full_login_flow(self):
        tc = self._tc(
            [
                {"action": "goto",  "value": "https://example.com/login"},
                {"action": "input", "target": "username", "value": "user"},
                {"action": "input", "target": "password", "value": "pass"},
                {"action": "click", "target": "login button"},
            ],
            assertions=[
                {"type": "text_visible", "value": "Dashboard"},
                {"type": "url_contains", "value": "/dashboard"},
            ],
        )
        steps = _build_runner_steps(tc)
        actions = [s["action"] for s in steps]
        assert actions == [
            "goto", "fill", "fill", "click",
            "assert_text_contains", "assert_url_contains",
        ]


# ── CRUD ──────────────────────────────────────────────────────────────────────

class TestCatalogCRUD:
    def test_create_and_get(self):
        svc = _fresh_service()
        payload = _make_payload(test_case_id="TC-CRUD-001")
        tc = svc.create_test_case(payload)
        assert tc.test_case_id == "TC-CRUD-001"
        assert tc.id  # uuid assigned
        found = svc.get_test_case("TC-CRUD-001")
        assert found is not None
        assert found.name == payload.name

    def test_duplicate_id_raises(self):
        svc = _fresh_service()
        svc.create_test_case(_make_payload(test_case_id="TC-DUP-001"))
        with pytest.raises(ValueError, match="already exists"):
            svc.create_test_case(_make_payload(test_case_id="TC-DUP-001"))

    def test_get_unknown_returns_none(self):
        svc = _fresh_service()
        assert svc.get_test_case("TC-NOPE-999") is None

    def test_delete_removes(self):
        svc = _fresh_service()
        svc.create_test_case(_make_payload(test_case_id="TC-DEL-001"))
        assert svc.delete_test_case("TC-DEL-001") is True
        assert svc.get_test_case("TC-DEL-001") is None

    def test_delete_missing_returns_false(self):
        svc = _fresh_service()
        assert svc.delete_test_case("TC-GHOST") is False

    def test_list_all(self):
        svc = _fresh_service()
        for i in range(3):
            svc.create_test_case(_make_payload(test_case_id=f"TC-LIST-{i:03d}"))
        cases = svc.list_test_cases(status=None)
        assert len(cases) == 3

    def test_list_filter_by_module(self):
        svc = _fresh_service()
        svc.create_test_case(_make_payload(test_case_id="TC-MOD-A", module="auth"))
        svc.create_test_case(_make_payload(test_case_id="TC-MOD-B", module="cart"))
        auth_cases = svc.list_test_cases(module="auth", status=None)
        assert len(auth_cases) == 1
        assert auth_cases[0].test_case_id == "TC-MOD-A"

    def test_list_filter_by_type(self):
        svc = _fresh_service()
        svc.create_test_case(_make_payload(test_case_id="TC-TYPE-S", type="smoke"))
        svc.create_test_case(_make_payload(test_case_id="TC-TYPE-R", type="regression"))
        smokes = svc.list_test_cases(type_="smoke", status=None)
        assert all(c.type == "smoke" for c in smokes)

    def test_list_filter_by_tags(self):
        svc = _fresh_service()
        svc.create_test_case(_make_payload(test_case_id="TC-TAG-A", tags=["login", "auth"]))
        svc.create_test_case(_make_payload(test_case_id="TC-TAG-B", tags=["cart"]))
        login_cases = svc.list_test_cases(tags=["login"], status=None)
        assert len(login_cases) == 1
        assert login_cases[0].test_case_id == "TC-TAG-A"

    def test_list_inactive_excluded_by_default(self):
        svc = _fresh_service()
        svc.create_test_case(_make_payload(test_case_id="TC-ACT-A", status="active"))
        svc.create_test_case(_make_payload(test_case_id="TC-ACT-B", status="inactive"))
        active = svc.list_test_cases(status="active")
        assert all(c.status == "active" for c in active)
        assert len(active) == 1

    def test_limit_respected(self):
        svc = _fresh_service()
        for i in range(10):
            svc.create_test_case(_make_payload(test_case_id=f"TC-LIM-{i:03d}"))
        cases = svc.list_test_cases(status=None, limit=3)
        assert len(cases) == 3


# ── Seed loader ───────────────────────────────────────────────────────────────

class TestSeedLoader:
    def test_seed_loads_without_error(self):
        from services.test_catalog_service import _reset_for_testing, load_seed_catalog
        _reset_for_testing()
        load_seed_catalog()
        svc = TestCatalogService()
        cases = svc.list_test_cases(status=None)
        assert len(cases) >= 5

    def test_seed_idempotent(self):
        from services.test_catalog_service import load_seed_catalog
        load_seed_catalog()  # second call — should not duplicate
        svc = TestCatalogService()
        cases = svc.list_test_cases(status=None)
        ids = [c.test_case_id for c in cases]
        assert len(ids) == len(set(ids)), "Duplicate test_case_ids after double seed"

    def test_seed_includes_demo001(self):
        from services.test_catalog_service import load_seed_catalog
        load_seed_catalog()
        svc = TestCatalogService()
        tc = svc.get_test_case("TC-DEMO-001")
        assert tc is not None
        assert "login" in tc.name.lower()

    def test_seed_includes_negative_case(self):
        from services.test_catalog_service import load_seed_catalog
        load_seed_catalog()
        svc = TestCatalogService()
        negatives = svc.list_test_cases(type_="negative", status=None)
        assert len(negatives) >= 1


# ── Run history (no browser) ──────────────────────────────────────────────────

class TestRunHistory:
    def test_save_and_retrieve_run(self):
        from services.test_catalog_service import _reset_for_testing
        _reset_for_testing()
        svc = TestCatalogService()

        run = TestRun(
            test_case_id="TC-HIST-001",
            test_name="History test",
            environment="staging",
            status="pass",
            duration_ms=1200,
        )
        svc._save_run(run)

        retrieved = svc.get_run(run.run_id)
        assert retrieved is not None
        assert retrieved.test_case_id == "TC-HIST-001"
        assert retrieved.status == "pass"

    def test_list_runs_most_recent_first(self):
        from services.test_catalog_service import _reset_for_testing
        _reset_for_testing()
        svc = TestCatalogService()

        for i in range(3):
            svc._save_run(TestRun(
                test_case_id=f"TC-R-{i}",
                environment="default",
                status="pass",
            ))

        runs = svc.list_runs()
        assert runs[0].test_case_id == "TC-R-2"   # most recent last saved → first in list

    def test_list_runs_filtered_by_test_case_id(self):
        from services.test_catalog_service import _reset_for_testing
        _reset_for_testing()
        svc = TestCatalogService()

        for tc_id in ("TC-A", "TC-B", "TC-A"):
            svc._save_run(TestRun(test_case_id=tc_id, environment="x", status="pass"))

        a_runs = svc.list_runs(test_case_id="TC-A")
        assert len(a_runs) == 2
        assert all(r.test_case_id == "TC-A" for r in a_runs)
