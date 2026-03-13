# tests/test_desktop_runner.py
"""
Desktop Runner tests.

All tests use MockDesktopBackend — no real Windows GUI, no real POS required.
All tests are deterministic and CI-safe.

Covers:
  1. DesktopAdapter (mock mode) — all actions
  2. Desktop target resolution (MockControl path)
  3. run_desktop_test — full POS-style flows
  4. Failure cases — assertion errors, bad actions, missing target
  5. Result dict format compatibility with TestRun model
  6. Evidence capture (screenshots)
  7. test_type=desktop in catalog model
  8. _build_desktop_steps() step normalization
  9. Orchestrator dispatch (TestCatalogService._execute() → desktop runner)
 10. Action alias normalization
"""
from __future__ import annotations

import pytest
from unittest.mock import MagicMock, patch


# ── 1. DesktopAdapter (mock mode) ─────────────────────────────────────────────

class TestDesktopAdapterMock:
    def _adapter(self):
        from runners.desktop_adapter import DesktopAdapter
        return DesktopAdapter(use_mock=True)

    def test_always_uses_mock_when_forced(self):
        adapter = self._adapter()
        assert adapter.is_mock is True

    def test_launch_app(self):
        adapter = self._adapter()
        adapter.launch_app("C:/POS/app.exe")
        assert any("launch_app" in entry for entry in adapter.call_log)

    def test_attach_window(self):
        adapter = self._adapter()
        adapter.attach_window("POS Main Window")
        assert any("attach_window" in entry for entry in adapter.call_log)

    def test_focus_window(self):
        adapter = self._adapter()
        adapter.focus_window("POS Main Window")
        assert any("focus_window" in entry for entry in adapter.call_log)

    def test_click(self):
        adapter = self._adapter()
        adapter.click("login_button")
        assert any("click" in e and "login_button" in e for e in adapter.call_log)

    def test_input_text(self):
        adapter = self._adapter()
        adapter.input_text("username_field", "cashier01")
        assert any("input_text" in e and "cashier01" in e for e in adapter.call_log)

    def test_type_keys(self):
        adapter = self._adapter()
        adapter.type_keys("username_field", "{ENTER}")
        assert any("type_keys" in e for e in adapter.call_log)

    def test_select(self):
        adapter = self._adapter()
        adapter.select("payment_combo", "Cash")
        assert any("select" in e and "Cash" in e for e in adapter.call_log)

    def test_read_text_returns_string(self):
        adapter = self._adapter()
        text = adapter.read_text("status_label")
        assert isinstance(text, str)

    def test_assert_text_contains_passes(self):
        adapter = self._adapter()
        # By default MockControl.window_text() returns the control name
        adapter.set_control_text("status_label", "Venta realizada")
        adapter.assert_text_contains("status_label", "Venta realizada")  # no raise

    def test_assert_text_contains_fails(self):
        adapter = self._adapter()
        adapter.set_control_text("status_label", "Login failed")
        with pytest.raises(AssertionError) as exc_info:
            adapter.assert_text_contains("status_label", "Venta realizada")
        assert "Venta realizada" in str(exc_info.value)

    def test_assert_exists_passes(self):
        adapter = self._adapter()
        adapter.assert_exists("any_control")  # mock always returns True

    def test_wait_for(self):
        adapter = self._adapter()
        adapter.wait_for("login_button", timeout_ms=1000)
        assert any("wait_for" in e for e in adapter.call_log)

    def test_screenshot_returns_base64(self):
        adapter = self._adapter()
        b64 = adapter.screenshot()
        assert isinstance(b64, str)
        assert len(b64) > 10

    def test_set_control_text_only_on_mock(self):
        adapter = self._adapter()
        adapter.set_control_text("lbl", "hello world")
        text = adapter.read_text("lbl")
        assert "hello world" in text


# ── 2. Desktop target resolution ──────────────────────────────────────────────

class TestDesktopTargetResolution:
    def _mock_window(self):
        from runners.desktop_adapter import MockControl
        return MockControl(name="MainWindow")

    def test_resolve_string_target(self):
        from runners.desktop_target import resolve_desktop_target
        window = self._mock_window()
        ctrl, used, resolved, meta = resolve_desktop_target(window, "login_button")
        assert used == "primary"
        assert resolved == "login_button"
        assert meta["fallback_index"] is None
        assert meta["attempts"] == 1

    def test_resolve_dict_target_primary(self):
        from runners.desktop_target import resolve_desktop_target
        window = self._mock_window()
        target = {"primary": "login_button", "primary_strategy": "control_name"}
        ctrl, used, resolved, meta = resolve_desktop_target(window, target)
        assert used == "primary"

    def test_resolve_dict_target_fallback(self):
        from runners.desktop_target import resolve_desktop_target
        window = self._mock_window()
        # Mock always succeeds, so primary won't fail — test the fallback path
        # by providing an empty primary which forces fallback evaluation
        target = {
            "primary": "",
            "fallbacks": [{"type": "automation_id", "value": "btn_login"}],
        }
        ctrl, used, resolved, meta = resolve_desktop_target(window, target)
        assert used == "automation_id"
        assert meta["fallback_index"] == 0

    def test_resolve_empty_target_raises(self):
        from runners.desktop_target import resolve_desktop_target
        window = self._mock_window()
        with pytest.raises(ValueError):
            resolve_desktop_target(window, "")

    def test_resolve_none_target_raises(self):
        from runners.desktop_target import resolve_desktop_target
        window = self._mock_window()
        with pytest.raises(ValueError):
            resolve_desktop_target(window, None)

    def test_supported_strategies(self):
        from runners.desktop_target import _SUPPORTED_STRATEGIES
        assert "automation_id" in _SUPPORTED_STRATEGIES
        assert "control_name"  in _SUPPORTED_STRATEGIES
        assert "class_name"    in _SUPPORTED_STRATEGIES
        assert "text_label"    in _SUPPORTED_STRATEGIES
        assert "control_type"  in _SUPPORTED_STRATEGIES


# ── 3. run_desktop_test — full POS flow ───────────────────────────────────────

class TestDesktopRunnerPOSFlow:
    def _pos_steps(self):
        return [
            {"action": "launch_app",          "target": "C:/POS/app.exe"},
            {"action": "attach_window",        "target": "POS Main Window"},
            {"action": "input",                "target": "username_field",  "value": "cashier01"},
            {"action": "input",                "target": "password_field",  "value": "pass123"},
            {"action": "click",                "target": "login_button"},
            {"action": "input",                "target": "product_code",    "value": "123456"},
            {"action": "click",                "target": "add_item_button"},
            {"action": "click",                "target": "payment_button"},
            {"action": "select",               "target": "payment_method",  "value": "Cash"},
            {"action": "click",                "target": "confirm_button"},
            {"action": "assert_text_contains", "target": "status_label",   "value": "status_label"},
        ]

    def _run(self, steps=None):
        from runners.desktop_runner import run_desktop_test
        return run_desktop_test(steps or self._pos_steps(), use_mock=True)

    def test_full_pos_flow_passes(self):
        result = self._run()
        assert result["ok"] is True
        assert result["status"] == "passed"

    def test_result_has_required_keys(self):
        result = self._run()
        required = ["ok", "status", "outcome", "reason", "evidence_id",
                    "steps", "logs", "screenshot_b64", "duration_ms", "meta", "evidence"]
        for k in required:
            assert k in result, f"Missing key: {k}"

    def test_steps_recorded(self):
        result = self._run()
        assert len(result["steps"]) == len(self._pos_steps())

    def test_each_step_has_status(self):
        result = self._run()
        for s in result["steps"]:
            assert s["status"] in ("passed", "failed", "error")

    def test_evidence_id_format(self):
        result = self._run()
        assert result["evidence_id"].startswith("DT-")

    def test_screenshots_captured(self):
        result = self._run()
        evidence = result["evidence"]
        assert isinstance(evidence["screenshots"], list)
        assert len(evidence["screenshots"]) > 0

    def test_duration_ms_positive(self):
        result = self._run()
        assert result["duration_ms"] >= 0

    def test_runner_meta(self):
        result = self._run()
        assert result["meta"]["runner"] == "desktop"
        assert result["meta"]["is_mock"] is True

    def test_logs_non_empty(self):
        result = self._run()
        assert len(result["logs"]) > 0
        assert any("[PLAN]" in l for l in result["logs"])
        assert any("[DONE]" in l for l in result["logs"])


# ── 4. Failure cases ──────────────────────────────────────────────────────────

class TestDesktopRunnerFailures:
    def _run(self, steps):
        from runners.desktop_runner import run_desktop_test
        return run_desktop_test(steps, use_mock=True)

    def test_empty_steps_returns_failed(self):
        result = self._run([])
        assert result["ok"] is False
        assert result["status"] == "failed"

    def test_assertion_failure_propagates(self):
        from runners.desktop_adapter import DesktopAdapter
        adapter = DesktopAdapter(use_mock=True)
        adapter.set_control_text("status_label", "Error: login failed")

        # Patch DesktopAdapter to return our pre-configured adapter
        with patch("runners.desktop_runner.DesktopAdapter", return_value=adapter):
            result = self._run([
                {"action": "assert_text_contains", "target": "status_label", "value": "Venta realizada"},
            ])
        assert result["ok"] is False
        assert result["status"] == "failed"
        assert "Venta realizada" in (result["reason"] or "")

    def test_missing_target_on_click(self):
        result = self._run([{"action": "click", "target": ""}])
        assert result["ok"] is False

    def test_unknown_action_fails(self):
        result = self._run([{"action": "nonexistent_action", "target": "btn"}])
        assert result["ok"] is False

    def test_failed_step_recorded_in_steps(self):
        result = self._run([{"action": "click", "target": ""}])
        assert any(s["status"] in ("failed", "error") for s in result["steps"])

    def test_missing_value_for_assert_text_contains(self):
        result = self._run([
            {"action": "assert_text_contains", "target": "label", "value": ""},
        ])
        assert result["ok"] is False


# ── 5. Action alias normalization ─────────────────────────────────────────────

class TestDesktopActionAliases:
    def test_input_alias(self):
        from runners.desktop_runner import _normalize_desktop_action
        assert _normalize_desktop_action("input") == "input_text"

    def test_fill_alias(self):
        from runners.desktop_runner import _normalize_desktop_action
        assert _normalize_desktop_action("fill") == "input_text"

    def test_assert_text_alias(self):
        from runners.desktop_runner import _normalize_desktop_action
        assert _normalize_desktop_action("assert_text") == "assert_text_contains"

    def test_launch_alias(self):
        from runners.desktop_runner import _normalize_desktop_action
        assert _normalize_desktop_action("launch") == "launch_app"
        assert _normalize_desktop_action("start") == "launch_app"

    def test_unknown_action_passthrough(self):
        from runners.desktop_runner import _normalize_desktop_action
        assert _normalize_desktop_action("custom_action") == "custom_action"

    def test_run_accepts_input_alias(self):
        from runners.desktop_runner import run_desktop_test
        result = run_desktop_test([
            {"action": "input", "target": "field", "value": "hello"},
        ], use_mock=True)
        assert result["steps"][0]["status"] == "passed"

    def test_run_accepts_fill_alias(self):
        from runners.desktop_runner import run_desktop_test
        result = run_desktop_test([
            {"action": "fill", "target": "field", "value": "hello"},
        ], use_mock=True)
        assert result["steps"][0]["status"] == "passed"


# ── 6. Evidence generation ────────────────────────────────────────────────────

class TestDesktopEvidence:
    def _run(self, steps):
        from runners.desktop_runner import run_desktop_test
        return run_desktop_test(steps, use_mock=True)

    def test_evidence_bundle_keys(self):
        result = self._run([{"action": "launch_app", "target": "app.exe"}])
        ev = result["evidence"]
        assert "run_id"         in ev
        assert "screenshots"    in ev
        assert "network_events" in ev
        assert "dom_snapshots"  in ev
        assert "trace_path"     in ev
        assert "metadata"       in ev

    def test_no_network_events(self):
        result = self._run([{"action": "click", "target": "btn"}])
        assert result["evidence"]["network_events"] == []

    def test_no_dom_snapshots(self):
        result = self._run([{"action": "click", "target": "btn"}])
        assert result["evidence"]["dom_snapshots"] == []

    def test_screenshots_have_b64(self):
        result = self._run([
            {"action": "launch_app",   "target": "app.exe"},
            {"action": "click",        "target": "btn"},
        ])
        screenshots = result["evidence"]["screenshots"]
        for s in screenshots:
            assert "b64" in s
            assert isinstance(s["b64"], str)

    def test_explicit_screenshot_step(self):
        result = self._run([{"action": "screenshot"}])
        assert result["steps"][0]["status"] == "passed"
        assert len(result["evidence"]["screenshots"]) > 0

    def test_screenshot_b64_at_top_level(self):
        result = self._run([{"action": "launch_app", "target": "app.exe"}])
        assert result["screenshot_b64"] is not None


# ── 7. test_type = "desktop" in models ────────────────────────────────────────

class TestDesktopTestType:
    def test_test_case_accepts_desktop(self):
        from models.test_case import TestCase, TestStep
        tc = TestCase(
            test_case_id="TC-POS-001",
            name="POS Login",
            module="pos",
            type="smoke",
            priority="high",
            test_type="desktop",
            steps=[TestStep(action="launch_app", target="C:/POS/app.exe")],
        )
        assert tc.test_type == "desktop"

    def test_test_case_create_accepts_desktop(self):
        from models.test_case import TestCaseCreate
        payload = TestCaseCreate(
            test_case_id="TC-POS-002",
            name="POS Sale",
            module="pos",
            type="functional",
            priority="critical",
            test_type="desktop",
            steps=[{"action": "launch_app", "target": "C:/POS/app.exe"}],
        )
        assert payload.test_type == "desktop"

    def test_test_case_rejects_invalid_test_type(self):
        from models.test_case import TestCase, TestStep
        with pytest.raises(Exception):
            TestCase(
                test_case_id="TC-BAD-001",
                name="Bad",
                module="x",
                type="smoke",
                priority="low",
                test_type="invalid_type",
                steps=[TestStep(action="click", target="btn")],
            )

    def test_ui_still_accepted(self):
        from models.test_case import TestCase, TestStep
        tc = TestCase(
            test_case_id="TC-UI-001",
            name="Web Login",
            module="web",
            type="smoke",
            priority="high",
            test_type="ui",
            steps=[TestStep(action="goto", url="https://example.com")],
        )
        assert tc.test_type == "ui"

    def test_api_still_accepted(self):
        from models.test_case import TestCase, TestStep
        tc = TestCase(
            test_case_id="TC-API-001",
            name="API Health",
            module="api",
            type="smoke",
            priority="low",
            test_type="api",
            steps=[TestStep(action="request", target="/health")],
        )
        assert tc.test_type == "api"


# ── 8. _build_desktop_steps normalization ─────────────────────────────────────

class TestBuildDesktopSteps:
    def _build(self, raw_steps, assertions=None):
        from services.test_catalog_service import _build_desktop_steps
        from models.test_case import TestCase, TestStep, TestAssertion
        tc = TestCase(
            test_case_id="TC-POS-BUILD",
            name="Build Test",
            module="pos",
            type="smoke",
            priority="low",
            test_type="desktop",
            steps=[TestStep(**s) for s in raw_steps],
            assertions=[TestAssertion(**a) for a in (assertions or [])],
        )
        return _build_desktop_steps(tc)

    def test_no_goto_injected(self):
        steps = self._build([{"action": "launch_app", "target": "app.exe"}])
        assert all(s["action"] != "goto" for s in steps)

    def test_input_normalized_to_input_text(self):
        steps = self._build([{"action": "input", "target": "field", "value": "x"}])
        assert steps[0]["action"] == "input_text"

    def test_fill_normalized_to_input_text(self):
        steps = self._build([{"action": "fill", "target": "field", "value": "x"}])
        assert steps[0]["action"] == "input_text"

    def test_target_preserved(self):
        steps = self._build([{"action": "click", "target": "login_button"}])
        assert steps[0]["target"] == "login_button"

    def test_assertion_appended_as_step(self):
        steps = self._build(
            [{"action": "click", "target": "btn"}],
            assertions=[{"type": "assert_text_contains", "target": "lbl", "value": "OK"}],
        )
        assert any(s["action"] == "assert_text_contains" for s in steps)
        assert any(s.get("value") == "OK" for s in steps)

    def test_assert_exists_assertion_appended(self):
        steps = self._build(
            [{"action": "launch_app", "target": "app.exe"}],
            assertions=[{"type": "assert_exists", "target": "main_window"}],
        )
        assert any(s["action"] == "assert_exists" for s in steps)

    def test_all_steps_preserved(self):
        raw = [
            {"action": "launch_app",   "target": "app.exe"},
            {"action": "attach_window","target": "POS"},
            {"action": "click",        "target": "btn"},
        ]
        steps = self._build(raw)
        assert len(steps) == 3


# ── 9. Orchestrator dispatch (TestCatalogService) ─────────────────────────────

class TestOrchestratorDesktopDispatch:
    def _seed_desktop_tc(self, catalog_svc, tc_id="TC-POS-ORCH-001"):
        from models.test_case import TestCaseCreate
        payload = TestCaseCreate(
            test_case_id=tc_id,
            name="POS Orchestrator Test",
            module="pos",
            type="smoke",
            priority="high",
            test_type="desktop",
            steps=[
                {"action": "launch_app",          "target": "C:/POS/app.exe"},
                {"action": "attach_window",        "target": "POS Main Window"},
                {"action": "click",                "target": "login_button"},
                {"action": "assert_text_contains", "target": "status_label", "value": "status_label"},
            ],
        )
        return catalog_svc.create_test_case(payload)

    def _fresh_svc(self):
        from services.test_catalog_service import _reset_for_testing, TestCatalogService
        _reset_for_testing()
        return TestCatalogService()

    def test_desktop_run_dispatched(self):
        """run_test_case on a desktop test should call run_desktop_test, not execute_test."""
        svc = self._fresh_svc()
        self._seed_desktop_tc(svc)

        with patch("runners.desktop_runner.run_desktop_test",
                   wraps=lambda steps, **kw: {
                       "ok": True, "status": "passed", "outcome": "pass",
                       "reason": "stub", "evidence_id": "DT-STUB",
                       "steps": [], "logs": [], "screenshot_b64": None,
                       "duration_ms": 1, "meta": {"runner": "desktop"},
                       "evidence": {"run_id": "DT-STUB", "screenshots": [],
                                    "network_events": [], "dom_snapshots": [],
                                    "trace_path": None, "metadata": {}},
                   }) as mock_run:
            run = svc.run_test_case("TC-POS-ORCH-001")

        mock_run.assert_called_once()
        assert run.status == "pass"

    def test_ui_run_not_dispatched_to_desktop(self):
        """UI test cases must NOT use the desktop runner."""
        from models.test_case import TestCaseCreate
        svc = self._fresh_svc()
        svc.create_test_case(TestCaseCreate(
            test_case_id="TC-UI-ORCH-001",
            name="Web Test",
            module="web",
            type="smoke",
            priority="low",
            test_type="ui",
            steps=[{"action": "goto", "url": "https://example.com"}],
        ))

        with patch("runners.desktop_runner.run_desktop_test") as mock_desktop:
            with patch("runner.execute_test", return_value={
                "ok": True, "status": "passed", "outcome": "pass",
                "reason": "OK", "evidence_id": "EV-1", "steps": [],
                "logs": [], "screenshot_b64": None, "duration_ms": 1,
                "meta": {}, "evidence": {},
            }):
                svc.run_test_case("TC-UI-ORCH-001")

        mock_desktop.assert_not_called()

    def test_run_persisted(self):
        """Desktop runs must be persisted via test_run_repo."""
        svc = self._fresh_svc()
        self._seed_desktop_tc(svc, tc_id="TC-POS-PERSIST-001")
        run = svc.run_test_case("TC-POS-PERSIST-001")
        assert run.run_id is not None
        persisted = svc.get_run(run.run_id)
        assert persisted is not None
        assert persisted.test_case_id == "TC-POS-PERSIST-001"

    def test_run_status_reflected(self):
        svc = self._fresh_svc()
        self._seed_desktop_tc(svc, tc_id="TC-POS-STATUS-001")
        run = svc.run_test_case("TC-POS-STATUS-001")
        assert run.status in ("pass", "fail", "error")

    def test_desktop_steps_not_goto_injected(self):
        """_build_desktop_steps should not prepend a goto step."""
        from services.test_catalog_service import _build_desktop_steps
        from models.test_case import TestCase, TestStep
        tc = TestCase(
            test_case_id="TC-NOGOTO",
            name="No goto test",
            module="pos",
            type="smoke",
            priority="low",
            test_type="desktop",
            base_url="https://irrelevant.example.com",  # should not be injected
            steps=[TestStep(action="launch_app", target="app.exe")],
        )
        steps = _build_desktop_steps(tc)
        assert all(s["action"] != "goto" for s in steps)
