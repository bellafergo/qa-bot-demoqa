# tests/test_failure_classify.py
"""
Tests for services/failure_intelligence.py — classify_failure(run) -> dict

NOTE: tests/test_failure_intelligence.py already exists for Block-14's
FailureIntelligenceService (clustering / flaky detection).  This file covers
the new run-level classify_failure() function in services/failure_intelligence.py.

Run: .venv/bin/python -m pytest tests/test_failure_classify.py -v
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.failure_intelligence import classify_failure, _format_target


# ── Tiny builders ─────────────────────────────────────────────────────────────

def _run(reason="", steps=None, failure_context=None, error_message=None, status="failed"):
    r: dict = {"status": status, "reason": reason}
    if steps is not None:
        r["steps"] = steps
    if failure_context is not None:
        r["failure_context"] = failure_context
    if error_message is not None:
        r["error_message"] = error_message
    return r


def _failed_step(action, selector=None, target=None):
    s: dict = {"action": action, "status": "failed"}
    if selector:
        s["selector"] = selector
    if target:
        s["target"] = target
    return s


# ══════════════════════════════════════════════════════════════
# 1. Output contract
# ══════════════════════════════════════════════════════════════

class TestContract:

    def test_has_failure_type(self):
        assert "failure_type" in classify_failure(_run("some error"))

    def test_has_layer(self):
        assert "layer" in classify_failure(_run("some error"))

    def test_has_target_key(self):
        r = classify_failure(_run("some error"))
        assert "target" in r   # value may be None

    def test_has_confidence(self):
        assert "confidence" in classify_failure(_run("some error"))

    def test_confidence_valid_values(self):
        valid = {"high", "medium", "low"}
        for reason in [
            "TimeoutError: timeout exceeded",
            "waiting for locator not found",
            "AssertionError: texto no encontrado",
            "net::ERR_CONNECTION_REFUSED",
            "something completely unexpected xyz123",
        ]:
            r = classify_failure(_run(reason))
            assert r["confidence"] in valid, f"bad confidence for: {reason}"

    def test_failure_type_valid_values(self):
        valid = {"selector_not_found", "assertion_failed", "navigation_failed", "timeout", "unknown"}
        for reason in [
            "TimeoutError: timeout exceeded",
            "waiting for locator not found",
            "AssertionError: texto no encontrado",
            "net::ERR_CONNECTION_REFUSED",
            "something unexpected xyz123",
        ]:
            assert classify_failure(_run(reason))["failure_type"] in valid

    def test_non_dict_input_returns_unknown(self):
        for bad in (None, "bad", 42, []):
            assert classify_failure(bad)["failure_type"] == "unknown"
            assert classify_failure(bad)["confidence"] == "low"


# ══════════════════════════════════════════════════════════════
# 2. Timeout detection
# ══════════════════════════════════════════════════════════════

class TestTimeout:

    def test_timeout_from_reason_string(self):
        r = classify_failure(_run("Timeout en step 2: click — TimeoutError: Timeout 10000ms exceeded."))
        assert r["failure_type"] == "timeout"
        assert r["confidence"] == "high"

    def test_timeout_layer_runner_for_click(self):
        r = classify_failure(_run(
            "Timeout en step 3: click — TimeoutError",
            steps=[_failed_step("click", "#login-button")],
        ))
        assert r["failure_type"] == "timeout"
        assert r["layer"] == "runner"

    def test_timeout_layer_navigation_for_goto(self):
        r = classify_failure(_run(
            "Timeout en step 1: goto — TimeoutError: Navigation timeout exceeded.",
            steps=[_failed_step("goto")],
        ))
        assert r["failure_type"] == "timeout"
        assert r["layer"] == "navigation"

    def test_timeout_from_error_message_field(self):
        r = classify_failure(_run(error_message="ConnectionError: timed out connecting to browser"))
        assert r["failure_type"] == "timeout"

    def test_timed_out_phrase(self):
        r = classify_failure(_run("operation timed out after 5 seconds"))
        assert r["failure_type"] == "timeout"

    def test_timeout_case_insensitive(self):
        r = classify_failure(_run("TIMEOUTERROR: SOMETHING FAILED"))
        assert r["failure_type"] == "timeout"


# ══════════════════════════════════════════════════════════════
# 3. Navigation failure detection
# ══════════════════════════════════════════════════════════════

class TestNavigationFailed:

    def test_navigation_from_net_err(self):
        r = classify_failure(_run("Error: net::ERR_CONNECTION_REFUSED at https://example.com"))
        assert r["failure_type"] == "navigation_failed"
        assert r["layer"] == "navigation"
        assert r["confidence"] == "high"

    def test_navigation_from_goto_action_in_step(self):
        r = classify_failure(_run(
            "Fallo en step 1: goto — ValueError: goto requiere url/base_url",
            steps=[_failed_step("goto")],
        ))
        assert r["failure_type"] == "navigation_failed"

    def test_navigation_from_failed_to_navigate(self):
        r = classify_failure(_run("failed to navigate to the page"))
        assert r["layer"] == "navigation"

    def test_navigation_from_failure_context_goto(self):
        r = classify_failure(_run(
            "goto failed",
            failure_context={"action": "goto", "original_selector": None, "error": "goto failed"},
        ))
        assert r["failure_type"] == "navigation_failed"

    def test_navigation_high_confidence(self):
        r = classify_failure(_run("net::ERR_NAME_NOT_RESOLVED"))
        assert r["confidence"] == "high"


# ══════════════════════════════════════════════════════════════
# 4. Selector not found detection
# ══════════════════════════════════════════════════════════════

class TestSelectorNotFound:

    def test_waiting_for_locator(self):
        r = classify_failure(_run("Fallo en step 2: assert_visible — Error: waiting for locator('#xyz')"))
        assert r["failure_type"] == "selector_not_found"
        assert r["layer"] == "resolver"
        assert r["confidence"] == "high"

    def test_not_found_keyword(self):
        r = classify_failure(_run("element not found in the DOM"))
        assert r["failure_type"] == "selector_not_found"

    def test_strict_mode_violation(self):
        r = classify_failure(_run("strict mode violation: locator resolved to 3 elements"))
        assert r["failure_type"] == "selector_not_found"

    def test_cannot_find(self):
        r = classify_failure(_run("cannot find element matching selector"))
        assert r["failure_type"] == "selector_not_found"
        assert r["layer"] == "resolver"

    def test_target_from_failed_step(self):
        r = classify_failure(_run(
            "waiting for locator not found",
            steps=[_failed_step("click", "#unknown-btn")],
        ))
        assert r["target"] is not None
        assert "#unknown-btn" in r["target"]


# ══════════════════════════════════════════════════════════════
# 5. Assertion failure detection
# ══════════════════════════════════════════════════════════════

class TestAssertionFailed:

    def test_assert_text_contains_action(self):
        r = classify_failure(_run(
            "Fallo en step 3: assert_text_contains — AssertionError: Texto no encontrado.",
            steps=[_failed_step("assert_text_contains", "body")],
        ))
        assert r["failure_type"] == "assertion_failed"
        assert r["layer"] == "assertion"
        assert r["confidence"] == "high"

    def test_assert_visible_action(self):
        r = classify_failure(_run(
            "Fallo en step 2: assert_visible — AssertionError",
            steps=[_failed_step("assert_visible", "#user-name")],
        ))
        assert r["failure_type"] == "assertion_failed"
        assert r["confidence"] == "high"

    def test_assert_not_visible_action(self):
        r = classify_failure(_run(
            "AssertionError",
            steps=[_failed_step("assert_not_visible", "[data-test='error']")],
        ))
        assert r["failure_type"] == "assertion_failed"
        assert r["confidence"] == "high"

    def test_assert_url_contains_action(self):
        r = classify_failure(_run(
            "Fallo: assert_url_contains failed",
            steps=[_failed_step("assert_url_contains")],
        ))
        assert r["failure_type"] == "assertion_failed"

    def test_assertion_medium_confidence_from_message_only(self):
        r = classify_failure(_run("assertion failed: unexpected state"))
        assert r["failure_type"] == "assertion_failed"
        assert r["confidence"] == "medium"

    def test_texto_no_encontrado(self):
        r = classify_failure(_run("Texto no encontrado. Expected contiene: 'Welcome'"))
        assert r["failure_type"] == "assertion_failed"

    def test_url_does_not_contain(self):
        r = classify_failure(_run("url does not contain 'inventory.html'"))
        assert r["failure_type"] == "assertion_failed"


# ══════════════════════════════════════════════════════════════
# 6. Unknown / fallback
# ══════════════════════════════════════════════════════════════

class TestUnknown:

    def test_unknown_for_unrecognized_message(self):
        r = classify_failure(_run("something completely unexpected xyz_abc_123"))
        assert r["failure_type"] == "unknown"

    def test_unknown_layer(self):
        assert classify_failure(_run("some random crash"))["layer"] == "unknown"

    def test_unknown_low_confidence(self):
        assert classify_failure(_run("unclassifiable error"))["confidence"] == "low"

    def test_empty_reason_is_unknown(self):
        assert classify_failure(_run(""))["failure_type"] == "unknown"


# ══════════════════════════════════════════════════════════════
# 7. Target formatting
# ══════════════════════════════════════════════════════════════

class TestTargetFormatting:

    def test_semantic_target_kind_name(self):
        """Semantic target {kind, name} → "kind(name)"."""
        step = {
            "action": "click",
            "selector": "#login-button",
            "status": "failed",
            "target": {"kind": "button", "name": "login", "primary": "#login-button"},
        }
        r = classify_failure(_run(
            "waiting for locator not found",
            steps=[step],
        ))
        assert r["target"] == "button(login)"

    def test_fallback_action_selector(self):
        """No semantic target → "action(selector)"."""
        step = {"action": "click", "selector": "#login-button", "status": "failed"}
        r = classify_failure(_run(
            "waiting for locator not found",
            steps=[step],
        ))
        assert r["target"] == "click(#login-button)"

    def test_target_none_when_no_steps_and_no_fc(self):
        r = classify_failure(_run("something completely unexpected xyz_abc"))
        assert r["target"] is None

    def test_target_from_failure_context_fallback(self):
        """failure_context used when no steps exist."""
        r = classify_failure(_run(
            "net::ERR_CONNECTION_REFUSED",
            failure_context={"action": "goto", "original_selector": "https://example.com"},
        ))
        assert r["target"] is not None

    def test_format_target_semantic(self):
        t = _format_target({"action": "fill", "selector": "#pw", "target": {"kind": "input", "name": "password"}})
        assert t == "input(password)"

    def test_format_target_no_semantic(self):
        t = _format_target({"action": "fill", "selector": "#pw"})
        assert t == "fill(#pw)"

    def test_format_target_none_input(self):
        assert _format_target(None) is None

    def test_format_target_empty_dict(self):
        assert _format_target({}) is None


# ══════════════════════════════════════════════════════════════
# 8. Jobs integration
# ══════════════════════════════════════════════════════════════

class TestJobsIntegration:

    def test_failure_intelligence_importable(self):
        from services.failure_intelligence import classify_failure  # noqa: F401

    def test_jobs_imports_without_error(self):
        import workers.jobs  # noqa: F401

    def test_manual_run_gets_failure_analysis(self):
        run = {
            "status": "failed",
            "reason": "Fallo en step 2: assert_visible — AssertionError",
            "steps": [_failed_step("assert_visible", "#user-name")],
        }
        run["failure_analysis"] = classify_failure(run)
        fa = run["failure_analysis"]
        assert fa["failure_type"] == "assertion_failed"
        assert fa["layer"] == "assertion"
        assert fa["confidence"] == "high"

    def test_exception_path_run_classified(self):
        """Run built from jobs.py exception handler (has error_message, no steps)."""
        run = {
            "status": "failed",
            "error_message": "TimeoutError: browser connection timed out\nTraceback...",
        }
        run["failure_analysis"] = classify_failure(run)
        assert run["failure_analysis"]["failure_type"] == "timeout"
