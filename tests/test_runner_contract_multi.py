"""Multi-runner validation and auto-fix (web vs desktop vs api)."""

from core.step_validator import validate_steps
from services.test_auto_fixer import auto_fix_test


def test_desktop_validate_rejects_goto():
    vr = validate_steps(
        [{"action": "attach_window", "target": "POS"}, {"action": "goto", "url": "https://x.com"}],
        runner_kind="desktop",
    )
    assert not vr.valid
    assert any("goto" in (e.action or "") or "goto" in e.message.lower() for e in vr.errors)


def test_desktop_validate_accepts_canonical_flow():
    vr = validate_steps(
        [
            {"action": "attach_window", "target": "GCC POS"},
            {"action": "click", "target": "OK"},
            {"action": "screenshot"},
        ],
        runner_kind="desktop",
    )
    assert vr.valid


def test_web_validate_rejects_attach_window():
    vr = validate_steps(
        [{"action": "goto", "url": "https://example.com"}, {"action": "attach_window", "target": "X"}],
        runner_kind="web",
    )
    assert not vr.valid
    assert any("attach_window" in (e.action or "") for e in vr.errors)


def test_auto_fix_desktop_no_goto_prepend():
    r = auto_fix_test(
        [{"action": "attach", "target": "GCC POS"}, {"action": "click_control", "target": "Login"}],
        [],
        test_type="desktop",
    )
    assert not any(s.get("action") == "goto" for s in r["steps"])
    assert r["steps"][0]["action"] == "attach_window"
    assert r["steps"][1]["action"] == "click"


def test_auto_fix_web_still_prepends_goto():
    r = auto_fix_test([{"action": "click", "selector": "#x"}], [], test_type="ui")
    assert r["steps"][0]["action"] == "goto"
