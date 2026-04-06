# tests/test_generic_login_compile.py
from __future__ import annotations

from core.login_intent_resolver import build_login_steps, has_explicit_login_flow
from core.step_compiler import compile_to_runner_steps, parse_steps_from_prompt
from core.step_validator import validate_steps


BASE_TALENT = "https://zuperio-talent-os.vercel.app"

EXPLICIT_PROMPT = (
    "Ve a /login, escribe user@test.com en el campo de email, "
    "escribe pass123 en el campo de password y haz clic en Sign in"
)


def test_explicit_login_flow_skips_build_login_macro():
    assert has_explicit_login_flow(EXPLICIT_PROMPT) is True
    assert build_login_steps(base_url=BASE_TALENT, prompt=EXPLICIT_PROMPT) is None


def test_talent_explicit_nl_compiles_to_goto_fill_fill_click():
    steps = parse_steps_from_prompt(EXPLICIT_PROMPT, BASE_TALENT)
    assert steps is not None
    assert steps[0]["action"] == "goto"
    assert steps[0]["url"] == f"{BASE_TALENT.rstrip('/')}/login"
    core = [s for s in steps if s["action"] in ("goto", "fill", "click")]
    assert [s["action"] for s in core][:4] == ["goto", "fill", "fill", "click"]
    assert not any(
        s.get("selector") in ("#user-name", "#password", "#login-button") for s in steps
    )
    vr = validate_steps(steps)
    assert vr.valid, [e.message for e in vr.errors]


def test_compile_login_dsl_any_domain_no_saucedemo_error():
    out = compile_to_runner_steps(
        [{"action": "login", "email": "u@x.com", "password": "secret"}],
        base_url=BASE_TALENT,
    )
    assert len(out) == 3
    assert [s["action"] for s in out] == ["fill", "fill", "click"]
    assert out[0]["selector"] == 'input[type="email"]'
    assert out[1]["selector"] == 'input[type="password"]'
    assert out[2]["selector"] == 'button[type="submit"]'
    assert isinstance(out[0].get("target"), dict)
    vr = validate_steps(out)
    assert vr.valid


def test_ambiguous_login_builds_generic_steps():
    steps = build_login_steps(
        base_url=BASE_TALENT,
        prompt="inicia sesión username: demo@x.com password: Secr3t!",
    )
    assert steps is not None
    assert any(s.get("action") == "goto" for s in steps)
    fills = [s for s in steps if s.get("action") == "fill"]
    assert len(fills) >= 2
    assert fills[0]["selector"] == 'input[type="email"]'
    vr = validate_steps(steps)
    assert vr.valid


def test_compile_drops_redundant_login_dsl_when_manual_fills_present():
    plan = [
        {"action": "goto", "url": "{base_url}/login"},
        {"action": "login", "email": "a@b.com", "password": "x"},
        {
            "action": "fill",
            "selector": 'input[type="email"]',
            "value": "a@b.com",
        },
        {
            "action": "fill",
            "selector": 'input[type="password"]',
            "value": "x",
        },
        {"action": "click", "selector": 'button[type="submit"]'},
    ]
    out = compile_to_runner_steps(plan, base_url=BASE_TALENT)
    assert not any(str(s.get("action")) == "login" for s in out)
    assert sum(1 for s in out if s.get("action") == "fill") == 2
    assert sum(1 for s in out if s.get("action") == "click") == 1
