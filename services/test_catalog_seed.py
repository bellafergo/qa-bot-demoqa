# services/test_catalog_seed.py
"""
Demo seed catalog for Vanya.

These test cases are loaded on startup when the catalog is empty.
They cover The Internet Herokuapp and DemoQA — two classic QA demo sites.

Demonstrates:
  - smoke tests (fast, basic sanity)
  - functional tests (feature-specific flows)
  - negative tests (expected failure scenarios)
  - e2e tests (multi-step flows)
"""
from typing import Any, Dict, List

SEED_TEST_CASES: List[Dict[str, Any]] = [

    # ── TC-DEMO-001 ── Smoke: Login valid user ────────────────────────────────
    {
        "test_case_id": "TC-DEMO-001",
        "name": "Login valid user",
        "module": "herokuapp-login",
        "type": "smoke",
        "priority": "critical",
        "tags": ["login", "auth", "smoke"],
        "base_url": "https://the-internet.herokuapp.com/login",
        "steps": [
            {"action": "goto",  "value": "https://the-internet.herokuapp.com/login"},
            {"action": "input", "target": "username field", "value": "tomsmith"},
            {"action": "input", "target": "password field", "value": "SuperSecretPassword!"},
            {"action": "click", "target": "login button"},
        ],
        "assertions": [
            {"type": "text_visible",  "value": "Secure Area"},
            {"type": "url_contains",  "value": "/secure"},
        ],
    },

    # ── TC-DEMO-002 ── Negative: Login invalid password ───────────────────────
    {
        "test_case_id": "TC-DEMO-002",
        "name": "Login with invalid password shows error",
        "module": "herokuapp-login",
        "type": "negative",
        "priority": "high",
        "tags": ["login", "auth", "negative", "error-message"],
        "base_url": "https://the-internet.herokuapp.com/login",
        "steps": [
            {"action": "goto",  "value": "https://the-internet.herokuapp.com/login"},
            {"action": "input", "target": "username field", "value": "tomsmith"},
            {"action": "input", "target": "password field", "value": "wrongpassword"},
            {"action": "click", "target": "login button"},
        ],
        "assertions": [
            {"type": "text_visible", "value": "Your password is invalid!"},
        ],
    },

    # ── TC-DEMO-003 ── Functional: Login page UI elements visible ─────────────
    {
        "test_case_id": "TC-DEMO-003",
        "name": "Login page renders required elements",
        "module": "herokuapp-login",
        "type": "functional",
        "priority": "medium",
        "tags": ["login", "ui", "smoke"],
        "base_url": "https://the-internet.herokuapp.com/login",
        "steps": [
            {"action": "goto", "value": "https://the-internet.herokuapp.com/login"},
        ],
        "assertions": [
            {"type": "text_visible",    "value": "Login Page"},
            {"type": "element_visible", "target": "#username"},
            {"type": "element_visible", "target": "#password"},
        ],
    },

    # ── TC-DEMO-004 ── Functional: Logout after login ─────────────────────────
    {
        "test_case_id": "TC-DEMO-004",
        "name": "Login then logout returns to login page",
        "module": "herokuapp-login",
        "type": "e2e",
        "priority": "high",
        "tags": ["login", "logout", "auth", "e2e"],
        "base_url": "https://the-internet.herokuapp.com/login",
        "steps": [
            {"action": "goto",  "value": "https://the-internet.herokuapp.com/login"},
            {"action": "input", "target": "username field", "value": "tomsmith"},
            {"action": "input", "target": "password field", "value": "SuperSecretPassword!"},
            {"action": "click", "target": "login button"},
            {"action": "wait_ms", "ms": 500},
            {"action": "click", "target": "logout button"},
        ],
        "assertions": [
            {"type": "text_visible", "value": "Login Page"},
        ],
    },

    # ── TC-DEMO-005 ── Smoke: DemoQA home page loads ──────────────────────────
    {
        "test_case_id": "TC-DEMO-005",
        "name": "DemoQA home page loads",
        "module": "demoqa",
        "type": "smoke",
        "priority": "low",
        "tags": ["demoqa", "smoke", "health"],
        "base_url": "https://demoqa.com",
        "steps": [
            {"action": "goto", "value": "https://demoqa.com"},
            {"action": "wait_ms", "ms": 1000},
        ],
        "assertions": [
            {"type": "text_visible", "value": "ToolsQA"},
        ],
    },

    # ── TC-DEMO-006 ── Functional: DemoQA text box form ──────────────────────
    {
        "test_case_id": "TC-DEMO-006",
        "name": "DemoQA text box form submits correctly",
        "module": "demoqa",
        "type": "functional",
        "priority": "medium",
        "tags": ["demoqa", "forms", "textbox"],
        "base_url": "https://demoqa.com/text-box",
        "steps": [
            {"action": "goto",  "value": "https://demoqa.com/text-box"},
            {"action": "input", "target": "#userName",      "value": "Jane Doe"},
            {"action": "input", "target": "#userEmail",     "value": "jane@example.com"},
            {"action": "input", "target": "#currentAddress","value": "123 Main St"},
            {"action": "click", "target": "#submit"},
        ],
        "assertions": [
            {"type": "text_visible", "value": "Jane Doe"},
            {"type": "text_visible", "value": "jane@example.com"},
        ],
    },

    # ── TC-DEMO-007 ── Smoke: Herokuapp checkbox page ─────────────────────────
    {
        "test_case_id": "TC-DEMO-007",
        "name": "Checkbox can be checked",
        "module": "herokuapp-elements",
        "type": "functional",
        "priority": "low",
        "tags": ["checkbox", "interaction"],
        "base_url": "https://the-internet.herokuapp.com/checkboxes",
        "steps": [
            {"action": "goto",  "value": "https://the-internet.herokuapp.com/checkboxes"},
            {"action": "click", "target": "input[type='checkbox']"},
        ],
        "assertions": [
            {"type": "text_visible", "value": "Checkboxes"},
        ],
    },

    # ── TC-DEMO-008 ── Regression: Herokuapp dynamic content loads ─────────────
    {
        "test_case_id": "TC-DEMO-008",
        "name": "Dynamic content page loads without error",
        "module": "herokuapp-elements",
        "type": "regression",
        "priority": "low",
        "tags": ["dynamic", "regression"],
        "base_url": "https://the-internet.herokuapp.com/dynamic_content",
        "steps": [
            {"action": "goto",    "value": "https://the-internet.herokuapp.com/dynamic_content"},
            {"action": "wait_ms", "ms": 1500},
        ],
        "assertions": [
            {"type": "text_visible", "value": "Dynamic Content"},
        ],
    },

]
