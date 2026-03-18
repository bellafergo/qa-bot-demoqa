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
  - desktop tests (POS / Win32 via desktop_runner)
"""
from typing import Any, Dict, List


# ── POS test case builder (defined first — used in SEED_TEST_CASES below) ─────

def _build_pos_test_cases_for_seed() -> List[Dict[str, Any]]:
    """
    Build the 3 POS desktop test cases using values from config/pos_desktop.py.

    Called at module load time (for SEED_TEST_CASES) and at runtime (for
    ensure_pos_seed), so config values are always fresh when needed.
    """
    try:
        import config.pos_desktop as cfg
    except ImportError:
        # Should not happen in a normal install; defensive fallback.
        class cfg:  # type: ignore[no-redef]
            APP_PATH = r"C:\POS\pos.exe"
            WIN_LOGIN = "POS Login"
            WIN_MAIN  = "POS Principal"
            USER_VALID   = "tester";    PASS_VALID   = "1234"
            USER_INVALID = "bad_user";  PASS_INVALID = "bad_pass"
            ERROR_TEXT_EXPECTED       = "invalid"
            TARGET_USUARIO            = "usuario"
            TARGET_PASSWORD           = "password"
            TARGET_BTN_ENTRAR         = "Entrar"
            TARGET_PANTALLA_PRINCIPAL = "pantalla_principal"
            TARGET_ERROR_LABEL        = "error_label"
            TARGET_BTN_VENTAS         = "Ventas"
            TARGET_PANEL_VENTAS       = "panel_ventas"
            TIMEOUT_LOGIN_MS  = 5000
            TIMEOUT_ERROR_MS  = 3000
            TIMEOUT_VENTAS_MS = 3000

    return [
        # ── TC-POS-001 ── Smoke: Login exitoso ───────────────────────────────
        {
            "test_case_id": "TC-POS-001",
            "name": "POS Login exitoso con credenciales válidas",
            "module": "pos",
            "type": "smoke",
            "priority": "critical",
            "test_type": "desktop",
            "tags": ["pos", "login", "desktop", "smoke"],
            "steps": [
                {"action": "launch_app",    "value":  cfg.APP_PATH},
                {"action": "attach_window", "target": cfg.WIN_LOGIN},
                {"action": "input",         "target": cfg.TARGET_USUARIO,            "value": cfg.USER_VALID},
                {"action": "input",         "target": cfg.TARGET_PASSWORD,           "value": cfg.PASS_VALID},
                {"action": "click",         "target": cfg.TARGET_BTN_ENTRAR},
                {"action": "wait_for",      "target": cfg.TARGET_PANTALLA_PRINCIPAL, "ms": cfg.TIMEOUT_LOGIN_MS},
                {"action": "assert_exists", "target": cfg.TARGET_PANTALLA_PRINCIPAL},
                {"action": "screenshot"},
            ],
            "assertions": [],
        },

        # ── TC-POS-002 ── Negative: Login con credenciales inválidas ─────────
        {
            "test_case_id": "TC-POS-002",
            "name": "POS Login con credenciales inválidas muestra error",
            "module": "pos",
            "type": "negative",
            "priority": "high",
            "test_type": "desktop",
            "tags": ["pos", "login", "desktop", "negative"],
            "steps": [
                {"action": "launch_app",          "value":  cfg.APP_PATH},
                {"action": "attach_window",       "target": cfg.WIN_LOGIN},
                {"action": "input",               "target": cfg.TARGET_USUARIO,    "value": cfg.USER_INVALID},
                {"action": "input",               "target": cfg.TARGET_PASSWORD,   "value": cfg.PASS_INVALID},
                {"action": "click",               "target": cfg.TARGET_BTN_ENTRAR},
                {"action": "wait_for",            "target": cfg.TARGET_ERROR_LABEL,"ms": cfg.TIMEOUT_ERROR_MS},
                {"action": "assert_text_contains","target": cfg.TARGET_ERROR_LABEL,"value": cfg.ERROR_TEXT_EXPECTED},
                {"action": "screenshot"},
            ],
            "assertions": [],
        },

        # ── TC-POS-003 ── Functional: Abrir módulo de ventas ─────────────────
        {
            "test_case_id": "TC-POS-003",
            "name": "POS Abrir módulo de ventas desde pantalla principal",
            "module": "pos",
            "type": "functional",
            "priority": "high",
            "test_type": "desktop",
            "tags": ["pos", "ventas", "desktop", "functional"],
            "steps": [
                {"action": "attach_window", "target": cfg.WIN_MAIN},
                {"action": "click",         "target": cfg.TARGET_BTN_VENTAS},
                {"action": "wait_for",      "target": cfg.TARGET_PANEL_VENTAS,  "ms": cfg.TIMEOUT_VENTAS_MS},
                {"action": "assert_exists", "target": cfg.TARGET_PANEL_VENTAS},
                {"action": "screenshot"},
            ],
            "assertions": [],
        },
    ]


# ─────────────────────────────────────────────────────────────────────────────

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

    # POS Desktop test cases are added via _build_pos_test_cases() below.
    # Values come from config/pos_desktop.py (overridable via env vars or direct edit).
    *_build_pos_test_cases_for_seed(),

]


# ── Ensure POS seed (idempotent upsert for existing catalogs) ─────────────────

def ensure_pos_seed(*, force_update: bool = False) -> Dict[str, int]:
    """
    Idempotently insert (or update) POS test cases in an existing catalog.

    Parameters
    ----------
    force_update : bool
        False (default) — skip test cases that already exist.
        True            — delete and recreate existing ones with current config values.
                          Use this after editing config/pos_desktop.py on the real machine.

    Returns {"created": N, "updated": N, "skipped": N}.
    """
    import logging
    from models.test_case import TestCaseCreate
    from services.test_catalog_service import catalog_service

    logger = logging.getLogger(__name__)
    # Always build fresh from current config, not from the cached SEED_TEST_CASES list.
    pos_cases = _build_pos_test_cases_for_seed()

    created = 0
    updated = 0
    skipped = 0

    for payload_dict in pos_cases:
        tc_id = payload_dict["test_case_id"]
        try:
            existing = catalog_service.get_test_case(tc_id)
            if existing is not None:
                if not force_update:
                    logger.debug("ensure_pos_seed: %s exists — skipped (pass force_update=True to update)", tc_id)
                    skipped += 1
                    continue
                # Delete existing and recreate with current config values.
                catalog_service.delete_test_case(tc_id)
                logger.info("ensure_pos_seed: deleted stale %s for update", tc_id)

            catalog_service.create_test_case(TestCaseCreate(**payload_dict))
            if existing is not None:
                logger.info("ensure_pos_seed: updated %s", tc_id)
                updated += 1
            else:
                logger.info("ensure_pos_seed: created %s", tc_id)
                created += 1

        except Exception as exc:  # noqa: BLE001
            logger.warning("ensure_pos_seed: failed to process %s — %s", tc_id, exc)
            skipped += 1

    return {"created": created, "updated": updated, "skipped": skipped}
