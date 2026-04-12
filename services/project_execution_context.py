# services/project_execution_context.py
"""Build step_compiler execution context from a persisted Project row."""
from __future__ import annotations

import json
import logging
import os
from typing import Any, Dict, Optional

from services.project_settings_service import parse_settings_json

logger = logging.getLogger("vanya.project_execution_context")


def _coerce_variable_scalar(v: Any) -> str:
    """
    Normalize a single variable value from project settings to a non-empty string.
    Ignores masked API shapes (no raw secret) and non-scalars that are not unwrappable.
    """
    if v is None:
        return ""
    if isinstance(v, str):
        return v.strip()
    if isinstance(v, (int, float)):
        return str(v).strip()
    if isinstance(v, bool):
        return ""
    if isinstance(v, dict):
        if v.get("sensitive") is True and "present" in v:
            return ""
        inner = v.get("value")
        if isinstance(inner, str) and inner.strip():
            return inner.strip()
        return ""
    return ""


def _variables_map_from_blob(raw: Any) -> Dict[str, str]:
    """
    Build UPPERCASE_UNDERSCORE_KEY -> string for a variables subdocument.
    Accepts dict or JSON string (double-encoded variables).
    """
    if raw is None:
        return {}
    if isinstance(raw, str):
        s = raw.strip()
        if not s:
            return {}
        try:
            parsed: Any = json.loads(s)
        except Exception:
            return {}
        raw = parsed
    if not isinstance(raw, dict):
        return {}
    out: Dict[str, str] = {}
    for k, v in raw.items():
        ku = str(k).upper().replace("-", "_")
        val = _coerce_variable_scalar(v)
        if val:
            out[ku] = val
    return out


def _top_level_credential(settings: Dict[str, Any], key_upper: str) -> str:
    """Read EMAIL / PASSWORD (any key casing) from settings root, skipping known namespaces."""
    skip = frozenset(
        {"variables", "login_profile", "credentials", "_security_note"},
    )
    for k, v in settings.items():
        if str(k).lower() in skip:
            continue
        ku = str(k).upper().replace("-", "_")
        if ku == key_upper:
            val = _coerce_variable_scalar(v)
            if val:
                return val
    return ""


def _credentials_subdoc(settings: Dict[str, Any]) -> tuple[str, str]:
    """Optional settings.credentials.{email,password} (any key casing)."""
    raw = settings.get("credentials")
    if not isinstance(raw, dict):
        return "", ""
    m = _variables_map_from_blob(raw)
    return m.get("EMAIL", ""), m.get("PASSWORD", "")


def _mask_email_for_log(email: str) -> str:
    e = (email or "").strip()
    if not e:
        return "(empty)"
    if "@" not in e:
        return "***"
    local, _, domain = e.partition("@")
    if len(local) <= 1:
        return f"*@{domain}"
    return f"{local[0]}***@{domain}"


def execution_context_from_project(project: Any) -> Dict[str, Any]:
    """
    Produces context for resolve_interpolated_credentials / _compile_login / build_login_steps.

    Keys:
      - project_variables: {"EMAIL": "...", "PASSWORD": "..."} (uppercase keys)
      - credentials: lowercase mirror for legacy prompt extraction
      - login_profile: dict with selectors + optional success_* (only if complete)
    """
    ctx: Dict[str, Any] = {}
    if project is None:
        return ctx
    settings = getattr(project, "settings", None)
    if not isinstance(settings, dict):
        return ctx

    vars_raw = settings.get("variables")
    vm = _variables_map_from_blob(vars_raw)
    if vm:
        pv = dict(vm)
        creds = {str(k).lower().replace("-", "_"): v for k, v in vm.items()}
        ctx["project_variables"] = pv
        ctx["credentials"] = creds

    lp = settings.get("login_profile")
    if isinstance(lp, dict):
        es = (lp.get("email_selector") or "").strip()
        ps = (lp.get("password_selector") or "").strip()
        ss = (lp.get("submit_selector") or "").strip()
        if es and ps and ss:
            ctx["login_profile"] = lp

    return ctx


def api_runner_credential_interpolation(project: Any) -> Dict[str, str]:
    """
    Variables for API runner {{project_email}} / {{project_password}}.

    Per field, resolution order:
      1) settings.variables (dict or JSON string) → EMAIL / PASSWORD (any key casing)
      2) settings root EMAIL / PASSWORD (any casing; skips variables/login_profile/credentials)
      3) settings.credentials (email/password or EMAIL/PASSWORD)
      4) Environment: VANYA_TEST_*, TOS_TEST_*
      5) Empty string
    """
    email = ""
    password = ""
    pid = ""
    settings: Dict[str, Any] = {}
    if project is not None:
        pid = str(getattr(project, "id", "") or "").strip()
        raw_s = getattr(project, "settings", None)
        if isinstance(raw_s, dict):
            settings = raw_s
        elif isinstance(raw_s, str) and raw_s.strip():
            settings = parse_settings_json(raw_s)

        top_keys = sorted(settings.keys()) if settings else []
        logger.debug(
            "api cred interpolation: project_id=%s settings_top_level_keys=%s",
            pid or "(none)",
            top_keys,
        )

        vm = _variables_map_from_blob(settings.get("variables"))
        if vm.get("EMAIL"):
            email = vm["EMAIL"]
        if vm.get("PASSWORD"):
            password = vm["PASSWORD"]

        if not email:
            email = _top_level_credential(settings, "EMAIL")
        if not password:
            password = _top_level_credential(settings, "PASSWORD")

        ce, cp = _credentials_subdoc(settings)
        if not email and ce:
            email = ce
        if not password and cp:
            password = cp

        logger.debug(
            "api cred interpolation: project_id=%s found_email=%s found_password=%s email_masked=%s",
            pid or "(none)",
            bool(email),
            bool(password),
            _mask_email_for_log(email),
        )

    if not email:
        email = (
            (os.getenv("VANYA_TEST_EMAIL") or os.getenv("TOS_TEST_EMAIL") or "")
            .strip()
        )
    if not password:
        password = (
            (os.getenv("VANYA_TEST_PASSWORD") or os.getenv("TOS_TEST_PASSWORD") or "")
            .strip()
        )
    return {"project_email": email, "project_password": password}
