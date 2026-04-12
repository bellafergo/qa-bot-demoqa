# services/project_execution_context.py
"""Build step_compiler execution context from a persisted Project row."""
from __future__ import annotations

import os
from typing import Any, Dict, Optional


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
    if isinstance(vars_raw, dict):
        pv: Dict[str, str] = {}
        creds: Dict[str, str] = {}
        for k, v in vars_raw.items():
            if v is None:
                continue
            vs = str(v).strip()
            if not vs:
                continue
            ku = str(k).upper().replace("-", "_")
            pv[ku] = vs
            kl = str(k).lower().replace("-", "_")
            creds[kl] = vs
        if pv:
            ctx["project_variables"] = pv
        if creds:
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

    Resolution order (each field independently):
      1) Project settings.variables → project_variables EMAIL / PASSWORD (same as UI)
      2) Legacy lowercase credentials.email / credentials.password from execution context
      3) Environment: VANYA_TEST_EMAIL, VANYA_TEST_PASSWORD, then TOS_TEST_*
      4) Empty string
    """
    email = ""
    password = ""
    if project is not None:
        ctx = execution_context_from_project(project)
        pv = ctx.get("project_variables")
        if isinstance(pv, dict):
            e0 = pv.get("EMAIL")
            if e0 is not None and str(e0).strip():
                email = str(e0).strip()
            p0 = pv.get("PASSWORD")
            if p0 is not None and str(p0).strip():
                password = str(p0).strip()
        creds = ctx.get("credentials")
        if isinstance(creds, dict):
            if not email:
                e = creds.get("email")
                if e is not None and str(e).strip():
                    email = str(e).strip()
            if not password:
                p = creds.get("password")
                if p is not None and str(p).strip():
                    password = str(p).strip()
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
