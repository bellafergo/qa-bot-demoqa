# services/project_execution_context.py
"""Build step_compiler execution context from a persisted Project row."""
from __future__ import annotations

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
