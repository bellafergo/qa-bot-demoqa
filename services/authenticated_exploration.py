# services/authenticated_exploration.py
"""
Optional project-scoped login before multi-page URL exploration (Generate → From URL).

When project_id is provided and the project has a complete login_profile + variables,
runs compile_to_runner_steps(login) + explorer_auth_bootstrap on a single browser,
then BFS exploration reuses that session.

Otherwise delegates to multi_page_explorer.explore_app (unchanged).
"""
from __future__ import annotations

import logging
from typing import Any, Dict, Optional

from core.step_compiler import CompileError, compile_to_runner_steps, resolve_fill_values_in_steps
from core.target_url_validation import validate_steps_navigation_urls
from services.db.project_repository import project_repo
from services.explorer_auth_bootstrap import run_explorer_login_steps, sanitize_error_for_client
from services.multi_page_explorer import explore_app, explore_app_in_session
from services.project_execution_context import execution_context_from_project

logger = logging.getLogger("vanya.auth_explore")


def _project_wants_auth(project: Any) -> bool:
    if project is None:
        return False
    ctx = execution_context_from_project(project)
    return bool(ctx.get("login_profile"))


def explore_app_with_optional_project_auth(
    start_url: str,
    max_pages: int,
    *,
    project_id: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Same top-level shape as explore_app: start_url, visited_count, pages, errors.

    Additional keys when login was attempted:
      - used_project_login: bool
      - auth_failed: bool (only if login attempted and failed)
      - auth_error: str (safe for clients; no secrets)
      - auth_error_code: str
    """
    pid = (project_id or "").strip()
    project = project_repo.get_project(pid) if pid else None

    if not _project_wants_auth(project):
        out = explore_app(start_url, max_pages=max_pages)
        out["used_project_login"] = False
        return out

    ctx = execution_context_from_project(project)
    base_for_compiler = (getattr(project, "base_url", None) or "").strip() or None

    try:
        login_steps = compile_to_runner_steps(
            [{"action": "login"}],
            base_url=base_for_compiler,
            context=ctx,
        )
        login_steps = resolve_fill_values_in_steps(login_steps, ctx)
        validate_steps_navigation_urls(login_steps, base_for_compiler)
    except CompileError as e:
        et = getattr(e, "error_type", None) or ""
        details = getattr(e, "details", None) or {}
        if et == "unresolved_variable":
            var = details.get("variable", "?")
            return {
                "start_url": start_url,
                "visited_count": 0,
                "pages": [],
                "errors": [],
                "used_project_login": True,
                "auth_failed": True,
                "auth_error": (
                    "Could not authenticate before exploration: a required project variable "
                    f"is not set ({var}). Configure variables on the project."
                ),
                "auth_error_code": "missing_variable",
            }
        msg = sanitize_error_for_client(str(e), ctx)
        return {
            "start_url": start_url,
            "visited_count": 0,
            "pages": [],
            "errors": [],
            "used_project_login": True,
            "auth_failed": True,
            "auth_error": (
                "Could not prepare login for exploration: "
                + (msg or "compile error")
            ),
            "auth_error_code": "compile_error",
        }
    except Exception as e:
        logger.exception("auth explore: compile failed")
        return {
            "start_url": start_url,
            "visited_count": 0,
            "pages": [],
            "errors": [],
            "used_project_login": True,
            "auth_failed": True,
            "auth_error": sanitize_error_for_client(str(e), ctx),
            "auth_error_code": "compile_error",
        }

    from playwright.sync_api import sync_playwright

    max_pages = max(1, int(max_pages))

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        try:
            context = browser.new_context(viewport={"width": 1366, "height": 768})
            page = context.new_page()
            ok, err_msg, err_code = run_explorer_login_steps(
                page,
                login_steps,
                base_url=base_for_compiler,
                context=ctx,
            )
            if not ok:
                logger.info("explorer auth failed code=%s", err_code)
                return {
                    "start_url": start_url,
                    "visited_count": 0,
                    "pages": [],
                    "errors": [],
                    "used_project_login": True,
                    "auth_failed": True,
                    "auth_error": (
                        "Could not authenticate before exploring the application. "
                        + (err_msg or "Login failed.")
                    ),
                    "auth_error_code": err_code or "auth_failed",
                }

            result = explore_app_in_session(page, start_url, max_pages=max_pages)
            result["used_project_login"] = True
            return result
        finally:
            browser.close()
