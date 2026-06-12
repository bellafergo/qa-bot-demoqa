# core/rbac_policy.py
"""Path → permission mapping for RBAC enforcement middleware."""
from __future__ import annotations

from typing import List, Optional


def _p(path: str) -> str:
    return (path or "").lower().rstrip("/") or "/"


def required_permissions_for_request(*, method: str, path: str) -> Optional[List[str]]:
    """
    Return required permissions (any one grants access) or None for default policy.

    None + GET → VIEW_DASHBOARD
    None + write + VIEWER → denied by caller
    None + write + non-VIEWER → allowed
    """
    m = (method or "GET").upper()
    p = _p(path)

    if p.startswith("/security/sso") and m != "GET":
        return ["MANAGE_SECURITY"]

    if p.startswith("/audit"):
        return ["MANAGE_SECURITY"]

    if "/reports/send" in p:
        return ["SEND_REPORTS"]

    if "/reports/preview" in p:
        return ["VIEW_REPORTS"]

    if p.startswith("/integrations"):
        if m in ("POST", "PUT", "PATCH", "DELETE"):
            return ["MANAGE_INTEGRATIONS"]
        return ["VIEW_DASHBOARD"]

    if p.startswith("/projects") and "/release-readiness" in p:
        return ["VIEW_RELEASE_INTELLIGENCE"]

    if p.startswith("/incidents") or (p.startswith("/projects") and "/incidents" in p):
        return ["VIEW_INCIDENTS"]

    if (
        p.startswith("/dashboard")
        or p.startswith("/analytics")
        or p.startswith("/platform")
        or "/value-dashboard" in p
        or "/executive-impact" in p
        or "/business-risk" in p
    ):
        return ["VIEW_DASHBOARD"]

    if "/approve" in p and m in ("POST", "PUT", "PATCH"):
        return ["APPROVE_ACTIONS"]

    return None
