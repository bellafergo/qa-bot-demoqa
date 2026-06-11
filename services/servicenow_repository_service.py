# services/servicenow_repository_service.py
"""
Low-level read-only ServiceNow Table API client.

GET operations only. Never logs passwords. No write operations.
"""
from __future__ import annotations

import base64
import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

import httpx

logger = logging.getLogger("vanya.servicenow_repository")


@dataclass(frozen=True)
class ServiceNowHttpConfig:
    instance_url: str
    username: str
    password: str


class ServiceNowAPIError(Exception):
    """Safe ServiceNow API failure — no credentials in message."""

    def __init__(self, message: str, *, status_code: int = 502, code: str = "servicenow_error"):
        super().__init__(message)
        self.status_code = status_code
        self.code = code


def _auth_header(http: ServiceNowHttpConfig) -> str:
    raw = f"{http.username}:{http.password}".encode("utf-8")
    return "Basic " + base64.b64encode(raw).decode("ascii")


def _headers(http: ServiceNowHttpConfig) -> Dict[str, str]:
    return {
        "Authorization": _auth_header(http),
        "Accept": "application/json",
        "Content-Type": "application/json",
    }


def _api_base(http: ServiceNowHttpConfig) -> str:
    return http.instance_url.rstrip("/")


def _raise_for_status(res: httpx.Response, *, context: str) -> None:
    sc = res.status_code
    if sc == 401:
        raise ServiceNowAPIError(
            "ServiceNow credentials are invalid or expired.",
            status_code=401,
            code="invalid_credentials",
        )
    if sc == 403:
        raise ServiceNowAPIError(
            "ServiceNow API access forbidden. Check user roles.",
            status_code=403,
            code="forbidden",
        )
    if sc == 404:
        raise ServiceNowAPIError(
            f"ServiceNow resource not found ({context}).",
            status_code=404,
            code="not_found",
        )
    if sc >= 500:
        raise ServiceNowAPIError(
            "ServiceNow is temporarily unavailable. Retry later.",
            status_code=502,
            code="servicenow_unavailable",
        )
    if not res.is_success:
        raise ServiceNowAPIError(
            f"ServiceNow API error ({sc}) for {context}.",
            status_code=502,
            code="servicenow_error",
        )


def _get(
    http: ServiceNowHttpConfig,
    path: str,
    *,
    params: Optional[Dict[str, Any]] = None,
) -> httpx.Response:
    url = f"{_api_base(http)}{path}"
    with httpx.Client(timeout=30.0) as client:
        return client.get(url, headers=_headers(http), params=params or {})


def validate_connection(http: ServiceNowHttpConfig) -> Dict[str, Any]:
    """Read-only connectivity check against sys_user."""
    res = _get(
        http,
        "/api/now/table/sys_user",
        params={"sysparm_limit": 1, "sysparm_fields": "user_name"},
    )
    _raise_for_status(res, context="sys_user")
    if not res.content:
        return {}
    return res.json()


def count_table(http: ServiceNowHttpConfig, table: str) -> int:
    """Return total row count using ServiceNow X-Total-Count header."""
    res = _get(
        http,
        f"/api/now/table/{table}",
        params={"sysparm_limit": 1},
    )
    _raise_for_status(res, context=table)
    header = res.headers.get("X-Total-Count") or res.headers.get("x-total-count")
    if header is not None:
        try:
            return max(0, int(header))
        except ValueError:
            pass
    payload = res.json() if res.content else {}
    rows = payload.get("result") if isinstance(payload, dict) else []
    return len(rows) if isinstance(rows, list) else 0


def list_table(
    http: ServiceNowHttpConfig,
    *,
    table: str,
    fields: str,
    limit: int = 50,
) -> List[Dict[str, Any]]:
    """GET table rows (read-only)."""
    res = _get(
        http,
        f"/api/now/table/{table}",
        params={
            "sysparm_limit": max(1, min(limit, 100)),
            "sysparm_fields": fields,
            "sysparm_display_value": "true",
        },
    )
    _raise_for_status(res, context=table)
    payload = res.json() if res.content else {}
    rows = payload.get("result") if isinstance(payload, dict) else []
    return rows if isinstance(rows, list) else []
