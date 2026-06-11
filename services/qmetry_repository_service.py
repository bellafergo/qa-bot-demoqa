# services/qmetry_repository_service.py
"""
Low-level read-only QMetry Test Management REST API client.

Uses QMetry ATM API (/rest/atm/1.0/). Never logs API keys. No write operations.
"""
from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

import httpx

logger = logging.getLogger("vanya.qmetry_repository")

_ATM_PREFIX = "/rest/atm/1.0"


@dataclass(frozen=True)
class QMetryHttpConfig:
    base_url: str
    api_key: str
    project_key: Optional[str] = None


class QMetryAPIError(Exception):
    """Safe QMetry API failure — no credentials in message."""

    def __init__(self, message: str, *, status_code: int = 502, code: str = "qmetry_error"):
        super().__init__(message)
        self.status_code = status_code
        self.code = code


def _headers(http: QMetryHttpConfig) -> Dict[str, str]:
    return {
        "Authorization": http.api_key,
        "Accept": "application/json",
        "Content-Type": "application/json",
    }


def _api_base(http: QMetryHttpConfig) -> str:
    return http.base_url.rstrip("/")


def _raise_for_status(res: httpx.Response, *, context: str) -> None:
    sc = res.status_code
    if sc == 401:
        raise QMetryAPIError(
            "QMetry credentials are invalid or expired. Update the API key.",
            status_code=401,
            code="invalid_api_key",
        )
    if sc == 403:
        raise QMetryAPIError(
            "QMetry API access forbidden. Check API key permissions.",
            status_code=403,
            code="forbidden",
        )
    if sc == 404:
        raise QMetryAPIError(
            f"QMetry resource not found ({context}).",
            status_code=404,
            code="not_found",
        )
    if sc >= 500:
        raise QMetryAPIError(
            "QMetry is temporarily unavailable. Retry later.",
            status_code=502,
            code="qmetry_unavailable",
        )
    if not res.is_success:
        raise QMetryAPIError(
            f"QMetry API error ({sc}) for {context}.",
            status_code=502,
            code="qmetry_error",
        )


def _get(http: QMetryHttpConfig, path: str, *, params: Optional[Dict[str, Any]] = None) -> Any:
    url = f"{_api_base(http)}{path}"
    with httpx.Client(timeout=30.0) as client:
        res = client.get(url, headers=_headers(http), params=params or {})
    _raise_for_status(res, context=path)
    if not res.content:
        return {}
    return res.json()


def _extract_results(data: Any) -> Tuple[List[Dict[str, Any]], int]:
    """Normalize QMetry list/search payloads."""
    if isinstance(data, list):
        items = [x for x in data if isinstance(x, dict)]
        return items, len(items)
    if not isinstance(data, dict):
        return [], 0
    for key in ("results", "values", "data", "items"):
        raw = data.get(key)
        if isinstance(raw, list):
            items = [x for x in raw if isinstance(x, dict)]
            total = int(data.get("total") or data.get("totalCount") or len(items))
            return items, total
    return [], 0


def _field_name(value: Any) -> Optional[str]:
    if value is None:
        return None
    if isinstance(value, str):
        return value or None
    if isinstance(value, dict):
        for key in ("name", "label", "value"):
            if value.get(key):
                return str(value[key])
    return str(value)


def _project_query(project_key: Optional[str]) -> Optional[str]:
    if not project_key:
        return None
    return f'projectKey = "{project_key}"'


def validate_connection(http: QMetryHttpConfig) -> Dict[str, Any]:
    """GET /rest/atm/1.0/project — read-only connectivity check."""
    data = _get(http, f"{_ATM_PREFIX}/project")
    items, _ = _extract_results(data)
    return {"projects": items}


def list_projects(http: QMetryHttpConfig) -> List[Dict[str, Any]]:
    """GET /rest/atm/1.0/project — project discovery."""
    data = _get(http, f"{_ATM_PREFIX}/project")
    items, _ = _extract_results(data)
    return items


def search_test_cases(
    http: QMetryHttpConfig,
    *,
    project_key: Optional[str] = None,
    max_results: int = 50,
) -> Tuple[List[Dict[str, Any]], int]:
    """GET /rest/atm/1.0/testcase/search — read-only test case discovery."""
    params: Dict[str, Any] = {"startAt": 0, "maxResults": max_results}
    query = _project_query(project_key or http.project_key)
    if query:
        params["query"] = query
    data = _get(http, f"{_ATM_PREFIX}/testcase/search", params=params)
    return _extract_results(data)


def search_test_cycles(
    http: QMetryHttpConfig,
    *,
    project_key: Optional[str] = None,
    max_results: int = 50,
) -> Tuple[List[Dict[str, Any]], int]:
    """GET /rest/atm/1.0/testcycle/search — read-only test cycle discovery."""
    params: Dict[str, Any] = {"startAt": 0, "maxResults": max_results}
    query = _project_query(project_key or http.project_key)
    if query:
        params["query"] = query
    data = _get(http, f"{_ATM_PREFIX}/testcycle/search", params=params)
    return _extract_results(data)


def search_test_suites(
    http: QMetryHttpConfig,
    *,
    project_key: Optional[str] = None,
    max_results: int = 50,
) -> Tuple[List[Dict[str, Any]], int]:
    """GET /rest/atm/1.0/testsuite/search — read-only test suite discovery."""
    params: Dict[str, Any] = {"startAt": 0, "maxResults": max_results}
    query = _project_query(project_key or http.project_key)
    if query:
        params["query"] = query
    data = _get(http, f"{_ATM_PREFIX}/testsuite/search", params=params)
    return _extract_results(data)


def search_test_runs(
    http: QMetryHttpConfig,
    *,
    project_key: Optional[str] = None,
    max_results: int = 50,
) -> Tuple[List[Dict[str, Any]], int]:
    """GET /rest/atm/1.0/testrun/search — read-only test run discovery."""
    params: Dict[str, Any] = {"startAt": 0, "maxResults": max_results}
    query = _project_query(project_key or http.project_key)
    if query:
        params["query"] = query
    data = _get(http, f"{_ATM_PREFIX}/testrun/search", params=params)
    return _extract_results(data)


def count_test_cases(http: QMetryHttpConfig, *, project_key: Optional[str] = None) -> int:
    """Return total test case count without fetching full bodies."""
    _, total = search_test_cases(http, project_key=project_key, max_results=0)
    return total


def count_test_runs(http: QMetryHttpConfig, *, project_key: Optional[str] = None) -> int:
    """Return total test run count without fetching full bodies."""
    _, total = search_test_runs(http, project_key=project_key, max_results=0)
    return total


def parse_project(raw: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "project_id": str(raw.get("id") or raw.get("projectId") or raw.get("key") or ""),
        "project_name": str(raw.get("name") or raw.get("projectName") or raw.get("key") or ""),
    }


def parse_test_case(raw: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "test_case_id": str(raw.get("id") or raw.get("key") or ""),
        "name": str(raw.get("summary") or raw.get("name") or raw.get("key") or ""),
        "priority": _field_name(raw.get("priority")),
        "status": _field_name(raw.get("status")),
    }


def parse_test_cycle(raw: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "cycle_id": str(raw.get("id") or raw.get("key") or ""),
        "cycle_name": str(raw.get("name") or raw.get("summary") or raw.get("key") or ""),
        "status": _field_name(raw.get("status")),
    }


def parse_test_suite(raw: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "suite_id": str(raw.get("id") or raw.get("key") or ""),
        "suite_name": str(raw.get("name") or raw.get("summary") or raw.get("key") or ""),
    }


def parse_test_run(raw: Dict[str, Any]) -> Dict[str, Any]:
    execution_date = (
        raw.get("executionDate")
        or raw.get("execution_date")
        or raw.get("startDate")
        or raw.get("created")
    )
    return {
        "run_id": str(raw.get("id") or raw.get("key") or ""),
        "run_name": str(raw.get("summary") or raw.get("name") or raw.get("key") or ""),
        "status": _field_name(raw.get("status")),
        "execution_date": str(execution_date) if execution_date else None,
    }
