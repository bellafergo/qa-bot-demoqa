# services/qmetry_integration_service.py
"""
Read-only QMetry integration service (QMETRY-01A).

Discovery only — no test runs, executions, cycle/suite modifications, or result uploads.
Reuses integration_service for connector configuration and in-memory secrets.

Canonical live discovery entry points (used by API routes and UI):
  - list_projects()
  - list_test_cases()
  - list_test_cycles()
  - list_test_suites()
  - list_test_runs()

Test result discovery is deferred to QMETRY-01B — not implemented here.
"""
from __future__ import annotations

import logging
import os
from datetime import datetime, timezone
from typing import Optional

from models.qmetry_models import (
    QMetryConnectionStatus,
    QMetryProject,
    QMetryProjectsResponse,
    QMetryTestCase,
    QMetryTestCasesResponse,
    QMetryTestCycle,
    QMetryTestCyclesResponse,
    QMetryTestRun,
    QMetryTestRunsResponse,
    QMetryTestSuite,
    QMetryTestSuitesResponse,
)
from services.integration_service import integration_service
from services.qmetry_repository_service import (
    QMetryAPIError,
    QMetryHttpConfig,
    count_test_cases,
    count_test_runs,
    list_projects as fetch_qmetry_projects,
    parse_project,
    parse_test_case,
    parse_test_cycle,
    parse_test_run,
    parse_test_suite,
    search_test_cases,
    search_test_cycles,
    search_test_runs,
    search_test_suites,
    validate_connection,
)

logger = logging.getLogger("vanya.qmetry_integration")

_LAST_SYNC: Optional[datetime] = None


def _touch_sync() -> datetime:
    global _LAST_SYNC
    now = datetime.now(timezone.utc)
    _LAST_SYNC = now
    return now


def _resolve_http_config() -> Optional[QMetryHttpConfig]:
    """Resolve QMetry credentials from integration config, in-memory secrets, and env."""
    try:
        cfg = integration_service.get_config("qmetry")
    except KeyError:
        return None

    if not cfg.enabled:
        return None

    base_url = (cfg.base_url or os.getenv("QMETRY_BASE_URL") or "").strip().rstrip("/")
    api_key = (
        integration_service.get_connector_secret("qmetry", "api_key")
        or os.getenv("QMETRY_API_KEY")
        or ""
    ).strip()

    has_key = cfg.api_key_present or bool(api_key)
    if not base_url or not has_key or not api_key:
        return None

    return QMetryHttpConfig(
        base_url=base_url,
        api_key=api_key,
        project_key=(cfg.project_key or os.getenv("QMETRY_PROJECT_KEY") or "").strip() or None,
    )


def _empty_status(*, base_url: Optional[str] = None) -> QMetryConnectionStatus:
    return QMetryConnectionStatus(
        connected=False,
        base_url=base_url,
        last_sync=_LAST_SYNC,
    )


def validate_qmetry_connection() -> QMetryConnectionStatus:
    """Validate connectivity and return aggregate discovery counts."""
    http = _resolve_http_config()
    if http is None:
        try:
            cfg = integration_service.get_config("qmetry")
            return _empty_status(base_url=cfg.base_url)
        except KeyError:
            return _empty_status()

    try:
        validate_connection(http)
        projects = fetch_qmetry_projects(http)
        project_count = len(projects)
        test_case_count = count_test_cases(http, project_key=http.project_key)
        run_count = count_test_runs(http, project_key=http.project_key)
        synced = _touch_sync()
        return QMetryConnectionStatus(
            connected=True,
            base_url=http.base_url,
            project_count=project_count,
            test_case_count=test_case_count,
            run_count=run_count,
            last_sync=synced,
        )
    except QMetryAPIError as exc:
        logger.warning("qmetry validate failed: %s", exc)
        return _empty_status(base_url=http.base_url)


def list_projects() -> QMetryProjectsResponse:
    http = _resolve_http_config()
    if http is None:
        return QMetryProjectsResponse()

    try:
        raw = fetch_qmetry_projects(http)
        projects = [QMetryProject(**parse_project(p)) for p in raw if parse_project(p).get("project_id")]
        _touch_sync()
        return QMetryProjectsResponse(projects=projects, total=len(projects))
    except QMetryAPIError as exc:
        logger.warning("qmetry list_projects failed: %s", exc)
        return QMetryProjectsResponse()


def list_test_cases(*, project_key: Optional[str] = None, max_results: int = 50) -> QMetryTestCasesResponse:
    http = _resolve_http_config()
    if http is None:
        return QMetryTestCasesResponse()

    try:
        raw, total = search_test_cases(
            http,
            project_key=project_key or http.project_key,
            max_results=max_results,
        )
        test_cases = [
            QMetryTestCase(**parse_test_case(tc))
            for tc in raw
            if parse_test_case(tc).get("test_case_id")
        ]
        _touch_sync()
        return QMetryTestCasesResponse(test_cases=test_cases, total=total)
    except QMetryAPIError as exc:
        logger.warning("qmetry list_test_cases failed: %s", exc)
        return QMetryTestCasesResponse()


def list_test_cycles(*, project_key: Optional[str] = None, max_results: int = 50) -> QMetryTestCyclesResponse:
    http = _resolve_http_config()
    if http is None:
        return QMetryTestCyclesResponse()

    try:
        raw, total = search_test_cycles(
            http,
            project_key=project_key or http.project_key,
            max_results=max_results,
        )
        test_cycles = [
            QMetryTestCycle(**parse_test_cycle(c))
            for c in raw
            if parse_test_cycle(c).get("cycle_id")
        ]
        _touch_sync()
        return QMetryTestCyclesResponse(test_cycles=test_cycles, total=total)
    except QMetryAPIError as exc:
        logger.warning("qmetry list_test_cycles failed: %s", exc)
        return QMetryTestCyclesResponse()


def list_test_suites(*, project_key: Optional[str] = None, max_results: int = 50) -> QMetryTestSuitesResponse:
    http = _resolve_http_config()
    if http is None:
        return QMetryTestSuitesResponse()

    try:
        raw, total = search_test_suites(
            http,
            project_key=project_key or http.project_key,
            max_results=max_results,
        )
        test_suites = [
            QMetryTestSuite(**parse_test_suite(s))
            for s in raw
            if parse_test_suite(s).get("suite_id")
        ]
        _touch_sync()
        return QMetryTestSuitesResponse(test_suites=test_suites, total=total)
    except QMetryAPIError as exc:
        logger.warning("qmetry list_test_suites failed: %s", exc)
        return QMetryTestSuitesResponse()


def list_test_runs(*, project_key: Optional[str] = None, max_results: int = 50) -> QMetryTestRunsResponse:
    http = _resolve_http_config()
    if http is None:
        return QMetryTestRunsResponse()

    try:
        raw, total = search_test_runs(
            http,
            project_key=project_key or http.project_key,
            max_results=max_results,
        )
        test_runs = [
            QMetryTestRun(**parse_test_run(r))
            for r in raw
            if parse_test_run(r).get("run_id")
        ]
        _touch_sync()
        return QMetryTestRunsResponse(test_runs=test_runs, total=total)
    except QMetryAPIError as exc:
        logger.warning("qmetry list_test_runs failed: %s", exc)
        return QMetryTestRunsResponse()
