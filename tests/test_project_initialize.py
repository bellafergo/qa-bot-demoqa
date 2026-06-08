# tests/test_project_initialize.py
"""POST /projects/{id}/initialize — orchestration with partial-step reporting."""
from __future__ import annotations

import json
from unittest.mock import MagicMock, patch

import pytest
from fastapi.testclient import TestClient

from app import app
from models.project_initialize_models import ProjectInitializeRequest


@pytest.fixture
def client() -> TestClient:
    return TestClient(app)


def test_initialize_unknown_project_returns_404(client: TestClient) -> None:
    resp = client.post("/projects/nonexistent-init-proj-xyz/initialize", json={})
    assert resp.status_code == 404
    assert "application/json" in resp.headers.get("content-type", "")
    body = resp.json()
    assert "detail" in body or "message" in body


def test_initialize_existing_project_returns_json_shape(client: TestClient) -> None:
    pid = "init-test-proj"
    create = client.post(
        "/projects",
        json={"id": pid, "name": "Init Test", "description": "pytest"},
    )
    if create.status_code == 409:
        pass
    else:
        assert create.status_code == 201

    resp = client.post(
        f"/projects/{pid}/initialize",
        json={"run_smoke": False, "refresh_knowledge": False},
    )
    content_type = resp.headers.get("content-type", "")
    assert "application/json" in content_type
    assert resp.status_code == 200
    data = resp.json()
    assert data.get("project_id") == pid
    assert "ok" in data
    assert "steps" in data
    assert isinstance(data["steps"], list)
    assert data["steps"], "expected at least catalog step"
    json.dumps(data)


def test_initialize_smoke_failure_still_reports_readiness(client: TestClient) -> None:
    pid = "init-smoke-fail-proj"
    create = client.post(
        "/projects",
        json={"id": pid, "name": "Smoke Fail Test", "description": "pytest"},
    )
    if create.status_code == 409:
        pass
    else:
        assert create.status_code == 201

    failed_job = MagicMock(
        job_id="job-fail-1",
        status="failed",
        error_message="no tests matched",
        total_count=0,
    )
    with patch(
        "services.project_initialize_service._count_catalog",
        return_value=(5, 2, 1, ["tc-a", "tc-b"]),
    ):
        with patch("services.catalog_orchestrator.orchestrator_service") as orch:
            orch.enqueue_suite.return_value = failed_job
            with patch(
                "services.project_initialize_service.refresh_project_knowledge",
            ) as rk:
                rk.return_value = None
                from services.project_initialize_service import initialize_project

                res = initialize_project(
                    pid,
                    ProjectInitializeRequest(run_smoke=True, refresh_knowledge=True),
                )

    steps = {s.step: s.status for s in res.steps}
    assert steps.get("knowledge") == "ok"
    assert steps.get("smoke_run") == "failed"
    assert steps.get("readiness") == "ok"
    assert res.ok is False


def test_evidence_summary_returns_json(client: TestClient) -> None:
    resp = client.get("/evidences/summary")
    assert resp.status_code == 200
    assert "application/json" in resp.headers.get("content-type", "")
    data = resp.json()
    assert "total" in data
    assert "screenshots" in data
    assert "reports" in data
    json.dumps(data)


def test_evidence_summary_project_filter(client: TestClient) -> None:
    resp = client.get("/evidences/summary?project_id=demo")
    assert resp.status_code == 200
    data = resp.json()
    assert isinstance(data.get("total"), int)
