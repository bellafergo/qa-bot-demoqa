# tests/test_incident_investigator.py
from __future__ import annotations

from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.incident_models import InvestigateIncidentRequest
from services.incident_diagnosis import build_incident_diagnosis
from services.incident_investigator_service import infer_target_url, investigate_incident


@pytest.fixture()
def client():
    from app import app

    return TestClient(app)


def test_infer_target_url_explicit():
    url, src = infer_target_url("login broken", target_url="https://app.example.com/login", project_id=None, module=None)
    assert url == "https://app.example.com/login"
    assert src == "explicit"


def test_infer_target_url_from_project_and_keyword():
    class _P:
        base_url = "https://demo.example.com"

    with patch("services.db.project_repository.project_repo.get_project", return_value=_P()):
        url, src = infer_target_url(
            "El dashboard se queda cargando",
            target_url=None,
            project_id="demo",
            module=None,
        )
    assert url == "https://demo.example.com/dashboard"
    assert src == "inferred_from_project"


def test_build_diagnosis_server_error():
    sev, rep, area, endpoint, summary, *_ = build_incident_diagnosis(
        incident_description="API fails",
        target_url="https://app.example.com",
        navigation_error=None,
        console_errors=[],
        network_errors=[],
        http_errors=[{"url": "https://app.example.com/api/runs", "status": 500, "method": "GET"}],
    )
    assert sev in ("high", "critical")
    assert rep == "true"
    assert area == "backend"
    assert endpoint == "https://app.example.com/api/runs"
    assert "5xx" in summary or "Server" in summary


def test_build_diagnosis_console_error():
    _, rep, area, _, _, symptom, *_ = build_incident_diagnosis(
        incident_description="UI crash",
        target_url="https://app.example.com",
        navigation_error=None,
        console_errors=[{"text": "TypeError: x is undefined", "location": None}],
        network_errors=[],
        http_errors=[],
    )
    assert rep == "true"
    assert area == "frontend"
    assert "console" in symptom.lower()


@patch("services.incident_investigator_service.validate_target_url", side_effect=lambda u, **_: u)
@patch("runners.browser_inspector_runner.run_browser_inspection")
@patch("services.browser_inspector_service.normalize_raw_runner_output")
def test_investigate_incident_happy_path(mock_norm, mock_run, _validate):
    mock_run.return_value = {
        "url": "https://example.com",
        "final_url": "https://example.com/",
        "title": "Ex",
        "status_code": 200,
        "inventory": {"headings": [], "inputs": [], "buttons": [], "links": []},
        "extras": {},
        "screenshot_b64": "abc",
        "screenshot_logs": [],
        "console_errors": [{"text": "boom", "location": None}],
        "network_errors": [],
        "http_error_responses": [],
        "navigation_error": None,
    }

    class _Insp:
        inspection_id = "insp-1"
        screenshot_url = "https://cdn.example/s.png"
        inspection_succeeded = True
        inventory_counts = {}
        warnings = []

    mock_norm.return_value = _Insp()

    req = InvestigateIncidentRequest(
        incident_description="Console error on homepage",
        target_url="https://example.com",
    )
    result = investigate_incident(req)
    assert result.status == "completed"
    assert result.reproduced == "true"
    assert result.suspected_area == "frontend"
    assert result.console_errors
    assert result.screenshot_url == "https://cdn.example/s.png"
    assert "launch_browser" in result.steps_executed


def test_post_investigate_endpoint(client: TestClient):
    from models.incident_models import IncidentInvestigationRun

    fake = IncidentInvestigationRun(
        id="test-id",
        created_at="2026-01-01T00:00:00+00:00",
        updated_at="2026-01-01T00:00:00+00:00",
        status="completed",
        incident_description="No URL provided",
        diagnosis_summary="No target URL",
    )
    with patch("api.routes.incident_routes.investigate_incident", return_value=fake):
        r = client.post(
            "/incidents/investigate",
            json={"incident_description": "No URL provided"},
        )
    assert r.status_code == 200
    body = r.json()
    assert body["status"] == "completed"
    assert body["incident_description"] == "No URL provided"


def test_list_and_get_incident_runs(client: TestClient):
    with patch("runners.browser_inspector_runner.run_browser_inspection") as mock_run:
        mock_run.return_value = {
            "url": "https://example.com",
            "final_url": "https://example.com/",
            "title": "Ex",
            "status_code": 200,
            "inventory": {},
            "extras": {},
            "screenshot_b64": None,
            "screenshot_logs": [],
            "console_errors": [],
            "network_errors": [],
            "http_error_responses": [],
            "navigation_error": None,
        }
        with patch("services.browser_inspector_service.normalize_raw_runner_output") as mock_norm:
            class _Insp:
                inspection_id = "insp-2"
                screenshot_url = None
                inspection_succeeded = True
                inventory_counts = {}
                warnings = []

            mock_norm.return_value = _Insp()
            created = client.post(
                "/incidents/investigate",
                json={
                    "incident_description": "Smoke test investigation",
                    "target_url": "https://example.com",
                },
            )
    assert created.status_code == 200
    run_id = created.json()["id"]

    listed = client.get("/incidents/runs")
    assert listed.status_code == 200
    items = listed.json()["items"]
    assert any(x["id"] == run_id for x in items)

    got = client.get(f"/incidents/runs/{run_id}")
    assert got.status_code == 200
    assert got.json()["id"] == run_id

    missing = client.get("/incidents/runs/does-not-exist")
    assert missing.status_code == 404


# ── Project-scoped QA Intelligence investigation ─────────────────────────────


def _mock_project():
    class _P:
        id = "demo"
        name = "Demo"
    return _P()


def _canonical_failed_run(**overrides):
    from models.run_contract import CanonicalRun, RunArtifacts

    base = dict(
        run_id="run-login-1",
        test_id="TC-LOGIN-01",
        test_name="Login with valid credentials",
        status="failed",
        started_at="2026-06-05T10:00:00+00:00",
        error_summary="AssertionError: redirect to dashboard failed",
        rca_summary="Auth redirect timeout",
        artifacts=RunArtifacts(evidence_url="https://cdn.example/ev-1"),
        meta={"module": "auth"},
    )
    base.update(overrides)
    return CanonicalRun(**base)


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_project_incident_no_data(
    _gp, _runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Something broke after deploy", "time_window_hours": 72},
    )
    assert r.status_code == 200
    body = r.json()
    assert body["project_id"] == "demo"
    assert body["related_runs"] == []
    assert body["related_evidence"] == []
    assert body["related_prs"] == []
    assert body["data_gaps"]
    assert body["confidence"] < 0.5
    assert body.get("id")
    assert body.get("timeline")
    assert body.get("confidence_breakdown")
    hypo_text = " ".join(h["statement"] for h in body["hypotheses"])
    assert "manual triage" in hypo_text.lower() or "no strong" in hypo_text.lower()


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_project_incident_with_failed_run(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    from models.incident_models import RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-login-1",
            test_id="TC-LOGIN-01",
            test_name="Login with valid credentials",
            status="failed",
            started_at="2026-06-05T10:00:00+00:00",
            error_summary="AssertionError: redirect failed",
            rca_summary="Auth redirect timeout",
            module="auth",
            evidence_url="https://cdn.example/ev-1",
        ),
    ]
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "El login falla después del deploy", "time_window_hours": 72},
    )
    assert r.status_code == 200
    body = r.json()
    assert len(body["related_runs"]) == 1
    assert body["related_runs"][0]["run_id"] == "run-login-1"
    assert len(body["related_evidence"]) >= 1
    assert body["evidence_found"]
    labels = {f["label"] for f in body["confidence_breakdown"]}
    assert "failed_runs" in labels
    assert body["confidence"] >= 0.12
    evidence_hypo = [h for h in body["hypotheses"] if h["basis"] == "evidence"]
    assert evidence_hypo
    assert "login" in evidence_hypo[0]["statement"].lower() or "TC-LOGIN" in evidence_hypo[0]["statement"]


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context")
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_project_incident_knowledge_module_match(
    _gp, _runs, _clusters, _regs, mock_know, _prs, _pra, _bw, client: TestClient,
):
    from models.project_knowledge_models import ProjectKnowledgeContext

    mock_know.return_value = ProjectKnowledgeContext(
        project_id="demo",
        modules=["auth", "dashboard"],
        hints=["Incident mentions known module 'auth' (12 tests)."],
        risk_level="HIGH",
        risk_score=72.0,
    )
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Login page returns 500", "module": "auth"},
    )
    assert r.status_code == 200
    body = r.json()
    assert "auth" in body["impacted_modules"]
    assert any("knowledge" in " ".join(h.get("supporting_refs", [])) for h in body["hypotheses"])
    assert body["evidence_found"]


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs")
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_project_incident_related_pr(
    _gp, _runs, _clusters, _regs, _know, mock_prs, _pra, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRSummary

    mock_prs.return_value = [
        RelatedPRSummary(
            provider="github",
            pr_id="42",
            title="Fix login redirect",
            branch="fix/login",
            author="dev1",
            html_url="https://github.com/org/repo/pull/42",
            updated_at="2026-06-05T09:00:00Z",
            match_reason="keyword_match",
        ),
    ]
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Login broken after merge"},
    )
    assert r.status_code == 200
    body = r.json()
    assert len(body["related_prs"]) == 1
    assert body["related_prs"][0]["pr_id"] == "42"
    pr_hypo = [h for h in body["hypotheses"] if "pr:" in " ".join(h.get("supporting_refs", []))]
    assert pr_hypo
    assert pr_hypo[0]["basis"] == "inference"


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_project_incident_does_not_invent_evidence(
    _gp, _runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Random outage"},
    )
    body = r.json()
    assert body["related_runs"] == []
    assert body["related_evidence"] == []
    assert not any("screenshot" in e.lower() for e in body["evidence_found"])
    for h in body["hypotheses"]:
        if h["basis"] == "evidence":
            assert h["supporting_refs"]


def test_project_incident_unknown_project(client: TestClient):
    with patch("services.db.project_repository.project_repo.get_project", return_value=None):
        r = client.post(
            "/projects/missing/incidents/investigate",
            json={"description": "Something broke"},
        )
    assert r.status_code == 404


# ── v1.1: persistence, PR Analysis correlation, timeline, confidence v2 ─────


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_incident_persistence_and_history(
    _gp, _runs, _clusters, _regs, _know, _prs, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRAnalysisSummary

    with patch(
        "services.incident_qa_investigator_service.gather_related_pr_analysis",
        return_value=[
            RelatedPRAnalysisSummary(
                pr_number="142",
                provider="github",
                pr_risk_score=62.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                reason="same module affected: auth",
                analyzed_at="2026-06-05T08:00:00+00:00",
            ),
        ],
    ):
        created = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Login fails after deploy", "module": "auth"},
        )
    assert created.status_code == 200
    body = created.json()
    incident_id = body["id"]
    assert incident_id

    history = client.get("/projects/demo/incidents/history")
    assert history.status_code == 200
    items = history.json()["items"]
    assert any(x["id"] == incident_id for x in items)

    got = client.get(f"/projects/demo/incidents/{incident_id}")
    assert got.status_code == 200
    full = got.json()
    assert full["id"] == incident_id
    assert full["related_pr_analysis"]
    assert full["related_pr_analysis"][0]["pr_number"] == "142"


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_incident_pr_analysis_correlation(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRAnalysisSummary, RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="TC-LOGIN",
            test_name="Login test",
            status="failed",
            started_at="2026-06-05T12:00:00+00:00",
            module="auth",
            error_summary="redirect failed",
        ),
    ]
    with patch(
        "services.incident_qa_investigator_service.gather_related_pr_analysis",
        return_value=[
            RelatedPRAnalysisSummary(
                pr_number="142",
                provider="github",
                pr_risk_score=62.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                recommended_tests=["TC-LOGIN"],
                reason="same module affected: auth",
                analyzed_at="2026-06-05T08:00:00+00:00",
            ),
        ],
    ):
        r = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Login broken", "module": "auth"},
        )
    body = r.json()
    assert body["related_pr_analysis"][0]["pr_risk_score"] == 62.0
    assert "auth" in body["related_pr_analysis"][0]["impacted_modules"]
    pr_hypo = [h for h in body["hypotheses"] if "pr_analysis" in " ".join(h.get("supporting_refs", []))]
    assert pr_hypo
    assert pr_hypo[0]["basis"] == "evidence"


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_incident_timeline_ordered(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRAnalysisSummary, RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="TC-1",
            status="failed",
            started_at="2026-06-05T12:00:00+00:00",
            module="auth",
        ),
    ]
    with patch(
        "services.incident_qa_investigator_service.gather_related_pr_analysis",
        return_value=[
            RelatedPRAnalysisSummary(
                pr_number="142",
                provider="github",
                pr_risk_score=62.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                analyzed_at="2026-06-05T08:00:00+00:00",
                reason="matched",
            ),
        ],
    ):
        r = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Login issue"},
        )
    timeline = r.json()["timeline"]
    assert timeline
    assert timeline[-1]["event_type"] == "incident_reported"
    ts = [e["timestamp"] for e in timeline if e["timestamp"]]
    assert ts == sorted(ts)


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_confidence_increases_with_evidence(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRAnalysisSummary, RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="TC-1",
            status="failed",
            started_at="2026-06-05T12:00:00+00:00",
            module="auth",
            error_summary="fail",
            evidence_url="https://cdn.example/ev",
        ),
    ]
    with patch(
        "services.incident_qa_investigator_service.gather_related_pr_analysis",
        return_value=[
            RelatedPRAnalysisSummary(
                pr_number="142",
                provider="github",
                pr_risk_score=62.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                analyzed_at="2026-06-05T08:00:00+00:00",
                reason="same module affected: auth",
            ),
        ],
    ):
        r = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Login broken"},
        )
    body = r.json()
    assert body["confidence"] >= 0.30
    labels = {f["label"] for f in body["confidence_breakdown"]}
    assert "failed_runs" in labels
    assert "pr_analysis" in labels


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_confidence_decreases_with_data_gaps(
    _gp, _runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Unknown issue"},
    )
    body = r.json()
    assert body["confidence"] < 0.35
    labels = {f["label"] for f in body["confidence_breakdown"]}
    assert "data_gaps" in labels
    assert "no_failed_runs" in labels
