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


# ── v1.3A: analyze-only ranking, confidence, actions, context improvements ────


def test_rank_hypotheses_orders_by_confidence_desc():
    from services.incident_qa_investigator_service import _rank_hypotheses
    from models.incident_models import IncidentHypothesis

    ranked = _rank_hypotheses([
        IncidentHypothesis(statement="Low", confidence=0.4, basis="inference"),
        IncidentHypothesis(statement="High", confidence=0.82, basis="evidence", supporting_refs=["run:1"]),
        IncidentHypothesis(statement="Mid", confidence=0.6, basis="evidence", supporting_refs=["cluster:x"]),
    ])
    assert ranked[0].id == "H1"
    assert ranked[0].rank == 1
    assert ranked[0].confidence == 0.82
    assert ranked[1].confidence == 0.6
    assert ranked[2].confidence == 0.4


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_v13_primary_hypothesis_and_confidence_alignment(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    from models.incident_models import RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-strong",
            test_id="TC-A",
            test_name="Auth login",
            status="fail",
            started_at="2026-06-05T10:00:00+00:00",
            error_summary="Assertion failed",
            module="auth",
            evidence_url="https://cdn.example/ev",
        ),
    ]
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={"description": "Login fails", "module": "auth"},
    )
    body = r.json()
    hypos = body["hypotheses"]
    assert hypos[0]["rank"] == 1
    assert body["primary_hypothesis_id"] == hypos[0]["id"]
    assert body["confidence"] == hypos[0]["confidence"]
    confidences = [h["confidence"] for h in hypos]
    assert confidences == sorted(confidences, reverse=True)


@patch("services.failure_intelligence_service.failure_intelligence_service")
def test_gather_failure_clusters_filters_by_in_window_run_ids(mock_fi):
    from services.incident_qa_context_service import gather_failure_clusters

    class _C:
        def model_dump(self):
            return {
                "cluster_id": "c-old",
                "module": "auth",
                "run_ids": ["run-old"],
                "total_failures": 2,
                "root_cause_category": "selector_issue",
            }

    class _C2:
        def model_dump(self):
            return {
                "cluster_id": "c-new",
                "module": "auth",
                "run_ids": ["run-new"],
                "total_failures": 3,
                "root_cause_category": "selector_issue",
            }

    mock_fi.get_clusters.return_value = [_C(), _C2()]
    out = gather_failure_clusters(
        "demo",
        time_window_hours=24,
        in_window_run_ids=["run-new"],
    )
    assert len(out) == 1
    assert out[0]["cluster_id"] == "c-new"


@patch("services.run_history_service.run_history_service")
@patch("services.incident_qa_context_service._catalog_module_lookup")
def test_gather_failed_runs_resolves_module_from_catalog(mock_catalog, mock_rh):
    from datetime import datetime, timezone

    from models.run_contract import CanonicalRun, RunArtifacts
    from services.incident_qa_context_service import gather_failed_runs

    mock_catalog.return_value = {"TC-CAT-01": "checkout"}
    mock_rh.list_runs.return_value = [
        CanonicalRun(
            run_id="run-cat",
            test_id="TC-CAT-01",
            test_name="Checkout flow",
            status="failed",
            started_at=datetime.now(timezone.utc).isoformat(),
            error_summary="boom",
            meta={},
            artifacts=RunArtifacts(),
        ),
    ]
    runs = gather_failed_runs("demo", time_window_hours=72)
    assert len(runs) == 1
    assert runs[0].module == "checkout"
    assert runs[0].status == "fail"


@patch("services.run_history_service.run_history_service")
@patch("services.incident_qa_context_service._catalog_module_lookup", return_value={})
def test_gather_failed_runs_normalizes_failed_status(mock_catalog, mock_rh):
    from datetime import datetime, timezone

    from models.run_contract import CanonicalRun, RunArtifacts
    from services.incident_qa_context_service import gather_failed_runs

    now = datetime.now(timezone.utc).isoformat()
    mock_rh.list_runs.return_value = [
        CanonicalRun(
            run_id="run-failed",
            test_id="TC-1",
            test_name="T",
            status="failed",
            started_at=now,
            error_summary="err",
            meta={"module": "auth"},
            artifacts=RunArtifacts(),
        ),
        CanonicalRun(
            run_id="run-error",
            test_id="TC-2",
            test_name="T2",
            status="error",
            started_at=now,
            error_summary="err2",
            meta={"module": "auth"},
            artifacts=RunArtifacts(),
        ),
        CanonicalRun(
            run_id="run-pass",
            test_id="TC-3",
            test_name="T3",
            status="passed",
            started_at=now,
            meta={"module": "auth"},
            artifacts=RunArtifacts(),
        ),
    ]
    runs = gather_failed_runs("demo", time_window_hours=72)
    assert {r.run_id for r in runs} == {"run-failed", "run-error"}
    assert all(r.status in ("fail", "error") for r in runs)


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs")
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_v13_actions_available_require_approval(
    _gp, mock_runs, _clusters, _regs, _know, mock_prs, _pra, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRSummary, RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="TC-1",
            status="fail",
            started_at="2026-06-05T10:00:00+00:00",
            module="auth",
            error_summary="fail",
        ),
    ]
    mock_prs.return_value = [
        RelatedPRSummary(
            provider="github",
            pr_id="99",
            title="Fix auth",
            branch="fix/auth",
            author="dev",
            html_url="https://github.com/x/y/pull/99",
            updated_at="2026-06-05T09:00:00Z",
            match_reason="keyword_match",
        ),
    ]
    r = client.post(
        "/projects/demo/incidents/investigate",
        json={
            "description": "Auth broken",
            "target_url": "https://app.example.com/login",
            "include_browser_probe": False,
        },
    )
    body = r.json()
    actions = body.get("actions_available") or []
    assert actions
    assert all(a["requires_user_approval"] is True for a in actions)
    action_names = {a["action"] for a in actions}
    assert "run_browser_probe" in action_names
    assert "generate_rca" in action_names
    assert "analyze_related_pr" in action_names
    assert "run_recommended_tests" in action_names
    assert body.get("browser_investigation") is None


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_v13_browser_probe_only_when_requested(
    _gp, _runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    with patch("services.incident_investigator_service.investigate_incident") as mock_probe:
        from models.incident_models import IncidentInvestigationRun

        mock_probe.return_value = IncidentInvestigationRun(
            id="browser-1",
            created_at="2026-06-05T12:00:00+00:00",
            updated_at="2026-06-05T12:00:00+00:00",
            status="completed",
            incident_description="probe",
            probable_cause="HTTP 500 on /api/auth",
            reproduced="true",
            severity="high",
        )
        r = client.post(
            "/projects/demo/incidents/investigate",
            json={
                "description": "Auth page 500",
                "target_url": "https://app.example.com/login",
                "include_browser_probe": True,
            },
        )
    assert r.status_code == 200
    body = r.json()
    mock_probe.assert_called_once()
    assert body.get("browser_investigation") is not None
    browser_hypos = [h for h in body["hypotheses"] if "browser:" in " ".join(h.get("supporting_refs", []))]
    assert browser_hypos
    assert browser_hypos[0]["basis"] == "evidence"


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_v13_no_browser_probe_when_not_requested(
    _gp, _runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    with patch("services.incident_investigator_service.investigate_incident") as mock_probe:
        r = client.post(
            "/projects/demo/incidents/investigate",
            json={
                "description": "Auth page 500",
                "target_url": "https://app.example.com/login",
                "include_browser_probe": False,
            },
        )
    assert r.status_code == 200
    mock_probe.assert_not_called()
    assert r.json().get("browser_investigation") is None


# ── v1.3B: evidence strength, blast radius, temporal correlation, tests v2 ────


def test_build_evidence_strength_separates_buckets():
    from models.incident_models import IncidentHypothesis, RelatedPRAnalysisSummary, RelatedRunSummary
    from services.incident_analysis_service import build_evidence_strength

    strength = build_evidence_strength(
        related_runs=[
            RelatedRunSummary(run_id="r1", test_id="TC-1", status="failed", module="auth"),
        ],
        related_evidence=[],
        related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="10",
                provider="github",
                pr_risk_score=70.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                reason="matched",
            ),
        ],
        browser_events=[{"timestamp": "2026-06-05T13:22:00+00:00", "summary": "Login alert", "watch_id": "w1"}],
        clusters=[{"cluster_id": "c1", "module": "auth", "total_failures": 3, "root_cause_category": "timeout"}],
        related_prs=[],
        hints={"login", "auth"},
        hypotheses=[
            IncidentHypothesis(id="H1", statement="Auth issue", confidence=0.4, basis="assumption", supporting_refs=["keyword:auth"]),
        ],
    )
    assert strength.evidence
    assert any("Failed runs" in e.label for e in strength.evidence)
    assert any("PR Analysis" in e.label for e in strength.evidence)
    assert strength.inference
    assert strength.assumptions
    assert strength.assumptions[0].kind == "assumption"


def test_build_blast_radius_ranks_modules():
    from models.incident_models import RelatedPRAnalysisSummary, RelatedRunSummary
    from services.incident_analysis_service import build_blast_radius

    ranked = build_blast_radius(
        related_runs=[
            RelatedRunSummary(run_id="r1", test_id="TC-AUTH", module="auth", status="failed"),
            RelatedRunSummary(run_id="r2", test_id="TC-PROF", module="profile", status="failed"),
        ],
        clusters=[{"module": "auth", "total_failures": 4, "cluster_id": "c-auth"}],
        related_pr_analysis=[
            RelatedPRAnalysisSummary(
                pr_number="42",
                provider="github",
                pr_risk_score=60.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                reason="same module",
            ),
        ],
        related_prs=[],
        primary_module="auth",
    )
    assert ranked
    assert ranked[0].module.lower() == "auth"
    assert ranked[0].score >= ranked[-1].score
    assert ranked[0].reason


def test_enrich_timeline_temporal_strong_correlation():
    from models.incident_models import IncidentTimelineEvent
    from services.incident_analysis_service import enrich_timeline_temporal

    events = [
        IncidentTimelineEvent(
            timestamp="2026-06-05T13:00:00+00:00",
            event_type="pr_analyzed",
            title="PR #42 analyzed",
            source="pr_analysis",
        ),
        IncidentTimelineEvent(
            timestamp="2026-06-05T13:12:00+00:00",
            event_type="browser_watch_alert",
            title="Browser alert",
            source="browser_watch",
        ),
        IncidentTimelineEvent(
            timestamp="2026-06-05T13:30:00+00:00",
            event_type="run_failed",
            title="Run failed",
            source="run_history",
        ),
        IncidentTimelineEvent(
            timestamp="2026-06-05T14:00:00+00:00",
            event_type="incident_reported",
            title="Incident reported",
            source="investigator",
        ),
    ]
    enriched, summary = enrich_timeline_temporal(events)
    assert enriched[1].time_distance_minutes == 12
    assert enriched[2].time_distance_minutes == 18
    assert summary.signal in ("strong", "medium")
    assert summary.event_chain


def test_build_recommended_tests_v2_with_reason_and_strength():
    from models.incident_models import BlastRadiusModule, RelatedRunSummary
    from services.incident_analysis_service import build_recommended_tests_v2

    recs = build_recommended_tests_v2(
        related_runs=[
            RelatedRunSummary(run_id="r1", test_id="AUTH-014", test_name="Login", module="auth", status="failed"),
        ],
        regressions=[{"test_case_id": "AUTH-022", "module": "auth", "repeated_failures": 2}],
        pr_analysis_tests=["PROFILE-001"],
        primary_module="auth",
        impacted_modules_ranked=[
            BlastRadiusModule(module="AUTH", score=88.0, reason="failed runs"),
            BlastRadiusModule(module="PROFILE", score=42.0, reason="referenced"),
        ],
        time_window_hours=168,
    )
    by_id = {r.test_case_id: r for r in recs}
    assert by_id["AUTH-014"].recommendation_strength == "high"
    assert "primary hypothesis module" in by_id["AUTH-014"].reason.lower()
    assert by_id["AUTH-022"].recommendation_strength == "high"
    assert "failed" in by_id["AUTH-022"].reason.lower()
    assert by_id["PROFILE-001"].recommendation_strength == "medium"


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs")
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_v13b_report_includes_analysis_fields(
    _gp, mock_runs, _clusters, _regs, _know, _prs, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRAnalysisSummary, RelatedRunSummary

    mock_runs.return_value = [
        RelatedRunSummary(
            run_id="run-1",
            test_id="AUTH-014",
            test_name="Login flow",
            status="failed",
            started_at="2026-06-05T13:30:00+00:00",
            module="auth",
            error_summary="redirect failed",
        ),
    ]
    with patch(
        "services.incident_qa_investigator_service.gather_related_pr_analysis",
        return_value=[
            RelatedPRAnalysisSummary(
                pr_number="42",
                provider="github",
                pr_risk_score=70.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                recommended_tests=["AUTH-014"],
                analyzed_at="2026-06-06T13:00:00+00:00",
                reason="same module affected: auth",
            ),
        ],
    ):
        r = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Login broken after deploy", "module": "auth"},
        )
    body = r.json()
    assert body["meta"]["engine_version"] == "incident-v1.3b"
    assert body["meta"]["analyze_only"] is True
    assert body.get("evidence_strength")
    assert body["evidence_strength"]["evidence"]
    assert body.get("impacted_modules_ranked")
    assert body["impacted_modules_ranked"][0]["score"] > 0
    assert body.get("recommended_tests_v2")
    assert body["recommended_tests_v2"][0].get("reason")
    assert body["recommended_tests_v2"][0].get("recommendation_strength")
    assert body.get("temporal_correlation")
    for ev in body.get("timeline") or []:
        if ev.get("relative_to_previous"):
            assert "time_distance_minutes" in ev


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_related_pr_analysis", return_value=[])
@patch("services.incident_qa_investigator_service.gather_open_prs", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_v13b_no_auto_execution(
    _gp, _runs, _clusters, _regs, _know, _prs, _pra, _bw, client: TestClient,
):
    with patch("services.incident_investigator_service.investigate_incident") as mock_probe:
        with patch("runners.browser_inspector_runner.run_browser_inspection") as mock_browser:
            r = client.post(
                "/projects/demo/incidents/investigate",
                json={
                    "description": "Auth outage",
                    "target_url": "https://app.example.com/login",
                    "include_browser_probe": False,
                },
            )
    assert r.status_code == 200
    mock_probe.assert_not_called()
    mock_browser.assert_not_called()
    body = r.json()
    assert body["meta"]["analyze_only"] is True
    for action in body.get("actions_available") or []:
        assert action["requires_user_approval"] is True


# ── Hotfix: RelatedPRAnalysisSummary uses pr_number, not pr_id ────────────────


def test_build_evidence_strength_uses_pr_number_not_pr_id():
    """Regression: production crash when open PRs coexist with PR analysis snapshots."""
    from models.incident_models import RelatedPRAnalysisSummary, RelatedPRSummary
    from services.incident_analysis_service import build_evidence_strength

    related_prs = [
        RelatedPRSummary(
            provider="github",
            pr_id="42",
            title="Fix login redirect",
            branch="fix/login",
            author="dev1",
            html_url="https://github.com/org/repo/pull/42",
            updated_at="2026-06-09T10:00:00Z",
            match_reason="keyword_match",
        ),
    ]
    related_pr_analysis = [
        RelatedPRAnalysisSummary(
            pr_number="99",
            provider="github",
            pr_risk_score=55.0,
            risk_level="MEDIUM",
            impacted_modules=["auth"],
            reason="same module affected: auth",
        ),
    ]

    strength = build_evidence_strength(
        related_runs=[],
        related_evidence=[],
        related_pr_analysis=related_pr_analysis,
        browser_events=[],
        clusters=[],
        related_prs=related_prs,
        hints={"login"},
        hypotheses=[],
    )

    assert any(item.label == "PR Analysis snapshot" for item in strength.evidence)
    assert any(item.label == "Open PR (no snapshot)" for item in strength.inference)
    assert any("42" in item.detail for item in strength.inference)


@patch("services.incident_qa_investigator_service.gather_browser_watch_events", return_value=[])
@patch("services.incident_qa_investigator_service.gather_knowledge_context", return_value=None)
@patch("services.incident_qa_investigator_service.gather_regressions", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failure_clusters", return_value=[])
@patch("services.incident_qa_investigator_service.gather_failed_runs", return_value=[])
@patch("services.db.project_repository.project_repo.get_project", return_value=_mock_project())
def test_investigate_with_pr_analysis_and_open_prs_does_not_crash(
    _gp, _runs, _clusters, _regs, _know, _bw, client: TestClient,
):
    from models.incident_models import RelatedPRAnalysisSummary, RelatedPRSummary

    with patch(
        "services.incident_qa_investigator_service.gather_related_pr_analysis",
        return_value=[
            RelatedPRAnalysisSummary(
                pr_number="99",
                provider="github",
                pr_risk_score=60.0,
                risk_level="HIGH",
                impacted_modules=["auth"],
                reason="same module affected: auth",
            ),
        ],
    ), patch(
        "services.incident_qa_investigator_service.gather_open_prs",
        return_value=[
            RelatedPRSummary(
                provider="github",
                pr_id="42",
                title="Fix login",
                branch="fix/login",
                author="dev",
                html_url="https://github.com/x/y/pull/42",
                updated_at="2026-06-09T09:00:00Z",
                match_reason="keyword_match",
            ),
        ],
    ):
        r = client.post(
            "/projects/demo/incidents/investigate",
            json={"description": "Login broken after deploy", "module": "auth"},
        )

    assert r.status_code == 200
    body = r.json()
    assert body.get("evidence_strength")
    assert body["meta"]["engine_version"] == "incident-v1.3b"
    assert body["meta"]["analyze_only"] is True
