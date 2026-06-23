# tests/test_onboarding_service.py
"""ENT-03A — Guided Onboarding (read-only)."""
from __future__ import annotations

from types import SimpleNamespace
from unittest.mock import MagicMock, patch

import pytest
from fastapi.testclient import TestClient

import pytest

from models.platform_asset_models import PlatformAssetBootstrapResponse
from services.onboarding_service import build_onboarding_checklist


@pytest.fixture(autouse=True)
def _noop_platform_bootstrap():
    with patch(
        "services.platform_asset_bootstrap_service.bootstrap_platform_assets",
        return_value=PlatformAssetBootstrapResponse(project_id="demo"),
    ):
        yield


def _project(**settings):
    base = dict(settings)
    base_url = base.pop("base_url", None)
    return SimpleNamespace(
        id="demo",
        name="Demo",
        base_url=base_url,
        settings=base,
    )


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_new_project(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project()
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    assert checklist.project_id == "demo"
    assert checklist.completed_steps == 0
    assert checklist.total_steps == 8
    assert checklist.readiness_level == "NOT_READY"
    assert checklist.next_recommended_step == "Connect Repository"


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_repository_connected(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        github={
            "enabled": True,
            "installation_id": "123",
            "owner": "acme",
            "repo": "shop",
        }
    )
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    repo_step = next(s for s in checklist.steps if s.step_id == "connect_repository")
    assert repo_step.status == "COMPLETED"
    assert repo_step.completion_percentage == 100


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_tests_imported(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project()
    mock_catalog_repo.list_test_cases.return_value = [{"test_case_id": "T1"}]
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 3}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    tests_step = next(s for s in checklist.steps if s.step_id == "import_tests")
    assert tests_step.status == "COMPLETED"


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_environments_base_url_only_in_progress_40(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        base_url="https://zuperio-talent-os.vercel.app",
    )
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    env_step = next(s for s in checklist.steps if s.step_id == "configure_environments")
    assert env_step.status == "IN_PROGRESS"
    assert env_step.completion_percentage == 40


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_environments_one_env_in_progress_50(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        base_url="https://example.com",
        environments=[{"name": "QA", "type": "QA"}],
    )
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    env_step = next(s for s in checklist.steps if s.step_id == "configure_environments")
    assert env_step.status == "IN_PROGRESS"
    assert env_step.completion_percentage == 50


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_environments_two_envs_completed(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        environments=[
            {"name": "QA", "type": "QA", "url": "https://qa.example.com"},
            {"name": "Staging", "type": "STAGING"},
        ],
    )
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    env_step = next(s for s in checklist.steps if s.step_id == "configure_environments")
    assert env_step.status == "COMPLETED"
    assert env_step.completion_percentage == 100


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_local_agent_configured(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project()
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = [{"agent_id": "agent-1", "status": "online"}]
    mock_db_repo.list_connections.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    agent_step = next(s for s in checklist.steps if s.step_id == "configure_local_agent")
    assert agent_step.status == "COMPLETED"


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_local_agent_completed_when_offline(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    """Registration completes onboarding; operational online status is separate (Local Agents UI)."""
    mock_project_repo.get_project.return_value = _project()
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = [{"agent_id": "agent-1", "status": "offline"}]
    mock_db_repo.list_connections.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    agent_step = next(s for s in checklist.steps if s.step_id == "configure_local_agent")
    assert agent_step.status == "COMPLETED"
    assert agent_step.completion_percentage == 100
    assert "registered" in agent_step.recommended_next_action.lower()
    assert "bring an agent online" not in agent_step.recommended_next_action.lower()


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_database_connector_configured(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project()
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = [{"agent_id": "agent-1", "status": "online"}]
    mock_db_repo.list_connections.return_value = [
        {
            "connection_id": "db-1",
            "name": "Payments DB",
            "host_label": "payments.internal",
            "database_name": "payments",
        }
    ]
    mock_db_repo.has_successful_execution.return_value = True
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    db_step = next(s for s in checklist.steps if s.step_id == "configure_database_validation")
    assert db_step.status == "COMPLETED"


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_completion_calculation(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        github={"enabled": True, "installation_id": "1", "owner": "a", "repo": "b"},
        environments=[{"name": "QA"}, {"name": "STAGING"}],
    )
    mock_catalog_repo.list_test_cases.return_value = [{"test_case_id": "T1"}]
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 2}
    mock_watch_repo.list_watches.return_value = [{"watch_id": "w1"}]
    mock_agent_repo.list_agents.return_value = [{"agent_id": "agent-1", "status": "online"}]
    mock_db_repo.list_connections.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    assert checklist.completed_steps >= 4
    assert 0 < checklist.overall_completion < 100


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_readiness_level_mapping(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        github={"enabled": True, "installation_id": "1", "owner": "a", "repo": "b"},
    )
    mock_catalog_repo.list_test_cases.return_value = [{"test_case_id": "T1"}]
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 1}
    mock_watch_repo.list_watches.return_value = [{"watch_id": "w1"}]
    mock_agent_repo.list_agents.return_value = [{"agent_id": "agent-1", "status": "online"}]
    mock_db_repo.list_connections.return_value = [
        {
            "connection_id": "db-1",
            "name": "Payments DB",
            "host_label": "payments.internal",
            "database_name": "payments",
        }
    ]
    mock_db_repo.has_successful_execution.return_value = True
    mock_intel_flags.return_value = {
        "has_executive_report": True,
        "has_contract_intelligence": True,
        "has_quality_health": True,
    }

    checklist = build_onboarding_checklist("demo")
    assert checklist.readiness_level in ("READY", "FULLY_OPERATIONAL", "PARTIALLY_READY")


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_next_recommended_step(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project(
        github={"enabled": True, "installation_id": "1", "owner": "a", "repo": "b"},
        environments=[{"name": "QA"}, {"name": "STAGING"}],
    )
    mock_catalog_repo.list_test_cases.return_value = [{"test_case_id": "T1"}]
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 2}
    mock_watch_repo.list_watches.return_value = [{"watch_id": "w1"}]
    mock_agent_repo.list_agents.return_value = [{"agent_id": "agent-1", "status": "online"}]
    mock_db_repo.list_connections.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    checklist = build_onboarding_checklist("demo")
    assert checklist.next_recommended_step == "Configure Database Validation"


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
@patch("services.db.database_connector_repository.database_connector_repo")
@patch("services.db.local_agent_repository.local_agent_repo")
@patch("services.db.browser_inspection_watch_repository.browser_inspection_watch_repo")
@patch("services.db.catalog_repository.catalog_repo")
@patch("services.db.project_repository.project_repo")
def test_deterministic_output(
    mock_project_repo,
    mock_catalog_repo,
    mock_watch_repo,
    mock_agent_repo,
    mock_db_repo,
    _mock_knowledge,
    mock_intel_flags,
):
    mock_project_repo.get_project.return_value = _project()
    mock_catalog_repo.list_test_cases.return_value = []
    mock_catalog_repo.count_by_status_for_project.return_value = {"active": 0}
    mock_watch_repo.list_watches.return_value = []
    mock_agent_repo.list_agents.return_value = []
    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    first = build_onboarding_checklist("demo")
    second = build_onboarding_checklist("demo")
    assert first.model_dump() == second.model_dump()


def test_no_external_calls():
    import services.onboarding_service as mod

    source = open(mod.__file__, encoding="utf-8").read()
    assert "requests." not in source
    assert "httpx." not in source


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=False)
def test_contract_intelligence_not_started_guidance(mock_knowledge, mock_intel_flags):
    from services.onboarding_service import _contract_intelligence_state

    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    status, completion, action = _contract_intelligence_state("demo")
    assert status == "NOT_STARTED"
    assert completion == 0
    assert "generated yet" in action.lower()
    assert "investigation" in action.lower()


@patch("services.onboarding_service._incident_intelligence_flags")
@patch("services.onboarding_service._knowledge_has_apis", return_value=True)
def test_contract_intelligence_in_progress_guidance(mock_knowledge, mock_intel_flags):
    from services.onboarding_service import _contract_intelligence_state

    mock_intel_flags.return_value = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }

    status, completion, action = _contract_intelligence_state("demo")
    assert status == "IN_PROGRESS"
    assert completion == 70
    assert "api knowledge detected" in action.lower()
    assert "investigation" in action.lower()


@pytest.fixture
def client():
    from app import app

    return TestClient(app)


@patch("services.onboarding_service.build_onboarding_checklist")
def test_onboarding_api_endpoint(mock_build, client: TestClient):
    from models.onboarding_models import OnboardingChecklist, OnboardingStep

    mock_build.return_value = OnboardingChecklist(
        project_id="demo",
        overall_completion=10,
        readiness_level="NOT_READY",
        completed_steps=0,
        total_steps=8,
        next_recommended_step="Connect Repository",
        steps=[
            OnboardingStep(
                step_id="connect_repository",
                title="Connect Repository",
                description="",
                category="repository",
                status="NOT_STARTED",
                priority=1,
                completion_percentage=0,
                recommended_next_action="Connect",
            )
        ],
    )
    resp = client.get("/projects/demo/onboarding")
    assert resp.status_code == 200
    body = resp.json()
    assert body["project_id"] == "demo"
    assert body["next_recommended_step"] == "Connect Repository"
