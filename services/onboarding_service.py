# services/onboarding_service.py
"""
Enterprise ENT-03A — Guided Onboarding (read-only).

Builds a deterministic onboarding checklist from existing project data only.
No scanning, execution, external API calls, or automatic configuration.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Tuple

from models.onboarding_models import OnboardingChecklist, OnboardingStep

logger = logging.getLogger("vanya.onboarding")

_DEFAULT_STEPS: Tuple[Tuple[str, str, str, str, int], ...] = (
    ("connect_repository", "Connect Repository", "Link GitHub or Azure DevOps to analyze pull requests and changes.", "repository", 1),
    ("import_tests", "Import Tests", "Add automated tests to the catalog so Vanya can measure quality.", "testing", 2),
    ("configure_browser_monitoring", "Configure Browser Monitoring", "Set up browser watches to detect UI regressions early.", "browser_monitoring", 3),
    ("configure_environments", "Configure Environments", "Define QA, staging, and production environments for release intelligence.", "environments", 4),
    ("configure_local_agent", "Configure Local Agent", "Register a local agent to run tests and validations in your network.", "agents", 5),
    ("configure_database_validation", "Configure Database Validation", "Add secure database connectors for data integrity checks.", "database_validation", 6),
    ("configure_contract_intelligence", "Generate Contract Intelligence", "Generate API contract intelligence from investigations, PR analysis, or API contract evidence.", "contract_intelligence", 7),
    ("generate_executive_report", "Generate Executive Report", "Run an incident investigation to produce executive quality reporting.", "reporting", 8),
)

_READINESS_LEVELS = (
    (100, "FULLY_OPERATIONAL"),
    (81, "READY"),
    (51, "PARTIALLY_READY"),
    (21, "EARLY_SETUP"),
    (0, "NOT_READY"),
)


def _readiness_level(completion: int) -> str:
    for threshold, label in _READINESS_LEVELS:
        if completion >= threshold:
            return label
    return "NOT_READY"


def _github_settings(project: Any) -> Dict[str, Any]:
    settings = getattr(project, "settings", None) or {}
    if not isinstance(settings, dict):
        return {}
    gh = settings.get("github")
    return gh if isinstance(gh, dict) else {}


def _azure_settings(project: Any) -> Dict[str, Any]:
    settings = getattr(project, "settings", None) or {}
    if not isinstance(settings, dict):
        return {}
    az = settings.get("azure_devops")
    return az if isinstance(az, dict) else {}


def _repository_state(project: Any) -> Tuple[str, int, str]:
    gh = _github_settings(project)
    az = _azure_settings(project)
    gh_owner = str(gh.get("owner") or "").strip()
    gh_repo = str(gh.get("repo") or "").strip()
    gh_install = str(gh.get("installation_id") or gh.get("github_installation_id") or "").strip()
    gh_enabled = bool(gh.get("enabled"))

    az_org = str(az.get("organization") or "").strip()
    az_project = str(az.get("azure_project") or "").strip()
    az_repo = str(az.get("repository_id") or "").strip()
    az_enabled = bool(az.get("enabled"))

    if gh_enabled and gh_owner and gh_repo:
        return "COMPLETED", 100, "Repository connected via GitHub."
    if az_enabled and az_org and az_project and az_repo:
        return "COMPLETED", 100, "Repository connected via Azure DevOps."
    if gh_enabled and gh_install:
        return "IN_PROGRESS", 50, "GitHub App linked — select a repository to finish setup."
    if az_enabled and az_org:
        return "IN_PROGRESS", 50, "Azure DevOps linked — select a repository to finish setup."
    return "NOT_STARTED", 0, "Connect GitHub or Azure DevOps from Integrations."


def _tests_state(project_id: str) -> Tuple[str, int, str]:
    from services.db.catalog_repository import catalog_repo

    cases = catalog_repo.list_test_cases(project_id=project_id, status="active", limit=5) or []
    total = catalog_repo.count_by_status_for_project(project_id).get("active", 0)
    if total > 0:
        return "COMPLETED", 100, f"{total} active test(s) in catalog."
    if cases:
        return "IN_PROGRESS", 50, "Tests detected — expand the catalog import."
    return "NOT_STARTED", 0, "Import or create tests in the catalog."


def _browser_state(project_id: str) -> Tuple[str, int, str]:
    from services.db.browser_inspection_watch_repository import browser_inspection_watch_repo

    watches = browser_inspection_watch_repo.list_watches(project_id=project_id, limit=5) or []
    if watches:
        return "COMPLETED", 100, f"{len(watches)} browser watch(es) configured."
    return "NOT_STARTED", 0, "Create a browser watch to monitor critical flows."


def _environments_state(project: Any) -> Tuple[str, int, str]:
    settings = getattr(project, "settings", None) or {}
    if not isinstance(settings, dict):
        settings = {}
    envs = settings.get("environments")
    metadata = settings.get("metadata") if isinstance(settings.get("metadata"), dict) else {}
    meta_envs = metadata.get("environments")
    candidates = envs if isinstance(envs, list) else meta_envs if isinstance(meta_envs, list) else []
    if len(candidates) >= 2:
        return "COMPLETED", 100, f"{len(candidates)} environment(s) configured."
    if len(candidates) == 1:
        return "IN_PROGRESS", 50, "One environment defined — add staging and production views."
    if getattr(project, "base_url", None):
        return "IN_PROGRESS", 40, "Base URL set — define named environments in project settings."
    return "NOT_STARTED", 0, "Define QA, staging, and production environments."


def _agents_state(project_id: str) -> Tuple[str, int, str]:
    from services.db.local_agent_repository import local_agent_repo

    agents = local_agent_repo.list_agents(project_id=project_id, limit=10) or []
    if not agents:
        return "NOT_STARTED", 0, "Register a local agent from Local Agents."
    online = sum(1 for a in agents if str(a.get("status") or "").lower() in ("online", "connected", "active"))
    if online > 0:
        return "COMPLETED", 100, f"{len(agents)} agent(s) registered ({online} online)."
    return "COMPLETED", 100, f"{len(agents)} agent(s) registered."


def _database_validation_state(project_id: str) -> Tuple[str, int, str]:
    from services.database_connector_service import is_real_monitored_asset
    from services.db.database_connector_repository import database_connector_repo
    from services.db.local_agent_repository import local_agent_repo

    agents = local_agent_repo.list_agents(project_id=project_id, limit=20) or []
    real_assets = 0
    validated_assets = 0
    for agent in agents:
        agent_id = str(agent.get("agent_id") or agent.get("id") or "").strip()
        if not agent_id:
            continue
        connections = database_connector_repo.list_connections(agent_id=agent_id, limit=20) or []
        for conn in connections:
            if not is_real_monitored_asset(conn):
                continue
            real_assets += 1
            if database_connector_repo.has_successful_execution(conn["connection_id"]):
                validated_assets += 1

    if real_assets > 0 and validated_assets > 0:
        return (
            "COMPLETED",
            100,
            f"{validated_assets} database asset(s) validated ({real_assets} registered).",
        )
    if real_assets > 0:
        return (
            "IN_PROGRESS",
            60,
            f"{real_assets} database asset(s) registered — awaiting successful read-only validation.",
        )
    if agents:
        return (
            "IN_PROGRESS",
            30,
            "Register platform assets or connect a customer database through a local agent.",
        )
    return (
        "NOT_STARTED",
        0,
        "Register platform assets or a local agent, then add database validation connectors.",
    )


def _incident_intelligence_flags(project_id: str) -> Dict[str, bool]:
    from services.db.incident_report_repository import incident_report_repo

    flags = {
        "has_executive_report": False,
        "has_contract_intelligence": False,
        "has_quality_health": False,
    }
    rows = incident_report_repo.list_reports(project_id=project_id, limit=25)
    for row in rows:
        report_id = str(row.get("id") or "").strip()
        if not report_id:
            continue
        full = incident_report_repo.get(report_id)
        if not full:
            continue
        if full.get("executive_quality_report"):
            flags["has_executive_report"] = True
        if full.get("quality_health"):
            flags["has_quality_health"] = True
        api_contracts = full.get("api_contract_intelligence")
        contract_risk = full.get("contract_risk_assessment")
        if api_contracts or contract_risk:
            flags["has_contract_intelligence"] = True
        if all(flags.values()):
            break
    return flags


def _knowledge_has_apis(project_id: str) -> bool:
    try:
        from services.project_knowledge_service import get_project_knowledge

        knowledge = get_project_knowledge(project_id)
        apis = getattr(knowledge, "apis", None) or []
        return len(apis) > 0
    except Exception:
        return False


def _contract_intelligence_state(project_id: str) -> Tuple[str, int, str]:
    flags = _incident_intelligence_flags(project_id)
    if flags["has_contract_intelligence"]:
        return "COMPLETED", 100, "Contract intelligence generated from investigations."
    if _knowledge_has_apis(project_id):
        return "IN_PROGRESS", 70, "API knowledge detected. Run an investigation to generate contract intelligence."
    return (
        "NOT_STARTED",
        0,
        "No API contract intelligence has been generated yet. "
        "Run an investigation involving APIs, contract changes, or OpenAPI evidence.",
    )


def _executive_report_state(project_id: str) -> Tuple[str, int, str]:
    flags = _incident_intelligence_flags(project_id)
    if flags["has_executive_report"] or flags["has_quality_health"]:
        return "COMPLETED", 100, "Executive quality reporting available."
    if flags["has_contract_intelligence"]:
        return "IN_PROGRESS", 50, "Investigation data exists — generate an executive quality report."
    return "NOT_STARTED", 0, "Run an incident investigation to generate executive reporting."


def _step(
    step_id: str,
    title: str,
    description: str,
    category: str,
    priority: int,
    status: str,
    completion_percentage: int,
    action: str,
) -> OnboardingStep:
    return OnboardingStep(
        step_id=step_id,
        title=title,
        description=description,
        category=category,
        status=status,
        priority=priority,
        completion_percentage=completion_percentage,
        recommended_next_action=action,
    )


def build_onboarding_checklist(project_id: str) -> OnboardingChecklist:
    pid = (project_id or "").strip().lower()
    if not pid:
        raise ValueError("project_id is required")

    from services.db.project_repository import project_repo

    project = project_repo.get_project(pid)
    if project is None:
        raise LookupError(f"project not found: {pid}")

    try:
        from services.platform_asset_bootstrap_service import bootstrap_platform_assets

        bootstrap_platform_assets(pid)
    except Exception:
        logger.debug("platform asset bootstrap skipped for project=%s", pid, exc_info=True)

    evaluators = {
        "connect_repository": lambda: _repository_state(project),
        "import_tests": lambda: _tests_state(pid),
        "configure_browser_monitoring": lambda: _browser_state(pid),
        "configure_environments": lambda: _environments_state(project),
        "configure_local_agent": lambda: _agents_state(pid),
        "configure_database_validation": lambda: _database_validation_state(pid),
        "configure_contract_intelligence": lambda: _contract_intelligence_state(pid),
        "generate_executive_report": lambda: _executive_report_state(pid),
    }

    steps: List[OnboardingStep] = []
    for step_id, title, description, category, priority in _DEFAULT_STEPS:
        status, completion, action = evaluators[step_id]()
        steps.append(
            _step(
                step_id,
                title,
                description,
                category,
                priority,
                status,
                completion,
                action,
            )
        )

    completed_steps = sum(1 for s in steps if s.status == "COMPLETED")
    total_steps = len(steps)
    overall_completion = round(sum(s.completion_percentage for s in steps) / total_steps) if total_steps else 0
    readiness_level = _readiness_level(overall_completion)

    next_step = ""
    for step in sorted(steps, key=lambda s: s.priority):
        if step.status != "COMPLETED":
            next_step = step.title
            break

    return OnboardingChecklist(
        project_id=pid,
        overall_completion=overall_completion,
        readiness_level=readiness_level,
        completed_steps=completed_steps,
        total_steps=total_steps,
        next_recommended_step=next_step or "None",
        steps=steps,
    )
