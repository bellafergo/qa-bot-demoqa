# services/jira_issue_intelligence_service.py
"""
JIRA-01B — Read-only Jira issue intelligence (deterministic correlation).

Correlates discovered Jira issues with existing Vanya intelligence slices.
No LLMs, embeddings, vector search, or Jira writes.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Set, Tuple

from models.incident_models import ProjectIncidentInvestigationReport
from models.jira_issue_intelligence_models import (
    JiraIssueCorrelation,
    JiraIssueIntelligenceReport,
)
from models.jira_models import JiraIssue
from models.release_readiness_models import ReleaseReadinessView
from services.jira_integration_service import (
    _issues_jql,
    _resolve_http_config,
    validate_jira_connection,
)
from services.jira_repository_service import JiraAPIError, parse_issue, search_issues

logger = logging.getLogger("vanya.jira_issue_intelligence")

_MODULE_KEYWORDS = (
    "Checkout",
    "Payments",
    "Authentication",
    "Candidates",
    "Orders",
    "Inventory",
)
_ENV_ALIASES: Dict[str, Tuple[str, ...]] = {
    "qa": ("qa", "quality assurance"),
    "staging": ("staging", "stage"),
    "prod": ("prod",),
    "production": ("production", "prod"),
}
_RESOLVED_STATUSES = frozenset({
    "done",
    "closed",
    "resolved",
    "complete",
    "completed",
    "cancelled",
    "canceled",
    "won't do",
    "wont do",
})
_HIGH_PRIORITIES = frozenset({"high", "highest", "blocker"})


def _empty_report(*, connected: bool = False, gap: Optional[str] = None) -> JiraIssueIntelligenceReport:
    gaps = [gap] if gap else []
    return JiraIssueIntelligenceReport(connected=connected, data_gaps=gaps)


def _plain_text_from_description(value: Any) -> str:
    if not value:
        return ""
    if isinstance(value, str):
        return value
    if isinstance(value, dict):
        parts: List[str] = []
        for block in value.get("content") or []:
            if not isinstance(block, dict):
                continue
            for item in block.get("content") or []:
                if isinstance(item, dict) and item.get("type") == "text":
                    parts.append(str(item.get("text") or ""))
        return " ".join(parts)
    return str(value)


def _fetch_issues(*, project_key: Optional[str], max_results: int = 100) -> Tuple[List[Dict[str, Any]], int]:
    http = _resolve_http_config()
    if http is None:
        return [], 0
    jql = _issues_jql(project_key or http.project_key)
    raw, total = search_issues(
        http,
        jql=jql,
        max_results=max_results,
        fields=["summary", "description", "issuetype", "status", "assignee", "priority"],
    )
    return raw, total


def _priority_weight(priority: Optional[str]) -> int:
    p = (priority or "").strip().lower()
    if p == "blocker":
        return 30
    if p == "highest":
        return 20
    if p == "high":
        return 10
    return 0


def _is_blocker(*, priority: Optional[str], status: Optional[str]) -> bool:
    p = (priority or "").strip().lower()
    s = (status or "").strip().lower()
    if p == "blocker":
        return True
    if p == "highest" and s not in _RESOLVED_STATUSES:
        return True
    return False


def _impacted_modules(incident_report: Optional[ProjectIncidentInvestigationReport]) -> List[str]:
    if incident_report is None:
        return []
    modules: List[str] = []
    for name in incident_report.impacted_modules or []:
        if name:
            modules.append(str(name))
    for item in incident_report.impacted_modules_ranked or []:
        if item.module:
            modules.append(str(item.module))
    seen: Set[str] = set()
    ordered: List[str] = []
    for mod in modules:
        key = mod.strip().lower()
        if key and key not in seen:
            seen.add(key)
            ordered.append(mod.strip())
    return ordered


def _degraded_environments(incident_report: Optional[ProjectIncidentInvestigationReport]) -> List[str]:
    if incident_report is None or incident_report.multi_environment is None:
        return []
    names: List[str] = []
    for env in incident_report.multi_environment.environments or []:
        if str(env.status or "").upper() in {"DEGRADED", "BROKEN"} and env.name:
            names.append(str(env.name))
    return names


def _release_risk_active(
    incident_report: Optional[ProjectIncidentInvestigationReport],
    release_readiness: Optional[ReleaseReadinessView],
) -> bool:
    if release_readiness is not None:
        status = str(release_readiness.overall_status or "").upper()
        if status in {"BLOCKED", "CAUTION"}:
            return True
    if incident_report and incident_report.deployment_risk_assessment is not None:
        level = str(incident_report.deployment_risk_assessment.risk_level or "").lower()
        if level in {"high", "critical"}:
            return True
    if incident_report and incident_report.decision_center is not None:
        status = str(incident_report.decision_center.overall_status or "").upper()
        if status in {"ORANGE", "RED"}:
            return True
    return False


def _module_match(
    text: str,
    impacted_modules: List[str],
) -> Tuple[int, Optional[str], Optional[str]]:
    if not impacted_modules:
        return 0, None, None
    text_l = text.lower()
    for mod in impacted_modules:
        mod_l = mod.lower()
        if mod_l and mod_l in text_l:
            return 25, mod, f"Impacted module match: {mod} (+25)"
    for keyword in _MODULE_KEYWORDS:
        kw_l = keyword.lower()
        if kw_l not in text_l:
            continue
        for mod in impacted_modules:
            mod_l = mod.lower()
            if kw_l in mod_l or mod_l in kw_l or mod_l == kw_l:
                return 25, mod, f"Module keyword match: {keyword} → {mod} (+25)"
    return 0, None, None


def _environment_match(
    text: str,
    degraded_environments: List[str],
) -> Tuple[int, Optional[str], Optional[str]]:
    if not degraded_environments:
        return 0, None, None
    text_l = text.lower()
    for env in degraded_environments:
        env_l = env.lower()
        if env_l and env_l in text_l:
            return 20, env, f"Degraded environment match: {env} (+20)"
    for env in degraded_environments:
        env_l = env.lower()
        for aliases in _ENV_ALIASES.values():
            if any(alias in text_l for alias in aliases) and any(alias in env_l for alias in aliases):
                return 20, env, f"Environment keyword match → {env} (+20)"
    return 0, None, None


def _correlate_issue(
    issue: JiraIssue,
    *,
    description: str,
    impacted_modules: List[str],
    degraded_environments: List[str],
    release_risk: bool,
) -> JiraIssueCorrelation:
    text = f"{issue.summary} {description}".strip()
    reasons: List[str] = []
    score = 0

    pw = _priority_weight(issue.priority)
    if pw:
        score += pw
        reasons.append(f"Priority weight (+{pw})")

    mod_score, related_module, mod_reason = _module_match(text, impacted_modules)
    if mod_score:
        score += mod_score
        reasons.append(mod_reason or "")

    env_score, related_environment, env_reason = _environment_match(text, degraded_environments)
    if env_score:
        score += env_score
        reasons.append(env_reason or "")

    if release_risk and score > 0:
        score += 15
        reasons.append("Release risk context (+15)")

    score = min(score, 100)
    blocker = _is_blocker(priority=issue.priority, status=issue.status)

    return JiraIssueCorrelation(
        issue_key=issue.issue_key,
        issue_type=issue.issue_type,
        status=issue.status,
        priority=issue.priority,
        summary=issue.summary,
        correlation_score=score,
        correlation_reason="; ".join(r for r in reasons if r),
        related_module=related_module,
        related_environment=related_environment,
        is_blocker=blocker,
    )


def build_jira_issue_intelligence_report(
    *,
    project_key: Optional[str] = None,
    incident_report: Optional[ProjectIncidentInvestigationReport] = None,
    release_readiness: Optional[ReleaseReadinessView] = None,
    max_issues: int = 100,
) -> JiraIssueIntelligenceReport:
    """Build deterministic Jira issue intelligence from discovery + stored intelligence."""
    connection = validate_jira_connection()
    if not connection.connected:
        return _empty_report(gap="No Jira connection configured.")

    try:
        raw_issues, total = _fetch_issues(project_key=project_key, max_results=max_issues)
    except JiraAPIError as exc:
        logger.warning("jira issue intelligence fetch failed: %s", exc)
        return _empty_report(connected=True, gap="Jira issues could not be loaded.")

    if not raw_issues:
        return JiraIssueIntelligenceReport(
            connected=True,
            total_issues=total,
            summary="Jira connected but no issues were discovered for correlation.",
            data_gaps=["No issues discovered."],
        )

    impacted_modules = _impacted_modules(incident_report)
    degraded_environments = _degraded_environments(incident_report)
    release_risk = _release_risk_active(incident_report, release_readiness)

    correlations: List[JiraIssueCorrelation] = []
    for raw in raw_issues:
        parsed = parse_issue(raw)
        issue = JiraIssue(**parsed)
        description = _plain_text_from_description((raw.get("fields") or {}).get("description"))
        correlations.append(
            _correlate_issue(
                issue,
                description=description,
                impacted_modules=impacted_modules,
                degraded_environments=degraded_environments,
                release_risk=release_risk,
            )
        )

    correlated = [c for c in correlations if c.correlation_score > 0]
    correlated.sort(key=lambda c: (-c.correlation_score, c.issue_key))

    blockers = [c for c in correlations if c.is_blocker]
    blockers.sort(key=lambda c: (-c.correlation_score, c.issue_key))

    high_priority_count = sum(
        1 for c in correlations if (c.priority or "").strip().lower() in _HIGH_PRIORITIES
    )

    if not correlated:
        summary = (
            f"{total} Jira issue(s) discovered; no deterministic correlations to current intelligence."
        )
        gaps = ["No correlated issues."]
        if not impacted_modules and not degraded_environments:
            gaps.append("No impacted modules or degraded environments in intelligence context.")
    else:
        summary = (
            f"{len(correlated)} of {total} Jira issue(s) correlate with incident intelligence; "
            f"{len(blockers)} blocker(s) detected."
        )
        gaps = []

    return JiraIssueIntelligenceReport(
        connected=True,
        total_issues=total,
        correlated_issues=len(correlated),
        blocker_count=len(blockers),
        high_priority_count=high_priority_count,
        top_blockers=blockers[:5],
        issue_correlations=correlated,
        summary=summary,
        data_gaps=gaps,
    )


def jira_executive_risk_lines(
    intel: Optional[JiraIssueIntelligenceReport],
    *,
    limit: int = 3,
) -> List[str]:
    """Convert stored Jira intelligence into executive-friendly risk lines (no scoring)."""
    if intel is None or not intel.connected or intel.blocker_count <= 0:
        return []

    lines: List[str] = []
    for blocker in (intel.top_blockers or [])[:limit]:
        key = str(blocker.issue_key or "").strip()
        if not key:
            continue
        module = str(blocker.related_module or "").strip()
        module_part = f" ({module})" if module else ""
        summary = str(blocker.summary or "").strip()
        if summary:
            text = f"Jira blocker {key}{module_part}: {summary}"
        else:
            text = f"Jira blocker {key}{module_part}"
        if text not in lines:
            lines.append(text)
    return lines


def enrich_executive_quality_top_risks_with_jira(
    executive_quality_report: Optional["ExecutiveQualityReport"],
    jira_intel: Optional[JiraIssueIntelligenceReport],
    *,
    risk_cap: int = 5,
) -> None:
    """Post-enrich executive quality top_risks with Jira blockers (mutates in place)."""
    if executive_quality_report is None:
        return
    lines = jira_executive_risk_lines(jira_intel)
    if not lines:
        return
    merged = list(lines)
    for line in list(executive_quality_report.top_risks or []):
        if line not in merged:
            merged.append(line)
    executive_quality_report.top_risks = merged[:risk_cap]
