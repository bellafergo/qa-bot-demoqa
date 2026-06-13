/** View helpers for Jira Issue Intelligence (JIRA-01B). */

import {
  attachCapabilityPresentation,
  resolveJiraCapabilityState,
} from "./capabilityStateViewUtils.js";

export const JIRA_ISSUE_INTELLIGENCE_I18N_KEYS = {
  title: "incident.qa.jira_issue_intelligence",
  correlatedIssues: "incident.qa.jira_correlated_issues",
  blockers: "incident.qa.jira_blockers",
  highPriority: "incident.qa.jira_high_priority",
  topBlockers: "incident.qa.jira_top_blockers",
  correlatedList: "incident.qa.jira_correlated_list",
  correlationScore: "incident.qa.jira_correlation_score",
  correlationReason: "incident.qa.jira_correlation_reason",
  relatedModule: "incident.qa.jira_related_module",
  relatedEnvironment: "incident.qa.jira_related_environment",
  emptyConnection: "incident.qa.jira_empty_connection",
  emptyIssues: "incident.qa.jira_empty_issues",
  emptyCorrelations: "incident.qa.jira_empty_correlations",
  readOnlyNote: "incident.qa.jira_read_only_note",
  summary: "incident.qa.summary",
};

export function hasJiraIssueIntelligenceSection(report) {
  return report?.jira_issue_intelligence != null;
}

export function isJiraIssueIntelligenceEmpty(report) {
  const intel = report?.jira_issue_intelligence;
  if (!intel) return true;
  if (!intel.connected) return true;
  return intel.total_issues === 0 && intel.correlated_issues === 0;
}

export function buildCorrelationCardViewModel(correlation, t) {
  if (!correlation?.issue_key) return null;
  return {
    issueKey: correlation.issue_key,
    summary: correlation.summary || "—",
    issueType: correlation.issue_type || "—",
    status: correlation.status || "—",
    priority: correlation.priority || "—",
    score: correlation.correlation_score ?? 0,
    scoreLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.correlationScore),
    reasonLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.correlationReason),
    reason: correlation.correlation_reason || "—",
    relatedModule: correlation.related_module,
    relatedEnvironment: correlation.related_environment,
    relatedModuleLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.relatedModule),
    relatedEnvironmentLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.relatedEnvironment),
    isBlocker: Boolean(correlation.is_blocker),
    blockerBadgeClass: correlation.is_blocker ? "badge badge-red" : "badge badge-gray",
  };
}

export function buildJiraIssueIntelligenceViewModel(report, t) {
  const intel = report?.jira_issue_intelligence ?? null;
  const show = hasJiraIssueIntelligenceSection(report);
  const connected = Boolean(intel?.connected);
  const correlations = (intel?.issue_correlations || [])
    .map((c) => buildCorrelationCardViewModel(c, t))
    .filter(Boolean);
  const topBlockers = (intel?.top_blockers || [])
    .map((c) => buildCorrelationCardViewModel(c, t))
    .filter(Boolean);

  const capabilityState = resolveJiraCapabilityState({
    connected,
    totalIssues: intel?.total_issues ?? 0,
    correlatedIssues: intel?.correlated_issues ?? 0,
    title: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.title),
    t,
  });

  return attachCapabilityPresentation({
    show,
    title: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.title),
    summary: intel?.summary || "",
    summaryLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.summary),
    readOnlyNote: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.readOnlyNote),
    correlatedIssuesLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.correlatedIssues),
    blockersLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.blockers),
    highPriorityLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.highPriority),
    topBlockersLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.topBlockers),
    correlatedListLabel: t(JIRA_ISSUE_INTELLIGENCE_I18N_KEYS.correlatedList),
    correlatedIssues: intel?.correlated_issues ?? 0,
    blockerCount: intel?.blocker_count ?? 0,
    highPriorityCount: intel?.high_priority_count ?? 0,
    correlations,
    topBlockers,
    showCorrelations: connected && correlations.length > 0,
    showTopBlockers: connected && topBlockers.length > 0,
  }, capabilityState);
}
