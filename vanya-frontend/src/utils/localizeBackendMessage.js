/**
 * Maps known backend English strings to i18n keys (frontend-only localization layer).
 * Unmatched messages are returned as-is with an optional system-origin marker in UI.
 */

export const BACKEND_MESSAGE_I18N_KEYS = {
  noFailedTestRuns: "backend_msg.no_failed_test_runs",
  noFailureClusters: "backend_msg.no_failure_clusters",
  memoryNotAvailable: "backend_msg.memory_not_available",
  noMatchingPrs: "backend_msg.no_matching_prs",
  noIncidentReport: "backend_msg.no_incident_report",
  deploymentRiskUnavailable: "backend_msg.deployment_risk_unavailable",
  contractRiskUnavailable: "backend_msg.contract_risk_unavailable",
  dataJourneyUnavailable: "backend_msg.data_journey_unavailable",
  enterpriseDependencyUnavailable: "backend_msg.enterprise_dependency_unavailable",
  multiEnvironmentUnavailable: "backend_msg.multi_environment_unavailable",
  qualityHealthUnavailable: "backend_msg.quality_health_unavailable",
  qualityTrendsUnavailable: "backend_msg.quality_trends_unavailable",
  earlyDegradationUnavailable: "backend_msg.early_degradation_unavailable",
  executiveQualityUnavailable: "backend_msg.executive_quality_unavailable",
  decisionCenterUnavailable: "backend_msg.decision_center_unavailable",
  promotionReadinessEmpty: "backend_msg.promotion_readiness_empty",
  jiraNotConnected: "backend_msg.jira_not_connected",
  repositoryNotConnected: "backend_msg.repository_not_connected",
  qualityTrendsUnavailableShort: "backend_msg.quality_trends_unavailable_short",
  reportEphemeral: "backend_msg.report_ephemeral",
  browserProbeFailed: "backend_msg.browser_probe_failed",
  noCoverageForRecommendations: "backend_msg.no_coverage_for_recommendations",
};

const EXACT_MESSAGE_KEYS = {
  "No failure intelligence clusters available.": BACKEND_MESSAGE_I18N_KEYS.noFailureClusters,
  "System Memory (project knowledge) not available — refresh knowledge first.": BACKEND_MESSAGE_I18N_KEYS.memoryNotAvailable,
  "No matching open PRs from connected GitHub/Azure DevOps (or SCM not connected).": BACKEND_MESSAGE_I18N_KEYS.noMatchingPrs,
  "No incident investigation report — run Incident Investigator for full release intelligence.": BACKEND_MESSAGE_I18N_KEYS.noIncidentReport,
  "Deployment risk assessment unavailable.": BACKEND_MESSAGE_I18N_KEYS.deploymentRiskUnavailable,
  "Contract risk assessment unavailable.": BACKEND_MESSAGE_I18N_KEYS.contractRiskUnavailable,
  "Data journey validation unavailable.": BACKEND_MESSAGE_I18N_KEYS.dataJourneyUnavailable,
  "Enterprise dependency map unavailable.": BACKEND_MESSAGE_I18N_KEYS.enterpriseDependencyUnavailable,
  "Multi-environment intelligence unavailable.": BACKEND_MESSAGE_I18N_KEYS.multiEnvironmentUnavailable,
  "Quality health report unavailable.": BACKEND_MESSAGE_I18N_KEYS.qualityHealthUnavailable,
  "Quality trend report unavailable.": BACKEND_MESSAGE_I18N_KEYS.qualityTrendsUnavailable,
  "Early degradation report unavailable.": BACKEND_MESSAGE_I18N_KEYS.earlyDegradationUnavailable,
  "Executive quality report unavailable.": BACKEND_MESSAGE_I18N_KEYS.executiveQualityUnavailable,
  "Decision center summary unavailable.": BACKEND_MESSAGE_I18N_KEYS.decisionCenterUnavailable,
  "multi_environment.promotion_readiness empty — configure environments.": BACKEND_MESSAGE_I18N_KEYS.promotionReadinessEmpty,
  "Jira not connected — external blockers may be missing.": BACKEND_MESSAGE_I18N_KEYS.jiraNotConnected,
  "Repository not connected — GitHub and Azure DevOps both disconnected.": BACKEND_MESSAGE_I18N_KEYS.repositoryNotConnected,
  "quality_trends unavailable.": BACKEND_MESSAGE_I18N_KEYS.qualityTrendsUnavailableShort,
  "Report could not be persisted — results are ephemeral for this request.": BACKEND_MESSAGE_I18N_KEYS.reportEphemeral,
  "No coverage intelligence available for recommendation correlation.": BACKEND_MESSAGE_I18N_KEYS.noCoverageForRecommendations,
};

const PATTERN_MESSAGE_RESOLVERS = [
  {
    pattern: /^No failed test runs found in the last (\d+)h for this project\.$/,
    resolve: (match) => ({
      key: BACKEND_MESSAGE_I18N_KEYS.noFailedTestRuns,
      params: { hours: match[1] },
    }),
  },
  {
    pattern: /^Browser probe failed: (.+)$/,
    resolve: (match) => ({
      key: BACKEND_MESSAGE_I18N_KEYS.browserProbeFailed,
      params: { error: match[1] },
    }),
  },
];

function resolveMessageKey(message) {
  const trimmed = String(message || "").trim();
  if (!trimmed) return null;

  const exact = EXACT_MESSAGE_KEYS[trimmed];
  if (exact) return { key: exact, params: {} };

  for (const { pattern, resolve } of PATTERN_MESSAGE_RESOLVERS) {
    const match = trimmed.match(pattern);
    if (match) return resolve(match);
  }

  return null;
}

/**
 * Localize a backend message when a known mapping exists; otherwise return original.
 */
export function localizeBackendMessage(message, t) {
  const resolved = resolveMessageKey(message);
  if (!resolved) return String(message || "").trim();
  return t(resolved.key, resolved.params);
}

export function localizeBackendMessages(messages, t) {
  if (!Array.isArray(messages)) return [];
  return messages
    .map((msg) => localizeBackendMessage(msg, t))
    .filter((msg) => String(msg || "").trim());
}
