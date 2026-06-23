/** View helpers for Release Readiness compositor (REL-01A). */

import { localizeBackendMessages } from "./localizeBackendMessage.js";
import { buildDecisionCenterViewModel } from "./qualityDecisionCenterViewUtils.js";
import { buildJiraIssueIntelligenceViewModel } from "./jiraIssueIntelligenceViewUtils.js";
import { buildMultiEnvironmentIntelligenceViewModel } from "./environmentIntelligenceViewUtils.js";
import { buildQualityHealthScoreViewModel } from "./qualityHealthScoreViewUtils.js";
import { riskLevelBadgeClass } from "./scheduledReportViewUtils.js";
import {
  buildReleaseReadinessTraceViewModel,
  RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS,
  shouldShowReleaseReadinessTrace,
} from "./releaseReadinessExplainabilityViewUtils.js";

export { RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS };

export const RELEASE_READINESS_I18N_KEYS = {
  title: "release_readiness.title",
  empty: "release_readiness.empty",
  overallStatus: "release_readiness.overall_status",
  summary: "release_readiness.summary",
  dataGaps: "release_readiness.data_gaps",
  scmConnections: "release_readiness.scm_connections",
  integrationReadiness: "release_readiness.integration_readiness",
  github: "release_readiness.github",
  azureDevops: "release_readiness.azure_devops",
  connected: "release_readiness.connected",
  disconnected: "release_readiness.disconnected",
  readOnlyNote: "release_readiness.read_only_note",
  statusGo: "release_readiness.status_go",
  statusCaution: "release_readiness.status_caution",
  statusBlocked: "release_readiness.status_blocked",
  statusUnknown: "release_readiness.status_unknown",
  deploymentRisk: "release_readiness.deployment_risk",
  qualityHealth: "release_readiness.quality_health",
  environments: "release_readiness.environments",
  decisionCenter: "release_readiness.decision_center",
  jiraBlockers: "release_readiness.jira_blockers",
  jiraBlockersEmpty: "release_readiness.jira_blockers_empty",
};

const OVERALL_STATUS_LABEL_KEY = {
  GO: RELEASE_READINESS_I18N_KEYS.statusGo,
  CAUTION: RELEASE_READINESS_I18N_KEYS.statusCaution,
  BLOCKED: RELEASE_READINESS_I18N_KEYS.statusBlocked,
  UNKNOWN: RELEASE_READINESS_I18N_KEYS.statusUnknown,
};

const OVERALL_STATUS_BADGE = {
  GO: "badge badge-green",
  CAUTION: "badge badge-orange",
  BLOCKED: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

export function overallStatusLabelKey(status) {
  return OVERALL_STATUS_LABEL_KEY[String(status || "UNKNOWN").toUpperCase()]
    || RELEASE_READINESS_I18N_KEYS.statusUnknown;
}

export function overallStatusBadgeClass(status) {
  return OVERALL_STATUS_BADGE[String(status || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

function scmLabel(provider, status, t) {
  if (!status) {
    return { provider, label: t(RELEASE_READINESS_I18N_KEYS.disconnected), badgeClass: "badge badge-gray" };
  }
  const connected = Boolean(status.connected && status.enabled);
  const hasTarget = provider === "github"
    ? Boolean(String(status.full_name || "").trim())
    : Boolean(String(status.repository_id || "").trim());
  const ok = connected && hasTarget;
  return {
    provider,
    label: ok ? (status.full_name || status.repository_name || t(RELEASE_READINESS_I18N_KEYS.connected))
      : t(RELEASE_READINESS_I18N_KEYS.disconnected),
    badgeClass: ok ? "badge badge-green" : "badge badge-orange",
    detail: status.validation_message || "",
  };
}

function mapIntegrationReadiness(readiness, t) {
  return Object.entries(readiness || {}).map(([connectorId, item]) => {
    const ready = Boolean(item?.ready);
    return {
      connector_id: connectorId,
      label: connectorId,
      ready,
      badgeClass: ready ? "badge badge-green" : "badge badge-gray",
      health: item?.health || "unknown",
      message: item?.message || "",
    };
  });
}

export function hasReleaseReadinessSection(payload) {
  return payload?.release_readiness != null;
}

export function isReleaseReadinessEmpty(payload) {
  const view = payload?.release_readiness;
  if (!view) return true;
  const hasIntel = Boolean(
    view.deployment_risk_assessment
    || view.contract_risk_assessment
    || view.data_journey_validation
    || view.enterprise_dependency_map
    || view.multi_environment
    || view.quality_health
    || view.quality_trends
    || view.early_degradation
    || view.executive_quality_report
    || view.decision_center
    || view.jira_issue_intelligence,
  );
  return !hasIntel && !(view.data_gaps || []).length;
}

export function buildReleaseReadinessViewModel(payload, t) {
  const view = payload?.release_readiness ?? payload ?? null;
  const empty = !view || isReleaseReadinessEmpty({ release_readiness: view });
  const reportLike = view || {};

  const decisionCenterVm = buildDecisionCenterViewModel(reportLike, t);
  const qualityHealthVm = buildQualityHealthScoreViewModel(reportLike, t);
  const multiEnvironmentVm = buildMultiEnvironmentIntelligenceViewModel(reportLike, t);
  const jiraIntelVm = buildJiraIssueIntelligenceViewModel(reportLike, t);
  const compactJiraBlockers = (jiraIntelVm.topBlockers || []).slice(0, 3);
  const showJiraBlockers = jiraIntelVm.show && !jiraIntelVm.empty && jiraIntelVm.blockerCount > 0;

  const deploymentRisk = view?.deployment_risk_assessment ?? null;
  const overallStatus = String(view?.overall_status || "UNKNOWN").toUpperCase();

  const baseVm = {
    show: view != null,
    empty,
    emptyMessage: t(RELEASE_READINESS_I18N_KEYS.empty),
    title: t(RELEASE_READINESS_I18N_KEYS.title),
    readOnlyNote: t(RELEASE_READINESS_I18N_KEYS.readOnlyNote),
    overallStatus,
    overallStatusLabel: t(RELEASE_READINESS_I18N_KEYS.overallStatus),
    overallStatusText: t(overallStatusLabelKey(overallStatus)),
    overallStatusBadgeClass: overallStatusBadgeClass(overallStatus),
    summaryLabel: t(RELEASE_READINESS_I18N_KEYS.summary),
    summary: view?.summary || "",
    dataGapsLabel: t(RELEASE_READINESS_I18N_KEYS.dataGaps),
    data_gaps: localizeBackendMessages(view?.data_gaps || [], t),
    scmConnectionsLabel: t(RELEASE_READINESS_I18N_KEYS.scmConnections),
    scm: [
      {
        ...scmLabel("github", view?.github, t),
        title: t(RELEASE_READINESS_I18N_KEYS.github),
      },
      {
        ...scmLabel("azure_devops", view?.azure_devops, t),
        title: t(RELEASE_READINESS_I18N_KEYS.azureDevops),
      },
    ],
    integrationReadinessLabel: t(RELEASE_READINESS_I18N_KEYS.integrationReadiness),
    integrations: mapIntegrationReadiness(view?.integration_readiness, t),
    deploymentRiskLabel: t(RELEASE_READINESS_I18N_KEYS.deploymentRisk),
    deploymentRisk,
    deploymentRiskBadgeClass: deploymentRisk
      ? riskLevelBadgeClass(deploymentRisk.risk_level)
      : "badge badge-gray",
    decisionCenterVm,
    qualityHealthVm,
    multiEnvironmentVm,
    jiraIntelVm,
    showJiraBlockers,
    jiraBlockersLabel: t(RELEASE_READINESS_I18N_KEYS.jiraBlockers),
    jiraBlockersEmptyMessage: t(RELEASE_READINESS_I18N_KEYS.jiraBlockersEmpty),
    compactJiraBlockers,
    generated_at: view?.generated_at || null,
    source_incident_report_id: view?.source_incident_report_id || null,
  };

  const trace = buildReleaseReadinessTraceViewModel(baseVm, view, t);

  return {
    ...baseVm,
    trace,
    showTrace: shouldShowReleaseReadinessTrace(baseVm),
    traceToggleShowLabel: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.whyTitle),
    traceToggleHideLabel: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.toggleHide),
  };
}
