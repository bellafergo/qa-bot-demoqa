/** View helpers for Quality Decision Center (II-05C). */

import {
  buildInsightTraceViewModel,
  isCriticalOrHighInsight,
} from "./incidentEvidenceTraceabilityViewUtils.js";

export const DECISION_CENTER_I18N_KEYS = {
  title: "incident.qa.decision_center",
  empty: "incident.qa.decision_center_empty",
  overallStatus: "incident.qa.decision_center_overall_status",
  executiveSummary: "incident.qa.decision_center_executive_summary",
  confidence: "incident.qa.decision_center_confidence",
  topRiskLevel: "incident.qa.decision_center_top_risk_level",
  topRiskScore: "incident.qa.decision_center_top_risk_score",
  topHypothesis: "incident.qa.decision_center_top_hypothesis",
  topImpactedArea: "incident.qa.decision_center_top_impacted_area",
  recommendedTests: "incident.qa.decision_center_recommended_tests",
  recommendedActions: "incident.qa.decision_center_recommended_actions",
  keyTakeaways: "incident.qa.decision_center_key_takeaways",
  statusGreen: "incident.qa.decision_center_status_green",
  statusYellow: "incident.qa.decision_center_status_yellow",
  statusOrange: "incident.qa.decision_center_status_orange",
  statusRed: "incident.qa.decision_center_status_red",
  readOnlyNote: "incident.qa.decision_center_read_only_note",
};

export function hasDecisionCenterSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "decision_center");
}

export function isDecisionCenterEmpty(report) {
  return report?.decision_center == null;
}

export function getOverallStatusLabelKey(status) {
  const v = String(status || "GREEN").toUpperCase();
  if (v === "RED") return DECISION_CENTER_I18N_KEYS.statusRed;
  if (v === "ORANGE") return DECISION_CENTER_I18N_KEYS.statusOrange;
  if (v === "YELLOW") return DECISION_CENTER_I18N_KEYS.statusYellow;
  return DECISION_CENTER_I18N_KEYS.statusGreen;
}

export function getOverallStatusBadgeClass(status) {
  const v = String(status || "GREEN").toUpperCase();
  if (v === "RED") return "badge badge-red";
  if (v === "ORANGE") return "badge badge-orange";
  if (v === "YELLOW") return "badge badge-orange";
  return "badge badge-gray";
}

export function formatDecisionCenterConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function buildDecisionCenterDrilldownItem(insight) {
  if (!insight?.related_entity_type || !insight?.related_entity_id) return null;
  const et = String(insight.related_entity_type);
  if (et === "hypothesis") return null;
  return {
    source: et,
    related_entity_type: et,
    related_entity_id: insight.related_entity_id,
    reason: insight.description,
    detail: insight.title,
    title: insight.title,
  };
}

export function buildDecisionCenterViewModel(report, t) {
  const center = report?.decision_center ?? null;
  return {
    show: hasDecisionCenterSection(report),
    empty: isDecisionCenterEmpty(report),
    emptyMessage: t(DECISION_CENTER_I18N_KEYS.empty),
    title: t(DECISION_CENTER_I18N_KEYS.title),
    overallStatusLabel: t(DECISION_CENTER_I18N_KEYS.overallStatus),
    executiveSummaryLabel: t(DECISION_CENTER_I18N_KEYS.executiveSummary),
    confidenceLabel: t(DECISION_CENTER_I18N_KEYS.confidence),
    topRiskLevelLabel: t(DECISION_CENTER_I18N_KEYS.topRiskLevel),
    topRiskScoreLabel: t(DECISION_CENTER_I18N_KEYS.topRiskScore),
    topHypothesisLabel: t(DECISION_CENTER_I18N_KEYS.topHypothesis),
    topImpactedAreaLabel: t(DECISION_CENTER_I18N_KEYS.topImpactedArea),
    recommendedTestsLabel: t(DECISION_CENTER_I18N_KEYS.recommendedTests),
    recommendedActionsLabel: t(DECISION_CENTER_I18N_KEYS.recommendedActions),
    keyTakeawaysLabel: t(DECISION_CENTER_I18N_KEYS.keyTakeaways),
    readOnlyNote: t(DECISION_CENTER_I18N_KEYS.readOnlyNote),
    center: center
      ? {
          ...center,
          statusLabel: t(getOverallStatusLabelKey(center.overall_status)),
          statusBadgeClass: getOverallStatusBadgeClass(center.overall_status),
          confidenceText: formatDecisionCenterConfidence(center.confidence),
          insightText: `Decision Center status is ${String(center.overall_status || "GREEN").toUpperCase()}`,
          trace: buildInsightTraceViewModel(
            report,
            `Decision Center status is ${String(center.overall_status || "GREEN").toUpperCase()}`,
            t,
          ),
          showTrace: isCriticalOrHighInsight(
            `Decision Center status is ${String(center.overall_status || "GREEN").toUpperCase()}`,
          ),
          takeaways: (center.key_takeaways || []).map((insight) => ({
            ...insight,
            drilldownItem: buildDecisionCenterDrilldownItem(insight),
          })),
        }
      : null,
  };
}
