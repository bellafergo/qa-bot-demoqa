/** View helpers for Test Recommendation Engine (II-05B). */

export const TEST_RECOMMENDATION_I18N_KEYS = {
  title: "incident.qa.test_recommendations",
  riskReduction: "incident.qa.test_recommendations_risk_reduction",
  approvalRequired: "incident.qa.test_recommendations_approval_required",
  preview: "incident.qa.test_recommendations_preview",
  empty: "incident.qa.test_recommendations_empty",
  priority: "incident.qa.test_recommendations_priority",
  confidence: "incident.qa.test_recommendations_confidence",
  reason: "incident.qa.test_recommendations_reason",
  previewSubtitle: "incident.qa.test_recommendations_preview_subtitle",
  previewObjective: "incident.qa.test_recommendations_preview_objective",
  previewFutureNote: "incident.qa.test_recommendations_preview_future_note",
};

const PREVIEW_OBJECTIVE_KEYS = {
  smoke: "incident.qa.test_recommendations_objective_smoke",
  regression: "incident.qa.test_recommendations_objective_regression",
  cluster_regression: "incident.qa.test_recommendations_objective_cluster_regression",
  ui_smoke: "incident.qa.test_recommendations_objective_ui_smoke",
  pr_validation: "incident.qa.test_recommendations_objective_pr_validation",
  hypothesis_validation: "incident.qa.test_recommendations_objective_hypothesis_validation",
  default: "incident.qa.test_recommendations_objective_default",
};

export function hasTestRecommendationsSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "test_recommendations");
}

export function isTestRecommendationsEmpty(report) {
  const tr = report?.test_recommendations;
  return !tr || !(tr.recommendations?.length > 0);
}

export function formatTestRecommendationPct(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function getPreviewObjectiveKey(testType) {
  return PREVIEW_OBJECTIVE_KEYS[String(testType || "")] || PREVIEW_OBJECTIVE_KEYS.default;
}

export function buildTestRecommendationDrilldownItem(rec) {
  if (!rec?.related_entity_type || !rec?.related_entity_id) return null;
  const et = String(rec.related_entity_type);
  if (et === "hypothesis") return null;
  return {
    source: et,
    related_entity_type: et,
    related_entity_id: rec.related_entity_id,
    reason: rec.reason,
    detail: rec.test_name,
    title: rec.test_name,
  };
}

export function buildTestRecommendationPreviewPayload(rec, t) {
  const objectiveKey = getPreviewObjectiveKey(rec?.test_type);
  return {
    title: rec?.test_name || t(TEST_RECOMMENDATION_I18N_KEYS.title),
    fields: [
      { label: t(TEST_RECOMMENDATION_I18N_KEYS.reason), value: rec?.reason || "—" },
      { label: t(TEST_RECOMMENDATION_I18N_KEYS.previewObjective), value: t(objectiveKey) },
      {
        label: t(TEST_RECOMMENDATION_I18N_KEYS.riskReduction),
        value: formatTestRecommendationPct(rec?.estimated_risk_reduction),
      },
      { label: t(TEST_RECOMMENDATION_I18N_KEYS.confidence), value: formatTestRecommendationPct(rec?.confidence) },
    ],
    futureNote: t(TEST_RECOMMENDATION_I18N_KEYS.previewFutureNote),
  };
}

export function buildTestRecommendationsViewModel(report, t) {
  const tr = report?.test_recommendations ?? null;
  const recs = Array.isArray(tr?.recommendations) ? tr.recommendations : [];
  return {
    show: hasTestRecommendationsSection(report),
    empty: isTestRecommendationsEmpty(report),
    emptyMessage: t(TEST_RECOMMENDATION_I18N_KEYS.empty),
    title: t(TEST_RECOMMENDATION_I18N_KEYS.title),
    approvalRequiredLabel: t(TEST_RECOMMENDATION_I18N_KEYS.approvalRequired),
    priorityLabel: t(TEST_RECOMMENDATION_I18N_KEYS.priority),
    confidenceLabel: t(TEST_RECOMMENDATION_I18N_KEYS.confidence),
    riskReductionLabel: t(TEST_RECOMMENDATION_I18N_KEYS.riskReduction),
    previewLabel: t(TEST_RECOMMENDATION_I18N_KEYS.preview),
    summary: tr?.summary || "",
    recommendationConfidenceText: formatTestRecommendationPct(tr?.recommendation_confidence),
    recommendations: recs.map((rec) => ({
      ...rec,
      confidenceText: formatTestRecommendationPct(rec.confidence),
      riskReductionText: formatTestRecommendationPct(rec.estimated_risk_reduction),
      drilldownItem: buildTestRecommendationDrilldownItem(rec),
      previewPayload: buildTestRecommendationPreviewPayload(rec, t),
    })),
  };
}
