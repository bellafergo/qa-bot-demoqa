/** Progressive disclosure layout for Incident Investigator report (UX only). */

export const REPORT_LAYOUT_I18N_KEYS = {
  executiveOverview: "incident.qa.layout.executive_overview",
  recommendedActions: "incident.qa.layout.recommended_actions",
  operationalAnalysis: "incident.qa.layout.operational_analysis",
  dependencyIntelligence: "incident.qa.layout.dependency_intelligence",
  technicalEvidence: "incident.qa.layout.technical_evidence",
  lowConfidenceWarning: "incident.qa.layout.low_confidence_warning",
  potentialInferredAreas: "incident.qa.layout.potential_inferred_areas",
  confirmedImpactedAreas: "incident.qa.layout.confirmed_impacted_areas",
  seeRecommendedActions: "incident.qa.layout.see_recommended_actions",
  allRecommendedActions: "incident.qa.layout.all_recommended_actions",
  additionalTestRecommendations: "incident.qa.layout.additional_test_recommendations",
  suggestedNextSteps: "incident.qa.layout.suggested_next_steps",
  confidenceTierLow: "incident.qa.confidence_tier.low",
  confidenceTierMedium: "incident.qa.confidence_tier.medium",
  confidenceTierHigh: "incident.qa.confidence_tier.high",
};

const CONFIDENCE_TIER_BADGE = {
  low: "badge badge-red",
  medium: "badge badge-orange",
  high: "badge badge-green",
};

const CONFIDENCE_TIER_LABEL_KEY = {
  low: REPORT_LAYOUT_I18N_KEYS.confidenceTierLow,
  medium: REPORT_LAYOUT_I18N_KEYS.confidenceTierMedium,
  high: REPORT_LAYOUT_I18N_KEYS.confidenceTierHigh,
};

export const CONFIDENCE_TIER_LOW_THRESHOLD = 0.4;
export const CONFIDENCE_TIER_HIGH_THRESHOLD = 0.8;

export const GENERIC_INFERRED_MODULE_TOKENS = new Set([
  "necesito",
  "general",
  "evaluar",
  "salud",
  "riesgos",
  "self",
  "operativa",
  "demostración",
  "demostracion",
  "vanya",
]);

export function normalizeInvestigationConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return 0;
  if (n > 1) return Math.min(n / 100, 1);
  return Math.max(0, Math.min(n, 1));
}

export function resolveConfidenceTier(confidence) {
  const value = normalizeInvestigationConfidence(confidence);
  if (value < CONFIDENCE_TIER_LOW_THRESHOLD) return "low";
  if (value < CONFIDENCE_TIER_HIGH_THRESHOLD) return "medium";
  return "high";
}

export function confidenceTierBadgeClass(tier) {
  return CONFIDENCE_TIER_BADGE[tier] || "badge badge-gray";
}

export function formatConfidencePercent(confidence) {
  const value = normalizeInvestigationConfidence(confidence);
  return `${Math.round(value * 100)}%`;
}

export function isGenericInferredModule(node) {
  const title = String(node?.title || node?.module || node?.name || "").trim().toLowerCase();
  if (!title) return true;
  if (GENERIC_INFERRED_MODULE_TOKENS.has(title)) return true;
  const confidence = Number(node?.confidence);
  if (Number.isFinite(confidence) && confidence < 0.35) return true;
  return false;
}

export function partitionImpactMapNodes(nodes = []) {
  const confirmed = [];
  const inferred = [];
  for (const node of nodes) {
    if (isGenericInferredModule(node)) inferred.push(node);
    else confirmed.push(node);
  }
  return { confirmed, inferred };
}

export function sliceTopItems(items = [], limit = 3) {
  const list = Array.isArray(items) ? items : [];
  return {
    top: list.slice(0, limit),
    remainder: list.slice(limit),
    hasRemainder: list.length > limit,
  };
}

export function buildQaInvestigationReportLayoutViewModel(report, t) {
  const confidence = normalizeInvestigationConfidence(report?.confidence);
  const tier = resolveConfidenceTier(confidence);
  const lowConfidence = tier === "low";

  return {
    confidence,
    tier,
    lowConfidence,
    confidencePctText: formatConfidencePercent(report?.confidence),
    confidenceBadgeClass: confidenceTierBadgeClass(tier),
    confidenceTierLabel: t(CONFIDENCE_TIER_LABEL_KEY[tier] || CONFIDENCE_TIER_LABEL_KEY.medium),
    degradedPresentation: lowConfidence,
    showLowConfidenceWarning: lowConfidence,
    lowConfidenceMessage: t(REPORT_LAYOUT_I18N_KEYS.lowConfidenceWarning),
    showNoisySections: !lowConfidence,
    expandOperationalAnalysis: tier === "high",
    expandDependencyIntelligence: false,
    expandTechnicalEvidence: false,
    expandAllRecommendedActions: false,
    expandAdditionalTestRecommendations: false,
    expandInferredImpactAreas: false,
    executiveOverviewTitle: t(REPORT_LAYOUT_I18N_KEYS.executiveOverview),
    recommendedActionsTitle: t(REPORT_LAYOUT_I18N_KEYS.recommendedActions),
    operationalAnalysisTitle: t(REPORT_LAYOUT_I18N_KEYS.operationalAnalysis),
    dependencyIntelligenceTitle: t(REPORT_LAYOUT_I18N_KEYS.dependencyIntelligence),
    technicalEvidenceTitle: t(REPORT_LAYOUT_I18N_KEYS.technicalEvidence),
    potentialInferredAreasTitle: t(REPORT_LAYOUT_I18N_KEYS.potentialInferredAreas),
    confirmedImpactedAreasTitle: t(REPORT_LAYOUT_I18N_KEYS.confirmedImpactedAreas),
    seeRecommendedActionsLabel: t(REPORT_LAYOUT_I18N_KEYS.seeRecommendedActions),
    allRecommendedActionsTitle: t(REPORT_LAYOUT_I18N_KEYS.allRecommendedActions),
    additionalTestRecommendationsTitle: t(REPORT_LAYOUT_I18N_KEYS.additionalTestRecommendations),
    suggestedNextStepsTitle: t(REPORT_LAYOUT_I18N_KEYS.suggestedNextSteps),
  };
}
