/** Executive Risk Brief — dashboard widget view model. */

export const EXECUTIVE_RISK_BRIEF_I18N_KEYS = {
  widgetTitle: "executive_risk_brief.widget_title",
  primaryRisk: "executive_risk_brief.primary_risk",
  confidence: "executive_risk_brief.confidence",
  confidenceHigh: "executive_risk_brief.confidence.high",
  confidenceMedium: "executive_risk_brief.confidence.medium",
  confidenceLow: "executive_risk_brief.confidence.low",
  evidence: "executive_risk_brief.evidence",
  impact: "executive_risk_brief.impact",
  recommendation: "executive_risk_brief.recommendation",
  noRisk: "executive_risk_brief.no_risk",
  readOnlyNote: "executive_risk_brief.read_only_note",
  evidenceOccurrences: "executive_risk_brief.evidence.occurrences",
  evidenceModule: "executive_risk_brief.evidence.module",
  evidenceRepresentativeTest: "executive_risk_brief.evidence.representative_test",
};

const CONFIDENCE_BADGE = {
  high: "badge badge-red",
  medium: "badge badge-orange",
  low: "badge badge-gray",
};

function formatEvidenceItem(item, t) {
  if (!item || !item.kind) return "";
  if (item.kind === "occurrences") {
    return t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.evidenceOccurrences, { count: item.count ?? 0 });
  }
  if (item.kind === "module") {
    return t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.evidenceModule, { module: item.module || "—" });
  }
  if (item.kind === "representative_test") {
    return t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.evidenceRepresentativeTest, {
      test: item.test_case_id || "—",
    });
  }
  if (item.kind === "summary" && item.text) {
    return item.text;
  }
  return "";
}

export function buildExecutiveRiskBriefViewModel(brief, t) {
  if (!brief || typeof brief !== "object") {
    return { show: false };
  }

  const confidence = (brief.confidence || "low").toLowerCase();
  const confidenceKey = EXECUTIVE_RISK_BRIEF_I18N_KEYS[
    `confidence${confidence.charAt(0).toUpperCase()}${confidence.slice(1)}`
  ] || EXECUTIVE_RISK_BRIEF_I18N_KEYS.confidenceLow;

  const evidence = (brief.evidence || [])
    .map((item) => formatEvidenceItem(item, t))
    .filter(Boolean);

  return {
    show: Boolean(brief.has_risk),
    widgetTitle: t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.widgetTitle),
    primaryRiskLabel: t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.primaryRisk),
    module: brief.module || t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.noRisk),
    confidenceLabel: t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.confidence),
    confidenceText: t(confidenceKey),
    confidenceBadgeClass: CONFIDENCE_BADGE[confidence] || CONFIDENCE_BADGE.low,
    evidenceLabel: t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.evidence),
    evidence,
    impactLabel: t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.impact),
    impact: brief.impact || "",
    recommendationLabel: t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.recommendation),
    recommendation: brief.recommendation || "",
    readOnlyNote: t(EXECUTIVE_RISK_BRIEF_I18N_KEYS.readOnlyNote),
    hasRisk: Boolean(brief.has_risk),
  };
}
