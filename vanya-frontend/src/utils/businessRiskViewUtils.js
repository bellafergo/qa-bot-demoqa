/** View helpers for Business Risk Estimation (ROI-01C). */

export const BUSINESS_RISK_I18N_KEYS = {
  title: "business_risk_overview.title",
  empty: "business_risk_overview.empty",
  insufficientIntelligence: "business_risk_overview.insufficient_intelligence",
  readOnlyNote: "business_risk_overview.read_only_note",
  overallRisk: "business_risk_overview.overall_risk",
  capabilitiesAtRisk: "business_risk_overview.capabilities_at_risk",
  businessRisks: "business_risk_overview.business_risks",
  evidence: "business_risk_overview.evidence",
  confidence: "business_risk_overview.confidence",
  severityLow: "business_risk_overview.severity.low",
  severityMedium: "business_risk_overview.severity.medium",
  severityHigh: "business_risk_overview.severity.high",
  severityCritical: "business_risk_overview.severity.critical",
  confidenceLow: "business_risk_overview.confidence.low",
  confidenceMedium: "business_risk_overview.confidence.medium",
  confidenceHigh: "business_risk_overview.confidence.high",
};

const SEVERITY_BADGE = {
  LOW: "badge badge-green",
  MEDIUM: "badge badge-blue",
  HIGH: "badge badge-orange",
  CRITICAL: "badge badge-red",
};

const CONFIDENCE_BADGE = {
  LOW: "badge badge-gray",
  MEDIUM: "badge badge-blue",
  HIGH: "badge badge-green",
};

const CONFIDENCE_LABEL_KEY = {
  LOW: BUSINESS_RISK_I18N_KEYS.confidenceLow,
  MEDIUM: BUSINESS_RISK_I18N_KEYS.confidenceMedium,
  HIGH: BUSINESS_RISK_I18N_KEYS.confidenceHigh,
};

export function severityBadgeClass(severity) {
  return SEVERITY_BADGE[String(severity || "LOW").toUpperCase()] || "badge badge-gray";
}

export function mapBusinessRisk(risk, t) {
  if (!risk) return null;
  const confidence = String(risk.confidence || "LOW").toUpperCase();
  return {
    ...risk,
    severity: String(risk.severity || "LOW").toUpperCase(),
    severityBadgeClass: severityBadgeClass(risk.severity),
    confidenceLabel: `${t(BUSINESS_RISK_I18N_KEYS.confidence)}: ${t(CONFIDENCE_LABEL_KEY[confidence] || CONFIDENCE_LABEL_KEY.LOW)}`,
    confidenceBadgeClass: CONFIDENCE_BADGE[confidence] || CONFIDENCE_BADGE.LOW,
  };
}

export function mapBusinessRiskSignal(signal) {
  if (!signal) return null;
  return {
    ...signal,
    severity: String(signal.severity || "LOW").toUpperCase(),
    severityBadgeClass: severityBadgeClass(signal.severity),
    capability: signal.impacted_capability,
  };
}

export function hasBusinessRiskSection(report) {
  return report != null;
}

export function isBusinessRiskEmpty(report) {
  if (!report) return true;
  if (!report.has_intelligence) return true;
  return !(report.business_risks || []).length && !(report.signals || []).length;
}

export function buildBusinessRiskViewModel(report, t) {
  const show = hasBusinessRiskSection(report);
  const empty = isBusinessRiskEmpty(report);

  const businessRisks = (report?.business_risks || []).map((r) => mapBusinessRisk(r, t)).filter(Boolean);
  const signals = (report?.signals || []).map(mapBusinessRiskSignal).filter(Boolean);

  return {
    show,
    empty,
    emptyMessage: empty
      ? (report?.has_intelligence === false
        ? t(BUSINESS_RISK_I18N_KEYS.insufficientIntelligence)
        : t(BUSINESS_RISK_I18N_KEYS.empty))
      : t(BUSINESS_RISK_I18N_KEYS.empty),
    title: t(BUSINESS_RISK_I18N_KEYS.title),
    readOnlyNote: t(BUSINESS_RISK_I18N_KEYS.readOnlyNote),
    overallRiskLabel: t(BUSINESS_RISK_I18N_KEYS.overallRisk),
    overallBusinessRisk: String(report?.overall_business_risk || "LOW").toUpperCase(),
    overallSeverityBadgeClass: severityBadgeClass(report?.overall_business_risk),
    executiveSummary: report?.executive_summary || "",
    capabilitiesLabel: t(BUSINESS_RISK_I18N_KEYS.capabilitiesAtRisk),
    topCapabilities: report?.top_capabilities_at_risk || [],
    risksLabel: t(BUSINESS_RISK_I18N_KEYS.businessRisks),
    evidenceLabel: t(BUSINESS_RISK_I18N_KEYS.evidence),
    businessRisks,
    signals,
    generated_at: report?.generated_at || null,
  };
}
