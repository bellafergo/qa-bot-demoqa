/** View helpers for Executive Quality Report (ENT-02A). */

export const EXECUTIVE_QUALITY_REPORT_I18N_KEYS = {
  title: "incident.qa.executive_quality_report",
  empty: "incident.qa.executive_quality_report_empty",
  qualityScore: "incident.qa.executive_quality_report_quality_score",
  riskLevel: "incident.qa.executive_quality_report_risk_level",
  confidence: "incident.qa.executive_quality_report_confidence",
  executiveSummary: "incident.qa.executive_quality_report_executive_summary",
  topRisks: "incident.qa.executive_quality_report_top_risks",
  topRecommendations: "incident.qa.executive_quality_report_top_recommendations",
  openIncidents: "incident.qa.executive_quality_report_open_incidents",
  criticalContracts: "incident.qa.executive_quality_report_critical_contracts",
  brokenJourneys: "incident.qa.executive_quality_report_broken_journeys",
  recommendedTests: "incident.qa.executive_quality_report_recommended_tests",
  qualityTrend: "incident.qa.executive_quality_report_quality_trend",
  scoreExcellent: "incident.qa.executive_quality_report_score_excellent",
  scoreGood: "incident.qa.executive_quality_report_score_good",
  scoreAttention: "incident.qa.executive_quality_report_score_attention",
  scoreHighRisk: "incident.qa.executive_quality_report_score_high_risk",
  readOnlyNote: "incident.qa.executive_quality_report_read_only_note",
};

const RISK_BADGE = {
  LOW: "badge badge-green",
  MEDIUM: "badge badge-orange",
  HIGH: "badge badge-red",
  CRITICAL: "badge badge-red",
};

export function hasExecutiveQualityReportSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "executive_quality_report");
}

export function isExecutiveQualityReportEmpty(report) {
  return report?.executive_quality_report == null;
}

export function formatExecutiveConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function qualityScoreBand(score) {
  const n = Number(score);
  if (!Number.isFinite(n)) return { labelKey: EXECUTIVE_QUALITY_REPORT_I18N_KEYS.scoreAttention, color: "#eab308" };
  if (n >= 90) return { labelKey: EXECUTIVE_QUALITY_REPORT_I18N_KEYS.scoreExcellent, color: "#22c55e" };
  if (n >= 75) return { labelKey: EXECUTIVE_QUALITY_REPORT_I18N_KEYS.scoreGood, color: "#3b82f6" };
  if (n >= 50) return { labelKey: EXECUTIVE_QUALITY_REPORT_I18N_KEYS.scoreAttention, color: "#f97316" };
  return { labelKey: EXECUTIVE_QUALITY_REPORT_I18N_KEYS.scoreHighRisk, color: "#ef4444" };
}

export function riskLevelBadgeClass(riskLevel) {
  const key = String(riskLevel || "LOW").toUpperCase();
  return RISK_BADGE[key] || "badge badge-gray";
}

export function buildExecutiveQualityReportViewModel(report, t) {
  const eqr = report?.executive_quality_report ?? null;
  const band = eqr ? qualityScoreBand(eqr.overall_quality_score) : null;

  return {
    show: hasExecutiveQualityReportSection(report),
    empty: isExecutiveQualityReportEmpty(report),
    emptyMessage: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.empty),
    title: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.title),
    qualityScoreLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.qualityScore),
    riskLevelLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.riskLevel),
    confidenceLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.confidence),
    executiveSummaryLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.executiveSummary),
    topRisksLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.topRisks),
    topRecommendationsLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.topRecommendations),
    openIncidentsLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.openIncidents),
    criticalContractsLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.criticalContracts),
    brokenJourneysLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.brokenJourneys),
    recommendedTestsLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.recommendedTests),
    qualityTrendLabel: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.qualityTrend),
    readOnlyNote: t(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.readOnlyNote),
    report: eqr
      ? {
          ...eqr,
          confidenceText: formatExecutiveConfidence(eqr.confidence),
          scoreBand: band,
          scoreBandLabel: t(band.labelKey),
          scoreColor: band.color,
          riskBadgeClass: riskLevelBadgeClass(eqr.overall_risk_level),
        }
      : null,
  };
}
