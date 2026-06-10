/** View helpers for Quality Health Score (OBS-01A). */

export const QUALITY_HEALTH_SCORE_I18N_KEYS = {
  title: "incident.qa.quality_health_score",
  empty: "incident.qa.quality_health_score_empty",
  overallQualityHealth: "incident.qa.quality_health_score_overall",
  status: "incident.qa.quality_health_score_status",
  trend: "incident.qa.quality_health_score_trend",
  confidence: "incident.qa.quality_health_score_confidence",
  summary: "incident.qa.quality_health_score_summary",
  contributingFactors: "incident.qa.quality_health_score_contributing_factors",
  projectScores: "incident.qa.quality_health_score_project",
  environmentScores: "incident.qa.quality_health_score_environments",
  moduleScores: "incident.qa.quality_health_score_modules",
  journeyScores: "incident.qa.quality_health_score_journeys",
  contractScores: "incident.qa.quality_health_score_contracts",
  readOnlyNote: "incident.qa.quality_health_score_read_only_note",
  trendImproving: "incident.qa.quality_health_score_trend_improving",
  trendStable: "incident.qa.quality_health_score_trend_stable",
  trendDegrading: "incident.qa.quality_health_score_trend_degrading",
  trendUnknown: "incident.qa.quality_health_score_trend_unknown",
  statusExcellent: "incident.qa.quality_health_score_status_excellent",
  statusGood: "incident.qa.quality_health_score_status_good",
  statusAttention: "incident.qa.quality_health_score_status_attention",
  statusHighRisk: "incident.qa.quality_health_score_status_high_risk",
  statusUnknown: "incident.qa.quality_health_score_status_unknown",
};

const STATUS_COLOR = {
  EXCELLENT: "#22c55e",
  GOOD: "#3b82f6",
  ATTENTION: "#f97316",
  HIGH_RISK: "#ef4444",
  UNKNOWN: "#9ca3af",
};

const STATUS_BADGE = {
  EXCELLENT: "badge badge-green",
  GOOD: "badge badge-blue",
  ATTENTION: "badge badge-orange",
  HIGH_RISK: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

const TREND_LABEL_KEY = {
  IMPROVING: QUALITY_HEALTH_SCORE_I18N_KEYS.trendImproving,
  STABLE: QUALITY_HEALTH_SCORE_I18N_KEYS.trendStable,
  DEGRADING: QUALITY_HEALTH_SCORE_I18N_KEYS.trendDegrading,
  UNKNOWN: QUALITY_HEALTH_SCORE_I18N_KEYS.trendUnknown,
};

const STATUS_LABEL_KEY = {
  EXCELLENT: QUALITY_HEALTH_SCORE_I18N_KEYS.statusExcellent,
  GOOD: QUALITY_HEALTH_SCORE_I18N_KEYS.statusGood,
  ATTENTION: QUALITY_HEALTH_SCORE_I18N_KEYS.statusAttention,
  HIGH_RISK: QUALITY_HEALTH_SCORE_I18N_KEYS.statusHighRisk,
  UNKNOWN: QUALITY_HEALTH_SCORE_I18N_KEYS.statusUnknown,
};

export function hasQualityHealthSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "quality_health");
}

export function isQualityHealthEmpty(report) {
  return report?.quality_health == null;
}

export function healthStatusColor(status) {
  return STATUS_COLOR[String(status || "UNKNOWN").toUpperCase()] || STATUS_COLOR.UNKNOWN;
}

export function healthStatusBadgeClass(status) {
  return STATUS_BADGE[String(status || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

export function formatHealthConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function healthStatusLabelKey(status) {
  return STATUS_LABEL_KEY[String(status || "UNKNOWN").toUpperCase()] || STATUS_LABEL_KEY.UNKNOWN;
}

export function healthTrendLabelKey(trend) {
  return TREND_LABEL_KEY[String(trend || "UNKNOWN").toUpperCase()] || TREND_LABEL_KEY.UNKNOWN;
}

export function buildFactorDrilldownItem(factor) {
  if (!factor?.related_entity_type || !factor?.related_entity_id) return null;
  return {
    source: "quality_health",
    related_entity_type: factor.related_entity_type,
    related_entity_id: factor.related_entity_id,
    reason: factor.description,
    detail: factor.title,
    title: factor.title,
  };
}

function enrichScore(score, t) {
  const statusKey = healthStatusLabelKey(score.status);
  const trendKey = healthTrendLabelKey(score.trend);
  return {
    ...score,
    statusLabel: t(statusKey),
    trendLabel: t(trendKey),
    statusColor: healthStatusColor(score.status),
    statusBadgeClass: healthStatusBadgeClass(score.status),
    factors: (score.contributing_factors || []).map((factor) => ({
      ...factor,
      drilldownItem: buildFactorDrilldownItem(factor),
    })),
  };
}

export function buildQualityHealthScoreViewModel(report, t) {
  const qh = report?.quality_health ?? null;
  const scores = (qh?.scores || []).map((score) => enrichScore(score, t));

  return {
    show: hasQualityHealthSection(report),
    empty: isQualityHealthEmpty(report),
    emptyMessage: t(QUALITY_HEALTH_SCORE_I18N_KEYS.empty),
    title: t(QUALITY_HEALTH_SCORE_I18N_KEYS.title),
    overallQualityHealthLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.overallQualityHealth),
    statusLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.status),
    trendLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.trend),
    confidenceLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.confidence),
    summaryLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.summary),
    contributingFactorsLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.contributingFactors),
    projectScoresLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.projectScores),
    environmentScoresLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.environmentScores),
    moduleScoresLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.moduleScores),
    journeyScoresLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.journeyScores),
    contractScoresLabel: t(QUALITY_HEALTH_SCORE_I18N_KEYS.contractScores),
    readOnlyNote: t(QUALITY_HEALTH_SCORE_I18N_KEYS.readOnlyNote),
    report: qh
      ? {
          ...qh,
          confidenceText: formatHealthConfidence(qh.confidence),
          overallStatusLabel: t(healthStatusLabelKey(qh.overall_status)),
          overallTrendLabel: t(healthTrendLabelKey(qh.trend)),
          overallStatusColor: healthStatusColor(qh.overall_status),
          overallStatusBadgeClass: healthStatusBadgeClass(qh.overall_status),
          projectScore: scores.find((s) => s.scope_type === "project") || null,
          environmentScores: scores.filter((s) => s.scope_type === "environment"),
          moduleScores: scores.filter((s) => s.scope_type === "module"),
          journeyScores: scores.filter((s) => s.scope_type === "journey"),
          contractScores: scores.filter((s) => s.scope_type === "contract"),
        }
      : null,
  };
}
