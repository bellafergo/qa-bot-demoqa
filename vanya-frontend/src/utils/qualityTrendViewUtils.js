/** View helpers for Quality Trends (OBS-01B). */

export const QUALITY_TREND_I18N_KEYS = {
  title: "incident.qa.quality_trends",
  empty: "incident.qa.quality_trends_empty",
  overallTrend: "incident.qa.quality_trends_overall_trend",
  trendDirection: "incident.qa.quality_trends_trend_direction",
  scoreChange: "incident.qa.quality_trends_score_change",
  confidence: "incident.qa.quality_trends_confidence",
  historicalPoints: "incident.qa.quality_trends_historical_points",
  readOnlyNote: "incident.qa.quality_trends_read_only_note",
  improving: "incident.qa.quality_trends_improving",
  stable: "incident.qa.quality_trends_stable",
  degrading: "incident.qa.quality_trends_degrading",
  unknown: "incident.qa.quality_trends_unknown",
};

const TREND_COLOR = {
  IMPROVING: "#22c55e",
  STABLE: "#3b82f6",
  DEGRADING: "#ef4444",
  UNKNOWN: "#9ca3af",
};

const TREND_BADGE = {
  IMPROVING: "badge badge-green",
  STABLE: "badge badge-blue",
  DEGRADING: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

const TREND_LABEL_KEY = {
  IMPROVING: QUALITY_TREND_I18N_KEYS.improving,
  STABLE: QUALITY_TREND_I18N_KEYS.stable,
  DEGRADING: QUALITY_TREND_I18N_KEYS.degrading,
  UNKNOWN: QUALITY_TREND_I18N_KEYS.unknown,
};

export function hasQualityTrendSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "quality_trends");
}

export function isQualityTrendEmpty(report) {
  return report?.quality_trends == null;
}

export function trendDirectionColor(direction) {
  return TREND_COLOR[String(direction || "UNKNOWN").toUpperCase()] || TREND_COLOR.UNKNOWN;
}

export function trendDirectionBadgeClass(direction) {
  return TREND_BADGE[String(direction || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

export function trendDirectionLabelKey(direction) {
  return TREND_LABEL_KEY[String(direction || "UNKNOWN").toUpperCase()] || TREND_LABEL_KEY.UNKNOWN;
}

export function formatTrendConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function formatScoreChange(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return n > 0 ? `+${n}` : String(n);
}

export function buildSparklinePath(points, width = 120, height = 36) {
  const list = Array.isArray(points) ? points : [];
  if (list.length < 2) return "";
  const scores = list.map((p) => Number(p.score) || 0);
  const min = Math.min(...scores, 0);
  const max = Math.max(...scores, 100);
  const range = Math.max(max - min, 1);
  const step = width / (list.length - 1);
  return list
    .map((point, index) => {
      const x = index * step;
      const y = height - ((Number(point.score) - min) / range) * (height - 4) - 2;
      return `${index === 0 ? "M" : "L"}${x.toFixed(1)},${y.toFixed(1)}`;
    })
    .join(" ");
}

export function buildHistoricalLearningDrilldownItem(historicalLearning) {
  const incident = historicalLearning?.similar_incidents?.[0];
  if (!incident?.incident_id) return null;
  return {
    source: "quality_trends",
    related_entity_type: "historical_learning",
    related_entity_id: incident.incident_id,
    reason: incident.summary || historicalLearning.pattern_summary,
    detail: incident.title || incident.incident_id,
    title: incident.title || incident.incident_id,
  };
}

export function buildQualityTrendViewModel(report, t) {
  const qt = report?.quality_trends ?? null;
  const drilldownItem = buildHistoricalLearningDrilldownItem(report?.historical_learning);

  return {
    show: hasQualityTrendSection(report),
    empty: isQualityTrendEmpty(report),
    emptyMessage: t(QUALITY_TREND_I18N_KEYS.empty),
    title: t(QUALITY_TREND_I18N_KEYS.title),
    overallTrendLabel: t(QUALITY_TREND_I18N_KEYS.overallTrend),
    trendDirectionLabel: t(QUALITY_TREND_I18N_KEYS.trendDirection),
    scoreChangeLabel: t(QUALITY_TREND_I18N_KEYS.scoreChange),
    confidenceLabel: t(QUALITY_TREND_I18N_KEYS.confidence),
    historicalPointsLabel: t(QUALITY_TREND_I18N_KEYS.historicalPoints),
    readOnlyNote: t(QUALITY_TREND_I18N_KEYS.readOnlyNote),
    drilldownItem,
    report: qt
      ? {
          ...qt,
          confidenceText: formatTrendConfidence(qt.confidence),
          overallTrendLabel: t(trendDirectionLabelKey(qt.overall_trend)),
          overallTrendColor: trendDirectionColor(qt.overall_trend),
          overallTrendBadgeClass: trendDirectionBadgeClass(qt.overall_trend),
          trends: (qt.trends || []).map((trend) => ({
            ...trend,
            trendDirectionLabel: t(trendDirectionLabelKey(trend.trend_direction)),
            trendColor: trendDirectionColor(trend.trend_direction),
            trendBadgeClass: trendDirectionBadgeClass(trend.trend_direction),
            scoreChangeText: formatScoreChange(trend.score_change),
            sparklinePath: buildSparklinePath(trend.points),
            pointsText: (trend.points || []).map((p) => p.score).join(" → "),
          })),
        }
      : null,
  };
}

/** Dashboard helper when trends come from project API instead of incident report. */
export function buildQualityTrendViewModelFromApi(trendReport, t, drilldownItem = null) {
  const syntheticReport = { quality_trends: trendReport, historical_learning: null };
  const vm = buildQualityTrendViewModel(syntheticReport, t);
  if (drilldownItem) vm.drilldownItem = drilldownItem;
  return vm;
}
