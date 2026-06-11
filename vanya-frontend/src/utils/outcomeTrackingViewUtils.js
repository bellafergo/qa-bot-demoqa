/** View helpers for Outcome Tracking (ROI-02A). */

export const OUTCOME_TRACKING_I18N_KEYS = {
  title: "outcome_tracking.title",
  empty: "outcome_tracking.empty",
  readOnlyNote: "outcome_tracking.read_only_note",
  executiveSummary: "outcome_tracking.executive_summary",
  blockersIdentified: "outcome_tracking.blockers_identified",
  releasesBlocked: "outcome_tracking.releases_blocked",
  recommendationsGenerated: "outcome_tracking.recommendations_generated",
  incidentsInvestigated: "outcome_tracking.incidents_investigated",
  reportsDelivered: "outcome_tracking.reports_delivered",
};

const METRIC_LABEL_KEY = {
  blockers_identified: OUTCOME_TRACKING_I18N_KEYS.blockersIdentified,
  releases_blocked: OUTCOME_TRACKING_I18N_KEYS.releasesBlocked,
  recommendations_generated: OUTCOME_TRACKING_I18N_KEYS.recommendationsGenerated,
  incidents_investigated: OUTCOME_TRACKING_I18N_KEYS.incidentsInvestigated,
  executive_reports_sent: OUTCOME_TRACKING_I18N_KEYS.reportsDelivered,
};

function formatMetricValue(value) {
  if (value == null || value === "") return "0";
  return String(value);
}

export function hasOutcomeTrackingSection(report) {
  return report != null;
}

export function isOutcomeTrackingEmpty(report) {
  if (!report) return true;
  return (
    (report.blockers_identified || 0) === 0
    && (report.releases_blocked || 0) === 0
    && (report.recommendations_generated || 0) === 0
    && (report.incidents_investigated || 0) === 0
    && (report.executive_reports_sent || 0) === 0
  );
}

export function mapOutcomeMetric(metric, t) {
  if (!metric) return null;
  const metricName = String(metric.metric_name || "").toLowerCase();
  const labelKey = METRIC_LABEL_KEY[metricName];
  return {
    metric_id: metricName,
    title: labelKey ? t(labelKey) : metric.metric_name,
    value: metric.value,
    displayValue: formatMetricValue(metric.value),
  };
}

export function buildOutcomeTrackingViewModel(report, t) {
  const show = hasOutcomeTrackingSection(report);
  const empty = isOutcomeTrackingEmpty(report);

  const metrics = [
    mapOutcomeMetric({ metric_name: "blockers_identified", value: report?.blockers_identified ?? 0 }, t),
    mapOutcomeMetric({ metric_name: "releases_blocked", value: report?.releases_blocked ?? 0 }, t),
    mapOutcomeMetric({ metric_name: "recommendations_generated", value: report?.recommendations_generated ?? 0 }, t),
    mapOutcomeMetric({ metric_name: "incidents_investigated", value: report?.incidents_investigated ?? 0 }, t),
    mapOutcomeMetric({ metric_name: "executive_reports_sent", value: report?.executive_reports_sent ?? 0 }, t),
  ].filter(Boolean);

  return {
    show,
    empty,
    title: t(OUTCOME_TRACKING_I18N_KEYS.title),
    emptyMessage: t(OUTCOME_TRACKING_I18N_KEYS.empty),
    readOnlyNote: t(OUTCOME_TRACKING_I18N_KEYS.readOnlyNote),
    executiveSummaryLabel: t(OUTCOME_TRACKING_I18N_KEYS.executiveSummary),
    executiveSummary: empty
      ? t(OUTCOME_TRACKING_I18N_KEYS.empty)
      : (report?.executive_summary || t(OUTCOME_TRACKING_I18N_KEYS.empty)),
    metrics,
    generated_at: report?.generated_at || null,
  };
}
