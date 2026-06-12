/** View helpers for Value Dashboard (ROI-01A). */

export const VALUE_DASHBOARD_I18N_KEYS = {
  title: "value_dashboard.title",
  empty: "value_dashboard.empty",
  readOnlyNote: "value_dashboard.read_only_note",
  topMetrics: "value_dashboard.top_metrics",
  activity: "value_dashboard.activity",
  riskVisibility: "value_dashboard.risk_visibility",
  qualityVisibility: "value_dashboard.quality_visibility",
  operationalImpact: "value_dashboard.operational_impact",
  incidentsInvestigated: "value_dashboard.incidents_investigated",
  executiveReportsGenerated: "value_dashboard.executive_reports_generated",
  releaseReadinessReports: "value_dashboard.release_readiness_reports",
  scheduledReportsGenerated: "value_dashboard.scheduled_reports_generated",
  blockedReleases: "value_dashboard.blocked_releases",
  criticalRisksIdentified: "value_dashboard.critical_risks_identified",
  jiraBlockersDetected: "value_dashboard.jira_blockers_detected",
  degradationEventsDetected: "value_dashboard.degradation_events_detected",
  qualityHealth: "value_dashboard.quality_health",
  qualityTrend: "value_dashboard.quality_trend",
  degradedEnvironments: "value_dashboard.degraded_environments",
  impactedJourneys: "value_dashboard.impacted_journeys",
  recommendationsGenerated: "value_dashboard.recommendations_generated",
  approvalsRequested: "value_dashboard.approvals_requested",
  validationsPlanned: "value_dashboard.validations_planned",
  correlatedJiraIssues: "value_dashboard.correlated_jira_issues",
};

function formatMetricValue(value) {
  if (value == null || value === "") return "0";
  return String(value);
}

function buildMetric(metricId, title, value, description) {
  return {
    metric_id: metricId,
    title,
    value,
    displayValue: formatMetricValue(value),
    description,
  };
}

export function hasValueDashboardSection(dashboard) {
  return dashboard != null;
}

export function isValueDashboardEmpty(dashboard) {
  if (!dashboard) return true;
  const hasActivity = (
    (dashboard.incidents_investigated || 0) > 0
    || (dashboard.executive_reports_generated || 0) > 0
    || (dashboard.release_readiness_reports || 0) > 0
    || (dashboard.scheduled_reports_generated || 0) > 0
  );
  const hasRisk = (
    (dashboard.blocked_releases || 0) > 0
    || (dashboard.critical_risks_identified || 0) > 0
    || (dashboard.jira_blockers_detected || 0) > 0
    || (dashboard.degradation_events_detected || 0) > 0
  );
  const hasQuality = (
    (dashboard.quality_health_score || 0) > 0
    || String(dashboard.quality_trend || "UNKNOWN").toUpperCase() !== "UNKNOWN"
    || (dashboard.degraded_environments || 0) > 0
    || (dashboard.impacted_journeys || 0) > 0
  );
  const hasOperational = (
    (dashboard.recommendations_generated || 0) > 0
    || (dashboard.approvals_requested || 0) > 0
    || (dashboard.validations_planned || 0) > 0
    || (dashboard.correlated_jira_issues || 0) > 0
  );
  const hasTop = (dashboard.top_value_metrics || []).length > 0;
  return !(hasActivity || hasRisk || hasQuality || hasOperational || hasTop);
}

export function buildValueDashboardViewModel(dashboard, t, { hideJiraBlockers = true } = {}) {
  const show = hasValueDashboardSection(dashboard);
  const empty = isValueDashboardEmpty(dashboard);

  const labels = {
    readOnlyNote: t(VALUE_DASHBOARD_I18N_KEYS.readOnlyNote),
  };

  const activityMetrics = [
    buildMetric(
      "incidents_investigated",
      t(VALUE_DASHBOARD_I18N_KEYS.incidentsInvestigated),
      dashboard?.incidents_investigated ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.incidentsInvestigated),
    ),
    buildMetric(
      "executive_reports_generated",
      t(VALUE_DASHBOARD_I18N_KEYS.executiveReportsGenerated),
      dashboard?.executive_reports_generated ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.executiveReportsGenerated),
    ),
    buildMetric(
      "release_readiness_reports",
      t(VALUE_DASHBOARD_I18N_KEYS.releaseReadinessReports),
      dashboard?.release_readiness_reports ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.releaseReadinessReports),
    ),
    buildMetric(
      "scheduled_reports_generated",
      t(VALUE_DASHBOARD_I18N_KEYS.scheduledReportsGenerated),
      dashboard?.scheduled_reports_generated ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.scheduledReportsGenerated),
    ),
  ];

  const riskMetrics = [
    buildMetric(
      "blocked_releases",
      t(VALUE_DASHBOARD_I18N_KEYS.blockedReleases),
      dashboard?.blocked_releases ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.blockedReleases),
    ),
    buildMetric(
      "critical_risks_identified",
      t(VALUE_DASHBOARD_I18N_KEYS.criticalRisksIdentified),
      dashboard?.critical_risks_identified ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.criticalRisksIdentified),
    ),
    ...(hideJiraBlockers ? [] : [
      buildMetric(
        "jira_blockers_detected",
        t(VALUE_DASHBOARD_I18N_KEYS.jiraBlockersDetected),
        dashboard?.jira_blockers_detected ?? 0,
        t(VALUE_DASHBOARD_I18N_KEYS.jiraBlockersDetected),
      ),
    ]),
    buildMetric(
      "degradation_events_detected",
      t(VALUE_DASHBOARD_I18N_KEYS.degradationEventsDetected),
      dashboard?.degradation_events_detected ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.degradationEventsDetected),
    ),
  ];

  const qualityMetrics = [
    buildMetric(
      "quality_health_score",
      t(VALUE_DASHBOARD_I18N_KEYS.qualityHealth),
      dashboard?.quality_health_score ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.qualityHealth),
    ),
    buildMetric(
      "quality_trend",
      t(VALUE_DASHBOARD_I18N_KEYS.qualityTrend),
      String(dashboard?.quality_trend || "UNKNOWN").toUpperCase(),
      t(VALUE_DASHBOARD_I18N_KEYS.qualityTrend),
    ),
    buildMetric(
      "degraded_environments",
      t(VALUE_DASHBOARD_I18N_KEYS.degradedEnvironments),
      dashboard?.degraded_environments ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.degradedEnvironments),
    ),
    buildMetric(
      "impacted_journeys",
      t(VALUE_DASHBOARD_I18N_KEYS.impactedJourneys),
      dashboard?.impacted_journeys ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.impactedJourneys),
    ),
  ];

  const operationalMetrics = [
    buildMetric(
      "recommendations_generated",
      t(VALUE_DASHBOARD_I18N_KEYS.recommendationsGenerated),
      dashboard?.recommendations_generated ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.recommendationsGenerated),
    ),
    buildMetric(
      "approvals_requested",
      t(VALUE_DASHBOARD_I18N_KEYS.approvalsRequested),
      dashboard?.approvals_requested ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.approvalsRequested),
    ),
    buildMetric(
      "validations_planned",
      t(VALUE_DASHBOARD_I18N_KEYS.validationsPlanned),
      dashboard?.validations_planned ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.validationsPlanned),
    ),
    buildMetric(
      "correlated_jira_issues",
      t(VALUE_DASHBOARD_I18N_KEYS.correlatedJiraIssues),
      dashboard?.correlated_jira_issues ?? 0,
      t(VALUE_DASHBOARD_I18N_KEYS.correlatedJiraIssues),
    ),
  ];

  const topMetrics = (dashboard?.top_value_metrics || []).map((metric) => ({
    ...metric,
    displayValue: formatMetricValue(metric.value),
  }));

  return {
    show,
    empty,
    emptyMessage: t(VALUE_DASHBOARD_I18N_KEYS.empty),
    title: t(VALUE_DASHBOARD_I18N_KEYS.title),
    topMetricsLabel: t(VALUE_DASHBOARD_I18N_KEYS.topMetrics),
    activityLabel: t(VALUE_DASHBOARD_I18N_KEYS.activity),
    riskLabel: t(VALUE_DASHBOARD_I18N_KEYS.riskVisibility),
    qualityLabel: t(VALUE_DASHBOARD_I18N_KEYS.qualityVisibility),
    operationalLabel: t(VALUE_DASHBOARD_I18N_KEYS.operationalImpact),
    labels,
    activityMetrics,
    riskMetrics,
    qualityMetrics,
    operationalMetrics,
    topMetrics,
    qualityTrend: String(dashboard?.quality_trend || "UNKNOWN").toUpperCase(),
    generated_at: dashboard?.generated_at || null,
  };
}
