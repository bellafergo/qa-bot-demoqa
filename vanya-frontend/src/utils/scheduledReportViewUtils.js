/** View helpers for Scheduled Executive Reports (ENT-02B). */

export const SCHEDULED_REPORT_I18N_KEYS = {
  title: "scheduled_reports.title",
  schedules: "scheduled_reports.schedules",
  latestPreview: "scheduled_reports.latest_preview",
  qualityBrief: "scheduled_reports.quality_brief",
  executiveSummary: "scheduled_reports.executive_summary",
  releaseReadiness: "scheduled_reports.release_readiness",
  incidentReview: "scheduled_reports.incident_review",
  empty: "scheduled_reports.empty",
  enabled: "scheduled_reports.enabled",
  disabled: "scheduled_reports.disabled",
  recipients: "scheduled_reports.recipients",
  nextRunPreview: "scheduled_reports.next_run_preview",
  qualityScore: "scheduled_reports.quality_score",
  trend: "scheduled_reports.trend",
  riskLevel: "scheduled_reports.risk_level",
  executiveSummaryLabel: "scheduled_reports.executive_summary_label",
  topRisks: "scheduled_reports.top_risks",
  topRecommendations: "scheduled_reports.top_recommendations",
  jiraBlockers: "scheduled_reports.jira_blockers",
  previewReport: "scheduled_reports.preview_report",
  editSchedule: "scheduled_reports.edit_schedule",
  sendReport: "scheduled_reports.send_report",
  sendDisabledNote: "scheduled_reports.send_disabled_note",
  readOnlyNote: "scheduled_reports.read_only_note",
  frequencyDaily: "scheduled_reports.frequency.daily",
  frequencyWeekly: "scheduled_reports.frequency.weekly",
  frequencyMonthly: "scheduled_reports.frequency.monthly",
};

const REPORT_TYPE_LABEL_KEY = {
  QUALITY_BRIEF: SCHEDULED_REPORT_I18N_KEYS.qualityBrief,
  EXECUTIVE_SUMMARY: SCHEDULED_REPORT_I18N_KEYS.executiveSummary,
  RELEASE_READINESS: SCHEDULED_REPORT_I18N_KEYS.releaseReadiness,
  INCIDENT_REVIEW: SCHEDULED_REPORT_I18N_KEYS.incidentReview,
};

const FREQUENCY_LABEL_KEY = {
  DAILY: SCHEDULED_REPORT_I18N_KEYS.frequencyDaily,
  WEEKLY: SCHEDULED_REPORT_I18N_KEYS.frequencyWeekly,
  MONTHLY: SCHEDULED_REPORT_I18N_KEYS.frequencyMonthly,
};

export function reportTypeLabelKey(reportType) {
  return REPORT_TYPE_LABEL_KEY[String(reportType || "").toUpperCase()] || reportType;
}

export function frequencyLabelKey(frequency) {
  return FREQUENCY_LABEL_KEY[String(frequency || "").toUpperCase()] || frequency;
}

export function riskLevelBadgeClass(riskLevel) {
  const level = String(riskLevel || "LOW").toUpperCase();
  if (level === "CRITICAL" || level === "HIGH") return "badge badge-red";
  if (level === "MEDIUM" || level === "ORANGE" || level === "YELLOW") return "badge badge-orange";
  return "badge badge-green";
}

export function trendBadgeClass(trend) {
  const value = String(trend || "UNKNOWN").toUpperCase();
  if (value === "IMPROVING") return "badge badge-green";
  if (value === "DEGRADING") return "badge badge-red";
  if (value === "STABLE") return "badge badge-blue";
  return "badge badge-gray";
}

function mapSchedule(schedule, t) {
  const reportType = String(schedule?.report_type || "").toUpperCase();
  return {
    ...schedule,
    reportTypeLabel: t(reportTypeLabelKey(reportType)),
    frequencyLabel: t(frequencyLabelKey(schedule?.frequency)),
    enabledLabel: schedule?.enabled ? t(SCHEDULED_REPORT_I18N_KEYS.enabled) : t(SCHEDULED_REPORT_I18N_KEYS.disabled),
    recipientsText: `${t(SCHEDULED_REPORT_I18N_KEYS.recipients)}: ${schedule?.recipients_count ?? 0}`,
    nextRunPreviewLabel: t(SCHEDULED_REPORT_I18N_KEYS.nextRunPreview),
    nextRunPreviewText: schedule?.next_run_preview || "—",
  };
}

function mapPreview(preview, t) {
  if (!preview) return null;
  const jiraBlockerCount = preview.jira_blocker_count ?? 0;
  return {
    ...preview,
    qualityScoreLabel: t(SCHEDULED_REPORT_I18N_KEYS.qualityScore),
    trendLabel: t(SCHEDULED_REPORT_I18N_KEYS.trend),
    riskLevelLabel: t(SCHEDULED_REPORT_I18N_KEYS.riskLevel),
    executiveSummaryLabel: t(SCHEDULED_REPORT_I18N_KEYS.executiveSummaryLabel),
    topRisksLabel: t(SCHEDULED_REPORT_I18N_KEYS.topRisks),
    topRecommendationsLabel: t(SCHEDULED_REPORT_I18N_KEYS.topRecommendations),
    jiraBlockersLabel: t(SCHEDULED_REPORT_I18N_KEYS.jiraBlockers),
    trendBadgeClass: trendBadgeClass(preview.quality_trend),
    riskBadgeClass: riskLevelBadgeClass(preview.risk_level),
    top_risks: preview.top_risks || [],
    top_recommendations: preview.top_recommendations || [],
    jiraBlockerCount,
    jiraBlockerKeys: preview.jira_blocker_keys || [],
    showJiraKpi: jiraBlockerCount > 0,
  };
}

export function buildScheduledReportViewModel(reportCenter, t) {
  const schedules = (reportCenter?.schedules || []).map((schedule) => mapSchedule(schedule, t));
  const empty = !reportCenter || schedules.length === 0;

  return {
    show: Boolean(reportCenter),
    empty,
    emptyMessage: t(SCHEDULED_REPORT_I18N_KEYS.empty),
    title: t(SCHEDULED_REPORT_I18N_KEYS.title),
    schedulesLabel: t(SCHEDULED_REPORT_I18N_KEYS.schedules),
    latestPreviewLabel: t(SCHEDULED_REPORT_I18N_KEYS.latestPreview),
    readOnlyNote: t(SCHEDULED_REPORT_I18N_KEYS.readOnlyNote),
    previewReportLabel: t(SCHEDULED_REPORT_I18N_KEYS.previewReport),
    editScheduleLabel: t(SCHEDULED_REPORT_I18N_KEYS.editSchedule),
    sendReportLabel: t(SCHEDULED_REPORT_I18N_KEYS.sendReport),
    sendDisabledNote: t(SCHEDULED_REPORT_I18N_KEYS.sendDisabledNote),
    schedules,
    preview: mapPreview(reportCenter?.latest_preview, t),
    totalSchedules: reportCenter?.total_schedules ?? schedules.length,
  };
}
