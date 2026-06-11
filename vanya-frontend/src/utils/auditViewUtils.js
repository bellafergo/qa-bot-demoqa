/** View helpers for Audit Trail (SEC-01E). */

export const AUDIT_I18N_KEYS = {
  title: "audit_trail.title",
  subtitle: "audit_trail.subtitle",
  empty: "audit_trail.empty",
  latestEvents: "audit_trail.latest_events",
  filterEventType: "audit_trail.filter_event_type",
  filterAll: "audit_trail.filter_all",
  summaryTotal: "audit_trail.summary_total",
  summaryRecent: "audit_trail.summary_recent",
  readOnlyNote: "audit_trail.read_only_note",
  user: "audit_trail.user",
  resource: "audit_trail.resource",
  action: "audit_trail.action",
  result: "audit_trail.result",
  timestamp: "audit_trail.timestamp",
  resultSuccess: "audit_trail.result.success",
  resultFailure: "audit_trail.result.failure",
};

const EVENT_TYPE_LABEL_KEY = {
  SSO_PROVIDER_VALIDATED: "audit_trail.event.sso_provider_validated",
  SSO_LOGIN_URL_GENERATED: "audit_trail.event.sso_login_url_generated",
  REPORT_PREVIEWED: "audit_trail.event.report_previewed",
  REPORT_SENT: "audit_trail.event.report_sent",
  INTEGRATION_CONFIG_UPDATED: "audit_trail.event.integration_config_updated",
  INTEGRATION_VALIDATED: "audit_trail.event.integration_validated",
  INCIDENT_INVESTIGATION_STARTED: "audit_trail.event.incident_investigation_started",
  INCIDENT_INVESTIGATION_COMPLETED: "audit_trail.event.incident_investigation_completed",
  RELEASE_READINESS_VIEWED: "audit_trail.event.release_readiness_viewed",
  PERMISSION_DENIED: "audit_trail.event.permission_denied",
  UNAUTHORIZED_ACCESS: "audit_trail.event.unauthorized_access",
};

export function eventTypeLabelKey(eventType) {
  return EVENT_TYPE_LABEL_KEY[String(eventType || "").toUpperCase()] || eventType;
}

export function resultBadgeClass(result) {
  return String(result || "").toUpperCase() === "SUCCESS" ? "badge badge-green" : "badge badge-red";
}

export function formatAuditTimestamp(value) {
  if (!value) return "—";
  try {
    return new Date(value).toLocaleString();
  } catch {
    return String(value);
  }
}

export function mapAuditEvent(event, t) {
  if (!event?.event_id) return null;
  const result = String(event.result || "SUCCESS").toUpperCase();
  const eventType = String(event.event_type || "");
  return {
    eventId: event.event_id,
    timestamp: formatAuditTimestamp(event.timestamp),
    userId: event.user_id || "—",
    userEmail: event.user_email || "—",
    eventType,
    eventTypeLabel: t(eventTypeLabelKey(eventType)),
    resourceType: event.resource_type || "—",
    resourceId: event.resource_id || "—",
    action: event.action || "—",
    result,
    resultLabel: result === "SUCCESS" ? t(AUDIT_I18N_KEYS.resultSuccess) : t(AUDIT_I18N_KEYS.resultFailure),
    resultBadgeClass: resultBadgeClass(result),
    userLabel: t(AUDIT_I18N_KEYS.user),
    resourceLabel: t(AUDIT_I18N_KEYS.resource),
    actionLabel: t(AUDIT_I18N_KEYS.action),
    resultLabelKey: t(AUDIT_I18N_KEYS.result),
    timestampLabel: t(AUDIT_I18N_KEYS.timestamp),
  };
}

export function buildAuditTrailViewModel({ events, summary, selectedEventType, t }) {
  const eventList = (events?.events || [])
    .map((event) => mapAuditEvent(event, t))
    .filter(Boolean);

  const eventTypeOptions = Object.keys(EVENT_TYPE_LABEL_KEY).map((type) => ({
    value: type,
    label: t(EVENT_TYPE_LABEL_KEY[type]),
  }));

  const latest = summary?.latest_event ? mapAuditEvent(summary.latest_event, t) : null;

  return {
    show: true,
    empty: eventList.length === 0,
    emptyMessage: t(AUDIT_I18N_KEYS.empty),
    title: t(AUDIT_I18N_KEYS.title),
    subtitle: t(AUDIT_I18N_KEYS.subtitle),
    readOnlyNote: t(AUDIT_I18N_KEYS.readOnlyNote),
    latestEventsLabel: t(AUDIT_I18N_KEYS.latestEvents),
    filterEventTypeLabel: t(AUDIT_I18N_KEYS.filterEventType),
    filterAllLabel: t(AUDIT_I18N_KEYS.filterAll),
    selectedEventType: selectedEventType || "",
    eventTypeOptions,
    events: eventList,
    summaryTotalLabel: t(AUDIT_I18N_KEYS.summaryTotal),
    summaryRecentLabel: t(AUDIT_I18N_KEYS.summaryRecent),
    totalEvents: summary?.total_events ?? 0,
    latestActivity: latest?.timestamp || "—",
    latestActivityDetail: latest
      ? `${latest.eventTypeLabel} · ${latest.userEmail !== "—" ? latest.userEmail : latest.userId}`
      : "—",
    eventTypeCounts: summary?.event_types || {},
  };
}
