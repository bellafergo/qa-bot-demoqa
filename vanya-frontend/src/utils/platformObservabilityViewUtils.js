/** View helpers for Platform Self Observability (OBS-02A). */

export const PLATFORM_OBSERVABILITY_I18N_KEYS = {
  title: "platform_observability.title",
  empty: "platform_observability.empty",
  readOnlyNote: "platform_observability.read_only_note",
  executiveSummary: "platform_observability.executive_summary",
  topPlatformRisks: "platform_observability.top_platform_risks",
  authenticationHealth: "platform_observability.authentication_health",
  integrationHealth: "platform_observability.integration_health",
  reportDeliveryHealth: "platform_observability.report_delivery_health",
  incidentInvestigationHealth: "platform_observability.incident_investigation_health",
  apiHealth: "platform_observability.api_health",
  statusHealthy: "platform_observability.status.healthy",
  statusDegraded: "platform_observability.status.degraded",
  statusUnhealthy: "platform_observability.status.unhealthy",
  statusUnknown: "platform_observability.status.unknown",
  integrationHealthy: "platform_observability.integration_summary.healthy",
  integrationDegraded: "platform_observability.integration_summary.degraded",
  integrationDisconnected: "platform_observability.integration_summary.disconnected",
};

const STATUS_BADGE = {
  HEALTHY: "badge badge-green",
  DEGRADED: "badge badge-orange",
  UNHEALTHY: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

const STATUS_LABEL_KEY = {
  HEALTHY: PLATFORM_OBSERVABILITY_I18N_KEYS.statusHealthy,
  DEGRADED: PLATFORM_OBSERVABILITY_I18N_KEYS.statusDegraded,
  UNHEALTHY: PLATFORM_OBSERVABILITY_I18N_KEYS.statusUnhealthy,
  UNKNOWN: PLATFORM_OBSERVABILITY_I18N_KEYS.statusUnknown,
};

export function healthStatusBadgeClass(status) {
  return STATUS_BADGE[String(status || "UNKNOWN").toUpperCase()] || STATUS_BADGE.UNKNOWN;
}

export function healthStatusLabel(status, t) {
  const key = STATUS_LABEL_KEY[String(status || "UNKNOWN").toUpperCase()] || STATUS_LABEL_KEY.UNKNOWN;
  return t(key);
}

export function mapHealthMetric(metric, t) {
  if (!metric) return null;
  const status = String(metric.status || "UNKNOWN").toUpperCase();
  return {
    ...metric,
    status,
    statusBadgeClass: healthStatusBadgeClass(status),
    statusLabel: healthStatusLabel(status, t),
  };
}

export function mapHealthArea(area, t) {
  if (!area) return null;
  const status = String(area.status || "UNKNOWN").toUpperCase();
  return {
    ...area,
    status,
    statusBadgeClass: healthStatusBadgeClass(status),
    statusLabel: healthStatusLabel(status, t),
    metrics: (area.metrics || []).map((metric) => mapHealthMetric(metric, t)).filter(Boolean),
    integrationSummary: area.integration_summary
      ? {
          healthy: area.integration_summary.healthy ?? 0,
          degraded: area.integration_summary.degraded ?? 0,
          disconnected: area.integration_summary.disconnected ?? 0,
        }
      : null,
  };
}

export function hasPlatformObservabilitySection(report) {
  return report != null;
}

export function isPlatformObservabilityEmpty(report) {
  if (!report) return true;
  const areas = [
    report.api_health,
    report.authentication_health,
    report.integration_health,
    report.report_delivery_health,
    report.incident_investigation_health,
  ];
  const hasSignal = areas.some((area) => String(area?.status || "UNKNOWN").toUpperCase() !== "UNKNOWN");
  const hasRisks = (report.top_platform_risks || []).length > 0;
  return !hasSignal && !hasRisks;
}

export function buildPlatformObservabilityViewModel(report, t) {
  const show = hasPlatformObservabilitySection(report);
  const empty = isPlatformObservabilityEmpty(report);

  return {
    show,
    empty,
    emptyMessage: t(PLATFORM_OBSERVABILITY_I18N_KEYS.empty),
    title: t(PLATFORM_OBSERVABILITY_I18N_KEYS.title),
    readOnlyNote: t(PLATFORM_OBSERVABILITY_I18N_KEYS.readOnlyNote),
    executiveSummaryLabel: t(PLATFORM_OBSERVABILITY_I18N_KEYS.executiveSummary),
    executiveSummary: report?.executive_summary || "",
    topPlatformRisksLabel: t(PLATFORM_OBSERVABILITY_I18N_KEYS.topPlatformRisks),
    topPlatformRisks: report?.top_platform_risks || [],
    apiHealth: mapHealthArea(report?.api_health, t),
    authenticationHealth: mapHealthArea(report?.authentication_health, t),
    integrationHealth: mapHealthArea(report?.integration_health, t),
    reportDeliveryHealth: mapHealthArea(report?.report_delivery_health, t),
    incidentInvestigationHealth: mapHealthArea(report?.incident_investigation_health, t),
    healthSections: [
      {
        key: "authentication",
        title: t(PLATFORM_OBSERVABILITY_I18N_KEYS.authenticationHealth),
        area: mapHealthArea(report?.authentication_health, t),
      },
      {
        key: "integration",
        title: t(PLATFORM_OBSERVABILITY_I18N_KEYS.integrationHealth),
        area: mapHealthArea(report?.integration_health, t),
      },
      {
        key: "report_delivery",
        title: t(PLATFORM_OBSERVABILITY_I18N_KEYS.reportDeliveryHealth),
        area: mapHealthArea(report?.report_delivery_health, t),
      },
      {
        key: "incident_investigation",
        title: t(PLATFORM_OBSERVABILITY_I18N_KEYS.incidentInvestigationHealth),
        area: mapHealthArea(report?.incident_investigation_health, t),
      },
    ].filter((section) => section.area),
    integrationSummaryLabels: {
      healthy: t(PLATFORM_OBSERVABILITY_I18N_KEYS.integrationHealthy),
      degraded: t(PLATFORM_OBSERVABILITY_I18N_KEYS.integrationDegraded),
      disconnected: t(PLATFORM_OBSERVABILITY_I18N_KEYS.integrationDisconnected),
    },
    generated_at: report?.generated_at || null,
  };
}
