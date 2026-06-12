/** View helpers for ServiceNow Intelligence (SNOW-01B). */

export const SERVICENOW_INTELLIGENCE_I18N_KEYS = {
  title: "servicenow_intelligence.title",
  dashboardTitle: "servicenow_intelligence.dashboard_title",
  incidentTitle: "incident.qa.servicenow_intelligence",
  operationalRisks: "servicenow_intelligence.operational_risks",
  incidents: "servicenow_intelligence.incidents",
  changes: "servicenow_intelligence.changes",
  services: "servicenow_intelligence.services",
  cmdb: "servicenow_intelligence.cmdb",
  executiveSummary: "servicenow_intelligence.executive_summary",
  correlationReason: "servicenow_intelligence.correlation_reason",
  capability: "servicenow_intelligence.capability",
  confidence: "servicenow_intelligence.confidence",
  emptyConnection: "servicenow_intelligence.empty_connection",
  emptyCorrelations: "servicenow_intelligence.empty_correlations",
  readOnlyNote: "servicenow_intelligence.read_only_note",
};

const CONFIDENCE_BADGE = {
  HIGH: "badge badge-red",
  MEDIUM: "badge badge-orange",
  LOW: "badge badge-gray",
};

export function hasServiceNowIntelligenceSection(report) {
  return report?.servicenow_intelligence != null;
}

export function isServiceNowIntelligenceEmpty(report) {
  const intel = report?.servicenow_intelligence;
  if (!intel) return true;
  if (!intel.connected) return true;
  const total = (
    (intel.incident_correlations?.length || 0)
    + (intel.change_correlations?.length || 0)
    + (intel.service_correlations?.length || 0)
    + (intel.cmdb_correlations?.length || 0)
  );
  return total === 0;
}

export function hasStandaloneServiceNowIntelligence(report) {
  return report != null;
}

export function isStandaloneServiceNowIntelligenceEmpty(report) {
  if (!report) return true;
  if (!report.connected) return true;
  return isServiceNowIntelligenceEmpty({ servicenow_intelligence: report });
}

export function buildCorrelationCardViewModel(correlation, t) {
  if (!correlation?.entity_id) return null;
  const confidence = String(correlation.confidence || "LOW").toUpperCase();
  return {
    entityType: correlation.entity_type || "—",
    entityId: correlation.entity_id,
    capability: correlation.capability || "—",
    capabilityLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.capability),
    reason: correlation.correlation_reason || "—",
    reasonLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.correlationReason),
    confidence,
    confidenceLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.confidence),
    confidenceBadgeClass: CONFIDENCE_BADGE[confidence] || CONFIDENCE_BADGE.LOW,
  };
}

function buildIntelligenceViewModelCore(intel, t, { titleKey }) {
  const connected = Boolean(intel?.connected);
  const incidents = (intel?.incident_correlations || [])
    .map((c) => buildCorrelationCardViewModel(c, t))
    .filter(Boolean);
  const changes = (intel?.change_correlations || [])
    .map((c) => buildCorrelationCardViewModel(c, t))
    .filter(Boolean);
  const services = (intel?.service_correlations || [])
    .map((c) => buildCorrelationCardViewModel(c, t))
    .filter(Boolean);
  const cmdb = (intel?.cmdb_correlations || [])
    .map((c) => buildCorrelationCardViewModel(c, t))
    .filter(Boolean);

  let emptyMessage = null;
  if (!connected) emptyMessage = t(SERVICENOW_INTELLIGENCE_I18N_KEYS.emptyConnection);
  else if (incidents.length + changes.length + services.length + cmdb.length === 0) {
    emptyMessage = t(SERVICENOW_INTELLIGENCE_I18N_KEYS.emptyCorrelations);
  }

  return {
    title: t(titleKey),
    show: true,
    empty: Boolean(emptyMessage),
    emptyMessage,
    connected,
    executiveSummary: intel?.executive_summary || "",
    executiveSummaryLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.executiveSummary),
    operationalRisksLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.operationalRisks),
    operationalRisks: intel?.top_operational_risks || [],
    incidentsLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.incidents),
    changesLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.changes),
    servicesLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.services),
    cmdbLabel: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.cmdb),
    incidents,
    changes,
    services,
    cmdb,
    readOnlyNote: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.readOnlyNote),
    showOperationalRisks: connected && (intel?.top_operational_risks?.length || 0) > 0,
    showIncidents: connected && incidents.length > 0,
    showChanges: connected && changes.length > 0,
    showServices: connected && services.length > 0,
    showCmdb: connected && cmdb.length > 0,
  };
}

export function buildServiceNowIntelligenceViewModel(report, t) {
  if (!hasServiceNowIntelligenceSection(report)) {
    return { show: false };
  }
  return buildIntelligenceViewModelCore(report.servicenow_intelligence, t, {
    titleKey: SERVICENOW_INTELLIGENCE_I18N_KEYS.incidentTitle,
  });
}

export function buildServiceNowIntelligenceOverviewViewModel(intel, t) {
  if (!hasStandaloneServiceNowIntelligence(intel)) {
    return { show: false, empty: true, emptyMessage: t(SERVICENOW_INTELLIGENCE_I18N_KEYS.emptyConnection) };
  }
  const vm = buildIntelligenceViewModelCore(intel, t, {
    titleKey: SERVICENOW_INTELLIGENCE_I18N_KEYS.dashboardTitle,
  });
  return {
    ...vm,
    show: true,
    empty: isStandaloneServiceNowIntelligenceEmpty(intel),
  };
}
