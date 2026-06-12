/** View helpers for read-only ServiceNow integration (SNOW-01A). */

export const SERVICENOW_I18N_KEYS = {
  title: "integrations.servicenow.panel_title",
  discoveryTitle: "integrations.servicenow.discovery_title",
  connectionStatus: "integrations.servicenow.connection_status",
  connected: "integrations.servicenow.connected",
  disconnected: "integrations.servicenow.disconnected",
  instanceUrl: "integrations.servicenow.instance_url",
  lastSync: "integrations.servicenow.last_sync",
  incidents: "integrations.servicenow.incidents",
  changes: "integrations.servicenow.changes",
  services: "integrations.servicenow.services",
  cmdb: "integrations.servicenow.cmdb",
  readOnlyNote: "integrations.servicenow.read_only_note",
  emptyConnection: "integrations.servicenow.empty_connection",
  emptyData: "integrations.servicenow.empty_data",
  emptyIncidents: "integrations.servicenow.empty_incidents",
  emptyChanges: "integrations.servicenow.empty_changes",
  emptyServices: "integrations.servicenow.empty_services",
  emptyCmdb: "integrations.servicenow.empty_cmdb",
  refresh: "integrations.servicenow.refresh",
  loading: "integrations.servicenow.loading",
  state: "integrations.servicenow.state",
  priority: "integrations.servicenow.priority",
  assignmentGroup: "integrations.servicenow.assignment_group",
  openedAt: "integrations.servicenow.opened_at",
  risk: "integrations.servicenow.risk",
  plannedStart: "integrations.servicenow.planned_start",
  plannedEnd: "integrations.servicenow.planned_end",
  businessCriticality: "integrations.servicenow.business_criticality",
  operationalStatus: "integrations.servicenow.operational_status",
  className: "integrations.servicenow.class_name",
};

export function formatCount(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "0";
  return n.toLocaleString();
}

export function connectionBadgeClass(connected) {
  return connected ? "badge badge-green" : "badge badge-gray";
}

export function buildConnectionCardViewModel(status, t) {
  const connected = Boolean(status?.connected);
  return {
    title: t(SERVICENOW_I18N_KEYS.connectionStatus),
    connectedLabel: connected ? t(SERVICENOW_I18N_KEYS.connected) : t(SERVICENOW_I18N_KEYS.disconnected),
    connectedBadgeClass: connectionBadgeClass(connected),
    instanceUrlLabel: t(SERVICENOW_I18N_KEYS.instanceUrl),
    instanceUrl: status?.instance_url || "—",
    lastSyncLabel: t(SERVICENOW_I18N_KEYS.lastSync),
    lastSync: status?.last_sync || null,
    counts: [
      { label: t(SERVICENOW_I18N_KEYS.incidents), value: formatCount(status?.incident_count) },
      { label: t(SERVICENOW_I18N_KEYS.changes), value: formatCount(status?.change_count) },
      { label: t(SERVICENOW_I18N_KEYS.services), value: formatCount(status?.service_count) },
      { label: t(SERVICENOW_I18N_KEYS.cmdb), value: formatCount(status?.cmdb_count) },
    ],
    readOnlyNote: t(SERVICENOW_I18N_KEYS.readOnlyNote),
    showEmptyConnection: !connected,
    emptyConnectionText: t(SERVICENOW_I18N_KEYS.emptyConnection),
  };
}

export function buildIncidentCardViewModel(incident) {
  if (!incident?.number) return null;
  return {
    number: incident.number,
    shortDescription: incident.short_description || "—",
    state: incident.state || "—",
    priority: incident.priority || "—",
    assignmentGroup: incident.assignment_group || "—",
    openedAt: incident.opened_at || null,
  };
}

export function buildChangeCardViewModel(change) {
  if (!change?.number) return null;
  return {
    number: change.number,
    shortDescription: change.short_description || "—",
    state: change.state || "—",
    risk: change.risk || "—",
    plannedStart: change.planned_start || null,
    plannedEnd: change.planned_end || null,
  };
}

function buildListItem(key, label, meta) {
  if (!key) return null;
  return { key, label: label || key, meta: meta || null };
}

function buildDiscoverySection({ label, items, emptyText, connected }) {
  return {
    label,
    items,
    showEmpty: connected && items.length === 0,
    emptyText,
  };
}

export function buildServiceNowIntegrationViewModel({
  status,
  incidents,
  changes,
  services,
  cmdbItems,
  t,
}) {
  const connection = buildConnectionCardViewModel(status, t);
  const connected = Boolean(status?.connected);
  const incidentItems = (incidents || []).map(buildIncidentCardViewModel).filter(Boolean);
  const changeItems = (changes || []).map(buildChangeCardViewModel).filter(Boolean);
  const serviceItems = (services || [])
    .map((s) => buildListItem(s.name, s.name, `${s.business_criticality || "—"} · ${s.operational_status || "—"}`))
    .filter(Boolean);
  const cmdbListItems = (cmdbItems || [])
    .map((c) => buildListItem(c.name, c.name, `${c.class_name || "—"} · ${c.operational_status || "—"}`))
    .filter(Boolean);

  return {
    connection,
    incidents: incidentItems,
    changes: changeItems,
    services: buildDiscoverySection({
      label: t(SERVICENOW_I18N_KEYS.services),
      items: serviceItems,
      emptyText: t(SERVICENOW_I18N_KEYS.emptyServices),
      connected,
    }),
    cmdb: buildDiscoverySection({
      label: t(SERVICENOW_I18N_KEYS.cmdb),
      items: cmdbListItems,
      emptyText: t(SERVICENOW_I18N_KEYS.emptyCmdb),
      connected,
    }),
    showEmptyIncidents: connected && incidentItems.length === 0,
    emptyIncidentsText: t(SERVICENOW_I18N_KEYS.emptyIncidents),
    showEmptyChanges: connected && changeItems.length === 0,
    emptyChangesText: t(SERVICENOW_I18N_KEYS.emptyChanges),
    loadingLabel: t(SERVICENOW_I18N_KEYS.loading),
    refreshLabel: t(SERVICENOW_I18N_KEYS.refresh),
    incidentsLabel: t(SERVICENOW_I18N_KEYS.incidents),
    changesLabel: t(SERVICENOW_I18N_KEYS.changes),
    showDisconnected: !connected && !status,
    emptyDataText: t(SERVICENOW_I18N_KEYS.emptyData),
  };
}

export function deriveServiceNowHeaderState(status) {
  if (!status?.connected) {
    return { enabled: false, health: "unconfigured", labelKey: "integrations.health.unconfigured" };
  }
  return { enabled: true, health: "ok", labelKey: "integrations.servicenow.header_connected" };
}
