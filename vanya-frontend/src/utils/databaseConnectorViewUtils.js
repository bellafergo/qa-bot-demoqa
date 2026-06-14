/** View helpers for secure database connectors (INT-03B). */

/** Pick agent for customer connection: selected → first with db cap → first project agent. */
export function resolveTargetAgentId(agents, selectedAgentId) {
  const list = Array.isArray(agents) ? agents : [];
  if (selectedAgentId && list.some((a) => a.agent_id === selectedAgentId)) {
    return selectedAgentId;
  }
  const withDbCap = list.find((a) =>
    (a.capabilities || []).some((c) => String(c).toLowerCase() === "database_validation"),
  );
  return (withDbCap || list[0])?.agent_id || null;
}

export const DATABASE_CONNECTOR_I18N_KEYS = {
  connectionsTitle: "localAgents.database.connections_title",
  connectionsEmptyTitle: "localAgents.database.connections_empty_title",
  connectionsEmptyDesc: "localAgents.database.connections_empty_desc",
  registerPlatformAssets: "localAgents.database.register_platform_assets",
  connectCustomerDatabase: "localAgents.database.connect_customer_database",
  toastPlatformRegistered: "localAgents.database.toast.platform_registered",
  toastNoAgent: "localAgents.database.toast.no_agent",
  executionsTitle: "localAgents.database.executions_title",
  executeValidation: "incident.qa.database_validation_execute",
  validationResult: "incident.qa.database_validation_result",
  approvalRequired: "incident.qa.database_validation_approval_required",
  connected: "localAgents.database.connected",
  disconnected: "localAgents.database.disconnected",
  degraded: "localAgents.database.degraded",
  error: "localAgents.database.error",
  pendingValidation: "localAgents.database.pending_validation",
  blocked: "localAgents.database.blocked",
  unknown: "localAgents.foundation.unknown",
  name: "localAgents.database.name",
  type: "localAgents.database.type",
  agent: "localAgents.database.agent",
  status: "localAgents.col.status",
  check: "localAgents.database.check",
  timestamp: "localAgents.database.timestamp",
  resultSummary: "localAgents.database.result_summary",
  simulateApprove: "localAgents.database.simulate_approve",
  readOnlyNote: "localAgents.database.read_only_note",
  assetScopePlatform: "localAgents.database.asset_scope_platform",
  assetScopeCustomer: "localAgents.database.asset_scope_customer",
  executionModePlatform: "localAgents.database.execution_mode_platform",
  executionModeLocalAgent: "localAgents.database.execution_mode_local_agent",
  lastProbe: "localAgents.database.last_probe",
};

const CONNECTION_STATUS_BADGE = {
  CONNECTED: "badge badge-green",
  DISCONNECTED: "badge badge-gray",
  UNKNOWN: "badge badge-orange",
  DEGRADED: "badge badge-orange",
  ERROR: "badge badge-red",
  PENDING_VALIDATION: "badge badge-gray",
};

const EXECUTION_STATUS_BADGE = {
  SUCCESS: "badge badge-green",
  FAILED: "badge badge-red",
  BLOCKED: "badge badge-orange",
  REJECTED: "badge badge-red",
};

export function connectionStatusBadgeClass(status) {
  return CONNECTION_STATUS_BADGE[String(status || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

export function executionStatusBadgeClass(status) {
  return EXECUTION_STATUS_BADGE[String(status || "FAILED").toUpperCase()] || "badge badge-gray";
}

export function connectionStatusLabel(status, t) {
  const key = String(status || "UNKNOWN").toUpperCase();
  if (key === "CONNECTED") return t(DATABASE_CONNECTOR_I18N_KEYS.connected);
  if (key === "DISCONNECTED") return t(DATABASE_CONNECTOR_I18N_KEYS.disconnected);
  if (key === "DEGRADED") return t(DATABASE_CONNECTOR_I18N_KEYS.degraded);
  if (key === "ERROR") return t(DATABASE_CONNECTOR_I18N_KEYS.error);
  if (key === "PENDING_VALIDATION") return t(DATABASE_CONNECTOR_I18N_KEYS.pendingValidation);
  return t(DATABASE_CONNECTOR_I18N_KEYS.unknown);
}

export function assetScopeLabel(scope, t) {
  return String(scope || "") === "platform_internal"
    ? t(DATABASE_CONNECTOR_I18N_KEYS.assetScopePlatform)
    : t(DATABASE_CONNECTOR_I18N_KEYS.assetScopeCustomer);
}

export function executionModeLabel(mode, t) {
  return String(mode || "") === "platform_backend"
    ? t(DATABASE_CONNECTOR_I18N_KEYS.executionModePlatform)
    : t(DATABASE_CONNECTOR_I18N_KEYS.executionModeLocalAgent);
}

export function executionStatusLabel(status, t) {
  const key = String(status || "FAILED").toUpperCase();
  if (key === "BLOCKED") return t(DATABASE_CONNECTOR_I18N_KEYS.blocked);
  return key;
}

export function buildDatabaseConnectionsViewModel(connections, t, formatTimestamp) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  return {
    title: t(DATABASE_CONNECTOR_I18N_KEYS.connectionsTitle),
    empty: !connections?.length,
    emptyTitle: t(DATABASE_CONNECTOR_I18N_KEYS.connectionsEmptyTitle),
    emptyDesc: t(DATABASE_CONNECTOR_I18N_KEYS.connectionsEmptyDesc),
    registerPlatformAssetsLabel: t(DATABASE_CONNECTOR_I18N_KEYS.registerPlatformAssets),
    connectCustomerDatabaseLabel: t(DATABASE_CONNECTOR_I18N_KEYS.connectCustomerDatabase),
    connections: (connections || []).map((conn) => ({
      ...conn,
      statusBadgeClass: connectionStatusBadgeClass(conn.status),
      statusLabel: connectionStatusLabel(conn.status, t),
      assetScopeLabel: assetScopeLabel(conn.asset_scope, t),
      executionModeLabel: executionModeLabel(conn.execution_mode, t),
      lastProbeText: fmt(conn.last_probe_at),
      createdAtText: fmt(conn.created_at),
    })),
  };
}

export function buildDatabaseExecutionsViewModel(executions, t, formatTimestamp) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  return {
    title: t(DATABASE_CONNECTOR_I18N_KEYS.executionsTitle),
    empty: !executions?.length,
    executions: (executions || []).map((item) => ({
      ...item,
      statusBadgeClass: executionStatusBadgeClass(item.status),
      statusLabel: executionStatusLabel(item.status, t),
      executedAtText: fmt(item.executed_at),
    })),
  };
}

export function pickConnectionForCheck(connections, check) {
  const dbType = String(check?.database_type || "").toLowerCase();
  return (
    (connections || []).find((c) => String(c.database_type || "").toLowerCase() === dbType) ||
    (connections || [])[0] ||
    null
  );
}

export function buildExecuteValidationPreviewPayload(check, connection, approvalStatus, t) {
  const approved = String(approvalStatus || "PENDING").toUpperCase() === "APPROVED";
  return {
    title: check?.name || t(DATABASE_CONNECTOR_I18N_KEYS.executeValidation),
    checkName: check?.name || "—",
    connectionName: connection?.name || "—",
    connectionLabel: connection?.host_label || "—",
    approvalStatus: String(approvalStatus || "PENDING").toUpperCase(),
    approvalRequired: !approved,
    approvalRequiredLabel: t(DATABASE_CONNECTOR_I18N_KEYS.approvalRequired),
    canExecute: approved && Boolean(connection),
    readOnlyNote: t(DATABASE_CONNECTOR_I18N_KEYS.readOnlyNote),
    simulateApproveLabel: t(DATABASE_CONNECTOR_I18N_KEYS.simulateApprove),
    executeLabel: t(DATABASE_CONNECTOR_I18N_KEYS.executeValidation),
    resultLabel: t(DATABASE_CONNECTOR_I18N_KEYS.validationResult),
  };
}
