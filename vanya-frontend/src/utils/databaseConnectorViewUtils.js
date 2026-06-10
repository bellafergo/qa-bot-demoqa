/** View helpers for secure database connectors (INT-03B). */

export const DATABASE_CONNECTOR_I18N_KEYS = {
  connectionsTitle: "localAgents.database.connections_title",
  executionsTitle: "localAgents.database.executions_title",
  executeValidation: "incident.qa.database_validation_execute",
  validationResult: "incident.qa.database_validation_result",
  approvalRequired: "incident.qa.database_validation_approval_required",
  connected: "localAgents.database.connected",
  disconnected: "localAgents.database.disconnected",
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
};

const CONNECTION_STATUS_BADGE = {
  CONNECTED: "badge badge-green",
  DISCONNECTED: "badge badge-gray",
  UNKNOWN: "badge badge-orange",
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
  return t(DATABASE_CONNECTOR_I18N_KEYS.unknown);
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
    connections: (connections || []).map((conn) => ({
      ...conn,
      statusBadgeClass: connectionStatusBadgeClass(conn.status),
      statusLabel: connectionStatusLabel(conn.status, t),
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
