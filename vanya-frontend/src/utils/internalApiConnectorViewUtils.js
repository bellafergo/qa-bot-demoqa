/** View helpers for Internal API Connectors (INT-03C). */

export const INTERNAL_API_CONNECTOR_I18N_KEYS = {
  title: "localAgents.internalApi.title",
  endpoints: "localAgents.internalApi.endpoints",
  validations: "localAgents.internalApi.validations",
  approvalRequired: "localAgents.internalApi.approval_required",
  readOnly: "localAgents.internalApi.read_only",
  empty: "localAgents.internalApi.empty",
  previewValidation: "localAgents.internalApi.preview_validation",
  executeValidation: "localAgents.internalApi.execute_validation",
  sendDisabledNote: "localAgents.internalApi.execute_disabled_note",
  environment: "localAgents.internalApi.environment",
  apiType: "localAgents.internalApi.api_type",
  status: "localAgents.foundation.unknown",
  endpointCount: "localAgents.internalApi.endpoint_count",
  validationType: "localAgents.internalApi.validation_type",
  blocked: "localAgents.internalApi.blocked",
  pending: "localAgents.internalApi.pending",
  readOnlyNote: "localAgents.internalApi.read_only_note",
};

const CONNECTOR_STATUS_BADGE = {
  ACTIVE: "badge badge-green",
  INACTIVE: "badge badge-gray",
  UNKNOWN: "badge badge-orange",
};

const VALIDATION_STATUS_BADGE = {
  PENDING: "badge badge-orange",
  APPROVED: "badge badge-green",
  BLOCKED: "badge badge-red",
  COMPLETED: "badge badge-blue",
};

export function connectorStatusBadgeClass(status) {
  return CONNECTOR_STATUS_BADGE[String(status || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

export function validationStatusBadgeClass(status) {
  return VALIDATION_STATUS_BADGE[String(status || "PENDING").toUpperCase()] || "badge badge-gray";
}

export function formatValidationType(value) {
  return String(value || "")
    .replace(/_/g, " ")
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

export function buildEndpointDrilldownItem(endpoint, connector) {
  if (!endpoint?.endpoint_id) return null;
  return {
    source: "internal_api_connector",
    related_entity_type: "api_endpoint",
    related_entity_id: endpoint.endpoint_id,
    reason: connector?.name || endpoint.path_label,
    detail: `${endpoint.method} ${endpoint.path_label}`,
    title: connector?.name || endpoint.path_label,
  };
}

export function buildValidationPreviewPayload(validation, endpoint, connector, t) {
  const blocked = String(validation?.status || "").toUpperCase() === "BLOCKED";
  const mutating = endpoint && !endpoint.read_only;
  return {
    title: t(INTERNAL_API_CONNECTOR_I18N_KEYS.previewValidation),
    connectorName: connector?.name || "—",
    endpointLabel: endpoint ? `${endpoint.method} ${endpoint.path_label}` : "—",
    validationType: formatValidationType(validation?.validation_type),
    status: validation?.status || "PENDING",
    approvalRequired: Boolean(validation?.requires_user_approval),
    approvalRequiredLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.approvalRequired),
    readOnlyLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.readOnly),
    blockedLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.blocked),
    readOnlyNote: t(INTERNAL_API_CONNECTOR_I18N_KEYS.readOnlyNote),
    executeLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.executeValidation),
    executeDisabledNote: t(INTERNAL_API_CONNECTOR_I18N_KEYS.sendDisabledNote),
    canExecute: false,
    mutatingBlocked: blocked || mutating,
    drilldownItem: endpoint ? buildEndpointDrilldownItem(endpoint, connector) : null,
  };
}

export function buildInternalApiConnectorViewModel(report, t) {
  const connectors = report?.connectors || [];
  const endpoints = report?.endpoints || [];
  const validations = report?.validations || [];
  const endpointsByConnector = new Map();
  for (const endpoint of endpoints) {
    const list = endpointsByConnector.get(endpoint.connector_id) || [];
    list.push({
      ...endpoint,
      readOnlyLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.readOnly),
      blockedLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.blocked),
      drilldownItem: buildEndpointDrilldownItem(
        endpoint,
        connectors.find((c) => c.connector_id === endpoint.connector_id),
      ),
    });
    endpointsByConnector.set(endpoint.connector_id, list);
  }

  const connectorById = new Map(connectors.map((c) => [c.connector_id, c]));

  return {
    show: true,
    empty: connectors.length === 0,
    emptyMessage: t(INTERNAL_API_CONNECTOR_I18N_KEYS.empty),
    title: t(INTERNAL_API_CONNECTOR_I18N_KEYS.title),
    endpointsLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.endpoints),
    validationsLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.validations),
    readOnlyNote: t(INTERNAL_API_CONNECTOR_I18N_KEYS.readOnlyNote),
    readOnlyLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.readOnly),
    blockedLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.blocked),
    previewValidationLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.previewValidation),
    executeValidationLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.executeValidation),
    executeDisabledNote: t(INTERNAL_API_CONNECTOR_I18N_KEYS.sendDisabledNote),
    approvalRequiredLabel: t(INTERNAL_API_CONNECTOR_I18N_KEYS.approvalRequired),
    connectors: connectors.map((connector) => ({
      ...connector,
      statusBadgeClass: connectorStatusBadgeClass(connector.status),
      endpoints: endpointsByConnector.get(connector.connector_id) || [],
    })),
    validations: validations.map((validation) => {
      const connector = connectorById.get(validation.connector_id);
      const endpoint = endpoints.find((e) => e.endpoint_id === validation.endpoint_id);
      return {
        ...validation,
        statusBadgeClass: validationStatusBadgeClass(validation.status),
        validationTypeLabel: formatValidationType(validation.validation_type),
        connectorName: connector?.name || validation.connector_id,
        endpointLabel: endpoint ? `${endpoint.method} ${endpoint.path_label}` : validation.endpoint_id,
        previewPayload: buildValidationPreviewPayload(validation, endpoint, connector, t),
      };
    }),
    summary: report?.summary || "",
  };
}
