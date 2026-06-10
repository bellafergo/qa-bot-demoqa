/** View helpers for Enterprise Systems Connectors (INT-03D). */

export const ENTERPRISE_SYSTEM_I18N_KEYS = {
  title: "localAgents.enterpriseSystems.title",
  modules: "localAgents.enterpriseSystems.modules",
  validations: "localAgents.enterpriseSystems.validations",
  approvalRequired: "localAgents.enterpriseSystems.approval_required",
  empty: "localAgents.enterpriseSystems.empty",
  previewValidation: "localAgents.enterpriseSystems.preview_validation",
  executeValidation: "localAgents.enterpriseSystems.execute_validation",
  executeDisabledNote: "localAgents.enterpriseSystems.execute_disabled_note",
  environment: "localAgents.enterpriseSystems.environment",
  systemType: "localAgents.enterpriseSystems.system_type",
  validationType: "localAgents.enterpriseSystems.validation_type",
  readOnlyNote: "localAgents.enterpriseSystems.read_only_note",
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

const CRITICALITY_BADGE = {
  CRITICAL: "badge badge-red",
  HIGH: "badge badge-orange",
  MEDIUM: "badge badge-blue",
  LOW: "badge badge-gray",
};

export function connectorStatusBadgeClass(status) {
  return CONNECTOR_STATUS_BADGE[String(status || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

export function validationStatusBadgeClass(status) {
  return VALIDATION_STATUS_BADGE[String(status || "PENDING").toUpperCase()] || "badge badge-gray";
}

export function criticalityBadgeClass(criticality) {
  return CRITICALITY_BADGE[String(criticality || "MEDIUM").toUpperCase()] || "badge badge-gray";
}

export function formatSystemType(value) {
  return String(value || "OTHER").replace(/_/g, " ");
}

export function formatValidationType(value) {
  return String(value || "")
    .replace(/_/g, " ")
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

export function buildModuleDrilldownItem(module, connector) {
  if (!module?.module_id) return null;
  return {
    source: "enterprise_system",
    related_entity_type: "enterprise_module",
    related_entity_id: module.module_id,
    reason: connector?.system_name || module.module_name,
    detail: module.module_name,
    title: module.module_name,
  };
}

export function buildValidationPreviewPayload(validation, module, connector, t) {
  return {
    title: t(ENTERPRISE_SYSTEM_I18N_KEYS.previewValidation),
    systemName: connector?.system_name || "—",
    moduleName: module?.module_name || "—",
    validationType: formatValidationType(validation?.validation_type),
    status: validation?.status || "PENDING",
    approvalRequired: Boolean(validation?.requires_user_approval),
    approvalRequiredLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.approvalRequired),
    readOnlyNote: t(ENTERPRISE_SYSTEM_I18N_KEYS.readOnlyNote),
    executeLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.executeValidation),
    executeDisabledNote: t(ENTERPRISE_SYSTEM_I18N_KEYS.executeDisabledNote),
    canExecute: false,
    drilldownItem: module ? buildModuleDrilldownItem(module, connector) : null,
  };
}

export function buildEnterpriseSystemViewModel(report, t) {
  const connectors = report?.connectors || [];
  const modules = report?.modules || [];
  const validations = report?.validations || [];

  const modulesByConnector = new Map();
  for (const module of modules) {
    const list = modulesByConnector.get(module.connector_id) || [];
    list.push({
      ...module,
      criticalityBadgeClass: criticalityBadgeClass(module.criticality),
      drilldownItem: buildModuleDrilldownItem(
        module,
        connectors.find((c) => c.connector_id === module.connector_id),
      ),
    });
    modulesByConnector.set(module.connector_id, list);
  }

  const connectorById = new Map(connectors.map((c) => [c.connector_id, c]));

  return {
    show: true,
    empty: connectors.length === 0,
    emptyMessage: t(ENTERPRISE_SYSTEM_I18N_KEYS.empty),
    title: t(ENTERPRISE_SYSTEM_I18N_KEYS.title),
    modulesLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.modules),
    validationsLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.validations),
    readOnlyNote: t(ENTERPRISE_SYSTEM_I18N_KEYS.readOnlyNote),
    previewValidationLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.previewValidation),
    executeValidationLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.executeValidation),
    executeDisabledNote: t(ENTERPRISE_SYSTEM_I18N_KEYS.executeDisabledNote),
    approvalRequiredLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.approvalRequired),
    environmentLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.environment),
    systemTypeLabel: t(ENTERPRISE_SYSTEM_I18N_KEYS.systemType),
    connectors: connectors.map((connector) => ({
      ...connector,
      systemTypeLabel: formatSystemType(connector.system_type),
      statusBadgeClass: connectorStatusBadgeClass(connector.status),
      modules: modulesByConnector.get(connector.connector_id) || [],
    })),
    validations: validations.map((validation) => {
      const connector = connectorById.get(validation.connector_id);
      const module = modules.find((m) => m.module_id === validation.module_id);
      return {
        ...validation,
        statusBadgeClass: validationStatusBadgeClass(validation.status),
        validationTypeLabel: formatValidationType(validation.validation_type),
        systemName: connector?.system_name || validation.connector_id,
        moduleName: module?.module_name || validation.module_id,
        previewPayload: buildValidationPreviewPayload(validation, module, connector, t),
      };
    }),
    summary: report?.summary || "",
  };
}
