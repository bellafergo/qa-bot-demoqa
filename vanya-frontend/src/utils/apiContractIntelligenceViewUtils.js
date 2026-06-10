/** View helpers for API Contract Intelligence (INT-02A). */

export const API_CONTRACT_INTELLIGENCE_I18N_KEYS = {
  title: "incident.qa.api_contract_intelligence",
  empty: "incident.qa.api_contract_intelligence_empty",
  contractChanges: "incident.qa.api_contract_intelligence_changes",
  riskAssessment: "incident.qa.api_contract_intelligence_risk_assessment",
  preview: "incident.qa.api_contract_intelligence_preview",
  previewSubtitle: "incident.qa.api_contract_intelligence_preview_subtitle",
  oldContract: "incident.qa.api_contract_intelligence_old_contract",
  newContract: "incident.qa.api_contract_intelligence_new_contract",
  changeClassification: "incident.qa.api_contract_intelligence_change_classification",
  severity: "incident.qa.api_contract_intelligence_severity",
  deploymentImpact: "incident.qa.api_contract_intelligence_deployment_impact",
  endpoint: "incident.qa.api_contract_intelligence_endpoint",
  method: "incident.qa.api_contract_intelligence_method",
  version: "incident.qa.api_contract_intelligence_version",
  riskLevel: "incident.qa.api_contract_intelligence_risk_level",
  changeCount: "incident.qa.api_contract_intelligence_change_count",
  confidence: "incident.qa.api_contract_intelligence_confidence",
  readOnlyNote: "incident.qa.api_contract_intelligence_read_only_note",
};

const RISK_BADGE = {
  CRITICAL: "badge badge-red",
  HIGH: "badge badge-orange",
  MEDIUM: "badge badge-orange",
  LOW: "badge badge-blue",
};

export function hasApiContractIntelligenceSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "api_contract_intelligence");
}

export function isApiContractIntelligenceEmpty(report) {
  return report?.api_contract_intelligence == null;
}

export function formatContractConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function riskBadgeClass(riskLevel) {
  const key = String(riskLevel || "LOW").toUpperCase();
  return RISK_BADGE[key] || "badge badge-gray";
}

export function schemaProperties(schema) {
  const props = schema?.properties ?? {};
  return Object.fromEntries(
    Object.entries(props).map(([name, def]) => [
      name,
      typeof def === "object" && def != null ? String(def.type || "unknown") : String(def),
    ]),
  );
}

export function contractExampleSnippet(properties) {
  const example = {};
  const entries = Object.entries(properties || {}).sort(([a], [b]) => a.localeCompare(b));
  for (const [name, ptype] of entries) {
    if (ptype === "integer" || ptype === "int") example[name] = 123;
    else if (ptype === "number" || ptype === "float") example[name] = 100;
    else if (ptype === "boolean") example[name] = true;
    else example[name] = "Bella";
  }
  return JSON.stringify(example, null, 2);
}

export function inferOldPropertiesFromChange(change, newProperties) {
  const props = { ...(newProperties || {}) };
  if (change.change_type === "added_field") {
    delete props[change.field_name];
    return props;
  }
  if (change.change_type === "removed_field") {
    props[change.field_name] = "number";
    return props;
  }
  if (change.change_type === "type_change") {
    props[change.field_name] = change.old_value || "number";
    return props;
  }
  if (change.change_type === "rename") {
    delete props[change.field_name];
    if (change.old_value) props[change.old_value] = change.new_value ? "number" : "number";
    return props;
  }
  return props;
}

export function buildContractDrilldownItem(contract, assessment) {
  if (!contract?.contract_id) return null;
  return {
    source: "api_contract",
    related_entity_type: "api_contract",
    related_entity_id: contract.contract_id,
    reason: assessment?.summary || contract.service_name,
    detail: `${contract.method} ${contract.endpoint}`,
    title: contract.service_name,
  };
}

export function buildContractChangePreviewPayload(change, contract, assessment, t) {
  const newProperties = schemaProperties(contract?.request_schema);
  const oldProperties = inferOldPropertiesFromChange(change, newProperties);
  return {
    title: `${contract?.service_name || "API"} — ${change.field_name}`,
    oldSnippet: contractExampleSnippet(oldProperties),
    newSnippet: contractExampleSnippet(newProperties),
    changeType: change.change_type,
    severity: change.severity,
    description: change.description,
    deploymentImpact: assessment?.summary || "—",
    fields: [
      {
        label: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.changeClassification),
        value: change.change_type,
      },
      {
        label: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.severity),
        value: change.severity,
      },
    ],
    readOnlyNote: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.readOnlyNote),
  };
}

export function buildApiContractIntelligenceViewModel(report, t) {
  const intel = report?.api_contract_intelligence ?? null;
  const assessmentsByContractId = new Map(
    (intel?.risk_assessments || []).map((assessment) => [
      String(assessment.assessment_id || "").replace(/^assessment:/, ""),
      assessment,
    ]),
  );

  const contracts = (intel?.contracts || []).map((contract) => {
    const assessment = assessmentsByContractId.get(contract.contract_id) || null;
    const changes = assessment?.changes || [];
    return {
      ...contract,
      assessment,
      riskLevel: assessment?.risk_level || "LOW",
      riskBadgeClass: riskBadgeClass(assessment?.risk_level),
      changeCount: changes.length,
      changes: changes.map((change) => ({
        ...change,
        previewPayload: buildContractChangePreviewPayload(change, contract, assessment, t),
      })),
      drilldownItem: buildContractDrilldownItem(contract, assessment),
    };
  });

  return {
    show: hasApiContractIntelligenceSection(report),
    empty: isApiContractIntelligenceEmpty(report),
    emptyMessage: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.empty),
    title: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.title),
    contractChangesLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.contractChanges),
    riskAssessmentLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.riskAssessment),
    previewLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.preview),
    endpointLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.endpoint),
    methodLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.method),
    versionLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.version),
    riskLevelLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.riskLevel),
    changeCountLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.changeCount),
    confidenceLabel: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.confidence),
    readOnlyNote: t(API_CONTRACT_INTELLIGENCE_I18N_KEYS.readOnlyNote),
    intel: intel
      ? {
          ...intel,
          confidenceText: formatContractConfidence(intel.confidence),
          contracts,
          riskAssessments: intel.risk_assessments || [],
        }
      : null,
  };
}
