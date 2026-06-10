import { describe, it, expect } from "vitest";
import {
  API_CONTRACT_INTELLIGENCE_I18N_KEYS,
  buildApiContractIntelligenceViewModel,
  buildContractChangePreviewPayload,
  buildContractDrilldownItem,
  contractExampleSnippet,
  formatContractConfidence,
  hasApiContractIntelligenceSection,
  isApiContractIntelligenceEmpty,
  riskBadgeClass,
  schemaProperties,
} from "./apiContractIntelligenceViewUtils.js";

const t = (key) => key;

const sampleContract = {
  contract_id: "contract:payments_api_post_payments_vv2",
  service_name: "Payments API",
  endpoint: "/payments",
  method: "POST",
  version: "v2",
  request_schema: {
    type: "object",
    properties: {
      customer_id: { type: "integer" },
      total_amount: { type: "number" },
    },
  },
  response_schema: {},
  source: "impact_map",
  confidence: 0.8,
};

const sampleAssessment = {
  assessment_id: "assessment:contract:payments_api_post_payments_vv2",
  risk_level: "HIGH",
  confidence: 0.8,
  summary: "Payment contract removed required field 'amount'.",
  changes: [
    {
      change_id: "change:contract:payments_api_post_payments_vv2:rename:total_amount",
      severity: "HIGH",
      change_type: "rename",
      field_name: "total_amount",
      old_value: "amount",
      new_value: "total_amount",
      description: "Renamed field 'amount' to 'total_amount'.",
    },
  ],
};

describe("apiContractIntelligenceViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasApiContractIntelligenceSection({ api_contract_intelligence: null })).toBe(true);
    expect(hasApiContractIntelligenceSection({})).toBe(false);
    expect(isApiContractIntelligenceEmpty({ api_contract_intelligence: null })).toBe(true);
  });

  it("renders contracts and assessments", () => {
    const vm = buildApiContractIntelligenceViewModel(
      {
        api_contract_intelligence: {
          summary: "1 API contract modeled.",
          confidence: 0.8,
          contracts: [sampleContract],
          risk_assessments: [sampleAssessment],
        },
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.intel.contracts[0].service_name).toBe("Payments API");
    expect(vm.intel.contracts[0].changeCount).toBe(1);
  });

  it("renders empty state via i18n key", () => {
    const vm = buildApiContractIntelligenceViewModel({ api_contract_intelligence: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.api_contract_intelligence_empty");
  });

  it("maps risk badges and change counts", () => {
    const vm = buildApiContractIntelligenceViewModel(
      {
        api_contract_intelligence: {
          contracts: [sampleContract],
          risk_assessments: [sampleAssessment],
          summary: "",
          confidence: 0.8,
        },
      },
      t,
    );
    expect(vm.intel.contracts[0].riskLevel).toBe("HIGH");
    expect(riskBadgeClass("CRITICAL")).toBe("badge badge-red");
    expect(vm.intel.contracts[0].changeCount).toBe(1);
  });

  it("builds preview modal payload", () => {
    const change = sampleAssessment.changes[0];
    const payload = buildContractChangePreviewPayload(change, sampleContract, sampleAssessment, t);
    expect(payload.oldSnippet).toContain("amount");
    expect(payload.newSnippet).toContain("total_amount");
    expect(payload.changeType).toBe("rename");
    expect(payload.deploymentImpact).toContain("amount");
  });

  it("exposes translation keys", () => {
    expect(API_CONTRACT_INTELLIGENCE_I18N_KEYS.title).toBe("incident.qa.api_contract_intelligence");
    expect(API_CONTRACT_INTELLIGENCE_I18N_KEYS.preview).toBe("incident.qa.api_contract_intelligence_preview");
    expect(API_CONTRACT_INTELLIGENCE_I18N_KEYS.contractChanges).toBe(
      "incident.qa.api_contract_intelligence_changes",
    );
  });

  it("builds drilldown integration item", () => {
    const item = buildContractDrilldownItem(sampleContract, sampleAssessment);
    expect(item.related_entity_type).toBe("api_contract");
    expect(item.related_entity_id).toBe(sampleContract.contract_id);
    const vm = buildApiContractIntelligenceViewModel(
      {
        api_contract_intelligence: {
          contracts: [sampleContract],
          risk_assessments: [sampleAssessment],
          summary: "",
          confidence: 0.8,
        },
      },
      t,
    );
    expect(vm.intel.contracts[0].drilldownItem.related_entity_id).toBe(sampleContract.contract_id);
  });

  it("formats schema snippets and confidence", () => {
    const props = schemaProperties(sampleContract.request_schema);
    expect(props.total_amount).toBe("number");
    expect(contractExampleSnippet(props)).toContain('"total_amount": 100');
    expect(formatContractConfidence(0.8)).toBe("80%");
  });
});
