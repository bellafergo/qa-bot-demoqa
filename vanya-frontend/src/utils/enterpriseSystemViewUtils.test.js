import { describe, it, expect } from "vitest";
import {
  ENTERPRISE_SYSTEM_I18N_KEYS,
  buildEnterpriseSystemViewModel,
  buildValidationPreviewPayload,
  connectorStatusBadgeClass,
  formatValidationType,
  validationStatusBadgeClass,
} from "./enterpriseSystemViewUtils.js";

const t = (key) => key;

const sampleReport = {
  summary: "1 enterprise system(s), 3 module(s), 1 validation request(s) — read-only inventory only.",
  connectors: [
    {
      connector_id: "erpconn:agent:sap_ecc",
      agent_id: "agent-1",
      system_name: "SAP ECC",
      system_type: "SAP_ECC",
      environment: "QA",
      status: "ACTIVE",
      read_only: true,
    },
  ],
  modules: [
    { module_id: "erpmodule:mm", connector_id: "erpconn:agent:sap_ecc", module_name: "MM", category: "logistics", criticality: "HIGH" },
    { module_id: "erpmodule:sd", connector_id: "erpconn:agent:sap_ecc", module_name: "SD", category: "sales", criticality: "HIGH" },
    { module_id: "erpmodule:fi", connector_id: "erpconn:agent:sap_ecc", module_name: "FI", category: "finance", criticality: "CRITICAL" },
  ],
  validations: [
    {
      request_id: "erpvalid:1",
      connector_id: "erpconn:agent:sap_ecc",
      module_id: "erpmodule:sd",
      validation_type: "interface_validation",
      status: "PENDING",
      requires_user_approval: true,
    },
  ],
};

describe("enterpriseSystemViewUtils", () => {
  it("renders enterprise systems", () => {
    const vm = buildEnterpriseSystemViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.connectors).toHaveLength(1);
    expect(vm.connectors[0].modules).toHaveLength(3);
  });

  it("renders empty state", () => {
    const vm = buildEnterpriseSystemViewModel({ connectors: [], modules: [], validations: [] }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(ENTERPRISE_SYSTEM_I18N_KEYS.empty);
  });

  it("renders connector cards", () => {
    const vm = buildEnterpriseSystemViewModel(sampleReport, t);
    expect(vm.connectors[0].system_name).toBe("SAP ECC");
    expect(connectorStatusBadgeClass("ACTIVE")).toContain("badge-green");
  });

  it("renders module cards", () => {
    const vm = buildEnterpriseSystemViewModel(sampleReport, t);
    expect(vm.connectors[0].modules[0].module_name).toBe("MM");
  });

  it("renders validation cards", () => {
    const vm = buildEnterpriseSystemViewModel(sampleReport, t);
    expect(vm.validations[0].validationTypeLabel).toBe("Interface Validation");
    expect(validationStatusBadgeClass("PENDING")).toContain("badge-orange");
  });

  it("disables execution in preview payload", () => {
    const vm = buildEnterpriseSystemViewModel(sampleReport, t);
    expect(vm.validations[0].previewPayload.canExecute).toBe(false);
    expect(vm.executeDisabledNote).toBe(ENTERPRISE_SYSTEM_I18N_KEYS.executeDisabledNote);
  });

  it("maps translation keys", () => {
    const vm = buildEnterpriseSystemViewModel(sampleReport, t);
    expect(vm.title).toBe(ENTERPRISE_SYSTEM_I18N_KEYS.title);
    expect(formatValidationType("data_consistency_validation")).toBe("Data Consistency Validation");
    const payload = buildValidationPreviewPayload(
      sampleReport.validations[0],
      sampleReport.modules[1],
      sampleReport.connectors[0],
      t,
    );
    expect(payload.approvalRequired).toBe(true);
  });
});
