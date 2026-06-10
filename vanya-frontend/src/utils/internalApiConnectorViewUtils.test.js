import { describe, it, expect } from "vitest";
import {
  INTERNAL_API_CONNECTOR_I18N_KEYS,
  buildInternalApiConnectorViewModel,
  buildValidationPreviewPayload,
  connectorStatusBadgeClass,
  formatValidationType,
  validationStatusBadgeClass,
} from "./internalApiConnectorViewUtils.js";

const t = (key) => key;

const sampleReport = {
  summary: "1 internal API connector(s), 3 endpoint(s), 1 validation request(s); 0 blocked mutating request(s).",
  connectors: [
    {
      connector_id: "apiconn:agent:payments_api",
      agent_id: "agent-1",
      name: "Payments API",
      api_type: "REST",
      environment: "STAGING",
      status: "ACTIVE",
      base_url_label: "https://payments.internal",
      endpoint_count: 3,
      read_only: true,
    },
  ],
  endpoints: [
    {
      endpoint_id: "apiendpoint:get1",
      connector_id: "apiconn:agent:payments_api",
      method: "GET",
      path_label: "/payments",
      category: "payments",
      read_only: true,
    },
    {
      endpoint_id: "apiendpoint:post1",
      connector_id: "apiconn:agent:payments_api",
      method: "POST",
      path_label: "/payments",
      category: "payments",
      read_only: false,
    },
  ],
  validations: [
    {
      request_id: "apivalid:1",
      connector_id: "apiconn:agent:payments_api",
      endpoint_id: "apiendpoint:get1",
      validation_type: "contract_validation",
      status: "PENDING",
      requires_user_approval: true,
    },
  ],
};

describe("internalApiConnectorViewUtils", () => {
  it("renders connector inventory", () => {
    const vm = buildInternalApiConnectorViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.connectors).toHaveLength(1);
    expect(vm.connectors[0].endpoints).toHaveLength(2);
  });

  it("renders empty state", () => {
    const vm = buildInternalApiConnectorViewModel({ connectors: [], endpoints: [], validations: [] }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(INTERNAL_API_CONNECTOR_I18N_KEYS.empty);
  });

  it("renders connector cards", () => {
    const vm = buildInternalApiConnectorViewModel(sampleReport, t);
    expect(vm.connectors[0].name).toBe("Payments API");
    expect(connectorStatusBadgeClass("ACTIVE")).toContain("badge-green");
  });

  it("renders endpoint cards", () => {
    const vm = buildInternalApiConnectorViewModel(sampleReport, t);
    const post = vm.connectors[0].endpoints.find((e) => e.method === "POST");
    expect(post.read_only).toBe(false);
    expect(post.blockedLabel).toBe(INTERNAL_API_CONNECTOR_I18N_KEYS.blocked);
  });

  it("renders validation cards", () => {
    const vm = buildInternalApiConnectorViewModel(sampleReport, t);
    expect(vm.validations[0].validationTypeLabel).toBe("Contract Validation");
    expect(validationStatusBadgeClass("PENDING")).toContain("badge-orange");
  });

  it("disables execution in preview payload", () => {
    const vm = buildInternalApiConnectorViewModel(sampleReport, t);
    expect(vm.validations[0].previewPayload.canExecute).toBe(false);
    expect(vm.executeDisabledNote).toBe(INTERNAL_API_CONNECTOR_I18N_KEYS.sendDisabledNote);
  });

  it("maps translation keys", () => {
    const vm = buildInternalApiConnectorViewModel(sampleReport, t);
    expect(vm.title).toBe(INTERNAL_API_CONNECTOR_I18N_KEYS.title);
    expect(formatValidationType("schema_validation")).toBe("Schema Validation");
    const payload = buildValidationPreviewPayload(
      sampleReport.validations[0],
      sampleReport.endpoints[1],
      sampleReport.connectors[0],
      t,
    );
    expect(payload.mutatingBlocked).toBe(true);
  });
});
