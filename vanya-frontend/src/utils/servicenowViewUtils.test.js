import { describe, it, expect } from "vitest";
import {
  SERVICENOW_I18N_KEYS,
  buildChangeCardViewModel,
  buildConnectionCardViewModel,
  buildIncidentCardViewModel,
  buildServiceNowIntegrationViewModel,
  connectionBadgeClass,
  deriveServiceNowHeaderState,
  formatCount,
} from "./servicenowViewUtils.js";

const t = (key) => key;

const sampleStatus = {
  connected: true,
  instance_url: "https://acme.service-now.com",
  incident_count: 42,
  change_count: 7,
  service_count: 5,
  cmdb_count: 120,
  last_sync: "2026-06-10T12:00:00Z",
};

describe("servicenowViewUtils", () => {
  it("formats counts for display", () => {
    expect(formatCount(42)).toBe("42");
    expect(formatCount(undefined)).toBe("0");
  });

  it("builds connection card view model", () => {
    const vm = buildConnectionCardViewModel(sampleStatus, t);
    expect(vm.connectedBadgeClass).toBe(connectionBadgeClass(true));
    expect(vm.counts).toHaveLength(4);
    expect(vm.counts[0].value).toBe("42");
    expect(vm.showEmptyConnection).toBe(false);
    expect(vm.instanceUrl).toBe("https://acme.service-now.com");
    expect(vm).not.toHaveProperty("username");
  });

  it("shows empty connection state", () => {
    const vm = buildConnectionCardViewModel({ connected: false }, t);
    expect(vm.showEmptyConnection).toBe(true);
    expect(vm.emptyConnectionText).toBe(SERVICENOW_I18N_KEYS.emptyConnection);
  });

  it("renders incident cards", () => {
    const vm = buildIncidentCardViewModel({
      number: "INC0001",
      short_description: "Email outage",
      state: "In Progress",
      priority: "1",
      assignment_group: "Service Desk",
      opened_at: "2026-06-01",
    });
    expect(vm.number).toBe("INC0001");
    expect(vm.shortDescription).toBe("Email outage");
  });

  it("renders change cards", () => {
    const vm = buildChangeCardViewModel({
      number: "CHG0001",
      short_description: "Patch",
      state: "Scheduled",
      risk: "Moderate",
    });
    expect(vm.number).toBe("CHG0001");
    expect(vm.risk).toBe("Moderate");
  });

  it("builds integration view model with empty states", () => {
    const vm = buildServiceNowIntegrationViewModel({
      status: sampleStatus,
      incidents: [],
      changes: [],
      services: [],
      cmdbItems: [],
      t,
    });
    expect(vm.showEmptyIncidents).toBe(true);
    expect(vm.showEmptyChanges).toBe(true);
    expect(vm.services.showEmpty).toBe(true);
    expect(vm.cmdb.showEmpty).toBe(true);
  });

  it("derives header state from connection status", () => {
    expect(deriveServiceNowHeaderState({ connected: true }).health).toBe("ok");
    expect(deriveServiceNowHeaderState({ connected: false }).health).toBe("unconfigured");
  });
});
