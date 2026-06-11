import { describe, it, expect } from "vitest";
import {
  AUDIT_I18N_KEYS,
  buildAuditTrailViewModel,
  mapAuditEvent,
  resultBadgeClass,
} from "./auditViewUtils.js";

const t = (key) => key;

const sampleEvents = {
  events: [
    {
      event_id: "evt-1",
      timestamp: "2026-06-11T10:00:00+00:00",
      user_id: "user-1",
      user_email: "admin@example.com",
      event_type: "SSO_PROVIDER_VALIDATED",
      resource_type: "SECURITY",
      resource_id: "MICROSOFT",
      action: "validate",
      result: "SUCCESS",
      metadata: {},
    },
    {
      event_id: "evt-2",
      timestamp: "2026-06-11T09:00:00+00:00",
      user_id: "user-2",
      user_email: "qa@example.com",
      event_type: "REPORT_SENT",
      resource_type: "REPORTS",
      resource_id: "demo:executive_quality",
      action: "send",
      result: "FAILURE",
      metadata: {},
    },
  ],
  total: 2,
};

const sampleSummary = {
  total_events: 2,
  event_types: {
    SSO_PROVIDER_VALIDATED: 1,
    REPORT_SENT: 1,
  },
  latest_event: sampleEvents.events[0],
};

describe("auditViewUtils", () => {
  it("maps audit event cards with priority display", () => {
    const vm = mapAuditEvent(sampleEvents.events[0], t);
    expect(vm.eventTypeLabel).toBe("audit_trail.event.sso_provider_validated");
    expect(vm.resultLabel).toBe(AUDIT_I18N_KEYS.resultSuccess);
    expect(resultBadgeClass("FAILURE")).toBe("badge badge-red");
  });

  it("builds audit trail view model with summary", () => {
    const vm = buildAuditTrailViewModel({
      events: sampleEvents,
      summary: sampleSummary,
      selectedEventType: "",
      t,
    });
    expect(vm.show).toBe(true);
    expect(vm.events).toHaveLength(2);
    expect(vm.totalEvents).toBe(2);
    expect(vm.latestActivityDetail).toContain("audit_trail.event.sso_provider_validated");
  });

  it("shows empty state", () => {
    const vm = buildAuditTrailViewModel({
      events: { events: [] },
      summary: { total_events: 0, event_types: {}, latest_event: null },
      selectedEventType: "",
      t,
    });
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(AUDIT_I18N_KEYS.empty);
  });

  it("exposes filter options", () => {
    const vm = buildAuditTrailViewModel({
      events: sampleEvents,
      summary: sampleSummary,
      selectedEventType: "REPORT_SENT",
      t,
    });
    expect(vm.selectedEventType).toBe("REPORT_SENT");
    expect(vm.eventTypeOptions.length).toBeGreaterThan(0);
  });
});
