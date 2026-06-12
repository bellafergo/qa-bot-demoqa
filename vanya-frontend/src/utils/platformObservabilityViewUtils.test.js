import { describe, expect, it } from "vitest";
import {
  PLATFORM_OBSERVABILITY_I18N_KEYS,
  buildPlatformObservabilityViewModel,
  hasPlatformObservabilitySection,
  healthStatusBadgeClass,
  isPlatformObservabilityEmpty,
  mapHealthArea,
} from "./platformObservabilityViewUtils.js";

const t = (key) => key;

const sampleReport = {
  generated_at: "2026-06-10T08:00:00+00:00",
  executive_summary: "Vanya platform health shows degraded signals that need attention.",
  top_platform_risks: ["Report delivery failures observed"],
  api_health: {
    status: "HEALTHY",
    summary: "Platform audit trail active with 12 recorded events.",
    metrics: [
      {
        metric_name: "audit_events",
        status: "HEALTHY",
        value: "12",
        summary: "Centralized audit events are being recorded.",
      },
    ],
  },
  authentication_health: {
    status: "HEALTHY",
    summary: "Authentication foundation is stable.",
    metrics: [
      {
        metric_name: "sso_validations",
        status: "HEALTHY",
        value: "2/2",
        summary: "Successful SSO provider validations: 2.",
      },
    ],
  },
  integration_health: {
    status: "DEGRADED",
    summary: "1 healthy and 1 degraded integrations.",
    integration_summary: { healthy: 1, degraded: 1, disconnected: 0 },
    metrics: [
      {
        metric_name: "enabled_integrations",
        status: "DEGRADED",
        value: "2",
        summary: "2 integrations enabled.",
      },
    ],
  },
  report_delivery_health: {
    status: "DEGRADED",
    summary: "Report delivery recorded 1 successes and 1 failures.",
    metrics: [
      {
        metric_name: "report_sends",
        status: "DEGRADED",
        value: "1/2",
        summary: "Report delivery recorded 1 successes and 1 failures.",
      },
    ],
  },
  incident_investigation_health: {
    status: "DEGRADED",
    summary: "1 investigations completed with 1 still in progress of 2 started.",
    metrics: [
      {
        metric_name: "investigations_started",
        status: "HEALTHY",
        value: "2",
        summary: "Investigations started: 2.",
      },
      {
        metric_name: "investigations_completed",
        status: "DEGRADED",
        value: "1",
        summary: "Investigations completed: 1.",
      },
    ],
  },
};

describe("platformObservabilityViewUtils", () => {
  it("detects section presence and empty state", () => {
    expect(hasPlatformObservabilitySection(sampleReport)).toBe(true);
    expect(hasPlatformObservabilitySection(null)).toBe(false);
    expect(isPlatformObservabilityEmpty(sampleReport)).toBe(false);
    expect(
      isPlatformObservabilityEmpty({
        ...sampleReport,
        api_health: { status: "UNKNOWN", summary: "", metrics: [] },
        authentication_health: { status: "UNKNOWN", summary: "", metrics: [] },
        integration_health: { status: "UNKNOWN", summary: "", metrics: [] },
        report_delivery_health: { status: "UNKNOWN", summary: "", metrics: [] },
        incident_investigation_health: { status: "UNKNOWN", summary: "", metrics: [] },
        top_platform_risks: [],
      }),
    ).toBe(true);
  });

  it("maps health status badges", () => {
    expect(healthStatusBadgeClass("HEALTHY")).toContain("badge-green");
    expect(healthStatusBadgeClass("DEGRADED")).toContain("badge-orange");
    expect(healthStatusBadgeClass("UNHEALTHY")).toContain("badge-red");
    expect(healthStatusBadgeClass("UNKNOWN")).toContain("badge-gray");
  });

  it("maps health areas and metrics", () => {
    const area = mapHealthArea(sampleReport.integration_health, t);
    expect(area.statusLabel).toBe(PLATFORM_OBSERVABILITY_I18N_KEYS.statusDegraded);
    expect(area.integrationSummary.degraded).toBe(1);
    expect(area.metrics[0].statusBadgeClass).toContain("badge-orange");
  });

  it("builds platform observability view model", () => {
    const vm = buildPlatformObservabilityViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(PLATFORM_OBSERVABILITY_I18N_KEYS.title);
    expect(vm.executiveSummary).toContain("degraded signals");
    expect(vm.healthSections).toHaveLength(4);
    expect(vm.topPlatformRisks).toHaveLength(1);
  });

  it("renders empty state message", () => {
    const vm = buildPlatformObservabilityViewModel(null, t);
    expect(vm.show).toBe(false);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(PLATFORM_OBSERVABILITY_I18N_KEYS.empty);
  });
});
