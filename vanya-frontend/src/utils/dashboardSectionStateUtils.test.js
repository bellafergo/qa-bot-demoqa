import { describe, expect, it } from "vitest";
import {
  buildColdProjectGuidanceViewModel,
  buildDashboardSectionState,
  isColdProject,
} from "./dashboardSectionStateUtils.js";

const t = (key) => key;

describe("dashboardSectionStateUtils", () => {
  it("builds error state with retry label", () => {
    const state = buildDashboardSectionState({
      data: null,
      loadError: "Failed to load",
      loading: false,
      empty: false,
      t,
    });
    expect(state.show).toBe(true);
    expect(state.error).toBe("Failed to load");
    expect(state.loading).toBe(false);
    expect(state.empty).toBe(false);
  });

  it("builds empty state with CTA", () => {
    const state = buildDashboardSectionState({
      data: {},
      loadError: "",
      loading: false,
      empty: true,
      emptyMessage: "No data yet",
      emptyCta: { path: "/integrations", labelKey: "dash.section.cta.integrations" },
      t,
    });
    expect(state.empty).toBe(true);
    expect(state.emptyMessage).toBe("No data yet");
    expect(state.emptyCta).toEqual({ path: "/integrations", label: "dash.section.cta.integrations" });
  });

  it("builds capability-gated state", () => {
    const state = buildDashboardSectionState({
      data: { connected: false },
      loadError: "",
      loading: false,
      empty: true,
      capabilityState: { state: "INTEGRATION_REQUIRED", icon: "🔒", title: "Coverage", description: "desc", cta: { path: "/integrations", label: "Connect" } },
      t,
    });
    expect(state.show).toBe(true);
    expect(state.empty).toBe(false);
    expect(state.capabilityState.state).toBe("INTEGRATION_REQUIRED");
  });

  it("detects cold project and builds guidance", () => {
    expect(isColdProject({
      releaseReadiness: { release_readiness: null },
      valueDashboard: { incidents_investigated: 0, executive_reports_generated: 0, release_readiness_reports: 0 },
      businessRisk: { has_intelligence: false },
      executiveImpact: { has_sufficient_history: false },
    })).toBe(true);

    const vm = buildColdProjectGuidanceViewModel(true, t);
    expect(vm.show).toBe(true);
    expect(vm.actions).toHaveLength(3);
  });
});
