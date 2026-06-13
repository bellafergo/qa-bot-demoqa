import { describe, it, expect } from "vitest";
import {
  SERVICENOW_INTELLIGENCE_I18N_KEYS,
  buildCorrelationCardViewModel,
  buildServiceNowIntelligenceOverviewViewModel,
  buildServiceNowIntelligenceViewModel,
  isServiceNowIntelligenceEmpty,
} from "./servicenowIntelligenceViewUtils.js";

const t = (key) => key;

const sampleIntel = {
  connected: true,
  executive_summary: "2 ServiceNow operational correlation(s) detected.",
  top_operational_risks: ["ServiceNow Incident INC0001: Revenue Collection (HIGH confidence)"],
  incident_correlations: [
    {
      entity_type: "incident",
      entity_id: "INC0001",
      capability: "Revenue Collection",
      correlation_reason: "Revenue Collection correlates with business risk",
      confidence: "HIGH",
    },
  ],
  change_correlations: [],
  service_correlations: [],
  cmdb_correlations: [],
};

describe("servicenowIntelligenceViewUtils", () => {
  it("builds correlation card view model", () => {
    const vm = buildCorrelationCardViewModel(sampleIntel.incident_correlations[0], t);
    expect(vm.entityId).toBe("INC0001");
    expect(vm.confidence).toBe("HIGH");
    expect(vm.capability).toBe("Revenue Collection");
  });

  it("builds incident intelligence view model", () => {
    const vm = buildServiceNowIntelligenceViewModel({ servicenow_intelligence: sampleIntel }, t);
    expect(vm.show).toBe(true);
    expect(vm.showIncidents).toBe(true);
    expect(vm.showOperationalRisks).toBe(true);
    expect(vm.incidents).toHaveLength(1);
  });

  it("builds dashboard overview view model", () => {
    const vm = buildServiceNowIntelligenceOverviewViewModel(sampleIntel, t);
    expect(vm.title).toBe(SERVICENOW_INTELLIGENCE_I18N_KEYS.dashboardTitle);
    expect(vm.empty).toBe(false);
  });

  it("shows empty state when disconnected", () => {
    expect(isServiceNowIntelligenceEmpty({ servicenow_intelligence: { connected: false } })).toBe(true);
    const vm = buildServiceNowIntelligenceOverviewViewModel({ connected: false }, t);
    expect(vm.empty).toBe(true);
  });

  it("shows empty correlations as insufficient history", () => {
    const vm = buildServiceNowIntelligenceOverviewViewModel(
      { connected: true, incident_correlations: [], change_correlations: [], service_correlations: [], cmdb_correlations: [] },
      t,
    );
    expect(vm.capabilityState.state).toBe("INSUFFICIENT_HISTORY");
  });
});
