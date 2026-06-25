import { describe, expect, it } from "vitest";
import { buildIncidentTechnicalDetailsViewModel } from "./incidentTechnicalDetailsViewUtils.js";

const t = (key) => key;

describe("buildIncidentTechnicalDetailsViewModel", () => {
  it("builds collapsible technical details from report meta", () => {
    const vm = buildIncidentTechnicalDetailsViewModel({
      id: "rep-123",
      incident_id: "inc-456",
      meta: { engine_version: "incident-v1.4a", analyze_only: true },
    }, t);

    expect(vm.show).toBe(true);
    expect(vm.engineVersion).toBe("incident-v1.4a");
    expect(vm.reportId).toBe("rep-123");
    expect(vm.incidentId).toBe("inc-456");
    expect(vm.analyzeOnlyText).toBe("incident.qa.technical_analyze_only_yes");
  });

  it("hides when no technical metadata exists", () => {
    const vm = buildIncidentTechnicalDetailsViewModel({}, t);
    expect(vm.show).toBe(false);
  });
});
