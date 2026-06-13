import { describe, expect, it } from "vitest";
import {
  BUSINESS_RISK_I18N_KEYS,
  buildBusinessRiskViewModel,
  hasBusinessRiskSection,
  isBusinessRiskEmpty,
  severityBadgeClass,
} from "./businessRiskViewUtils.js";

const t = (key) => key;

const sampleReport = {
  generated_at: "2026-06-10T08:00:00+00:00",
  has_intelligence: true,
  overall_business_risk: "HIGH",
  executive_summary: "Revenue Collection is at elevated risk due to open Jira blockers, degraded environments.",
  top_capabilities_at_risk: ["Revenue Collection", "Customer Purchase Flow"],
  business_risks: [
    {
      risk_id: "risk:revenue_collection",
      capability: "Revenue Collection",
      severity: "HIGH",
      confidence: "MEDIUM",
      summary: "Revenue Collection is at high business risk supported by open Jira blockers.",
      evidence: ["open Jira blockers", "critical contract changes"],
    },
    {
      risk_id: "risk:customer_purchase_flow",
      capability: "Customer Purchase Flow",
      severity: "CRITICAL",
      confidence: "HIGH",
      summary: "Customer Purchase Flow is at critical business risk.",
      evidence: ["broken data journeys", "critical contract changes"],
    },
  ],
  signals: [
    {
      signal_id: "signal:revenue_collection:jira_blockers:0",
      title: "Open Jira Blockers",
      severity: "HIGH",
      confidence: "MEDIUM",
      impacted_capability: "Revenue Collection",
      evidence_count: 2,
    },
  ],
};

describe("businessRiskViewUtils", () => {
  it("detects section and empty states", () => {
    expect(hasBusinessRiskSection(sampleReport)).toBe(true);
    expect(hasBusinessRiskSection(null)).toBe(false);
    expect(isBusinessRiskEmpty(sampleReport)).toBe(false);
    expect(isBusinessRiskEmpty({ has_intelligence: false, business_risks: [], signals: [] })).toBe(true);
  });

  it("maps severity badges", () => {
    expect(severityBadgeClass("CRITICAL")).toContain("badge-red");
    expect(severityBadgeClass("HIGH")).toContain("badge-orange");
    expect(severityBadgeClass("MEDIUM")).toContain("badge-blue");
  });

  it("builds business risk view model", () => {
    const vm = buildBusinessRiskViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(BUSINESS_RISK_I18N_KEYS.title);
    expect(vm.overallBusinessRisk).toBe("HIGH");
    expect(vm.topCapabilities).toHaveLength(2);
    expect(vm.businessRisks).toHaveLength(2);
    expect(vm.businessRisks[0].severityBadgeClass).toContain("badge-orange");
    expect(vm.businessRisks[0].showTrace).toBe(true);
    expect(vm.businessRisks[0].trace.show).toBe(true);
    expect(vm.signals).toHaveLength(1);
    expect(vm.signals[0].capability).toBe("Revenue Collection");
  });

  it("renders insufficient intelligence capability state", () => {
    const vm = buildBusinessRiskViewModel({
      generated_at: "2026-06-10T08:00:00+00:00",
      has_intelligence: false,
      overall_business_risk: "LOW",
      executive_summary: "Insufficient intelligence available to assess business risk.",
      business_risks: [],
      signals: [],
      top_capabilities_at_risk: [],
    }, t);
    expect(vm.empty).toBe(true);
    expect(vm.capabilityState.state).toBe("INSUFFICIENT_HISTORY");
  });
});
