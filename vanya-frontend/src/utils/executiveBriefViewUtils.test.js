import { describe, expect, it } from "vitest";
import { buildExecutiveBriefViewModel } from "./executiveBriefViewUtils.js";

const t = (key) => key;

describe("executiveBriefViewUtils", () => {
  it("rolls up release status, risks, trend, and action from existing VMs", () => {
    const vm = buildExecutiveBriefViewModel({
      releaseReadinessVm: {
        show: true,
        empty: false,
        overallStatusText: "READY",
        overallStatusBadgeClass: "badge badge-green",
        data_gaps: [],
      },
      businessRiskVm: {
        businessRisks: [{ title: "Payment API latency" }],
        overallBusinessRisk: "HIGH",
      },
      executiveImpactVm: {
        qualityMetrics: [{ direction: "IMPROVING" }],
      },
      valueDashboardVm: { qualityTrend: "STABLE" },
      platformObservabilityVm: { topPlatformRisks: ["Queue backlog"] },
      onboardingVm: { checklist: { steps: [] } },
      t,
    });

    expect(vm.show).toBe(true);
    expect(vm.releaseStatus).toBe("READY");
    expect(vm.topRisks).toContain("Payment API latency");
    expect(vm.topRisks).toContain("Queue backlog");
    expect(vm.trendDirection).toBe("executive_impact.direction.improving");
    expect(vm.action.path).toBe("/dashboard");
  });

  it("points cold projects to incidents via onboarding step", () => {
    const vm = buildExecutiveBriefViewModel({
      releaseReadinessVm: { show: true, empty: true, overallStatusText: "UNKNOWN" },
      businessRiskVm: {},
      executiveImpactVm: {},
      valueDashboardVm: {},
      platformObservabilityVm: {},
      onboardingVm: {
        checklist: {
          steps: [{
            status: "PENDING",
            navigation: { label: "Connect Jira", path: "/integrations" },
          }],
        },
      },
      t,
    });

    expect(vm.action).toEqual({ label: "Connect Jira", path: "/integrations" });
  });
});
