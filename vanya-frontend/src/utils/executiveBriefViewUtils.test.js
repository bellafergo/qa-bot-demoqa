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
        isComplete: false,
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

  it("uses operational actions when onboarding is complete", () => {
    const vm = buildExecutiveBriefViewModel({
      releaseReadinessVm: { show: true, empty: false, overallStatusText: "READY" },
      businessRiskVm: {},
      executiveImpactVm: {},
      valueDashboardVm: {},
      platformObservabilityVm: {},
      onboardingVm: { isComplete: true, checklist: { steps: [] } },
      hasKnowledge: false,
      totalRuns: 0,
      fi: {},
      passRateValid: false,
      passRateNum: null,
      t,
    });

    expect(vm.operationalMode).toBe(true);
    expect(vm.title).toBe("command_center.executive_summary");
    expect(vm.action.path).toBe("/knowledge");
  });

  it("never shows Unknown for release status or trend on cold projects", () => {
    const vm = buildExecutiveBriefViewModel({
      releaseReadinessVm: { show: true, empty: true, overallStatusText: "UNKNOWN" },
      businessRiskVm: {},
      executiveImpactVm: { hasSufficientHistory: false },
      valueDashboardVm: { qualityTrend: "UNKNOWN" },
      platformObservabilityVm: {},
      onboardingVm: {
        isComplete: false,
        checklist: {
          steps: [{
            status: "PENDING",
            navigation: { label: "Connect Jira", path: "/integrations" },
          }],
        },
      },
      totalRuns: 0,
      t,
    });

    expect(vm.releaseStatus).toBe("executive_brief.integration_required");
    expect(vm.trendDirection).toBe("executive_brief.insufficient_history");
    expect(vm.releaseStatus).not.toBe("executive_brief.empty");
    expect(vm.trendDirection).not.toBe("executive_impact.direction.unknown");
  });

  it("shows explicit no-risks message when risk list is empty", () => {
    const vm = buildExecutiveBriefViewModel({
      releaseReadinessVm: { show: true, empty: false, overallStatusText: "READY", overallStatusBadgeClass: "badge badge-green", data_gaps: [] },
      businessRiskVm: { businessRisks: [] },
      executiveImpactVm: { qualityMetrics: [{ direction: "STABLE" }] },
      valueDashboardVm: { qualityTrend: "STABLE" },
      platformObservabilityVm: { topPlatformRisks: [] },
      onboardingVm: { isComplete: true, checklist: { steps: [] } },
      totalRuns: 5,
      fi: {},
      passRateValid: true,
      passRateNum: 90,
      t,
    });

    expect(vm.topRisks).toHaveLength(0);
    expect(vm.noRisksMessage).toBe("executive_brief.no_risks");
  });
});
