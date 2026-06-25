import { describe, expect, it } from "vitest";
import { buildExecutiveDashboardV2ViewModel } from "./executiveDashboardV2ViewUtils.js";

const t = (key) => key;

describe("executiveDashboardV2ViewUtils", () => {
  it("maps BLOCKED release status to STOP decision", () => {
    const vm = buildExecutiveDashboardV2ViewModel({
      releaseReadinessVm: {
        show: true,
        empty: false,
        overallStatus: "BLOCKED",
        overallStatusText: "BLOCKED",
        deploymentRisk: { risk_level: "HIGH", confidence: 0.8 },
        summary: "Open blockers detected",
      },
      executiveBriefVm: { topRisks: ["Auth regression"], trendDirection: "STABLE", action: { label: "Review", path: "/incidents" } },
      earlyDegradationVm: { show: false, empty: true },
      qualityTrendVm: { show: false, empty: true },
      platformObservabilityVm: { show: false, empty: true },
      businessRiskVm: {},
      executiveRiskBriefVm: { show: false },
      executiveImpactVm: {},
      nextRecommendedActionVm: null,
      totalRuns: 10,
      loading: false,
      t,
    });

    expect(vm.block1.decision).toBe("STOP");
    expect(vm.block1.decisionLabel).toBe("dash.v2.decision.stop");
    expect(vm.block1.deploymentRisk.level).toBe("HIGH");
  });

  it("downgrades GO to CAUTION when quality trend is degrading", () => {
    const vm = buildExecutiveDashboardV2ViewModel({
      releaseReadinessVm: {
        show: true,
        empty: false,
        overallStatus: "GO",
        deploymentRisk: { risk_level: "LOW", confidence: 0.6 },
      },
      executiveBriefVm: {
        topRisks: [],
        trendDirection: "executive_impact.direction.degrading",
        action: { label: "Run smoke", path: "/batch" },
      },
      earlyDegradationVm: { show: true, empty: false, report: { overall_status: "DEGRADING", overallStatusText: "Degrading" } },
      qualityTrendVm: { show: true, empty: false },
      platformObservabilityVm: { show: false, empty: true },
      businessRiskVm: {},
      executiveRiskBriefVm: { show: false },
      executiveImpactVm: {},
      nextRecommendedActionVm: null,
      totalRuns: 20,
      loading: false,
      t,
    });

    expect(vm.block1.decision).toBe("CAUTION");
    expect(vm.block1.cautionReason).toBe("dash.v2.caution_degrading");
  });

  it("limits recommended actions to three items", () => {
    const vm = buildExecutiveDashboardV2ViewModel({
      releaseReadinessVm: { show: true, empty: false, overallStatus: "GO", deploymentRisk: { risk_level: "LOW" } },
      executiveBriefVm: { action: { label: "A1", path: "/a1" }, topRisks: [] },
      earlyDegradationVm: { show: false, empty: true },
      qualityTrendVm: { show: false, empty: true },
      platformObservabilityVm: { show: false, empty: true },
      businessRiskVm: {},
      executiveRiskBriefVm: { show: true, hasRisk: true, recommendation: "Investigate", primaryRiskLabel: "Risk", impact: "High" },
      executiveImpactVm: {},
      nextRecommendedActionVm: { action: { label: "A2", path: "/a2" }, message: "Because" },
      totalRuns: 5,
      loading: false,
      t,
    });

    expect(vm.block3.actions.length).toBeLessThanOrEqual(3);
    expect(vm.block2.topRisks.length).toBeLessThanOrEqual(3);
  });
});
