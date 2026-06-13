import { describe, expect, it } from "vitest";
import {
  COMMAND_CENTER_I18N_KEYS,
  buildCommandCenterHeaderViewModel,
  buildCommandCenterViewModel,
  buildNextRecommendedActionViewModel,
  computeCommandCenterHealthContext,
} from "./commandCenterViewUtils.js";

const t = (key) => key;

describe("commandCenterViewUtils", () => {
  it("builds four command center KPIs", () => {
    const vm = buildCommandCenterViewModel({
      summary: { total_runs: 0, pass_rate: null },
      fi: {},
      hasKnowledge: false,
      passRateValid: false,
      passRateNum: null,
      loading: false,
      t,
    });
    expect(vm.kpis).toHaveLength(4);
    expect(vm.kpis[0].id).toBe("system_health");
    expect(vm.kpis[1].label).toBe(COMMAND_CENTER_I18N_KEYS.memoryCoverage);
    expect(vm.kpis[2].value).toBe(COMMAND_CENTER_I18N_KEYS.statusNoRuns);
    expect(vm.kpis[3].value).toBe(COMMAND_CENTER_I18N_KEYS.statusNotEvaluated);
  });

  it("builds operational header with fully operational status", () => {
    const vm = buildCommandCenterHeaderViewModel({
      project: { name: "Acme Corp", settings: { environments: [{ name: "QA" }] } },
      onboardingChecklist: {
        overall_completion: 100,
        completed_steps: 8,
        total_steps: 8,
        readiness_level: "FULLY_OPERATIONAL",
      },
      summary: { total_runs: 5, pass_rate: 90 },
      fi: {},
      passRateValid: true,
      passRateNum: 90,
      lastRefresh: new Date("2026-06-12T10:00:00Z"),
      t,
      formatTimestamp: (v) => v,
    });
    expect(vm.projectName).toBe("Acme Corp");
    expect(vm.environment).toBe("QA");
    expect(vm.globalStatus).toBe("onboarding.readiness.fully_operational");
  });

  it("recommends build memory when knowledge is missing", () => {
    const vm = buildNextRecommendedActionViewModel({
      hasKnowledge: false,
      summary: { total_runs: 2, pass_rate: 80 },
      fi: {},
      passRateValid: true,
      passRateNum: 80,
      businessRiskVm: {},
      loading: false,
      t,
    });
    expect(vm.action.path).toBe("/knowledge");
    expect(vm.action.label).toBe(COMMAND_CENTER_I18N_KEYS.actionBuildMemory);
  });

  it("recommends smoke tests when no runs exist", () => {
    const vm = buildNextRecommendedActionViewModel({
      hasKnowledge: true,
      summary: { total_runs: 0 },
      fi: {},
      passRateValid: false,
      passRateNum: null,
      businessRiskVm: {},
      loading: false,
      t,
    });
    expect(vm.action.path).toBe("/batch");
  });

  it("shows no action required when project is healthy", () => {
    const vm = buildNextRecommendedActionViewModel({
      hasKnowledge: true,
      summary: { total_runs: 12, pass_rate: 92 },
      fi: {},
      passRateValid: true,
      passRateNum: 92,
      businessRiskVm: { overallBusinessRisk: "LOW" },
      loading: false,
      t,
    });
    expect(vm.noActionRequired).toBe(true);
    expect(vm.action).toBeNull();
  });

  it("computes elevated risk from recurrent regressions", () => {
    const ctx = computeCommandCenterHealthContext({
      summary: { total_runs: 10, pass_rate: 85 },
      fi: { recurrent_regressions_count: 2 },
      hasKnowledge: true,
      passRateValid: true,
      passRateNum: 85,
    });
    expect(ctx.riskLevel).toBe("elevated");
  });
});
