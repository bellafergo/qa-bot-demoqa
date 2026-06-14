import { describe, expect, it } from "vitest";
import { buildExecutiveRiskBriefViewModel } from "./executiveRiskBriefViewUtils.js";

const t = (key, vars = {}) => {
  if (vars.count != null) return `${key}:${vars.count}`;
  if (vars.module) return `${key}:${vars.module}`;
  if (vars.test) return `${key}:${vars.test}`;
  return key;
};

describe("executiveRiskBriefViewUtils", () => {
  it("builds view model from cluster-backed brief", () => {
    const vm = buildExecutiveRiskBriefViewModel(
      {
        has_risk: true,
        module: "AUTH",
        confidence: "high",
        evidence: [
          { kind: "occurrences", count: 2 },
          { kind: "module", module: "AUTH" },
          { kind: "representative_test", test_case_id: "TC-TOS-009" },
        ],
        impact: "Users may be unable to sign in.",
        recommendation: "Run authentication smoke tests.",
      },
      t,
    );

    expect(vm.show).toBe(true);
    expect(vm.module).toBe("AUTH");
    expect(vm.evidence).toHaveLength(3);
    expect(vm.confidenceBadgeClass).toContain("badge-red");
  });

  it("hides when brief is null", () => {
    expect(buildExecutiveRiskBriefViewModel(null, t).show).toBe(false);
  });
});
