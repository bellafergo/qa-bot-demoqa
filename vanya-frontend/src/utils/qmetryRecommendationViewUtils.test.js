import { describe, it, expect } from "vitest";
import {
  QMETRY_RECOMMENDATION_I18N_KEYS,
  buildRecommendationCorrelationViewModel,
  buildRecommendedTestsOverviewViewModel,
  mapRecommendedTest,
  mapRecommendationGroup,
} from "./qmetryRecommendationViewUtils.js";

const t = (key) => key;

const sampleRecommendations = {
  connected: true,
  total_recommendations: 3,
  executive_summary: "Revenue Collection is associated with open Jira blockers and weak coverage. Three QMetry test cases are recommended for review.",
  recommendation_groups: [
    {
      capability: "Revenue Collection",
      recommended_tests: [
        {
          test_case_id: "1",
          test_case_name: "Payment gateway timeout",
          capability: "Revenue Collection",
          recommendation_reason: "open Jira blocker overlap and critical business or release risk",
          priority: "CRITICAL",
        },
      ],
    },
    {
      capability: "Customer Access",
      recommended_tests: [
        {
          test_case_id: "3",
          test_case_name: "Login with MFA",
          capability: "Customer Access",
          recommendation_reason: "weak coverage",
          priority: "MEDIUM",
        },
      ],
    },
    {
      capability: "Customer Purchase Flow",
      recommended_tests: [
        {
          test_case_id: "2",
          test_case_name: "Checkout happy path",
          capability: "Customer Purchase Flow",
          recommendation_reason: "high business risk",
          priority: "HIGH",
        },
      ],
    },
  ],
};

describe("qmetryRecommendationViewUtils", () => {
  it("maps recommended test cards with priority", () => {
    const vm = mapRecommendedTest(sampleRecommendations.recommendation_groups[0].recommended_tests[0], t);
    expect(vm.priority).toBe("CRITICAL");
    expect(vm.priorityLabel).toBe(QMETRY_RECOMMENDATION_I18N_KEYS.priorityCritical);
  });

  it("maps recommendation groups", () => {
    const vm = mapRecommendationGroup(sampleRecommendations.recommendation_groups[0], t);
    expect(vm.capability).toBe("Revenue Collection");
    expect(vm.tests).toHaveLength(1);
    expect(vm.topPriority).toBe("CRITICAL");
  });

  it("builds incident recommendation view model with grouping and priorities", () => {
    const vm = buildRecommendationCorrelationViewModel(
      { qmetry_recommendation_report: sampleRecommendations },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.groups).toHaveLength(3);
    expect(vm.criticalTests).toHaveLength(1);
    expect(vm.highTests).toHaveLength(1);
    expect(vm.mediumTests).toHaveLength(1);
    expect(vm.executiveSummary).toContain("Revenue Collection");
  });

  it("shows empty connection state", () => {
    const vm = buildRecommendedTestsOverviewViewModel({ connected: false }, t);
    expect(vm.empty).toBe(true);
    expect(vm.capabilityState.state).toBe("INTEGRATION_REQUIRED");
  });

  it("shows empty recommendations state", () => {
    const vm = buildRecommendedTestsOverviewViewModel(
      { connected: true, total_recommendations: 0, recommendation_groups: [] },
      t,
    );
    expect(vm.capabilityState.state).toBe("INSUFFICIENT_HISTORY");
  });

  it("shows insufficient history when connected without recommendations", () => {
    const vm = buildRecommendedTestsOverviewViewModel(
      {
        connected: true,
        total_recommendations: 0,
        data_gaps: ["No coverage intelligence available for recommendation correlation."],
      },
      t,
    );
    expect(vm.capabilityState.state).toBe("INSUFFICIENT_HISTORY");
  });
});
