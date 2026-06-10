import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  TEST_RECOMMENDATION_I18N_KEYS,
  buildTestRecommendationDrilldownItem,
  buildTestRecommendationPreviewPayload,
  buildTestRecommendationsViewModel,
  formatTestRecommendationPct,
  getPreviewObjectiveKey,
  hasTestRecommendationsSection,
  isTestRecommendationsEmpty,
} from "./testRecommendationViewUtils.js";

const t = (key) => key;

describe("testRecommendationViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasTestRecommendationsSection({ test_recommendations: null })).toBe(true);
    expect(hasTestRecommendationsSection({})).toBe(false);
    expect(isTestRecommendationsEmpty({ test_recommendations: { recommendations: [] } })).toBe(true);
  });

  it("renders recommendation cards with confidence and risk reduction", () => {
    const vm = buildTestRecommendationsViewModel(
      {
        test_recommendations: {
          summary: "Prioritize checkout validation.",
          recommendation_confidence: 0.88,
          recommendations: [
            {
              recommendation_id: "smoke:failure_cluster:cluster_7",
              test_name: "Checkout Smoke Suite",
              test_type: "smoke",
              priority: 1,
              confidence: 0.91,
              reason: "Multiple checkout failures and browser alerts were detected.",
              estimated_risk_reduction: 0.34,
              requires_user_approval: true,
              related_entity_type: "failure_cluster",
              related_entity_id: "cluster_7",
            },
          ],
        },
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(TEST_RECOMMENDATION_I18N_KEYS.title);
    expect(vm.recommendations[0].confidenceText).toBe("91%");
    expect(vm.recommendations[0].riskReductionText).toBe("34%");
    expect(vm.recommendations[0].previewPayload.futureNote).toBe(
      TEST_RECOMMENDATION_I18N_KEYS.previewFutureNote,
    );
  });

  it("renders empty state via i18n key", () => {
    const vm = buildTestRecommendationsViewModel({ test_recommendations: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.test_recommendations_empty");
  });

  it("formats percentage values", () => {
    expect(formatTestRecommendationPct(0.345)).toBe("35%");
    expect(formatTestRecommendationPct("bad")).toBe("—");
  });

  it("builds drilldown item for II-02D navigation", () => {
    const item = buildTestRecommendationDrilldownItem({
      test_name: "Checkout Smoke Suite",
      reason: "Cluster overlap",
      related_entity_type: "failure_cluster",
      related_entity_id: "cluster_7",
    });
    expect(buildDrilldownNavigation(item)?.path).toContain("failure-intel");
  });

  it("builds preview payload with objective key", () => {
    const payload = buildTestRecommendationPreviewPayload(
      {
        test_name: "Checkout Smoke Suite",
        test_type: "smoke",
        reason: "Failures detected",
        confidence: 0.91,
        estimated_risk_reduction: 0.34,
      },
      t,
    );
    expect(payload.fields).toHaveLength(4);
    expect(getPreviewObjectiveKey("smoke")).toBe("incident.qa.test_recommendations_objective_smoke");
  });

  it("exposes translation keys", () => {
    expect(TEST_RECOMMENDATION_I18N_KEYS.preview).toBe("incident.qa.test_recommendations_preview");
    expect(TEST_RECOMMENDATION_I18N_KEYS.approvalRequired).toBe(
      "incident.qa.test_recommendations_approval_required",
    );
  });
});
