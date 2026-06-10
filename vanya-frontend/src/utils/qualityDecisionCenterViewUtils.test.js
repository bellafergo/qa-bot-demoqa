import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  DECISION_CENTER_I18N_KEYS,
  buildDecisionCenterDrilldownItem,
  buildDecisionCenterViewModel,
  formatDecisionCenterConfidence,
  getOverallStatusLabelKey,
  hasDecisionCenterSection,
  isDecisionCenterEmpty,
} from "./qualityDecisionCenterViewUtils.js";

const t = (key) => key;

describe("qualityDecisionCenterViewUtils", () => {
  it("detects decision center section and empty state", () => {
    expect(hasDecisionCenterSection({ decision_center: null })).toBe(true);
    expect(hasDecisionCenterSection({})).toBe(false);
    expect(isDecisionCenterEmpty({ decision_center: null })).toBe(true);
  });

  it("renders status badge, summary, and takeaways", () => {
    const vm = buildDecisionCenterViewModel(
      {
        decision_center: {
          overall_status: "RED",
          executive_summary: "Critical deployment risk detected.",
          confidence: 0.88,
          top_risk_level: "CRITICAL",
          top_risk_score: 82,
          top_hypothesis: "Checkout timeout after deploy",
          top_impacted_area: "Checkout",
          recommended_test_count: 3,
          recommended_action_count: 2,
          key_takeaways: [
            {
              title: "Checkout is the primary impacted area",
              description: "Multiple signals concentrate on Checkout.",
              priority: 1,
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
    expect(vm.center.statusLabel).toBe(DECISION_CENTER_I18N_KEYS.statusRed);
    expect(vm.center.confidenceText).toBe("88%");
    expect(vm.center.takeaways).toHaveLength(1);
    expect(vm.center.takeaways[0].drilldownItem).not.toBeNull();
  });

  it("renders empty state via i18n key", () => {
    const vm = buildDecisionCenterViewModel({ decision_center: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.decision_center_empty");
  });

  it("maps overall status label keys", () => {
    expect(getOverallStatusLabelKey("ORANGE")).toBe(DECISION_CENTER_I18N_KEYS.statusOrange);
    expect(getOverallStatusLabelKey("GREEN")).toBe(DECISION_CENTER_I18N_KEYS.statusGreen);
  });

  it("formats confidence", () => {
    expect(formatDecisionCenterConfidence(0.885)).toBe("89%");
  });

  it("builds drilldown item for II-02D navigation", () => {
    const item = buildDecisionCenterDrilldownItem({
      title: "Checkout is the primary impacted area",
      description: "Cluster overlap",
      related_entity_type: "failure_cluster",
      related_entity_id: "cluster_7",
    });
    const target = buildDrilldownNavigation(item);
    expect(target?.path).toContain("failure-intel");
  });

  it("exposes translation keys", () => {
    expect(DECISION_CENTER_I18N_KEYS.title).toBe("incident.qa.decision_center");
    expect(DECISION_CENTER_I18N_KEYS.keyTakeaways).toBe("incident.qa.decision_center_key_takeaways");
  });
});
