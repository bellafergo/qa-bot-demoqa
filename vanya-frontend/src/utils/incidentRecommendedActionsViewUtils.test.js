import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  RECOMMENDED_ACTIONS_I18N_KEYS,
  buildRecommendedActionDrilldownItem,
  buildRecommendedActionPreviewPayload,
  buildRecommendedActionsViewModel,
  formatRecommendedActionConfidence,
  getApproveButtonLabelKey,
  getPreviewOutcomeKey,
  hasRecommendedActionsSection,
  isRecommendedActionsEmpty,
} from "./incidentRecommendedActionsViewUtils.js";

const t = (key) => key;

describe("incidentRecommendedActionsViewUtils", () => {
  it("detects recommended actions section and empty state", () => {
    expect(hasRecommendedActionsSection({ recommended_actions: [] })).toBe(true);
    expect(hasRecommendedActionsSection({})).toBe(false);
    expect(isRecommendedActionsEmpty({ recommended_actions: [] })).toBe(true);
  });

  it("renders action cards with badges, priority, and confidence", () => {
    const vm = buildRecommendedActionsViewModel(
      {
        recommended_actions: [
          {
            action_id: "analyze_pr:pr_analysis:github:483",
            title: "Analyze PR #483",
            description: "Review PR Analysis",
            reason: "High PR correlation",
            priority: 5,
            confidence: 0.9,
            action_type: "analyze_pr",
            requires_user_approval: true,
            related_entity_type: "pr_analysis",
            related_entity_id: "github:483",
          },
        ],
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(RECOMMENDED_ACTIONS_I18N_KEYS.title);
    expect(vm.approvalRequiredLabel).toBe(RECOMMENDED_ACTIONS_I18N_KEYS.approvalRequired);
    expect(vm.actions[0].priority).toBe(5);
    expect(vm.actions[0].confidenceText).toBe("90%");
    expect(vm.actions[0].approveButtonLabel).toBe(RECOMMENDED_ACTIONS_I18N_KEYS.approveAnalyze);
  });

  it("renders empty state via i18n key", () => {
    const vm = buildRecommendedActionsViewModel({ recommended_actions: [] }, t);
    expect(vm.emptyMessage).toBe("incident.qa.recommended_actions_empty");
  });

  it("maps approve button labels by action type", () => {
    expect(getApproveButtonLabelKey("run_test_suite")).toBe(RECOMMENDED_ACTIONS_I18N_KEYS.approveRun);
    expect(getApproveButtonLabelKey("analyze_pr")).toBe(RECOMMENDED_ACTIONS_I18N_KEYS.approveAnalyze);
    expect(getApproveButtonLabelKey("inspect_failed_run")).toBe(RECOMMENDED_ACTIONS_I18N_KEYS.approveInspect);
  });

  it("formats confidence percentage", () => {
    expect(formatRecommendedActionConfidence(0.845)).toBe("85%");
    expect(formatRecommendedActionConfidence("x")).toBe("—");
  });

  it("builds drilldown item for II-02D navigation", () => {
    const item = buildRecommendedActionDrilldownItem({
      title: "Inspect Failed Run",
      reason: "Correlated failure",
      related_entity_type: "run",
      related_entity_id: "RUN-99",
    });
    const target = buildDrilldownNavigation(item);
    expect(target?.state?.run_id).toBe("RUN-99");
  });

  it("exposes EN translation keys", () => {
    expect(RECOMMENDED_ACTIONS_I18N_KEYS.priority).toBe("incident.qa.recommended_actions_priority");
    expect(RECOMMENDED_ACTIONS_I18N_KEYS.confidence).toBe("incident.qa.recommended_actions_confidence");
    expect(RECOMMENDED_ACTIONS_I18N_KEYS.previewFutureNote).toBe(
      "incident.qa.recommended_actions_preview_future_note",
    );
  });

  it("builds read-only preview payload without execution hooks", () => {
    const payload = buildRecommendedActionPreviewPayload(
      {
        title: "Analyze PR #483",
        description: "Review PR Analysis",
        reason: "High PR correlation",
        action_type: "analyze_pr",
      },
      t,
    );
    expect(payload.title).toBe("Analyze PR #483");
    expect(payload.fields).toHaveLength(4);
    expect(payload.futureNote).toBe("incident.qa.recommended_actions_preview_future_note");
    expect(getPreviewOutcomeKey("analyze_pr")).toBe(
      "incident.qa.recommended_actions_preview_outcome_analyze_pr",
    );
  });
});
