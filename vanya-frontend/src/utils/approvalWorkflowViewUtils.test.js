import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  APPROVAL_WORKFLOW_I18N_KEYS,
  buildApprovalDrilldownItem,
  buildApprovalPreviewPayload,
  buildApprovalWorkflowViewModel,
  getApprovalStatusBadgeClass,
  getApprovalStatusLabelKey,
  hasApprovalWorkflowSection,
  isApprovalWorkflowEmpty,
} from "./approvalWorkflowViewUtils.js";

const t = (key) => key;

describe("approvalWorkflowViewUtils", () => {
  it("detects approval workflow section and empty state", () => {
    expect(hasApprovalWorkflowSection({ approval_workflow: null })).toBe(true);
    expect(hasApprovalWorkflowSection({})).toBe(false);
    expect(isApprovalWorkflowEmpty({ approval_workflow: null })).toBe(true);
  });

  it("renders summary counts and request cards", () => {
    const vm = buildApprovalWorkflowViewModel(
      {
        approval_workflow: {
          pending_count: 2,
          approved_count: 0,
          rejected_count: 0,
          requests: [
            {
              approval_id: "approval:analyze_pr:pr_analysis:github:483",
              approval_type: "analyze_pr",
              title: "Analyze PR #483",
              description: "Review correlated pull request.",
              status: "PENDING",
              created_at: "2026-06-10T12:00:00+00:00",
              related_entity_type: "pr_analysis",
              related_entity_id: "github:483",
            },
          ],
        },
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.workflow.pending_count).toBe(2);
    expect(vm.workflow.requests[0].statusLabel).toBe(APPROVAL_WORKFLOW_I18N_KEYS.statusPending);
    expect(vm.workflow.requests[0].statusBadgeClass).toBe("badge badge-orange");
  });

  it("renders empty state via i18n key", () => {
    const vm = buildApprovalWorkflowViewModel({ approval_workflow: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.approval_workflow_empty");
  });

  it("maps status label keys and badge classes", () => {
    expect(getApprovalStatusLabelKey("APPROVED")).toBe(APPROVAL_WORKFLOW_I18N_KEYS.statusApproved);
    expect(getApprovalStatusBadgeClass("REJECTED")).toBe("badge badge-red");
  });

  it("builds preview modal payload", () => {
    const payload = buildApprovalPreviewPayload(
      {
        title: "Checkout Smoke Suite",
        approval_type: "smoke",
        status: "PENDING",
        description: "Validate checkout.",
        related_entity_type: "failure_cluster",
        related_entity_id: "cluster_7",
      },
      t,
    );
    expect(payload.title).toBe("Checkout Smoke Suite");
    expect(payload.futureFooter).toBe(APPROVAL_WORKFLOW_I18N_KEYS.futureFooter);
    expect(payload.foundationNote).toBe(APPROVAL_WORKFLOW_I18N_KEYS.previewSubtitle);
  });

  it("builds drilldown item for II-02D navigation", () => {
    const item = buildApprovalDrilldownItem({
      title: "Analyze PR #483",
      description: "PR overlap",
      related_entity_type: "pr_analysis",
      related_entity_id: "github:483",
    });
    const target = buildDrilldownNavigation(item);
    expect(target?.path).toContain("/pr-analysis");
  });

  it("exposes approve and reject translation keys", () => {
    const vm = buildApprovalWorkflowViewModel({ approval_workflow: null }, t);
    expect(vm.approveLabel).toBe("incident.qa.approval_workflow_approve");
    expect(vm.rejectLabel).toBe("incident.qa.approval_workflow_reject");
  });

  it("exposes translation keys", () => {
    expect(APPROVAL_WORKFLOW_I18N_KEYS.title).toBe("incident.qa.approval_workflow");
    expect(APPROVAL_WORKFLOW_I18N_KEYS.pending).toBe("incident.qa.approval_workflow_pending");
  });
});
