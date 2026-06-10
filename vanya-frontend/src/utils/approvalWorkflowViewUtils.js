/** View helpers for Approval Workflow Foundation (II-06B). */

export const APPROVAL_WORKFLOW_I18N_KEYS = {
  title: "incident.qa.approval_workflow",
  empty: "incident.qa.approval_workflow_empty",
  pending: "incident.qa.approval_workflow_pending",
  approved: "incident.qa.approval_workflow_approved",
  rejected: "incident.qa.approval_workflow_rejected",
  approvalType: "incident.qa.approval_workflow_type",
  status: "incident.qa.approval_workflow_status",
  approve: "incident.qa.approval_workflow_approve",
  reject: "incident.qa.approval_workflow_reject",
  preview: "incident.qa.approval_workflow_preview",
  previewSubtitle: "incident.qa.approval_workflow_preview_subtitle",
  relatedEntity: "incident.qa.approval_workflow_related_entity",
  currentStatus: "incident.qa.approval_workflow_current_status",
  futureCapability: "incident.qa.approval_workflow_future_capability",
  futureFooter: "incident.qa.approval_workflow_future_footer",
  readOnlyNote: "incident.qa.approval_workflow_read_only_note",
  statusPending: "incident.qa.approval_workflow_status_pending",
  statusApproved: "incident.qa.approval_workflow_status_approved",
  statusRejected: "incident.qa.approval_workflow_status_rejected",
};

const STATUS_LABEL_KEYS = {
  PENDING: APPROVAL_WORKFLOW_I18N_KEYS.statusPending,
  APPROVED: APPROVAL_WORKFLOW_I18N_KEYS.statusApproved,
  REJECTED: APPROVAL_WORKFLOW_I18N_KEYS.statusRejected,
};

const STATUS_BADGE_CLASS = {
  PENDING: "badge badge-orange",
  APPROVED: "badge badge-gray",
  REJECTED: "badge badge-red",
};

export function hasApprovalWorkflowSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "approval_workflow");
}

export function isApprovalWorkflowEmpty(report) {
  return report?.approval_workflow == null;
}

export function getApprovalStatusLabelKey(status) {
  const key = String(status || "PENDING").toUpperCase();
  return STATUS_LABEL_KEYS[key] || APPROVAL_WORKFLOW_I18N_KEYS.statusPending;
}

export function getApprovalStatusBadgeClass(status) {
  const key = String(status || "PENDING").toUpperCase();
  return STATUS_BADGE_CLASS[key] || STATUS_BADGE_CLASS.PENDING;
}

export function formatRelatedEntity(request) {
  const et = String(request?.related_entity_type || "").trim();
  const eid = String(request?.related_entity_id || "").trim();
  if (!et && !eid) return "—";
  if (et && eid) return `${et}: ${eid}`;
  return et || eid;
}

export function buildApprovalDrilldownItem(request) {
  if (!request?.related_entity_type || !request?.related_entity_id) return null;
  const et = String(request.related_entity_type);
  if (et === "hypothesis") return null;
  return {
    source: et,
    related_entity_type: et,
    related_entity_id: request.related_entity_id,
    reason: request.description,
    detail: request.title,
    title: request.title,
  };
}

export function buildApprovalPreviewPayload(request, t) {
  return {
    title: request?.title || t(APPROVAL_WORKFLOW_I18N_KEYS.title),
    fields: [
      { label: t(APPROVAL_WORKFLOW_I18N_KEYS.approvalType), value: request?.approval_type || "—" },
      { label: t(APPROVAL_WORKFLOW_I18N_KEYS.relatedEntity), value: formatRelatedEntity(request) },
      {
        label: t(APPROVAL_WORKFLOW_I18N_KEYS.currentStatus),
        value: t(getApprovalStatusLabelKey(request?.status)),
      },
      { label: t(APPROVAL_WORKFLOW_I18N_KEYS.futureCapability), value: t(APPROVAL_WORKFLOW_I18N_KEYS.previewSubtitle) },
      { label: t(APPROVAL_WORKFLOW_I18N_KEYS.status), value: request?.description || "—" },
    ],
    foundationNote: t(APPROVAL_WORKFLOW_I18N_KEYS.previewSubtitle),
    futureFooter: t(APPROVAL_WORKFLOW_I18N_KEYS.futureFooter),
    readOnlyNote: t(APPROVAL_WORKFLOW_I18N_KEYS.readOnlyNote),
  };
}

export function buildApprovalWorkflowViewModel(report, t) {
  const workflow = report?.approval_workflow ?? null;
  return {
    show: hasApprovalWorkflowSection(report),
    empty: isApprovalWorkflowEmpty(report),
    emptyMessage: t(APPROVAL_WORKFLOW_I18N_KEYS.empty),
    title: t(APPROVAL_WORKFLOW_I18N_KEYS.title),
    pendingLabel: t(APPROVAL_WORKFLOW_I18N_KEYS.pending),
    approvedLabel: t(APPROVAL_WORKFLOW_I18N_KEYS.approved),
    rejectedLabel: t(APPROVAL_WORKFLOW_I18N_KEYS.rejected),
    approvalTypeLabel: t(APPROVAL_WORKFLOW_I18N_KEYS.approvalType),
    statusLabel: t(APPROVAL_WORKFLOW_I18N_KEYS.status),
    approveLabel: t(APPROVAL_WORKFLOW_I18N_KEYS.approve),
    rejectLabel: t(APPROVAL_WORKFLOW_I18N_KEYS.reject),
    readOnlyNote: t(APPROVAL_WORKFLOW_I18N_KEYS.readOnlyNote),
    workflow: workflow
      ? {
          ...workflow,
          requests: (workflow.requests || []).map((request) => ({
            ...request,
            statusLabel: t(getApprovalStatusLabelKey(request.status)),
            statusBadgeClass: getApprovalStatusBadgeClass(request.status),
            drilldownItem: buildApprovalDrilldownItem(request),
            previewPayload: buildApprovalPreviewPayload(request, t),
          })),
        }
      : null,
  };
}
