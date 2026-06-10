/** View helpers for Human Approved Actions (II-04A). */

export const RECOMMENDED_ACTIONS_I18N_KEYS = {
  title: "incident.qa.recommended_actions",
  approvalRequired: "incident.qa.recommended_actions_approval_required",
  priority: "incident.qa.recommended_actions_priority",
  confidence: "incident.qa.recommended_actions_confidence",
  empty: "incident.qa.recommended_actions_empty",
  actionType: "incident.qa.recommended_actions_action_type",
  approveRun: "incident.qa.recommended_actions_approve_run",
  approveAnalyze: "incident.qa.recommended_actions_approve_analyze",
  approveInspect: "incident.qa.recommended_actions_approve_inspect",
  previewSubtitle: "incident.qa.recommended_actions_preview_subtitle",
  previewWhatHappens: "incident.qa.recommended_actions_preview_what_happens",
  previewReason: "incident.qa.recommended_actions_preview_reason",
  previewDescription: "incident.qa.recommended_actions_preview_description",
  previewFutureNote: "incident.qa.recommended_actions_preview_future_note",
};

const PREVIEW_OUTCOME_KEYS = {
  run_test_suite: "incident.qa.recommended_actions_preview_outcome_run_test_suite",
  run_browser_probe: "incident.qa.recommended_actions_preview_outcome_run_browser_probe",
  analyze_pr: "incident.qa.recommended_actions_preview_outcome_analyze_pr",
  inspect_failure_cluster: "incident.qa.recommended_actions_preview_outcome_inspect_failure_cluster",
  inspect_failed_run: "incident.qa.recommended_actions_preview_outcome_inspect_failed_run",
  review_browser_watch: "incident.qa.recommended_actions_preview_outcome_review_browser_watch",
  review_impacted_area: "incident.qa.recommended_actions_preview_outcome_review_impacted_area",
  review_hypothesis: "incident.qa.recommended_actions_preview_outcome_review_hypothesis",
  review_timeline: "incident.qa.recommended_actions_preview_outcome_review_timeline",
  default: "incident.qa.recommended_actions_preview_outcome_default",
};

const RUN_ACTION_TYPES = new Set(["run_test_suite", "run_browser_probe"]);
const ANALYZE_ACTION_TYPES = new Set(["analyze_pr"]);
const INSPECT_ACTION_TYPES = new Set([
  "inspect_failure_cluster",
  "inspect_failed_run",
  "review_browser_watch",
  "review_impacted_area",
  "review_hypothesis",
  "review_timeline",
]);

export function hasRecommendedActionsSection(report) {
  return Array.isArray(report?.recommended_actions);
}

export function isRecommendedActionsEmpty(report) {
  return !report?.recommended_actions?.length;
}

export function formatRecommendedActionConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function getApproveButtonLabelKey(actionType) {
  const t = String(actionType || "");
  if (RUN_ACTION_TYPES.has(t)) return RECOMMENDED_ACTIONS_I18N_KEYS.approveRun;
  if (ANALYZE_ACTION_TYPES.has(t)) return RECOMMENDED_ACTIONS_I18N_KEYS.approveAnalyze;
  if (INSPECT_ACTION_TYPES.has(t)) return RECOMMENDED_ACTIONS_I18N_KEYS.approveInspect;
  return RECOMMENDED_ACTIONS_I18N_KEYS.approveInspect;
}

export function getPreviewOutcomeKey(actionType) {
  return PREVIEW_OUTCOME_KEYS[String(actionType || "")] || PREVIEW_OUTCOME_KEYS.default;
}

export function buildRecommendedActionPreviewPayload(action, t) {
  const outcomeKey = getPreviewOutcomeKey(action?.action_type);
  return {
    title: action?.title || t(RECOMMENDED_ACTIONS_I18N_KEYS.title),
    fields: [
      { label: t(RECOMMENDED_ACTIONS_I18N_KEYS.actionType), value: action?.action_type || "—" },
      { label: t(RECOMMENDED_ACTIONS_I18N_KEYS.previewWhatHappens), value: t(outcomeKey) },
      { label: t(RECOMMENDED_ACTIONS_I18N_KEYS.previewReason), value: action?.reason || "—" },
      { label: t(RECOMMENDED_ACTIONS_I18N_KEYS.previewDescription), value: action?.description || "—" },
    ],
    futureNote: t(RECOMMENDED_ACTIONS_I18N_KEYS.previewFutureNote),
  };
}

export function buildRecommendedActionDrilldownItem(action) {
  if (!action?.related_entity_type || !action?.related_entity_id) return null;
  return {
    source: action.related_entity_type,
    related_entity_type: action.related_entity_type,
    related_entity_id: action.related_entity_id,
    reason: action.reason,
    detail: action.title,
    title: action.title,
  };
}

export function buildRecommendedActionsViewModel(report, t) {
  const actions = Array.isArray(report?.recommended_actions) ? report.recommended_actions : [];
  return {
    show: hasRecommendedActionsSection(report),
    empty: actions.length === 0,
    emptyMessage: t(RECOMMENDED_ACTIONS_I18N_KEYS.empty),
    title: t(RECOMMENDED_ACTIONS_I18N_KEYS.title),
    approvalRequiredLabel: t(RECOMMENDED_ACTIONS_I18N_KEYS.approvalRequired),
    priorityLabel: t(RECOMMENDED_ACTIONS_I18N_KEYS.priority),
    confidenceLabel: t(RECOMMENDED_ACTIONS_I18N_KEYS.confidence),
    actionTypeLabel: t(RECOMMENDED_ACTIONS_I18N_KEYS.actionType),
    actions: actions.map((action) => ({
      ...action,
      confidenceText: formatRecommendedActionConfidence(action.confidence),
      approveButtonLabel: t(getApproveButtonLabelKey(action.action_type)),
      drilldownItem: buildRecommendedActionDrilldownItem(action),
    })),
  };
}
