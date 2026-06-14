/** Business Workflows — view model for System Memory page. */

export const BUSINESS_WORKFLOW_I18N_KEYS = {
  title: "knowledge.workflows.title",
  empty: "knowledge.workflows.empty",
  confidence: "knowledge.workflows.confidence",
  confidenceHigh: "knowledge.workflows.confidence_high",
  confidenceMedium: "knowledge.workflows.confidence_medium",
  confidenceLow: "knowledge.workflows.confidence_low",
  modules: "knowledge.workflows.modules",
  routes: "knowledge.workflows.routes",
  apis: "knowledge.workflows.apis",
  tests: "knowledge.workflows.tests",
  clusters: "knowledge.workflows.clusters",
  summary: "knowledge.workflows.summary",
  noModules: "knowledge.workflows.no_modules",
  noRoutes: "knowledge.workflows.no_routes",
  noApis: "knowledge.workflows.no_apis",
  noTests: "knowledge.workflows.no_tests",
  noClusters: "knowledge.workflows.no_clusters",
};

function confidenceBadge(confidence) {
  const c = (confidence || "low").toLowerCase();
  if (c === "high") return "badge badge-green";
  if (c === "medium") return "badge badge-orange";
  return "badge badge-gray";
}

function confidenceLabel(confidence, t) {
  const c = (confidence || "low").toLowerCase();
  if (c === "high") return t(BUSINESS_WORKFLOW_I18N_KEYS.confidenceHigh);
  if (c === "medium") return t(BUSINESS_WORKFLOW_I18N_KEYS.confidenceMedium);
  return t(BUSINESS_WORKFLOW_I18N_KEYS.confidenceLow);
}

export function buildBusinessWorkflowsViewModel(workflowsPayload, t) {
  if (!workflowsPayload || !Array.isArray(workflowsPayload.workflows)) {
    return { show: true, workflows: [], emptyMessage: t(BUSINESS_WORKFLOW_I18N_KEYS.empty) };
  }

  const workflows = (workflowsPayload.workflows || []).map((wf) => ({
    name: wf.name || "—",
    type: wf.type || "",
    confidence: (wf.confidence || "low").toLowerCase(),
    confidenceLabel: confidenceLabel(wf.confidence, t),
    confidenceBadgeClass: confidenceBadge(wf.confidence),
    modules: wf.modules || [],
    routes: wf.routes || [],
    apis: wf.apis || [],
    tests: wf.tests || [],
    failure_clusters: wf.failure_clusters || [],
    summary: wf.summary || "",
    counts: {
      routes: wf.coverage?.routes ?? (wf.routes || []).length,
      apis: wf.coverage?.apis ?? (wf.apis || []).length,
      tests: wf.coverage?.tests ?? (wf.tests || []).length,
      clusters: wf.coverage?.clusters ?? (wf.failure_clusters || []).length,
    },
    sectionLabels: {
      modules: t(BUSINESS_WORKFLOW_I18N_KEYS.modules),
      routes: t(BUSINESS_WORKFLOW_I18N_KEYS.routes),
      apis: t(BUSINESS_WORKFLOW_I18N_KEYS.apis),
      tests: t(BUSINESS_WORKFLOW_I18N_KEYS.tests),
      clusters: t(BUSINESS_WORKFLOW_I18N_KEYS.clusters),
      summary: t(BUSINESS_WORKFLOW_I18N_KEYS.summary),
      confidence: t(BUSINESS_WORKFLOW_I18N_KEYS.confidence),
    },
    emptyLabels: {
      modules: t(BUSINESS_WORKFLOW_I18N_KEYS.noModules),
      routes: t(BUSINESS_WORKFLOW_I18N_KEYS.noRoutes),
      apis: t(BUSINESS_WORKFLOW_I18N_KEYS.noApis),
      tests: t(BUSINESS_WORKFLOW_I18N_KEYS.noTests),
      clusters: t(BUSINESS_WORKFLOW_I18N_KEYS.noClusters),
    },
  }));

  return {
    show: true,
    title: t(BUSINESS_WORKFLOW_I18N_KEYS.title),
    emptyMessage: t(BUSINESS_WORKFLOW_I18N_KEYS.empty),
    workflows,
    hasWorkflows: workflows.length > 0,
  };
}
