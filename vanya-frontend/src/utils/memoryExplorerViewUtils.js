/** Memory Explorer — accordion view model for System Memory page. */

export const MEMORY_EXPLORER_I18N_KEYS = {
  title: "knowledge.explorer.title",
  empty: "knowledge.explorer.empty",
  routes: "knowledge.explorer.routes",
  apis: "knowledge.explorer.apis",
  tests: "knowledge.explorer.tests",
  clusters: "knowledge.explorer.clusters",
  summary: "knowledge.explorer.summary",
  noRoutes: "knowledge.explorer.no_routes",
  noApis: "knowledge.explorer.no_apis",
  noTests: "knowledge.explorer.no_tests",
  noClusters: "knowledge.explorer.no_clusters",
  source: "knowledge.explorer.source",
  confidence: "knowledge.explorer.confidence",
  occurrences: "knowledge.explorer.occurrences",
};

function confidenceBadge(confidence) {
  const c = (confidence || "low").toLowerCase();
  if (c === "high") return "badge badge-red";
  if (c === "medium") return "badge badge-orange";
  return "badge badge-gray";
}

export function buildMemoryExplorerViewModel(explorer, t) {
  if (!explorer || !Array.isArray(explorer.modules)) {
    return { show: false, modules: [], defaultExpanded: "" };
  }

  const modules = (explorer.modules || []).map((mod) => ({
    module: mod.module || "—",
    summary: mod.summary || "",
    counts: {
      routes: mod.counts?.routes ?? (mod.routes || []).length,
      apis: mod.counts?.apis ?? (mod.apis || []).length,
      tests: mod.counts?.tests ?? (mod.tests || []).length,
      failure_clusters: mod.counts?.failure_clusters ?? (mod.failure_clusters || []).length,
    },
    routes: mod.routes || [],
    apis: mod.apis || [],
    tests: mod.tests || [],
    failure_clusters: (mod.failure_clusters || []).map((c) => ({
      ...c,
      confidenceBadgeClass: confidenceBadge(c.confidence),
    })),
    sectionLabels: {
      routes: t(MEMORY_EXPLORER_I18N_KEYS.routes),
      apis: t(MEMORY_EXPLORER_I18N_KEYS.apis),
      tests: t(MEMORY_EXPLORER_I18N_KEYS.tests),
      clusters: t(MEMORY_EXPLORER_I18N_KEYS.clusters),
    },
    emptyLabels: {
      routes: t(MEMORY_EXPLORER_I18N_KEYS.noRoutes),
      apis: t(MEMORY_EXPLORER_I18N_KEYS.noApis),
      tests: t(MEMORY_EXPLORER_I18N_KEYS.noTests),
      clusters: t(MEMORY_EXPLORER_I18N_KEYS.noClusters),
    },
  }));

  const hasLinks = modules.some(
    (m) => m.counts.routes || m.counts.apis || m.counts.tests || m.counts.failure_clusters,
  );

  return {
    show: modules.length > 0,
    title: t(MEMORY_EXPLORER_I18N_KEYS.title),
    emptyMessage: t(MEMORY_EXPLORER_I18N_KEYS.empty),
    hasLinks,
    modules,
    defaultExpanded: explorer.default_expanded_module || modules[0]?.module || "",
    summaryLabel: t(MEMORY_EXPLORER_I18N_KEYS.summary),
    sourceLabel: t(MEMORY_EXPLORER_I18N_KEYS.source),
    confidenceLabel: t(MEMORY_EXPLORER_I18N_KEYS.confidence),
    occurrencesLabel: t(MEMORY_EXPLORER_I18N_KEYS.occurrences),
  };
}
