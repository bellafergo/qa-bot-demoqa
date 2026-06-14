/**
 * Memory Depth scoring for System Memory (Phase B).
 * Replaces misleading tests/modules coverage percentage.
 */

const DEPTH_WEIGHTS = {
  modules: 0.2,
  routes: 0.2,
  apis: 0.15,
  tests: 0.15,
  prismaModels: 0.15,
  repositoryBonus: 0.15,
};

function clamp01(n) {
  return Math.max(0, Math.min(1, n));
}

function normalizeCount(count, cap) {
  return clamp01((Number(count) || 0) / cap);
}

export function computeMemoryDepth(knowledge) {
  if (!knowledge) {
    return { score: 0, label: "low", sources: [], repositoryIndexed: false };
  }

  const modules = knowledge.modules || [];
  const routes = knowledge.routes || [];
  const apis = knowledge.apis || [];
  const tests = knowledge.related_tests || [];
  const meta = knowledge.metadata || {};
  const sources = meta.reconstruction_sources || [];

  const prismaModels = modules.filter(
    (m) => (m.entity_type || "") === "prisma_model" || (m.fields || []).length > 0,
  ).length;

  const moduleSignal = normalizeCount(modules.length, 12);
  const routeSignal = normalizeCount(routes.length, 25);
  const apiSignal = normalizeCount(apis.length, 20);
  const testSignal = normalizeCount(tests.length, 20);
  const prismaSignal = normalizeCount(prismaModels, 10);
  const repoBonus = meta.repository_indexed ? 1 : 0;

  const raw =
    moduleSignal * DEPTH_WEIGHTS.modules +
    routeSignal * DEPTH_WEIGHTS.routes +
    apiSignal * DEPTH_WEIGHTS.apis +
    testSignal * DEPTH_WEIGHTS.tests +
    prismaSignal * DEPTH_WEIGHTS.prismaModels +
    repoBonus * DEPTH_WEIGHTS.repositoryBonus;

  const score = Math.round(clamp01(raw) * 100);
  const label = score >= 70 ? "high" : score >= 40 ? "medium" : "low";

  return {
    score,
    label,
    sources,
    repositoryIndexed: Boolean(meta.repository_indexed),
    repositoryFilesScanned: meta.repository_files_scanned || 0,
    repositoryWarnings: meta.repository_warnings || [],
  };
}

export function formatMemoryDepthLabel(label, t) {
  const key = `knowledge.kpi.memory_depth.${label || "low"}`;
  return t(key);
}

export function formatSourcesLabel(sources, t) {
  if (!sources || sources.length === 0) return t("knowledge.sources.none");
  return sources
    .map((s) => t(`knowledge.sources.${s}`, { defaultValue: s }))
    .join(" · ");
}
