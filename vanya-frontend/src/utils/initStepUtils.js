// src/utils/initStepUtils.js
/** Pure helpers for project initialize step UI — testable without DOM. */

export const INIT_STEP_ORDER = ["catalog", "knowledge", "smoke_run", "readiness"];

export const INIT_CHECKLIST_KEYS = [
  "init.check.memory",
  "init.check.detect",
  "init.check.smoke",
  "init.check.dashboard",
];

export function stepStatusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s === "ok") return "badge-green";
  if (s === "skipped") return "badge-gray";
  if (s === "failed") return "badge-red";
  if (s === "partial") return "badge-orange";
  return "badge-gray";
}

export function stepStatusIcon(status) {
  const s = String(status || "").toLowerCase();
  if (s === "ok") return "✓";
  if (s === "skipped") return "○";
  if (s === "failed") return "✕";
  if (s === "partial") return "◐";
  return "•";
}

/** True when smoke_run step succeeded and a job was queued. */
export function isSmokeQueued(result) {
  if (!result) return false;
  const smoke = (result.steps || []).find((s) => s.step === "smoke_run");
  if (!smoke || smoke.status !== "ok") return false;
  return Boolean(result.job_id || smoke.details?.job_id);
}

const INIT_PANEL_DISMISS_PREFIX = "vanya_init_panel_dismissed_";

export function isInitializeProjectPanelDismissed(projectId) {
  const pid = String(projectId || "").trim();
  if (!pid) return false;
  try {
    return localStorage.getItem(`${INIT_PANEL_DISMISS_PREFIX}${pid}`) === "1";
  } catch {
    return false;
  }
}

export function dismissInitializeProjectPanel(projectId) {
  const pid = String(projectId || "").trim();
  if (!pid) return;
  try {
    localStorage.setItem(`${INIT_PANEL_DISMISS_PREFIX}${pid}`, "1");
  } catch {
    void 0;
  }
}

export function clearInitializeProjectPanelDismiss(projectId) {
  const pid = String(projectId || "").trim();
  if (!pid) return;
  try {
    localStorage.removeItem(`${INIT_PANEL_DISMISS_PREFIX}${pid}`);
  } catch {
    void 0;
  }
}

export function sortInitSteps(steps) {
  const list = Array.isArray(steps) ? steps : [];
  const rank = Object.fromEntries(INIT_STEP_ORDER.map((k, i) => [k, i]));
  return [...list].sort((a, b) => (rank[a.step] ?? 99) - (rank[b.step] ?? 99));
}
