/**
 * Browser Watch UI helpers (stabilization — no React).
 * @module browserWatchUi
 */

/** @typedef {'none' | 'agent' | 'project_local' | 'generic'} WatchFieldErrorKind */

/**
 * Whether switching execution_mode to "cloud" should clear the current field error.
 * Only agent / project-local validation errors are tied to local_agent mode.
 * @param {WatchFieldErrorKind} kind
 * @returns {boolean}
 */
export function shouldClearWatchFieldErrorWhenSwitchingToCloud(kind) {
  return kind === "agent" || kind === "project_local";
}

/**
 * Normalize run_origin from API.
 * @param {unknown} raw
 * @returns {"cloud" | "local_agent" | null}
 */
export function normalizeRunOrigin(raw) {
  if (raw == null || String(raw).trim() === "") return null;
  const v = String(raw).trim().toLowerCase();
  if (v === "local_agent") return "local_agent";
  if (v === "cloud") return "cloud";
  return null;
}

/**
 * Effective run origin for an event row: prefer API `run_origin` (from visual_meta);
 * for lifecycle events without persisted meta, infer from watch.execution_mode.
 * @param {{ run_origin?: string | null, event_type?: string | null }} event
 * @param {{ execution_mode?: string | null } | null | undefined} watch
 * @returns {"cloud" | "local_agent" | null}
 */
export function effectiveEventRunOrigin(event, watch) {
  const direct = normalizeRunOrigin(event?.run_origin);
  if (direct) return direct;
  const et = String(event?.event_type || "").toLowerCase();
  if (et === "run_started" || et === "run_completed") {
    const w = String(watch?.execution_mode || "cloud").toLowerCase();
    return w === "local_agent" ? "local_agent" : "cloud";
  }
  return null;
}
