/** BW-04A — Baseline explorer helpers (frontend-only). */

export const BASELINE_HEALTH_I18N = {
  fresh: "watch.baseline.health.fresh",
  aging: "watch.baseline.health.aging",
  stale: "watch.baseline.health.stale",
  none: "watch.baseline.health.none",
};

const HEALTH_BADGE = {
  fresh: "badge-green",
  aging: "badge-orange",
  stale: "badge-red",
  none: "badge-gray",
};

const MS_PER_DAY = 1000 * 60 * 60 * 24;

/** Classify baseline age: <7d fresh, 7–30d aging, >30d stale. */
export function resolveBaselineHealthBucket(baselineSetAt, nowMs = Date.now()) {
  if (!baselineSetAt) return "none";
  try {
    const ms = nowMs - new Date(baselineSetAt).getTime();
    if (!Number.isFinite(ms) || ms < 0) return "none";
    const days = ms / MS_PER_DAY;
    if (days < 7) return "fresh";
    if (days <= 30) return "aging";
    return "stale";
  } catch {
    return "none";
  }
}

export function baselineHealthBadgeClass(bucket) {
  return HEALTH_BADGE[bucket] || "badge-gray";
}

export function baselineHealthLabelKey(bucket) {
  return BASELINE_HEALTH_I18N[bucket] || BASELINE_HEALTH_I18N.none;
}

export function shortInspectionId(id, maxLen = 10) {
  if (id == null || id === "" || id === "_") return "";
  const s = String(id);
  return s.length <= maxLen ? s : `${s.slice(0, 8)}…`;
}

export function formatBaselineDate(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      year: "numeric",
      month: "short",
      day: "numeric",
      hour: "numeric",
      minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

export function filterBaselineSetEvents(events) {
  return (Array.isArray(events) ? events : []).filter((e) => e?.event_type === "baseline_set");
}

export function buildBaselineHistoryRow(event) {
  return {
    eventId: event?.event_id,
    createdAt: event?.created_at,
    inspectionId: event?.target_inspection_id,
  };
}

export function canCompareBaseline(watch) {
  const base = String(watch?.baseline_inspection_id || "").trim();
  const latest = String(watch?.last_inspection_id || "").trim();
  return Boolean(base && latest && base !== "_" && latest !== "_" && base !== latest);
}

export function resolveBaselineUpdatedByLabel(updatedBy, t) {
  const v = String(updatedBy || "").trim();
  if (!v) return t("watch.baseline.updated_by_system");
  return v;
}

export function buildDiffHighlights(diff, t) {
  if (!diff || typeof diff !== "object") return [];
  const changes = diff.changes || {};
  const counts = changes.counts_delta || {};
  const rows = [];

  if (diff.visual_hash_changed != null) {
    rows.push({
      label: t("watch.baseline.diff.visual_hash"),
      value: diff.visual_hash_changed ? t("watch.baseline.diff.yes") : t("watch.baseline.diff.no"),
    });
  }
  if (diff.visual_change_level) {
    rows.push({
      label: t("watch.baseline.diff.visual_level"),
      value: String(diff.visual_change_level),
    });
  }
  if (changes.title_changed) {
    rows.push({ label: t("watch.baseline.diff.title"), value: t("watch.baseline.diff.changed") });
  }
  if (changes.final_url_changed) {
    rows.push({ label: t("watch.baseline.diff.url"), value: t("watch.baseline.diff.changed") });
  }
  if (changes.page_type_changed) {
    rows.push({ label: t("watch.baseline.diff.page_type"), value: t("watch.baseline.diff.changed") });
  }

  const selectorDelta = Number(counts.selector_candidates_count) || 0;
  if (selectorDelta !== 0) {
    rows.push({
      label: t("watch.baseline.diff.selectors"),
      value: selectorDelta > 0 ? `+${selectorDelta}` : String(selectorDelta),
    });
  }

  const consoleDelta = Number(changes.console_errors_delta) || 0;
  if (consoleDelta !== 0) {
    rows.push({
      label: t("watch.baseline.diff.console"),
      value: consoleDelta > 0 ? `+${consoleDelta}` : String(consoleDelta),
    });
  }

  const networkDelta = Number(changes.network_errors_delta) || 0;
  if (networkDelta !== 0) {
    rows.push({
      label: t("watch.baseline.diff.network"),
      value: networkDelta > 0 ? `+${networkDelta}` : String(networkDelta),
    });
  }

  const navSignals = Array.isArray(changes.primary_actions_changed) ? changes.primary_actions_changed : [];
  if (navSignals.length > 0) {
    rows.push({
      label: t("watch.baseline.diff.navigation"),
      value: String(navSignals.length),
    });
  }

  return rows;
}
