/** UX-03 — Browser Monitoring enterprise view helpers (frontend-only). */

export const BROWSER_WATCH_I18N_KEYS = {
  title: "watch.enterprise.title",
  subtitle: "watch.enterprise.subtitle",
  summaryTotal: "watch.enterprise.summary.total",
  summaryHealthy: "watch.enterprise.summary.healthy",
  summaryWarnings: "watch.enterprise.summary.warnings",
  summaryCritical: "watch.enterprise.summary.critical",
  emptyTitle: "watch.enterprise.empty.title",
  emptyDesc: "watch.enterprise.empty.desc",
  listUrl: "watch.enterprise.list.url",
  listStatus: "watch.enterprise.list.status",
  listExecution: "watch.enterprise.list.execution",
  listCompare: "watch.enterprise.list.compare",
  listLastRun: "watch.enterprise.list.last_run",
  listLastChange: "watch.enterprise.list.last_change",
  statusHealthy: "watch.enterprise.status.healthy",
  statusWarning: "watch.enterprise.status.warning",
  statusCritical: "watch.enterprise.status.critical",
  statusNeverRun: "watch.enterprise.status.never_run",
  statusDisabled: "watch.enterprise.status.disabled",
  detailInfo: "watch.enterprise.detail.info",
  detailBaselineMode: "watch.enterprise.detail.baseline_mode",
  detailCurrentStatus: "watch.enterprise.detail.current_status",
  detailTotalRuns: "watch.enterprise.detail.total_runs",
  detailDiffsFound: "watch.enterprise.detail.diffs_found",
  detailAlertsTriggered: "watch.enterprise.detail.alerts_triggered",
  detailTimeline: "watch.enterprise.detail.timeline",
  detailSelect: "watch.enterprise.detail.select",
  actionRunNow: "watch.action.run",
  actionEdit: "watch.action.edit",
  actionBaseline: "watch.action.baseline",
  actionDisable: "watch.action.disable",
  actionEnable: "watch.action.enable",
  toastCreated: "watch.enterprise.toast.created",
};

const STATUS_BADGE = {
  healthy: "badge-green",
  warning: "badge-orange",
  critical: "badge-red",
  never_run: "badge-gray",
  disabled: "badge-gray",
};

/** Classify watch for summary buckets and row badges. */
export function resolveWatchHealthBucket(watch) {
  if (!watch?.enabled) return "disabled";
  const status = String(watch.current_status || watch.last_status || "").toLowerCase();
  if (!watch.last_run_at && !status) return "never_run";
  if (status === "failed") return "critical";
  if (status === "changed") return "warning";
  const change = String(watch.last_change_level || watch.last_visual_change_level || "").toLowerCase();
  if (change === "high") return "critical";
  if (change === "medium" || change === "low") return "warning";
  if (status === "healthy") return "healthy";
  if (!watch.last_run_at) return "never_run";
  return "healthy";
}

export function watchStatusBadgeClass(bucket) {
  return STATUS_BADGE[bucket] || "badge-gray";
}

export function watchStatusLabelKey(bucket) {
  if (bucket === "healthy") return BROWSER_WATCH_I18N_KEYS.statusHealthy;
  if (bucket === "warning") return BROWSER_WATCH_I18N_KEYS.statusWarning;
  if (bucket === "critical") return BROWSER_WATCH_I18N_KEYS.statusCritical;
  if (bucket === "disabled") return BROWSER_WATCH_I18N_KEYS.statusDisabled;
  return BROWSER_WATCH_I18N_KEYS.statusNeverRun;
}

export function buildWatchSummaryStats(watches) {
  const list = Array.isArray(watches) ? watches : [];
  let healthy = 0;
  let warnings = 0;
  let critical = 0;
  for (const w of list) {
    const bucket = resolveWatchHealthBucket(w);
    if (bucket === "healthy") healthy += 1;
    else if (bucket === "warning") warnings += 1;
    else if (bucket === "critical") critical += 1;
  }
  return { total: list.length, healthy, warnings, critical };
}

/** Display name from URL hostname. */
export function resolveWatchName(watch) {
  const url = String(watch?.url || "").trim();
  if (!url) return "Watch";
  try {
    const u = new URL(url);
    return u.hostname || url;
  } catch {
    return url.length > 48 ? `${url.slice(0, 45)}…` : url;
  }
}

/** Truncate URL for list rows — single line, no vertical break. */
export function truncateWatchUrl(url, maxLen = 52) {
  const s = String(url || "").trim();
  if (!s) return "—";
  if (s.length <= maxLen) return s;
  const head = Math.max(20, Math.floor(maxLen * 0.55));
  const tail = Math.max(8, maxLen - head - 1);
  return `${s.slice(0, head)}…${s.slice(-tail)}`;
}

export function formatRelativeTime(iso, nowMs = Date.now()) {
  if (!iso) return "—";
  try {
    const d = new Date(iso);
    const ms = nowMs - d.getTime();
    if (!Number.isFinite(ms)) return "—";
    const sec = Math.floor(ms / 1000);
    if (sec < 45) return "just now";
    const min = Math.floor(sec / 60);
    if (min < 60) return `${min} minute${min === 1 ? "" : "s"} ago`;
    const hr = Math.floor(min / 60);
    if (hr < 24) return `${hr} hour${hr === 1 ? "" : "s"} ago`;
    const day = Math.floor(hr / 24);
    return `${day} day${day === 1 ? "" : "s"} ago`;
  } catch {
    return "—";
  }
}

export function formatWatchTimestamp(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      hour: "numeric",
      minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

export function compareModeLabel(mode, t) {
  const v = String(mode || "last").toLowerCase();
  return v === "baseline" ? t("watch.compare.baseline") : t("watch.compare.last");
}

export function executionModeLabel(mode, t) {
  const v = String(mode || "cloud").toLowerCase();
  return v === "local_agent" ? t("watch.mode.badge_local_agent") : t("watch.mode.badge_cloud");
}

export function changeLevelLabel(watch) {
  return (
    watch?.last_change_level
    ?? watch?.last_effective_change_level
    ?? watch?.last_visual_change_level
    ?? "—"
  );
}

/** Human-readable event label for timeline. */
export function formatEventTimelineLabel(event, t) {
  const type = String(event?.event_type || "").toLowerCase();
  const summary = String(event?.summary || "").trim();
  if (summary) return summary.length > 120 ? `${summary.slice(0, 117)}…` : summary;
  if (type.includes("alert")) return t("watch.enterprise.event.alert_triggered");
  if (type.includes("baseline")) return t("watch.enterprise.event.baseline_updated");
  if (type.includes("diff") || type.includes("change")) return t("watch.enterprise.event.visual_diff");
  if (type.includes("run_completed") || type.includes("completed")) return t("watch.enterprise.event.watch_executed");
  if (type.includes("run_started") || type.includes("started")) return t("watch.enterprise.event.run_started");
  return type.replace(/_/g, " ") || t("watch.enterprise.event.generic");
}

export function buildWatchListItemViewModel(watch, t, nowMs) {
  const bucket = resolveWatchHealthBucket(watch);
  return {
    watchId: watch.watch_id,
    name: resolveWatchName(watch),
    url: watch.url,
    urlDisplay: truncateWatchUrl(watch.url),
    statusBucket: bucket,
    statusBadgeClass: watchStatusBadgeClass(bucket),
    statusLabel: t(watchStatusLabelKey(bucket)),
    executionLabel: executionModeLabel(watch.execution_mode, t),
    compareLabel: compareModeLabel(watch.compare_mode, t),
    lastRunText: formatRelativeTime(watch.last_run_at, nowMs),
    lastChangeText: changeLevelLabel(watch),
    needsAttention: bucket === "critical" || bucket === "warning",
  };
}

export function buildWatchSummaryViewModel(watches, t) {
  const stats = buildWatchSummaryStats(watches);
  return {
    totalLabel: t(BROWSER_WATCH_I18N_KEYS.summaryTotal),
    healthyLabel: t(BROWSER_WATCH_I18N_KEYS.summaryHealthy),
    warningsLabel: t(BROWSER_WATCH_I18N_KEYS.summaryWarnings),
    criticalLabel: t(BROWSER_WATCH_I18N_KEYS.summaryCritical),
    ...stats,
  };
}
