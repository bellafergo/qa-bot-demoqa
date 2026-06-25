/** Human-readable labels for raw browser-watch status values. */

export const WATCH_STATUS_VALUE_KEYS = {
  healthy: "watch.enterprise.status.healthy",
  changed: "watch.enterprise.status.changed",
  failed: "watch.enterprise.status.failed",
  warning: "watch.enterprise.status.warning",
  none: "watch.enterprise.status.never_run",
  never_run: "watch.enterprise.status.never_run",
};

export function formatWatchStatusValue(status, t) {
  const key = String(status || "").trim().toLowerCase();
  if (!key || key === "none") return t(WATCH_STATUS_VALUE_KEYS.never_run);
  const labelKey = WATCH_STATUS_VALUE_KEYS[key];
  return labelKey ? t(labelKey) : key.replace(/_/g, " ");
}

export function formatWatchChangeLevel(level, t) {
  const key = String(level || "").trim().toLowerCase();
  if (!key || key === "none" || key === "—") return t("watch.enterprise.change.none");
  if (key === "low") return t("watch.enterprise.change.low");
  if (key === "medium") return t("watch.enterprise.change.medium");
  if (key === "high") return t("watch.enterprise.change.high");
  return key;
}
