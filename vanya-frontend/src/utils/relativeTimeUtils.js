/** Locale-aware relative time formatting for customer-facing UI. */

export const RELATIVE_TIME_I18N_KEYS = {
  justNow: "time.relative.just_now",
  minutesAgo: "time.relative.minutes_ago",
  hoursAgo: "time.relative.hours_ago",
  daysAgo: "time.relative.days_ago",
  noExecutions: "time.relative.no_executions",
};

export function formatRelativeTime(iso, t, nowMs = Date.now()) {
  if (!iso) {
    return t ? t(RELATIVE_TIME_I18N_KEYS.noExecutions) : "—";
  }
  try {
    const d = new Date(iso);
    const ms = nowMs - d.getTime();
    if (!Number.isFinite(ms)) return "—";
    const sec = Math.floor(ms / 1000);
    if (sec < 45) return t(RELATIVE_TIME_I18N_KEYS.justNow);
    const min = Math.floor(sec / 60);
    if (min < 60) return t(RELATIVE_TIME_I18N_KEYS.minutesAgo, { count: min });
    const hr = Math.floor(min / 60);
    if (hr < 24) return t(RELATIVE_TIME_I18N_KEYS.hoursAgo, { count: hr });
    const day = Math.floor(hr / 24);
    return t(RELATIVE_TIME_I18N_KEYS.daysAgo, { count: day });
  } catch {
    return "—";
  }
}
