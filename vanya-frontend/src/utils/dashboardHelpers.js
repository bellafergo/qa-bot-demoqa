export function fmtDate(iso) {
  if (!iso) return "—";
  try {
    const d = new Date(iso);
    return d.toLocaleDateString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
  } catch { return "—"; }
}

export function fmtMs(ms) {
  if (ms == null) return "—";
  if (ms < 1000) return `${ms}ms`;
  return `${(ms / 1000).toFixed(1)}s`;
}

export function statusClass(s) {
  const v = String(s || "").toLowerCase();
  if (v === "pass" || v === "passed" || v === "completed") return "badge-green";
  if (v === "fail" || v === "failed" || v === "error")     return "badge-red";
  if (v === "running")                                     return "badge-blue";
  if (v === "queued" || v === "partial")                   return "badge-orange";
  return "badge-gray";
}

export function isPassStatus(s) {
  return ["pass", "passed", "completed"].includes(String(s || "").toLowerCase());
}

export function isFailStatus(s) {
  return ["fail", "failed", "error"].includes(String(s || "").toLowerCase());
}

/** Local calendar day bounds for MEJORA #4 "Hoy" filter. */
export function isSameLocalDay(iso, dayStart, dayEnd) {
  // QA FINAL — tolerate empty / non-string timestamps without producing Invalid Date matches
  if (iso == null || iso === "") return false;
  try {
    const raw = typeof iso === "number" ? iso : String(iso).trim();
    if (raw === "") return false;
    const d = new Date(raw);
    if (Number.isNaN(d.getTime())) return false;
    return d >= dayStart && d <= dayEnd;
  } catch {
    return false;
  }
}

export function filterDashboardRuns(runs, filterKey) {
  const list = Array.isArray(runs) ? runs : [];
  if (filterKey === "failed") return list.filter((r) => isFailStatus(r.status));
  if (filterKey === "passed") return list.filter((r) => isPassStatus(r.status));
  if (filterKey === "today") {
    const start = new Date();
    start.setHours(0, 0, 0, 0);
    const end = new Date();
    end.setHours(23, 59, 59, 999);
    return list.filter((r) =>
      isSameLocalDay(r.started_at || r.executed_at || r.finished_at, start, end),
    );
  }
  return list;
}

/** Match recent dashboard run to analytics top-failure test (failed status only). */
export function findRecentFailedRunForTest(recentRuns, testCaseId) {
  const want = String(testCaseId || "").trim();
  if (!want) return null;
  for (const r of recentRuns || []) {
    if (!isFailStatus(r.status)) continue;
    const tid = String(r.test_id || r.test_case_id || "").trim();
    if (tid === want) return r;
  }
  return null;
}

/**
 * Merge GET /execution/status orchestrator maps into one row per project.
 * "reserved" is the backend dispatch slot count (proxy for in-flight jobs per project).
 */
export function buildProjectCapacityRows(execStatus) {
  if (!execStatus || typeof execStatus !== "object") return [];
  const pending = execStatus.orchestrator_pending_by_project;
  const reserved = execStatus.orchestrator_reserved_by_project;
  const rawMax = execStatus.max_concurrent_jobs_per_project;
  const max = typeof rawMax === "number" && rawMax > 0 ? rawMax : 2;
  const pObj = pending && typeof pending === "object" && !Array.isArray(pending) ? pending : {};
  const rObj = reserved && typeof reserved === "object" && !Array.isArray(reserved) ? reserved : {};
  const ids = new Set([...Object.keys(pObj), ...Object.keys(rObj)]);
  const rows = [];
  for (const projectId of ids) {
    const queued = Number(pObj[projectId]) || 0;
    const res = Number(rObj[projectId]) || 0;
    if (queued === 0 && res === 0) continue;
    rows.push({ projectId, queued, reserved: res, max });
  }
  rows.sort((a, b) => a.projectId.localeCompare(b.projectId));
  return rows;
}

export function projectCapacityAccent(reserved, max) {
  if (max <= 0) return { border: "var(--border)", bar: "var(--text-3)", label: "var(--text-3)" };
  const ratio = reserved / max;
  if (ratio >= 1) {
    return { border: "var(--red)", bar: "var(--red)", label: "var(--red)" };
  }
  if (ratio >= 0.75) {
    return { border: "var(--orange-border)", bar: "var(--orange)", label: "var(--orange-text)" };
  }
  return { border: "var(--green)", bar: "var(--green)", label: "var(--green)" };
}

/**
 * System status ribbon — priority rules (summary + fi + idle flags only; recentRuns reserved for future).
 * Idle flag matches existing KPI `idleExecKpis`: active_workers === 0 && total_jobs === 0.
 */
export function deriveSystemRibbonState({ loading, s, fi, passRateValid, passRateNum, idleExecKpis, t }) {
  if (loading) {
    return {
      variant: "loading",
      headline: t("dash.ribbon.loading"),
      cta: null,
    };
  }
  const totalRuns = s.total_runs ?? 0;
  if (passRateValid && passRateNum < 60 && totalRuns >= 5) {
    return {
      variant: "danger",
      headline: t("dash.ribbon.alert_quality"),
      cta: { to: "/insights", label: t("dash.ribbon.cta_insights"), secondary: false },
    };
  }
  if ((fi?.recurrent_regressions_count ?? 0) > 0) {
    return {
      variant: "warning",
      headline: t("dash.ribbon.regressions"),
      cta: { to: "/insights", label: t("dash.ribbon.cta_insights"), secondary: false },
    };
  }
  if (!idleExecKpis) {
    return {
      variant: "info",
      headline: t("dash.ribbon.execution_active"),
      cta: { to: "/batch", label: t("dash.ribbon.cta_batch"), secondary: false },
    };
  }
  if (passRateValid && passRateNum >= 80) {
    return {
      variant: "success",
      headline: t("dash.ribbon.stable"),
      cta: {
        to: "/insights",
        label: t("dash.ribbon.cta_insights_secondary"),
        secondary: true,
      },
    };
  }
  return {
    variant: "neutral",
    headline: t("dash.ribbon.idle"),
    cta: { to: "/batch", label: t("dash.ribbon.cta_batch"), secondary: false },
  };
}
