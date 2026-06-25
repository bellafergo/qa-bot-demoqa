/** Dashboard KPI display helpers — avoid bare zeros when data is insufficient. */

export function isDashboardDataSparse(summary) {
  const s = summary || {};
  return (s.total_runs ?? 0) === 0 && (s.total_test_cases ?? 0) === 0;
}

export function formatDashboardCount(value, { loading = false, sparse = false } = {}) {
  if (loading) return null;
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  if (sparse && n === 0) return "—";
  return n;
}

export function buildDashboardSparseHintsViewModel(sparse, t) {
  if (!sparse) return { show: false, cards: [] };
  return {
    show: true,
    cards: [
      { title: t("dash.empty.insufficient_runs_title"), message: t("dash.empty.insufficient_runs_desc") },
      { title: t("dash.empty.connect_browser_title"), message: t("dash.empty.connect_browser_desc"), path: "/browser-watch", cta: t("dash.empty.connect_browser_cta") },
      { title: t("dash.empty.connect_pr_title"), message: t("dash.empty.connect_pr_desc"), path: "/pr-analysis", cta: t("dash.empty.connect_pr_cta") },
    ],
  };
}
