// src/pages/DashboardPage.jsx
/**
 * Vanya QA Intelligence — Executive Dashboard
 * Wired to live backend: /dashboard/summary, /dashboard/recent-runs,
 * /dashboard/recent-jobs, /failure-intelligence/summary
 */
import React, { useEffect, useState, useCallback } from "react";
import { Link } from "react-router-dom";
import {
  getDashboardSummary,
  getDashboardRecentRuns,
  getDashboardRecentJobs,
  getFailureIntel,
  getRunsAnalytics,
} from "../api";
import { useLang } from "../i18n/LangContext";

// ── Helpers ───────────────────────────────────────────────────────────────────

function fmtDate(iso) {
  if (!iso) return "—";
  try {
    const d = new Date(iso);
    return d.toLocaleDateString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
  } catch { return "—"; }
}

function fmtMs(ms) {
  if (ms == null) return "—";
  if (ms < 1000) return `${ms}ms`;
  return `${(ms / 1000).toFixed(1)}s`;
}

function statusClass(s) {
  const v = String(s || "").toLowerCase();
  if (v === "pass" || v === "passed" || v === "completed") return "badge-green";
  if (v === "fail" || v === "failed" || v === "error")     return "badge-red";
  if (v === "running")                                     return "badge-blue";
  if (v === "queued" || v === "partial")                   return "badge-orange";
  return "badge-gray";
}

function isPassStatus(s) {
  return ["pass", "passed", "completed"].includes(String(s || "").toLowerCase());
}

// ── Visual widget: Pass Rate Trend ────────────────────────────────────────────

function PassRateTrendChart({ runs, loading, t }) {
  const data = [...(runs || [])].reverse().slice(-10);

  // Guard: must have at least 2 points to draw the chart.
  // When loading, show skeleton. When done but insufficient data, show no-data state.
  if (data.length < 2) {
    if (loading) {
      return (
        <div style={{ textAlign: "center", padding: "24px 0", fontSize: 12, color: "var(--text-3)" }}>…</div>
      );
    }
    return (
      <div style={{ textAlign: "center", padding: "24px 0" }}>
        <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text-2)" }}>{t("dash.trends.no_data")}</div>
        <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>{t("dash.trends.no_data_sub")}</div>
      </div>
    );
  }

  const W = 400, H = 110, PAD_L = 32, PAD_R = 8, PAD_T = 10, PAD_B = 24;
  const innerW = W - PAD_L - PAD_R;
  const innerH = H - PAD_T - PAD_B;

  // Sliding 3-run window pass rate
  const rates = data.map((_, i) => {
    const window = data.slice(Math.max(0, i - 2), i + 1);
    const passed = window.filter(r => isPassStatus(r.status)).length;
    return passed / window.length;
  });

  const avgRate = rates.reduce((a, b) => a + b, 0) / Math.max(rates.length, 1);

  const pts = rates.map((rate, i) => {
    const x = PAD_L + (data.length <= 1 ? innerW / 2 : (i * innerW) / (data.length - 1));
    const y = PAD_T + innerH * (1 - rate);
    return [x, y];
  });

  // Safety net: should never be empty here, but guard anyway
  if (pts.length < 2) return null;

  const lineStr = pts.map(([x, y]) => `${x.toFixed(1)},${y.toFixed(1)}`).join(" ");

  const areaPath =
    `M ${pts[0][0].toFixed(1)},${(PAD_T + innerH).toFixed(1)} ` +
    `L ${pts[0][0].toFixed(1)},${pts[0][1].toFixed(1)} ` +
    pts.slice(1).map(([x, y]) => `L ${x.toFixed(1)},${y.toFixed(1)}`).join(" ") +
    ` L ${pts[pts.length - 1][0].toFixed(1)},${(PAD_T + innerH).toFixed(1)} Z`;

  const avgY = (PAD_T + innerH * (1 - avgRate)).toFixed(1);

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} style={{ width: "100%", height: 110, display: "block" }}>
        <defs>
          <linearGradient id="vanyaTrendGrad" x1="0" y1="0" x2="0" y2="1">
            <stop offset="0%"   stopColor="var(--accent)" stopOpacity="0.22" />
            <stop offset="100%" stopColor="var(--accent)" stopOpacity="0.02" />
          </linearGradient>
        </defs>

        {/* Grid lines */}
        {[0, 0.5, 1].map(pct => {
          const gy = (PAD_T + innerH * (1 - pct)).toFixed(1);
          return (
            <line key={pct} x1={PAD_L} y1={gy} x2={W - PAD_R} y2={gy}
              stroke="var(--border)" strokeWidth="0.5" strokeDasharray="2,4" />
          );
        })}

        {/* Area fill */}
        <path d={areaPath} fill="url(#vanyaTrendGrad)" />

        {/* Avg line */}
        <line x1={PAD_L} y1={avgY} x2={W - PAD_R} y2={avgY}
          stroke="var(--text-3)" strokeWidth="1" strokeDasharray="3,3" />

        {/* Trend line */}
        <polyline points={lineStr} fill="none" stroke="var(--accent)"
          strokeWidth="2" strokeLinejoin="round" strokeLinecap="round" />

        {/* Data points */}
        {pts.map(([x, y], i) => (
          <circle key={i} cx={x.toFixed(1)} cy={y.toFixed(1)} r="3.5"
            fill={isPassStatus(data[i]?.status) ? "var(--green)" : "var(--red)"}
            stroke="var(--surface)" strokeWidth="1.5" />
        ))}

        {/* Y-axis labels */}
        <text x={PAD_L - 4} y={PAD_T + 4}           textAnchor="end" fontSize="9" fill="var(--text-3)">100%</text>
        <text x={PAD_L - 4} y={PAD_T + innerH / 2 + 3} textAnchor="end" fontSize="9" fill="var(--text-3)">50%</text>
        <text x={PAD_L - 4} y={PAD_T + innerH + 4}  textAnchor="end" fontSize="9" fill="var(--text-3)">0%</text>

        {/* X-axis labels — show first, middle, last */}
        {pts.length > 0 && [0, Math.floor((pts.length - 1) / 2), pts.length - 1]
          .filter((v, i, arr) => arr.indexOf(v) === i)
          .map(i => (
            <text key={i} x={pts[i][0].toFixed(1)} y={H - 5}
              textAnchor="middle" fontSize="8" fill="var(--text-3)">
              {t("dash.trends.run")} {i + 1}
            </text>
          ))}
      </svg>

      <div style={{ display: "flex", gap: 12, marginTop: 6, fontSize: 11, color: "var(--text-3)" }}>
        <span>{t("dash.trends.subtitle")}: <strong style={{ color: "var(--text-2)" }}>{data.length}</strong></span>
        <span>· {t("dash.trends.avg")}: <strong style={{ color: "var(--accent)" }}>{(avgRate * 100).toFixed(0)}%</strong></span>
      </div>
    </div>
  );
}

// ── Visual widget: Coverage Donut ─────────────────────────────────────────────

function CoverageDonutChart({ summary, loading, t }) {
  const ui    = summary?.total_ui_tests  ?? 0;
  const api   = summary?.total_api_tests ?? 0;
  const total = ui + api;

  if (!loading && total === 0) {
    return (
      <div style={{ textAlign: "center", padding: "24px 0", fontSize: 12, color: "var(--text-3)" }}>
        {t("dash.coverage.no_data")}
      </div>
    );
  }

  const CX = 56, CY = 56, R = 38, STROKE = 13;
  const CIRC = 2 * Math.PI * R;
  const uiArc  = total > 0 ? (ui  / total) * CIRC : 0;
  const apiArc = total > 0 ? (api / total) * CIRC : 0;
  const uiDeg  = total > 0 ? (ui  / total) * 360  : 0;

  return (
    <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
      <svg viewBox="0 0 112 112" style={{ width: 112, height: 112, flexShrink: 0 }}>
        {/* Track */}
        <circle cx={CX} cy={CY} r={R} fill="none" stroke="var(--border)" strokeWidth={STROKE} />

        {/* UI segment */}
        {ui > 0 && (
          <circle cx={CX} cy={CY} r={R} fill="none"
            stroke="var(--accent)" strokeWidth={STROKE}
            strokeDasharray={`${uiArc.toFixed(2)} ${(CIRC - uiArc).toFixed(2)}`}
            style={{ transform: `rotate(-90deg)`, transformOrigin: `${CX}px ${CY}px` }}
          />
        )}

        {/* API segment */}
        {api > 0 && (
          <circle cx={CX} cy={CY} r={R} fill="none"
            stroke="var(--green)" strokeWidth={STROKE}
            strokeDasharray={`${apiArc.toFixed(2)} ${(CIRC - apiArc).toFixed(2)}`}
            style={{ transform: `rotate(${(-90 + uiDeg).toFixed(2)}deg)`, transformOrigin: `${CX}px ${CY}px` }}
          />
        )}

        {/* Center */}
        <text x={CX} y={CY - 7} textAnchor="middle" fontSize="17" fontWeight="800" fill="var(--text)">
          {loading ? "…" : total}
        </text>
        <text x={CX} y={CY + 9} textAnchor="middle" fontSize="9" fill="var(--text-3)">
          {t("dash.coverage.total")}
        </text>
      </svg>

      <div style={{ display: "flex", flexDirection: "column", gap: 10, fontSize: 12, flex: 1 }}>
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <span style={{ width: 9, height: 9, borderRadius: "50%", background: "var(--accent)", flexShrink: 0 }} />
          <span style={{ color: "var(--text-2)", flex: 1 }}>{t("dash.coverage.ui")}</span>
          <span style={{ fontWeight: 700, color: "var(--text)" }}>{loading ? "…" : ui}</span>
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <span style={{ width: 9, height: 9, borderRadius: "50%", background: "var(--green)", flexShrink: 0 }} />
          <span style={{ color: "var(--text-2)", flex: 1 }}>{t("dash.coverage.api")}</span>
          <span style={{ fontWeight: 700, color: "var(--text)" }}>{loading ? "…" : api}</span>
        </div>
        {total > 0 && (
          <div style={{ fontSize: 10, color: "var(--text-3)", borderTop: "1px solid var(--border)", paddingTop: 6 }}>
            UI {ui > 0 ? ((ui / total) * 100).toFixed(0) : 0}%
            {" · "}
            API {api > 0 ? ((api / total) * 100).toFixed(0) : 0}%
          </div>
        )}
      </div>
    </div>
  );
}

// ── Visual widget: Failure Distribution ───────────────────────────────────────

function FailureDistributionChart({ fi, loading, t }) {
  if (!fi && !loading) {
    return (
      <div style={{ textAlign: "center", padding: "20px 0" }}>
        <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text-2)" }}>{t("dash.failures.no_data")}</div>
        <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>{t("dash.failures.no_data_sub")}</div>
      </div>
    );
  }

  const flaky       = fi?.flaky_tests_count             ?? 0;
  const clusters    = fi?.total_clusters                ?? 0;
  const regressions = fi?.recurrent_regressions_count   ?? 0;
  const maxVal      = Math.max(flaky, clusters, regressions, 1);
  const allZero     = flaky === 0 && clusters === 0 && regressions === 0;

  const bars = [
    { key: "dash.failures.flaky",       val: flaky,       color: "var(--orange)" },
    { key: "dash.failures.clusters",    val: clusters,    color: "var(--accent)"  },
    { key: "dash.failures.regressions", val: regressions, color: "var(--red)"     },
  ];

  if (allZero && !loading) {
    return (
      <div style={{ padding: "14px 0", fontSize: 12, color: "var(--green)", fontWeight: 600 }}>
        ✓ {t("dash.failures.all_clear")}
      </div>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
      {bars.map(({ key, val, color }) => (
        <div key={key}>
          <div style={{ display: "flex", justifyContent: "space-between", marginBottom: 5, fontSize: 12 }}>
            <span style={{ color: "var(--text-2)" }}>{t(key)}</span>
            <span style={{ fontWeight: 700, color: val > 0 ? color : "var(--text-3)" }}>
              {loading ? "…" : val}
            </span>
          </div>
          <div style={{ height: 7, borderRadius: 4, background: "var(--border)", overflow: "hidden" }}>
            <div style={{
              height: "100%",
              borderRadius: 4,
              background: color,
              width: loading ? "0%" : `${((val / maxVal) * 100).toFixed(1)}%`,
              transition: "width 0.5s ease",
              opacity: val === 0 ? 0.18 : 1,
            }} />
          </div>
        </div>
      ))}
    </div>
  );
}

// ── Visual widget: Risk Summary ───────────────────────────────────────────────

function RiskSummaryCard({ summary, fi, loading, t }) {
  if (loading || !summary) {
    return (
      <div style={{ padding: "12px 0", fontSize: 12, color: "var(--text-3)" }}>
        {t("dash.risk.no_data")}
      </div>
    );
  }

  const passRate   = summary.pass_rate ?? null;
  const flakyCount = fi?.flaky_tests_count ?? 0;
  const totalRuns  = summary.total_runs ?? 0;
  const failRuns   = summary.fail_runs  ?? 0;
  const failRate   = totalRuns > 0 ? (failRuns / totalRuns) * 100 : null;
  const limitedHistory = totalRuns < 5;

  let level = "LOW";
  const reasons = [];

  // HIGH threshold
  if (passRate !== null && passRate < 60) level = "HIGH";
  if (flakyCount >= 5)                    level = "HIGH";
  if (failRate !== null && failRate >= 40) level = "HIGH";

  // MEDIUM threshold (only if not already HIGH)
  if (level !== "HIGH") {
    if (passRate !== null && passRate < 80)   { level = "MEDIUM"; }
    if (flakyCount >= 2)                      { level = "MEDIUM"; }
    if (failRate !== null && failRate >= 20)   { level = "MEDIUM"; }
  }

  // Collect reasons
  if (level === "HIGH" || level === "MEDIUM") {
    if (passRate !== null && passRate < 80)    reasons.push(t("dash.risk.reason.pass_rate"));
    if (flakyCount >= 2)                       reasons.push(t("dash.risk.reason.flaky"));
    if (failRate !== null && failRate >= 20)    reasons.push(t("dash.risk.reason.fail_rate"));
  }

  const THEME = {
    HIGH:   { bg: "#fff5f5", border: "#fca5a5", text: "#dc2626", pillBg: "#fee2e2", icon: "⚠",  labelKey: "dash.risk.high"   },
    MEDIUM: { bg: "#fffbeb", border: "#fcd34d", text: "#d97706", pillBg: "#fef3c7", icon: "◎",  labelKey: "dash.risk.medium" },
    LOW:    { bg: "#f0fdf4", border: "#86efac", text: "#16a34a", pillBg: "#dcfce7", icon: "✓",  labelKey: "dash.risk.low"    },
  };
  const c = THEME[level];

  const metrics = [
    {
      labelKey: "dash.risk.pass_rate",
      value:    passRate !== null ? `${passRate.toFixed(1)}%` : "—",
      accent:   passRate !== null && passRate < 80 ? "var(--orange)" : "var(--green)",
    },
    {
      labelKey: "dash.risk.flaky_count",
      value:    String(flakyCount),
      accent:   flakyCount > 0 ? "var(--orange)" : "var(--text-2)",
    },
    {
      labelKey: "dash.risk.fail_rate",
      value:    failRate !== null ? `${failRate.toFixed(1)}%` : "—",
      accent:   failRate !== null && failRate >= 20 ? "var(--red)" : "var(--text-2)",
    },
  ];

  return (
    <div style={{
      borderRadius: "var(--r-sm)",
      border: `1.5px solid ${c.border}`,
      background: c.bg,
      padding: "14px 16px",
      boxSizing: "border-box",
    }}>
      {/* Header row: icon + level badge + optional sub */}
      <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 12 }}>
        <span style={{ fontSize: 16, lineHeight: 1, flexShrink: 0 }}>{c.icon}</span>
        <span style={{
          fontWeight: 700,
          fontSize: 13,
          color: c.text,
          background: c.pillBg,
          border: `1px solid ${c.border}`,
          borderRadius: 4,
          padding: "2px 8px",
          letterSpacing: "0.01em",
        }}>
          {t(c.labelKey)}
        </span>
        {limitedHistory && (
          <span style={{ fontSize: 10, color: "var(--text-3)", marginLeft: "auto", fontStyle: "italic" }}>
            {t("dash.risk.limited_history")}
          </span>
        )}
        {level === "LOW" && !limitedHistory && (
          <span style={{ fontSize: 10, color: "var(--text-3)", marginLeft: "auto" }}>
            {t("dash.risk.healthy")}
          </span>
        )}
      </div>

      {/* Metrics: 3-column mini grid */}
      <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: "6px 8px", marginBottom: reasons.length > 0 ? 10 : 0 }}>
        {metrics.map(({ labelKey, value, accent }) => (
          <div key={labelKey} style={{
            background: "rgba(255,255,255,0.55)",
            border: "1px solid rgba(0,0,0,0.06)",
            borderRadius: 6,
            padding: "7px 8px",
            textAlign: "center",
          }}>
            <div style={{ fontSize: 14, fontWeight: 800, color: accent, lineHeight: 1.1, marginBottom: 3 }}>
              {value}
            </div>
            <div style={{ fontSize: 10, color: "var(--text-3)", lineHeight: 1.2 }}>
              {t(labelKey)}
            </div>
          </div>
        ))}
      </div>

      {/* Reasons: inline pills */}
      {reasons.length > 0 && (
        <div style={{ display: "flex", flexWrap: "wrap", gap: 5, paddingTop: 8, borderTop: `1px solid ${c.border}` }}>
          {reasons.map((r, i) => (
            <span key={i} style={{
              fontSize: 10,
              color: c.text,
              background: c.pillBg,
              border: `1px solid ${c.border}`,
              borderRadius: 3,
              padding: "2px 7px",
              fontWeight: 500,
            }}>
              {r}
            </span>
          ))}
        </div>
      )}
    </div>
  );
}

// ── Existing sub-components ───────────────────────────────────────────────────

function KpiCard({ label, value, sub, accent, icon }) {
  return (
    <div className="kpi-card">
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start" }}>
        <div className="kpi-label">{label}</div>
        {icon && <span style={{ fontSize: 16, opacity: 0.55 }}>{icon}</span>}
      </div>
      <div className="kpi-value" style={accent ? { color: accent } : {}}>
        {value ?? "—"}
      </div>
      {sub && <div className="kpi-sub">{sub}</div>}
    </div>
  );
}

function SectionCard({ title, link, linkLabel, children }) {
  return (
    <div className="card" style={{ padding: 0, overflow: "hidden" }}>
      <div style={{
        padding: "14px 20px", borderBottom: "1px solid var(--border)",
        display: "flex", alignItems: "center", justifyContent: "space-between",
      }}>
        <div className="section-title" style={{ margin: 0 }}>{title}</div>
        {link && (
          <Link to={link} style={{ fontSize: 12, color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>
            {linkLabel || "View all →"}
          </Link>
        )}
      </div>
      {children}
    </div>
  );
}

function WidgetCard({ title, subtitle, children }) {
  return (
    <div className="card" style={{ padding: "16px 20px" }}>
      <div style={{ marginBottom: 14 }}>
        <div className="section-title" style={{ margin: 0 }}>{title}</div>
        {subtitle && <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{subtitle}</div>}
      </div>
      {children}
    </div>
  );
}

// ── Main component ────────────────────────────────────────────────────────────

export default function DashboardPage() {
  const { t } = useLang();

  const [summary, setSummary]         = useState(null);
  const [recentRuns, setRecentRuns]   = useState([]);
  const [recentJobs, setRecentJobs]   = useState([]);
  const [fi, setFi]                   = useState(null);
  const [analytics, setAnalytics]     = useState(null);
  const [loading, setLoading]         = useState(true);
  const [lastRefresh, setLastRefresh] = useState(null);

  const load = useCallback(async () => {
    setLoading(true);
    try {
      const [s, runs, jobs] = await Promise.all([
        getDashboardSummary().catch(() => null),
        getDashboardRecentRuns(10).catch(() => []),
        getDashboardRecentJobs(10).catch(() => []),
      ]);
      setSummary(s);
      setRecentRuns(Array.isArray(runs) ? runs : []);
      setRecentJobs(Array.isArray(jobs) ? jobs : []);
      setLastRefresh(new Date());

      // Non-critical — load separately so they don't block the main render
      getFailureIntel().then(f => setFi(f)).catch(() => {});
      getRunsAnalytics().then(a => setAnalytics(a)).catch(() => {});
    } catch {
      // partial load is fine
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { load(); }, [load]);

  const s        = summary || {};
  const passRate = s.pass_rate != null ? `${s.pass_rate.toFixed(1)}%` : "—";

  return (
    <div style={{ height: "100%", overflow: "auto", background: "var(--bg)" }}>

      {/* ── Hero ─────────────────────────────────────────────────────────── */}
      <div className="dash-hero">
        <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", gap: 20, flexWrap: "wrap" }}>
          <div>
            <h1 style={{ margin: 0, fontSize: 26, fontWeight: 900, color: "var(--text)", letterSpacing: "-0.03em", lineHeight: 1.1 }}>
              {t("dash.title")}
            </h1>
            <p style={{ margin: "6px 0 14px", fontSize: 14, color: "var(--text-2)", lineHeight: 1.5 }}>
              {t("dash.subtitle")}
            </p>
            <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
              <span className="badge badge-green">● {t("common.live")}</span>
              <span className="badge badge-accent">{t("common.production")}</span>
              {lastRefresh && (
                <span className="badge badge-gray">
                  {t("dash.refreshed")} {lastRefresh.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}
                </span>
              )}
            </div>
          </div>
          <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading} style={{ flexShrink: 0 }}>
            {loading ? t("dash.refreshing") : t("dash.refresh")}
          </button>
        </div>
      </div>

      <div style={{ padding: "24px 28px" }}>

        {/* ── KPI grid ─────────────────────────────────────────────────────── */}
        <div className="kpi-grid" style={{ gridTemplateColumns: "repeat(auto-fit, minmax(150px, 1fr))", marginBottom: 24 }}>
          <KpiCard label={t("dash.kpi.total_tests")}    value={loading ? "…" : s.total_test_cases} sub={`${s.active_test_cases ?? "—"} ${t("dash.kpi.active")}`}                        icon="☰" />
          <KpiCard label={t("dash.kpi.total_runs")}     value={loading ? "…" : s.total_runs}        sub={`${s.pass_runs ?? 0} ${t("dash.kpi.pass")} · ${s.fail_runs ?? 0} ${t("dash.kpi.fail")}`} icon="▶" />
          <KpiCard label={t("dash.kpi.pass_rate")}      value={loading ? "…" : passRate}            sub={t("dash.kpi.all_time")}                                                          accent={s.pass_rate >= 80 ? "var(--green)" : "var(--orange)"} icon="✓" />
          <KpiCard label={t("dash.kpi.active_workers")} value={loading ? "…" : s.active_workers}    sub={`${s.queue_depth ?? 0} ${t("dash.kpi.queued")}`}                                icon="⚙" />
          <KpiCard label={t("dash.kpi.total_jobs")}     value={loading ? "…" : s.total_jobs}        sub={`${s.running_jobs ?? 0} ${t("dash.kpi.running")} · ${s.queued_jobs ?? 0} ${t("dash.kpi.queued")}`} icon="◈" />
          <KpiCard label={t("dash.kpi.ui_tests")}       value={loading ? "…" : s.total_ui_tests}    sub={t("dash.kpi.in_catalog")}                                                        icon="◻" />
          <KpiCard label={t("dash.kpi.api_tests")}      value={loading ? "…" : s.total_api_tests}   sub={t("dash.kpi.in_catalog")}                                                        icon="⌥" />
          {fi && <KpiCard label={t("dash.kpi.flaky_tests")} value={fi.flaky_tests_count ?? 0} sub={`${fi.total_clusters ?? 0} ${t("dash.kpi.clusters")}`} accent={fi.flaky_tests_count > 0 ? "var(--orange)" : undefined} icon="⚠" />}
        </div>

        {/* ── Visual row 1: Trend (2/3) + Coverage Donut (1/3) ─────────────── */}
        <div style={{ display: "grid", gridTemplateColumns: "minmax(0,2fr) minmax(0,1fr)", gap: 20, marginBottom: 20 }}>
          <WidgetCard title={t("dash.trends.title")} subtitle={`${t("dash.trends.subtitle")} · ${recentRuns.length}`}>
            <PassRateTrendChart runs={recentRuns} loading={loading} t={t} />
          </WidgetCard>
          <WidgetCard title={t("dash.coverage.title")}>
            <CoverageDonutChart summary={summary} loading={loading} t={t} />
          </WidgetCard>
        </div>

        {/* ── Visual row 2: Failure Distribution (1/2) + Risk Summary (1/2) ── */}
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 20, marginBottom: 24 }}>
          <WidgetCard title={t("dash.failures.title")} subtitle={t("dash.failures.subtitle")}>
            <FailureDistributionChart fi={fi} loading={loading} t={t} />
          </WidgetCard>
          <WidgetCard title={t("dash.risk.title")} subtitle={t("dash.risk.subtitle")}>
            <RiskSummaryCard summary={summary} fi={fi} loading={loading} t={t} />
          </WidgetCard>
        </div>

        {/* ── Bottom 2-col grid: runs/jobs (left) + intel/actions (right) ──── */}
        <div style={{ display: "grid", gridTemplateColumns: "minmax(0,2fr) minmax(0,1fr)", gap: 24, alignItems: "start" }}>

          {/* LEFT */}
          <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>

            {/* Recent Runs */}
            <SectionCard title={t("dash.recent_runs")} link="/runs" linkLabel={t("dash.recent_runs.link")}>
              {loading ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("dash.loading_runs")}</div>
              ) : recentRuns.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  {t("dash.no_runs")} <Link to="/catalog" style={{ color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>{t("dash.run_a_test")}</Link>
                </div>
              ) : (
                <table className="data-table">
                  <thead><tr>
                    <th>{t("dash.col.test_case")}</th>
                    <th>{t("dash.col.status")}</th>
                    <th>{t("dash.col.duration")}</th>
                    <th>{t("dash.col.executed")}</th>
                  </tr></thead>
                  <tbody>
                    {recentRuns.slice(0, 8).map((r, i) => (
                      <tr key={r.run_id || i}>
                        <td style={{ fontWeight: 600, fontSize: 12, fontFamily: "monospace" }}>
                          {r.test_id || r.test_case_id || "—"}
                        </td>
                        <td><span className={`badge ${statusClass(r.status)}`}>{r.status}</span></td>
                        <td style={{ fontSize: 12, color: "var(--text-2)" }}>{fmtMs(r.duration_ms)}</td>
                        <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>
                          {fmtDate(r.started_at || r.executed_at)}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </SectionCard>

            {/* Recent Jobs */}
            <SectionCard title={t("dash.recent_jobs")} link="/execution" linkLabel={t("dash.recent_jobs.link")}>
              {loading ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("dash.loading_jobs")}</div>
              ) : recentJobs.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  {t("dash.no_jobs")} <Link to="/execution" style={{ color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>{t("dash.run_batch")}</Link>
                </div>
              ) : (
                <table className="data-table">
                  <thead><tr>
                    <th>{t("dash.col.job_id")}</th>
                    <th>{t("dash.col.status")}</th>
                    <th style={{ width: 120 }}>{t("dash.col.progress")}</th>
                    <th>{t("dash.col.created")}</th>
                  </tr></thead>
                  <tbody>
                    {recentJobs.slice(0, 6).map((j, i) => (
                      <tr key={j.job_id || i}>
                        <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>{(j.job_id || "").slice(0, 12)}…</td>
                        <td><span className={`badge ${statusClass(j.status)}`}>{j.status}</span></td>
                        <td style={{ fontSize: 12, color: "var(--text-2)" }}>
                          {j.passed_count ?? 0}✓ {j.failed_count ?? 0}✗ / {j.total_count ?? 0}
                        </td>
                        <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(j.created_at)}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </SectionCard>

          </div>

          {/* RIGHT */}
          <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>

            {/* Failure Intelligence */}
            {fi && (
              <div className="card">
                <div className="section-title">{t("dash.failure_intel")}</div>
                <div style={{ display: "flex", flexDirection: "column", gap: 8, marginTop: 4 }}>
                  <Row label={t("dash.fi.flaky_tests")}  value={fi.flaky_tests_count ?? 0}           accent={fi.flaky_tests_count > 0 ? "var(--orange)" : undefined} />
                  <Row label={t("dash.fi.clusters")}      value={fi.total_clusters ?? 0} />
                  <Row label={t("dash.fi.regressions")}   value={fi.recurrent_regressions_count ?? 0} accent={fi.recurrent_regressions_count > 0 ? "var(--red)" : undefined} />
                  {fi.notes && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 4, lineHeight: 1.5, borderTop: "1px solid var(--border)", paddingTop: 8 }}>
                      {fi.notes}
                    </div>
                  )}
                </div>
                <div style={{ marginTop: 12 }}>
                  <Link to="/runs" style={{ fontSize: 12, color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>
                    {t("dash.view_rca")}
                  </Link>
                </div>
              </div>
            )}

            {/* Quick Actions */}
            <div className="card">
              <div className="section-title">{t("dash.quick_actions")}</div>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {[
                  { to: "/catalog",     icon: "☰", labelKey: "nav.catalog",      subKey: "dash.qa.catalog_sub"   },
                  { to: "/execution",   icon: "⚙", labelKey: "nav.execution",    subKey: "dash.qa.execution_sub" },
                  { to: "/drafts",      icon: "⊕", labelKey: "nav.drafts",       subKey: "dash.qa.drafts_sub"    },
                  { to: "/pr-analysis", icon: "◎", labelKey: "nav.pr_analysis",  subKey: "dash.qa.pr_sub"        },
                  { to: "/api-testing", icon: "⌥", labelKey: "nav.api_testing",  subKey: "dash.qa.api_sub"       },
                  { to: "/coverage",    icon: "◐", labelKey: "nav.coverage",     subKey: "dash.qa.coverage_sub"  },
                  { to: "/chat",        icon: "✦", labelKey: "nav.chat",         subKey: "dash.qa.chat_sub"      },
                ].map(({ to, icon, labelKey, subKey }) => (
                  <Link key={to} to={to} className="quick-action" style={{ flexDirection: "row", gap: 12 }}>
                    <span className="quick-action-icon" style={{ fontSize: 15 }}>{icon}</span>
                    <div>
                      <span className="quick-action-label">{t(labelKey)}</span>
                      <span className="quick-action-sub" style={{ display: "block" }}>{t(subKey)}</span>
                    </div>
                  </Link>
                ))}
              </div>
            </div>

          </div>
        </div>
      </div>

      {/* ── Run Analytics ────────────────────────────────────────────────── */}
      <div style={{ marginTop: 24 }}>
        <div className="card" style={{ padding: "16px 20px" }}>
          <div style={{ marginBottom: 14, display: "flex", alignItems: "baseline", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
            <div>
              <div className="section-title" style={{ margin: 0 }}>{t("dash.analytics.title")}</div>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{t("dash.analytics.subtitle")}</div>
            </div>
          </div>

          {!analytics && (
            <div style={{ fontSize: 13, color: "var(--text-3)" }}>
              {loading ? t("dash.analytics.loading") : t("dash.analytics.empty")}
            </div>
          )}

          {analytics && (
            <>
              {/* Summary mini-KPIs */}
              <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(120px, 1fr))", gap: 10, marginBottom: 20 }}>
                {[
                  { label: t("dash.analytics.runs_7d"),      value: analytics.summary.runs_last_7_days },
                  { label: t("dash.analytics.runs_30d"),     value: analytics.summary.runs_last_30_days },
                  { label: t("dash.analytics.pass_rate"),    value: `${analytics.summary.pass_rate}%`,
                    accent: analytics.summary.pass_rate >= 80 ? "var(--green)" : "var(--orange)" },
                  { label: t("dash.analytics.avg_duration"), value: fmtMs(analytics.summary.avg_duration_ms) },
                ].map(({ label, value, accent }) => (
                  <div key={label} style={{
                    background: "var(--surface-2, var(--bg))",
                    border: "1px solid var(--border)",
                    borderRadius: 6,
                    padding: "8px 12px",
                    textAlign: "center",
                  }}>
                    <div style={{ fontSize: 18, fontWeight: 800, color: accent || "var(--text)", lineHeight: 1.1, marginBottom: 4 }}>
                      {value ?? "—"}
                    </div>
                    <div style={{ fontSize: 10, color: "var(--text-3)", lineHeight: 1.2 }}>{label}</div>
                  </div>
                ))}
              </div>

              {/* Two-column: top failures + 7-day trend */}
              <div style={{ display: "grid", gridTemplateColumns: "minmax(0,3fr) minmax(0,2fr)", gap: 20 }}>

                {/* Top Failing Tests */}
                <div>
                  <div style={{ fontSize: 12, fontWeight: 700, color: "var(--text-2)", marginBottom: 8 }}>
                    {t("dash.analytics.top_failures")}
                  </div>
                  {analytics.top_failures.length === 0 ? (
                    <div style={{ fontSize: 12, color: "var(--text-3)", fontStyle: "italic" }}>
                      {t("dash.analytics.no_failures")}
                    </div>
                  ) : (
                    <table className="data-table" style={{ fontSize: 12 }}>
                      <thead><tr>
                        <th>{t("dash.analytics.col.test")}</th>
                        <th style={{ width: 54, textAlign: "right" }}>{t("dash.analytics.col.runs")}</th>
                        <th style={{ width: 64, textAlign: "right" }}>{t("dash.analytics.col.failures")}</th>
                        <th style={{ width: 76, textAlign: "right" }}>{t("dash.analytics.col.pass_rate")}</th>
                      </tr></thead>
                      <tbody>
                        {analytics.top_failures.slice(0, 7).map(tf => (
                          <tr key={tf.test_case_id}>
                            <td>
                              <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>{tf.test_case_id}</div>
                              {tf.test_name !== tf.test_case_id && (
                                <div style={{ fontSize: 10, color: "var(--text-3)", marginTop: 1 }}>{tf.test_name}</div>
                              )}
                            </td>
                            <td style={{ textAlign: "right" }}>{tf.total_runs}</td>
                            <td style={{ textAlign: "right" }}>
                              <span style={{ color: "var(--red)", fontWeight: 700 }}>{tf.failed_runs}</span>
                            </td>
                            <td style={{ textAlign: "right" }}>
                              <span style={{ color: tf.pass_rate >= 80 ? "var(--green)" : "var(--orange)", fontWeight: 600 }}>
                                {tf.pass_rate}%
                              </span>
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  )}
                </div>

                {/* 7-day daily trend */}
                <div>
                  <div style={{ fontSize: 12, fontWeight: 700, color: "var(--text-2)", marginBottom: 8 }}>
                    {t("dash.analytics.trend_7d")}
                  </div>
                  <table className="data-table" style={{ fontSize: 12 }}>
                    <thead><tr>
                      <th>{t("dash.analytics.col.date")}</th>
                      <th style={{ width: 48, textAlign: "right" }}>{t("dash.analytics.col.total")}</th>
                      <th style={{ width: 76, textAlign: "right" }}>{t("dash.analytics.col.pass_rate")}</th>
                    </tr></thead>
                    <tbody>
                      {[...analytics.trend].reverse().map(pt => (
                        <tr key={pt.date}>
                          <td style={{ fontFamily: "monospace", fontSize: 11 }}>{pt.date}</td>
                          <td style={{ textAlign: "right" }}>{pt.total}</td>
                          <td style={{ textAlign: "right" }}>
                            {pt.pass_rate != null ? (
                              <span style={{ color: pt.pass_rate >= 80 ? "var(--green)" : pt.total > 0 ? "var(--orange)" : "var(--text-3)", fontWeight: 600 }}>
                                {pt.total > 0 ? `${pt.pass_rate}%` : "—"}
                              </span>
                            ) : (
                              <span style={{ color: "var(--text-3)" }}>—</span>
                            )}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>

              </div>
            </>
          )}
        </div>
      </div>

    </div>
    </div>
  );
}

function Row({ label, value, accent }) {
  return (
    <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", fontSize: 13 }}>
      <span style={{ color: "var(--text-2)" }}>{label}</span>
      <span style={{ fontWeight: 700, color: accent || "var(--text)" }}>{value}</span>
    </div>
  );
}
