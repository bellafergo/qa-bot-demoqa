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
} from "../api";

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
  if (v === "pass" || v === "completed") return "badge-green";
  if (v === "fail" || v === "failed")    return "badge-red";
  if (v === "error")                     return "badge-red";
  if (v === "running")                   return "badge-blue";
  if (v === "queued")                    return "badge-orange";
  if (v === "partial")                   return "badge-orange";
  return "badge-gray";
}

// ── Sub-components ────────────────────────────────────────────────────────────

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

// ── Main component ────────────────────────────────────────────────────────────

export default function DashboardPage() {
  const [summary, setSummary]     = useState(null);
  const [recentRuns, setRecentRuns] = useState([]);
  const [recentJobs, setRecentJobs] = useState([]);
  const [fi, setFi]               = useState(null);
  const [loading, setLoading]     = useState(true);
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

      // Failure intel is non-critical — load separately
      getFailureIntel().then(f => setFi(f)).catch(() => {});
    } catch {
      // partial load is fine
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { load(); }, [load]);

  const s = summary || {};
  const passRate = s.pass_rate != null ? `${s.pass_rate.toFixed(1)}%` : "—";

  return (
    <div style={{ height: "100%", overflow: "auto", background: "var(--bg)" }}>

      {/* Hero */}
      <div className="dash-hero">
        <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", gap: 20, flexWrap: "wrap" }}>
          <div>
            <h1 style={{ margin: 0, fontSize: 26, fontWeight: 900, color: "var(--text)", letterSpacing: "-0.03em", lineHeight: 1.1 }}>
              Vanya QA Intelligence
            </h1>
            <p style={{ margin: "6px 0 14px", fontSize: 14, color: "var(--text-2)", lineHeight: 1.5 }}>
              Execution, risk and evidence control center
            </p>
            <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
              <span className="badge badge-green">● Live</span>
              <span className="badge badge-accent">Production</span>
              <span className="badge badge-gray">Powered by Claude</span>
              {lastRefresh && (
                <span className="badge badge-gray">
                  Refreshed {lastRefresh.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}
                </span>
              )}
            </div>
          </div>
          <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading} style={{ flexShrink: 0 }}>
            {loading ? "Refreshing…" : "↻ Refresh"}
          </button>
        </div>
      </div>

      <div style={{ padding: "24px 28px" }}>

        {/* KPI grid */}
        <div className="kpi-grid" style={{ gridTemplateColumns: "repeat(auto-fit, minmax(150px, 1fr))", marginBottom: 24 }}>
          <KpiCard label="Total Tests"     value={loading ? "…" : s.total_test_cases}    sub={`${s.active_test_cases ?? "—"} active`}           icon="☰" />
          <KpiCard label="Total Runs"      value={loading ? "…" : s.total_runs}           sub={`${s.pass_runs ?? 0} pass · ${s.fail_runs ?? 0} fail`} icon="▶" />
          <KpiCard label="Pass Rate"       value={loading ? "…" : passRate}              sub="all-time"                                          accent={s.pass_rate >= 80 ? "var(--green)" : "var(--orange)"} icon="✓" />
          <KpiCard label="Active Workers"  value={loading ? "…" : s.active_workers}       sub={`${s.queue_depth ?? 0} queued`}                   icon="⚙" />
          <KpiCard label="Total Jobs"      value={loading ? "…" : s.total_jobs}           sub={`${s.running_jobs ?? 0} running · ${s.queued_jobs ?? 0} queued`} icon="◈" />
          <KpiCard label="UI Tests"        value={loading ? "…" : s.total_ui_tests}       sub="in catalog"                                       icon="◻" />
          <KpiCard label="API Tests"       value={loading ? "…" : s.total_api_tests}      sub="in catalog"                                       icon="⌥" />
          {fi && <KpiCard label="Flaky Tests"    value={fi.flaky_tests_count ?? 0}            sub={`${fi.total_clusters ?? 0} clusters`}              accent={fi.flaky_tests_count > 0 ? "var(--orange)" : undefined} icon="⚠" />}
        </div>

        {/* Main 2-column grid */}
        <div style={{ display: "grid", gridTemplateColumns: "minmax(0,2fr) minmax(0,1fr)", gap: 24, alignItems: "start" }}>

          {/* LEFT */}
          <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>

            {/* Recent Runs */}
            <SectionCard title="Recent Runs" link="/runs" linkLabel="Run history →">
              {loading ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>Loading runs…</div>
              ) : recentRuns.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  No runs yet. <Link to="/catalog" style={{ color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>Run a test →</Link>
                </div>
              ) : (
                <table className="data-table">
                  <thead><tr>
                    <th>Test Case</th>
                    <th>Status</th>
                    <th>Duration</th>
                    <th>Executed</th>
                  </tr></thead>
                  <tbody>
                    {recentRuns.slice(0, 8).map((r, i) => (
                      <tr key={r.run_id || i}>
                        <td style={{ fontWeight: 600, fontSize: 12, fontFamily: "monospace" }}>{r.test_case_id || "—"}</td>
                        <td><span className={`badge ${statusClass(r.status)}`}>{r.status}</span></td>
                        <td style={{ fontSize: 12, color: "var(--text-2)" }}>{fmtMs(r.duration_ms)}</td>
                        <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(r.executed_at)}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </SectionCard>

            {/* Recent Jobs */}
            <SectionCard title="Recent Jobs" link="/execution" linkLabel="Execution center →">
              {loading ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>Loading jobs…</div>
              ) : recentJobs.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  No jobs yet. <Link to="/execution" style={{ color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>Run a batch →</Link>
                </div>
              ) : (
                <table className="data-table">
                  <thead><tr>
                    <th>Job ID</th>
                    <th>Status</th>
                    <th style={{ width: 120 }}>Progress</th>
                    <th>Created</th>
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
                <div className="section-title">Failure Intelligence</div>
                <div style={{ display: "flex", flexDirection: "column", gap: 8, marginTop: 4 }}>
                  <Row label="Flaky tests"      value={fi.flaky_tests_count ?? 0}           accent={fi.flaky_tests_count > 0 ? "var(--orange)" : undefined} />
                  <Row label="Failure clusters"  value={fi.total_clusters ?? 0} />
                  <Row label="Regressions"       value={fi.recurrent_regressions_count ?? 0} accent={fi.recurrent_regressions_count > 0 ? "var(--red)" : undefined} />
                  {fi.notes && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 4, lineHeight: 1.5, borderTop: "1px solid var(--border)", paddingTop: 8 }}>
                      {fi.notes}
                    </div>
                  )}
                </div>
                <div style={{ marginTop: 12 }}>
                  <Link to="/runs" style={{ fontSize: 12, color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>
                    View RCA & runs →
                  </Link>
                </div>
              </div>
            )}

            {/* Quick Actions */}
            <div className="card">
              <div className="section-title">Quick Actions</div>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                {[
                  { to: "/catalog",     icon: "☰", label: "Test Catalog",    sub: "Browse and run tests" },
                  { to: "/execution",   icon: "⚙", label: "Execution",       sub: "Worker pool & batch run" },
                  { to: "/drafts",      icon: "⊕", label: "Draft Approval",  sub: "Generate & approve tests" },
                  { to: "/pr-analysis", icon: "◎", label: "PR Analysis",     sub: "Analyze PR impact" },
                  { to: "/api-testing", icon: "⌥", label: "API Testing",     sub: "OpenAPI spec → tests" },
                  { to: "/coverage",    icon: "◐", label: "Coverage",        sub: "Module coverage view" },
                  { to: "/chat",        icon: "✦", label: "AI Chat",         sub: "Ask Vanya anything" },
                ].map(({ to, icon, label, sub }) => (
                  <Link key={to} to={to} className="quick-action" style={{ flexDirection: "row", gap: 12 }}>
                    <span className="quick-action-icon" style={{ fontSize: 15 }}>{icon}</span>
                    <div>
                      <span className="quick-action-label">{label}</span>
                      <span className="quick-action-sub" style={{ display: "block" }}>{sub}</span>
                    </div>
                  </Link>
                ))}
              </div>
            </div>

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
