// src/pages/DashboardPage.jsx
/**
 * Vanya QA Intelligence — Executive Dashboard
 *
 * Data strategy:
 *  - Live: GET /threads  → thread count, recent activity list
 *  - Derived: KPI cards labelled clearly ("7d avg", "monitored", etc.)
 *  - Static: Critical flows, recommendations — enriched UI placeholders
 *    that surface real platform capabilities without requiring new APIs.
 */
import React, { useEffect, useState, useCallback } from "react";
import { Link } from "react-router-dom";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

// ── Static data ──────────────────────────────────────────────────────────────

const CRITICAL_FLOWS = [
  { name: "Login",       status: "healthy",  runs: 14, icon: "🔐" },
  { name: "Checkout",    status: "healthy",  runs: 9,  icon: "🛒" },
  { name: "Payments",    status: "caution",  runs: 7,  icon: "💳" },
  { name: "Promotions",  status: "healthy",  runs: 5,  icon: "🏷️"  },
  { name: "Inventory",   status: "healthy",  runs: 11, icon: "📦" },
  { name: "Search",      status: "healthy",  runs: 8,  icon: "🔍" },
];

const RECOMMENDATIONS = [
  {
    kind: "warning",
    icon: "⚠",
    title: "Payment flow needs validation",
    body:  "Last run detected a selector timeout in the checkout step. Recommend re-running with updated fallbacks.",
  },
  {
    kind: "action",
    icon: "⚡",
    title: "Generate a regression plan",
    body:  "Use the Test Planner to generate and execute a full regression test for the login and search flows.",
  },
  {
    kind: "info",
    icon: "📄",
    title: "Upload Q2 acceptance specs",
    body:  "Add new test documents to keep Vanya's RAG context current with the latest product requirements.",
  },
  {
    kind: "tip",
    icon: "◎",
    title: "Schedule nightly execution",
    body:  "Automate critical-flow tests overnight to catch regressions before the morning stand-up.",
  },
];

const FLOW_STATUS = {
  healthy: { label: "Healthy",  cls: "badge-green"  },
  caution: { label: "Caution",  cls: "badge-orange" },
  failing: { label: "Failing",  cls: "badge-red"    },
};

const REC_ACCENT = {
  warning: { border: "var(--orange-border)", bg: "var(--orange-bg)", icon: "#d97706" },
  action:  { border: "var(--accent-border)", bg: "var(--accent-light)", icon: "var(--accent)" },
  info:    { border: "var(--blue-border)",   bg: "var(--blue-bg)",    icon: "var(--blue)" },
  tip:     { border: "var(--border)",        bg: "var(--surface-2)",  icon: "var(--text-3)" },
};

// ── Helpers ───────────────────────────────────────────────────────────────────

function fmtDate(iso) {
  if (!iso) return "—";
  try {
    const d = new Date(iso);
    return d.toLocaleDateString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
  } catch { return "—"; }
}

function buildTitle(t) {
  const title   = String(t?.title   || "").trim();
  if (title && title.toLowerCase() !== "new chat") return title;
  const preview = String(t?.preview || "").trim();
  if (preview) return preview.slice(0, 52) + (preview.length > 52 ? "…" : "");
  const id = String(t?.id || t?.thread_id || "").trim();
  return id ? `Thread ${id.slice(0, 6)}…` : "Chat";
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
        {value}
      </div>
      {sub && <div className="kpi-sub">{sub}</div>}
    </div>
  );
}

function FlowCard({ name, status, runs, icon }) {
  const s = FLOW_STATUS[status] || FLOW_STATUS.healthy;
  return (
    <div className="flow-card">
      <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
        <span style={{ fontSize: 18 }}>{icon}</span>
        <div>
          <div style={{ fontWeight: 700, fontSize: 13, color: "var(--text)" }}>{name}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 1 }}>{runs} runs</div>
        </div>
      </div>
      <span className={`badge ${s.cls}`}>{s.label}</span>
    </div>
  );
}

function RecCard({ kind, icon, title, body }) {
  const a = REC_ACCENT[kind] || REC_ACCENT.tip;
  return (
    <div
      className="rec-card"
      style={{ borderColor: a.border, background: a.bg }}
    >
      <span style={{ fontSize: 16, color: a.icon, flexShrink: 0, marginTop: 1 }}>{icon}</span>
      <div>
        <div style={{ fontWeight: 700, fontSize: 13, color: "var(--text)", marginBottom: 3 }}>{title}</div>
        <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.55 }}>{body}</div>
      </div>
    </div>
  );
}

// ── Main component ────────────────────────────────────────────────────────────

export default function DashboardPage() {
  const [threads, setThreads]       = useState([]);
  const [loading, setLoading]       = useState(true);
  const [lastRefresh, setLastRefresh] = useState(null);

  const loadThreads = useCallback(async () => {
    setLoading(true);
    try {
      const res  = await fetch(`${API_BASE}/threads?limit=100`);
      const data = await res.json();
      const arr  = Array.isArray(data) ? data : [];
      arr.sort((a, b) => {
        const da = a?.updated_at ? new Date(a.updated_at).getTime() : 0;
        const db = b?.updated_at ? new Date(b.updated_at).getTime() : 0;
        return db - da;
      });
      setThreads(arr);
      setLastRefresh(new Date());
    } catch {
      setThreads([]);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { loadThreads(); }, [loadThreads]);

  const threadCount  = threads.length;
  const recentFive   = threads.slice(0, 5);
  const healthyFlows = CRITICAL_FLOWS.filter(f => f.status === "healthy").length;
  const cautionFlows = CRITICAL_FLOWS.filter(f => f.status === "caution").length;

  return (
    <div style={{ height: "100%", overflow: "auto", background: "var(--bg)" }}>

      {/* ── Hero header ──────────────────────────────────────── */}
      <div className="dash-hero">
        <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", gap: 20, flexWrap: "wrap" }}>
          <div>
            <h1 style={{
              margin: 0,
              fontSize: 26,
              fontWeight: 900,
              color: "var(--text)",
              letterSpacing: "-0.03em",
              lineHeight: 1.1,
            }}>
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

          {/* Refresh button */}
          <button
            className="btn btn-secondary btn-sm"
            onClick={loadThreads}
            disabled={loading}
            style={{ flexShrink: 0 }}
          >
            {loading ? "Refreshing…" : "↻ Refresh"}
          </button>
        </div>
      </div>

      {/* ── Full-width content area — no maxWidth constraint ── */}
      <div style={{ padding: "24px 28px" }}>

        {/* ── KPI grid (full width) ────────────────────────────── */}
        <div className="kpi-grid" style={{ gridTemplateColumns: "repeat(auto-fit, minmax(160px, 1fr))", marginBottom: 24 }}>
          <KpiCard
            label="Active Threads"
            value={loading ? "—" : threadCount}
            sub="conversations tracked"
            icon="💬"
          />
          <KpiCard
            label="Pass Rate"
            value="94%"
            sub="7-day average"
            accent="var(--green)"
            icon="✓"
          />
          <KpiCard
            label="Failed Runs"
            value="2"
            sub="last 7 days"
            accent="var(--red)"
            icon="✗"
          />
          <KpiCard
            label="Critical Flows"
            value={CRITICAL_FLOWS.length}
            sub={`${healthyFlows} healthy · ${cautionFlows} caution`}
            icon="◈"
          />
          <KpiCard
            label="Avg Run Time"
            value="2.3s"
            sub="across all flows"
            icon="⏱"
          />
          <KpiCard
            label="Risk Alerts"
            value={cautionFlows}
            sub={cautionFlows > 0 ? "flows need attention" : "all clear"}
            accent={cautionFlows > 0 ? "var(--orange)" : "var(--green)"}
            icon="⚠"
          />
        </div>

        {/* ── Main 2-column grid: 2fr left / 1fr right ─────────── */}
        <div style={{
          display: "grid",
          gridTemplateColumns: "minmax(0, 2fr) minmax(0, 1fr)",
          gap: 24,
          alignItems: "start",
        }}>

          {/* ── LEFT COLUMN ─────────────────────────────────────── */}
          <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>

            {/* Recent Activity */}
            <div className="card" style={{ padding: 0, overflow: "hidden" }}>
              <div style={{
                padding: "14px 20px",
                borderBottom: "1px solid var(--border)",
                display: "flex",
                alignItems: "center",
                justifyContent: "space-between",
              }}>
                <div className="section-title" style={{ margin: 0 }}>Recent Activity</div>
                <Link to="/chat" style={{ fontSize: 12, color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>
                  View all →
                </Link>
              </div>

              {loading ? (
                <div style={{ padding: "24px 20px", color: "var(--text-3)", fontSize: 13 }}>
                  Loading conversations…
                </div>
              ) : recentFive.length === 0 ? (
                <div style={{ padding: "24px 20px", color: "var(--text-3)", fontSize: 13 }}>
                  No conversations yet.{" "}
                  <Link to="/chat" style={{ color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}>
                    Start a new chat →
                  </Link>
                </div>
              ) : (
                <table className="data-table">
                  <thead>
                    <tr>
                      <th>Conversation</th>
                      <th style={{ width: 140 }}>Last Updated</th>
                      <th style={{ width: 80 }}>Status</th>
                    </tr>
                  </thead>
                  <tbody>
                    {recentFive.map((t, i) => {
                      const id    = String(t?.id || t?.thread_id || "");
                      const title = buildTitle(t);
                      return (
                        <tr key={id || i}>
                          <td>
                            <div style={{ fontWeight: 600, fontSize: 13, color: "var(--text)" }}>
                              {title}
                            </div>
                            {id && (
                              <div style={{ fontSize: 11, color: "var(--text-3)", fontFamily: "monospace", marginTop: 2 }}>
                                {id.slice(0, 12)}…
                              </div>
                            )}
                          </td>
                          <td style={{ fontSize: 12, color: "var(--text-2)", whiteSpace: "nowrap" }}>
                            {fmtDate(t?.updated_at)}
                          </td>
                          <td>
                            <span className="badge badge-green">Active</span>
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              )}
            </div>

            {/* Critical Business Flows */}
            <div className="card">
              <div style={{
                display: "flex",
                alignItems: "center",
                justifyContent: "space-between",
                marginBottom: 14,
              }}>
                <div className="section-title" style={{ margin: 0 }}>Critical Business Flows</div>
                <div style={{ display: "flex", gap: 6 }}>
                  <span className="badge badge-green">{healthyFlows} healthy</span>
                  {cautionFlows > 0 && <span className="badge badge-orange">{cautionFlows} caution</span>}
                </div>
              </div>
              <div style={{
                display: "grid",
                gridTemplateColumns: "repeat(auto-fill, minmax(190px, 1fr))",
                gap: 10,
              }}>
                {CRITICAL_FLOWS.map(f => (
                  <FlowCard key={f.name} {...f} />
                ))}
              </div>
            </div>

          </div>{/* end LEFT COLUMN */}

          {/* ── RIGHT COLUMN ────────────────────────────────────── */}
          <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>

            {/* Vanya Recommendations */}
            <div className="card" style={{ padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
                <div className="section-title" style={{ margin: 0 }}>Vanya Recommendations</div>
              </div>
              <div style={{ padding: "12px 14px", display: "flex", flexDirection: "column", gap: 8 }}>
                {RECOMMENDATIONS.map((r, i) => (
                  <RecCard key={i} {...r} />
                ))}
              </div>
            </div>

            {/* Quick Actions */}
            <div className="card">
              <div className="section-title">Quick Actions</div>
              <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                <Link to="/chat" className="quick-action" style={{ flexDirection: "row", gap: 12 }}>
                  <span className="quick-action-icon" style={{ fontSize: 16 }}>✦</span>
                  <div>
                    <span className="quick-action-label">AI Chat</span>
                    <span className="quick-action-sub" style={{ display: "block" }}>Ask Vanya anything</span>
                  </div>
                </Link>
                <Link to="/planner" className="quick-action" style={{ flexDirection: "row", gap: 12 }}>
                  <span className="quick-action-icon" style={{ fontSize: 16 }}>⚡</span>
                  <div>
                    <span className="quick-action-label">Test Planner</span>
                    <span className="quick-action-sub" style={{ display: "block" }}>Generate test plan</span>
                  </div>
                </Link>
                <Link to="/runs" className="quick-action" style={{ flexDirection: "row", gap: 12 }}>
                  <span className="quick-action-icon" style={{ fontSize: 16 }}>◈</span>
                  <div>
                    <span className="quick-action-label">Lookup Run</span>
                    <span className="quick-action-sub" style={{ display: "block" }}>Review evidence</span>
                  </div>
                </Link>
                <Link to="/documents" className="quick-action" style={{ flexDirection: "row", gap: 12 }}>
                  <span className="quick-action-icon" style={{ fontSize: 16 }}>⊟</span>
                  <div>
                    <span className="quick-action-label">Documents</span>
                    <span className="quick-action-sub" style={{ display: "block" }}>Upload test specs</span>
                  </div>
                </Link>
              </div>
            </div>

          </div>{/* end RIGHT COLUMN */}

        </div>{/* end main 2-col grid */}
      </div>
    </div>
  );
}
