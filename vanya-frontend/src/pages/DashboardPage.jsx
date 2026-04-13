// src/pages/DashboardPage.jsx
/**
 * Vanya QA Intelligence — Executive Dashboard
 * Wired to live backend: /dashboard/summary, /dashboard/recent-runs,
 * /dashboard/recent-jobs, /failure-intelligence/summary
 */
import React, { useEffect, useState, useCallback, useMemo } from "react";
import { Link, useNavigate } from "react-router-dom";
import {
  getDashboardSummary,
  getDashboardRecentRuns,
  getDashboardRecentJobs,
  getFailureIntel,
  getRunsAnalytics,
  getExecStatus,
  runTest,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";

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

function isFailStatus(s) {
  return ["fail", "failed", "error"].includes(String(s || "").toLowerCase());
}

/** Local calendar day bounds for MEJORA #4 "Hoy" filter. */
function isSameLocalDay(iso, dayStart, dayEnd) {
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

function filterDashboardRuns(runs, filterKey) {
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

/**
 * Merge GET /execution/status orchestrator maps into one row per project.
 * "reserved" is the backend dispatch slot count (proxy for in-flight jobs per project).
 */
function buildProjectCapacityRows(execStatus) {
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

function projectCapacityAccent(reserved, max) {
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

function ProjectCapacitySection({ execStatus, projects, t }) {
  const rows = buildProjectCapacityRows(execStatus);
  if (rows.length === 0) return null;

  return (
    <div className="card" style={{ padding: "20px 24px", marginBottom: 28 }}>
      <div className="section-title" style={{ marginBottom: 16 }}>{t("dashboard.project_capacity")}</div>
      <div style={{
        display: "grid",
        gridTemplateColumns: "repeat(auto-fill, minmax(220px, 1fr))",
        gap: 16,
      }}>
        {rows.map((row) => {
          const name = Array.isArray(projects)
            ? (projects.find((p) => p && p.id === row.projectId)?.name || "")
            : "";
          const accent = projectCapacityAccent(row.reserved, row.max);
          const pct = Math.min(100, row.max > 0 ? (row.reserved / row.max) * 100 : 0);
          return (
            <div
              key={row.projectId}
              style={{
                borderRadius: "var(--r-sm)",
                border: `1px solid ${accent.border}`,
                background: "var(--surface)",
                padding: "14px 16px",
                boxSizing: "border-box",
              }}
            >
              <div style={{ marginBottom: 10 }}>
                <div style={{ fontFamily: "monospace", fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>
                  {row.projectId}
                </div>
                {name && name !== row.projectId && (
                  <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 3 }}>{name}</div>
                )}
              </div>

              <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 6 }}>
                <strong style={{ color: "var(--text-3)", fontWeight: 500 }}>{t("dashboard.running")}:</strong>{" "}
                <span style={{ fontWeight: 600, color: accent.label }}>{row.reserved}</span>
                {" / "}
                <span style={{ color: "var(--text-3)" }}>{row.max}</span>
                <span style={{ color: "var(--text-3)", fontSize: 10, marginLeft: 4 }}>({t("dashboard.max")})</span>
              </div>
              <div style={{ height: 6, borderRadius: 3, background: "var(--border)", overflow: "hidden", marginBottom: 10 }}>
                <div style={{
                  width: `${pct}%`,
                  height: "100%",
                  borderRadius: 3,
                  background: accent.bar,
                  transition: "width 0.35s ease",
                }} />
              </div>
              <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 4 }}>
                <strong style={{ color: "var(--text-3)", fontWeight: 500 }}>{t("dashboard.queued")}:</strong>{" "}
                <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{row.queued}</span>
              </div>
              <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                <strong style={{ color: "var(--text-3)", fontWeight: 500 }}>{t("dashboard.reserved")}:</strong>{" "}
                <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{row.reserved}</span>
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
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
        <div style={{ fontSize: 13, fontWeight: 500, color: "var(--text-3)" }}>{t("dash.trends.no_data")}</div>
        <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>{t("dash.trends.no_data_sub")}</div>
      </div>
    );
  }

  // QA FINAL — extra right padding so "Meta 80%" does not collide with the last plot point
  const W = 400, H = 110, PAD_L = 32, PAD_R = 40, PAD_T = 10, PAD_B = 24;
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

  // MEJORA #7 — reference band 80–100% and horizontal line at 80%
  const yAt = (rate) => PAD_T + innerH * (1 - rate);
  const y80 = yAt(0.8);
  const y100 = yAt(1);
  const bandH = y80 - y100;

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} style={{ width: "100%", height: 110, display: "block" }}>

        {/* Grid lines */}
        {[0, 0.5, 1].map(pct => {
          const gy = (PAD_T + innerH * (1 - pct)).toFixed(1);
          return (
            <line key={pct} x1={PAD_L} y1={gy} x2={W - PAD_R} y2={gy}
              stroke="var(--border)" strokeWidth="0.5" strokeDasharray="2,4" />
          );
        })}

        {/* MEJORA #7 — success band between 80% and 100% (behind series) */}
        <rect
          x={PAD_L}
          y={y100}
          width={innerW}
          height={Math.max(0, bandH)}
          fill="var(--green)"
          opacity={0.08}
        />
        <line
          x1={PAD_L}
          y1={y80}
          x2={W - PAD_R}
          y2={y80}
          stroke="var(--green)"
          strokeWidth="1"
          strokeDasharray="4,3"
          opacity={0.85}
        />
        <text
          x={PAD_L + 4}
          y={Math.max(PAD_T + 11, y80 - 4)}
          textAnchor="start"
          fontSize="9"
          fill="var(--green)"
          fontWeight={500}
        >
          {t("dash.trends.meta_80")}
        </text>

        {/* Area fill */}
        <path d={areaPath} fill="var(--chart-trend-fill)" />

        {/* Avg line */}
        <line x1={PAD_L} y1={avgY} x2={W - PAD_R} y2={avgY}
          stroke="var(--text-3)" strokeWidth="1" strokeDasharray="3,3" />

        {/* Trend line */}
        <polyline points={lineStr} fill="none" stroke="var(--chart-line)"
          strokeWidth="1.75" strokeLinejoin="round" strokeLinecap="round" />

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
        <span>· {t("dash.trends.avg")}: <strong style={{ color: "var(--chart-line)" }}>{(avgRate * 100).toFixed(0)}%</strong></span>
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
            stroke="var(--chart-donut-ui)" strokeWidth={STROKE}
            strokeDasharray={`${uiArc.toFixed(2)} ${(CIRC - uiArc).toFixed(2)}`}
            style={{ transform: `rotate(-90deg)`, transformOrigin: `${CX}px ${CY}px` }}
          />
        )}

        {/* API segment */}
        {api > 0 && (
          <circle cx={CX} cy={CY} r={R} fill="none"
            stroke="var(--chart-donut-api)" strokeWidth={STROKE}
            strokeDasharray={`${apiArc.toFixed(2)} ${(CIRC - apiArc).toFixed(2)}`}
            style={{ transform: `rotate(${(-90 + uiDeg).toFixed(2)}deg)`, transformOrigin: `${CX}px ${CY}px` }}
          />
        )}

        {/* Center */}
        <text x={CX} y={CY - 7} textAnchor="middle" fontSize="18" fontWeight="600" fill="var(--text-1)">
          {loading ? "…" : total}
        </text>
        <text x={CX} y={CY + 9} textAnchor="middle" fontSize="9" fill="var(--text-3)">
          {t("dash.coverage.total")}
        </text>
      </svg>

      <div style={{ display: "flex", flexDirection: "column", gap: 10, fontSize: 12, flex: 1 }}>
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <span style={{ width: 9, height: 9, borderRadius: "50%", background: "var(--chart-donut-ui)", flexShrink: 0 }} />
          <span style={{ color: "var(--text-2)", flex: 1 }}>{t("dash.coverage.ui")}</span>
          <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{loading ? "…" : ui}</span>
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <span style={{ width: 9, height: 9, borderRadius: "50%", background: "var(--chart-donut-api)", flexShrink: 0 }} />
          <span style={{ color: "var(--text-2)", flex: 1 }}>{t("dash.coverage.api")}</span>
          <span style={{ fontWeight: 600, color: "var(--text-1)" }}>{loading ? "…" : api}</span>
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
        <div style={{ fontSize: 13, fontWeight: 500, color: "var(--text-3)" }}>{t("dash.failures.no_data")}</div>
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
    { key: "dash.failures.clusters",    val: clusters,    color: "var(--blue)"  },
    { key: "dash.failures.regressions", val: regressions, color: "var(--red)"     },
  ];

  if (allZero && !loading) {
    return (
      <div style={{ padding: "14px 0", fontSize: 12, color: "var(--green)", fontWeight: 500 }}>
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
            <span style={{ fontWeight: 600, color: val > 0 ? color : "var(--text-3)" }}>
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

// ── Visual widget: Failure Intelligence mini panel ──────────────────────────
function FailureIntelMiniPanel({ fi, t }) {
  const top = fi?.top_failure_categories;
  const notes = typeof fi?.notes === "string" ? fi.notes.trim() : "";

  const topEntries = top && typeof top === "object"
    ? Object.entries(top)
        .filter(([_, v]) => v != null)
        .sort((a, b) => Number(b[1]) - Number(a[1]))
        .slice(0, 5)
    : [];

  const hasAny = topEntries.length > 0 || !!notes;
  if (!hasAny) return null;

  return (
    <div style={{ marginTop: 14, paddingTop: 12, borderTop: "1px solid var(--border)" }}>
      {topEntries.length > 0 && (
        <div style={{ display: "flex", flexDirection: "column", gap: 6, marginBottom: notes ? 10 : 0 }}>
          <div style={{
            fontSize: 11,
            fontWeight: 500,
            textTransform: "uppercase",
            letterSpacing: "0.08em",
            color: "var(--text-3)",
            marginBottom: 4,
          }}>
            {t("dash.analytics.top_failures")}
          </div>
          {topEntries.map(([cat, count]) => (
            <div key={cat} style={{ display: "flex", justifyContent: "space-between", gap: 10, fontSize: 12, color: "var(--text-2)" }}>
              <span style={{ fontWeight: 500 }}>{cat}</span>
              <span style={{ color: "var(--text-3)", whiteSpace: "nowrap", fontFamily: "monospace" }}>{count}</span>
            </div>
          ))}
        </div>
      )}

      {!!notes && (
        <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
          <div style={{ fontWeight: 500, color: "var(--text-4)", marginBottom: 6 }}>{t("fi.col.notes")}</div>
          <div style={{ whiteSpace: "pre-wrap" }}>{notes}</div>
        </div>
      )}
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
    HIGH:   { bg: "#fef2f2", border: "#fecaca", text: "#b91c1c", pillBg: "#fee2e2", icon: "⚠",  labelKey: "dash.risk.high"   },
    MEDIUM: { bg: "#fffbeb", border: "#fde68a", text: "#b45309", pillBg: "#fef3c7", icon: "◎",  labelKey: "dash.risk.medium" },
    LOW:    { bg: "#f0fdf4", border: "#bbf7d0", text: "#15803d", pillBg: "#dcfce7", icon: "✓",  labelKey: "dash.risk.low"    },
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
      border: `1px solid ${c.border}`,
      background: c.bg,
      padding: "16px 18px",
      boxSizing: "border-box",
    }}>
      {/* Header row: icon + level badge + optional sub */}
      <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 12 }}>
        <span style={{ fontSize: 16, lineHeight: 1, flexShrink: 0 }}>{c.icon}</span>
        <span style={{
          fontWeight: 500,
          fontSize: 12,
          color: c.text,
          background: c.pillBg,
          border: `1px solid ${c.border}`,
          borderRadius: 999,
          padding: "4px 10px",
          letterSpacing: "0.02em",
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
            background: "rgba(255,255,255,0.7)",
            border: "1px solid var(--border-light)",
            borderRadius: 8,
            padding: "8px 10px",
            textAlign: "center",
          }}>
            <div style={{ fontSize: 15, fontWeight: 600, color: accent, lineHeight: 1.1, marginBottom: 3 }}>
              {value}
            </div>
            <div style={{ fontSize: 10, fontWeight: 400, color: "var(--text-4)", lineHeight: 1.2 }}>
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

      {/* MEJORA #8 — discrete link to Insights */}
      <div style={{ marginTop: 12, paddingTop: 10, borderTop: `1px solid ${c.border}` }}>
        <Link
          to="/insights"
          style={{ fontSize: 11, color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}
        >
          {t("dash.risk.view_recommendations")}
        </Link>
      </div>
    </div>
  );
}

// ── Existing sub-components ───────────────────────────────────────────────────

/** `valueExtra` supports MEJORA #1 (benchmark under pass rate) without changing other KPI cards. */
function KpiCard({ label, value, sub, accent, icon, valueExtra }) {
  return (
    <div className="kpi-card">
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start" }}>
        <div className="kpi-label">{label}</div>
        {icon && <span style={{ fontSize: 16, opacity: 0.55 }}>{icon}</span>}
      </div>
      <div className="kpi-value" style={accent ? { color: accent } : {}}>
        {value ?? "—"}
      </div>
      {valueExtra}
      {sub && <div className="kpi-sub">{sub}</div>}
    </div>
  );
}

function SectionCard({ title, link, linkLabel, children }) {
  return (
    <div className="card" style={{ padding: 0, overflow: "hidden" }}>
      <div style={{
        padding: "16px 22px", borderBottom: "1px solid var(--border)",
        display: "flex", alignItems: "center", justifyContent: "space-between",
      }}>
        <div className="section-title" style={{ margin: 0 }}>{title}</div>
        {link && (
          <Link to={link} style={{ fontSize: 12, color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>
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
    <div className="card" style={{ padding: "20px 24px" }}>
      <div style={{ marginBottom: 16 }}>
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
  const navigate = useNavigate();
  const { currentProject, projects } = useProject();
  const projectId = currentProject?.id;

  const [summary, setSummary]         = useState(null);
  const [recentRuns, setRecentRuns]   = useState([]);
  const [recentJobs, setRecentJobs]   = useState([]);
  const [fi, setFi]                   = useState(null);
  const [analytics, setAnalytics]     = useState(null);
  const [execStatus, setExecStatus]   = useState(null);
  const [loading, setLoading]         = useState(true);
  const [lastRefresh, setLastRefresh] = useState(null);
  // MEJORA #4 — client-side filters for recent runs (no extra API calls)
  const [recentRunsFilter, setRecentRunsFilter] = useState("all");
  // MEJORA #5 — single-flight re-run per row
  const [rerunBusyRunId, setRerunBusyRunId] = useState(null);
  const [runInlineError, setRunInlineError]   = useState("");

  const load = useCallback(async () => {
    setLoading(true);
    try {
      const pid = projectId;
      const [s, runs, jobs, ex] = await Promise.all([
        getDashboardSummary(pid ? { project_id: pid } : {}).catch(() => null),
        getDashboardRecentRuns(10, pid).catch(() => []),
        getDashboardRecentJobs(10, pid).catch(() => []),
        getExecStatus().catch(() => null),
      ]);
      setSummary(s);
      setRecentRuns(Array.isArray(runs) ? runs : []);
      setRecentJobs(Array.isArray(jobs) ? jobs : []);
      setExecStatus(ex && typeof ex === "object" ? ex : null);
      setLastRefresh(new Date());

      // Non-critical — load separately so they don't block the main render
      getFailureIntel(pid).then(f => setFi(f)).catch(() => {});
      getRunsAnalytics(pid).then(a => setAnalytics(a)).catch(() => {});
    } catch {
      // partial load is fine
    } finally {
      setLoading(false);
    }
  }, [projectId]);

  useEffect(() => {
    load();
  }, [load]);

  const s = summary || {};
  // QA FINAL — numeric pass_rate only (avoids NaN / string edge cases breaking toFixed / delta)
  const passRateRaw = s.pass_rate;
  const passRateNum =
    passRateRaw == null || passRateRaw === "" ? null : Number(passRateRaw);
  const passRateValid = passRateNum != null && !Number.isNaN(passRateNum);
  const passRate = passRateValid ? `${passRateNum.toFixed(1)}%` : "—";

  const filteredRecentRuns = useMemo(
    () => filterDashboardRuns(recentRuns, recentRunsFilter),
    [recentRuns, recentRunsFilter],
  );

  const handleDashboardRerun = useCallback(
    async (r) => {
      const tcId = r.test_id || r.test_case_id;
      if (!tcId) return;
      setRunInlineError("");
      const busyKey = r.run_id || tcId;
      setRerunBusyRunId(busyKey);
      try {
        await runTest(tcId, { headless: true });
        await load();
      } catch (e) {
        setRunInlineError(apiErrorMessage(e) || t("dash.rerun.error"));
      } finally {
        setRerunBusyRunId(null);
      }
    },
    [load, t],
  );

  const openRunEvidence = useCallback(
    (r) => {
      const id = String(r.run_id || "").trim();
      if (!id) return;
      navigate(`/evidence/run/${encodeURIComponent(id)}`);
    },
    [navigate],
  );

  // KPI "Total Ejecuciones" — subtexto dinámico con solo los estados > 0
  const runsSubParts = [];
  if ((s.pass_runs  ?? 0) > 0) runsSubParts.push(`${s.pass_runs}  ${t("dash.kpi.pass")}`);
  if ((s.fail_runs  ?? 0) > 0) runsSubParts.push(`${s.fail_runs}  ${t("dash.kpi.fail")}`);
  if ((s.error_runs ?? 0) > 0) runsSubParts.push(`${s.error_runs} ${t("dash.kpi.error")}`);
  const runsSub = runsSubParts.length > 0 ? runsSubParts.join(" · ") : t("dash.kpi.no_runs");

  // KPI "Tasa de Éxito" — subtexto con total_runs + nota discreta si historial < 5
  const totalRunsN  = s.total_runs ?? 0;
  const passRateSub = (
    <>
      {`${t("dash.kpi.based_on")} ${totalRunsN} ${t("dash.kpi.runs_unit")}`}
      {totalRunsN < 5 && (
        <span style={{ fontStyle: "italic", color: "var(--text-3)", marginLeft: 4 }}>
          {`· ${t("dash.risk.limited_history")}`}
        </span>
      )}
    </>
  );

  // MEJORA #1 — benchmark vs 80% target (delta in percentage points)
  const passBench = 80;
  const passRateDeltaPp = passRateValid ? passRateNum - passBench : null;
  const passRateBenchmarkExtra =
    !loading && passRateValid && passRateDeltaPp != null ? (
      // MEJORA #1 — benchmark line (does not replace main pass rate value)
      <div
        style={{
          fontSize: 11,
          fontWeight: 500,
          color: "var(--text-2)",
          lineHeight: 1.35,
          display: "flex",
          flexWrap: "wrap",
          alignItems: "baseline",
          gap: "0 8px",
          marginTop: 4,
        }}
      >
        <span>{t("dash.kpi.meta_target")}</span>
        <span
          style={{
            fontVariantNumeric: "tabular-nums",
            color:
              passRateNum >= passBench
                ? "var(--green)"
                : passRateNum < 60
                  ? "var(--red)"
                  : "var(--orange)",
          }}
        >
          {passRateDeltaPp >= 0 ? "+" : ""}
          {passRateDeltaPp.toFixed(1)} pp
        </span>
      </div>
    ) : null;

  // MEJORA #2 — hide workers + jobs KPIs when both are zero (after load)
  const idleExecKpis =
    !loading && (s.active_workers ?? 0) === 0 && (s.total_jobs ?? 0) === 0;

  return (
    <div style={{ height: "100%", overflow: "auto", background: "var(--bg)" }}>

      {/* ── Hero ─────────────────────────────────────────────────────────── */}
      <div className="dash-hero">
        <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", gap: 24, flexWrap: "wrap" }}>
          <div style={{ flex: "1 1 280px", minWidth: 0 }}>
            <h1 style={{ margin: 0, fontSize: 26, fontWeight: 600, color: "var(--text-1)", letterSpacing: "-0.02em", lineHeight: 1.2 }}>
              {t("dash.title")}
            </h1>
            <p style={{ margin: "10px 0 18px", fontSize: 13, fontWeight: 400, color: "var(--text-3)", lineHeight: 1.55 }}>
              {t("dash.subtitle")}
            </p>
            <div className="dash-hero-pill-row" style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
              {currentProject && (
                <span className="badge badge-gray" style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
                  <span aria-hidden style={{ width: 8, height: 8, borderRadius: "50%", background: currentProject.color || "var(--accent)" }} />
                  {t("dash.active_project", { name: currentProject.name })}
                </span>
              )}
              <span className="badge badge-green">● {t("common.live")}</span>
              <span className="badge badge-accent">{t("common.production")}</span>
              {lastRefresh && (
                <span className="badge badge-gray">
                  {t("dash.refreshed")} {lastRefresh.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}
                </span>
              )}
            </div>
          </div>
          <div style={{ display: "flex", flexDirection: "column", alignItems: "stretch", gap: 10, flexShrink: 0 }}>
            <div className="zu-action-row" style={{ justifyContent: "flex-end" }}>
              <Link to="/generate" className="btn btn-primary btn-lg">
                {t("nav.quick_generate")}
              </Link>
              <Link to="/projects?new=1" className="btn btn-secondary btn-lg">
                {t("projects.create_new")}
              </Link>
            </div>
            <button type="button" className="btn btn-ghost btn-sm" onClick={load} disabled={loading} style={{ alignSelf: "flex-end" }}>
              {loading ? t("dash.refreshing") : t("dash.refresh")}
            </button>
          </div>
        </div>
      </div>

      <div style={{ padding: "32px 40px" }}>

        {/* ── KPI grid (MEJORA #3 — rely on .kpi-grid auto-fit for variable card count) ── */}
        <div className="kpi-grid" style={{ marginBottom: 28 }}>
          <KpiCard label={t("dash.kpi.total_tests")}    value={loading ? "…" : s.total_test_cases} sub={`${s.active_test_cases ?? "—"} ${t("dash.kpi.active")}`}                        icon="☰" />
          <KpiCard label={t("dash.kpi.total_runs")}     value={loading ? "…" : s.total_runs}        sub={runsSub}      icon="▶" />
          <KpiCard
            label={t("dash.kpi.pass_rate")}
            value={loading ? "…" : passRate}
            valueExtra={passRateBenchmarkExtra}
            sub={passRateSub}
            accent={
              !loading && passRateValid
                ? passRateNum >= passBench
                  ? "var(--green)"
                  : "var(--orange)"
                : undefined
            }
            icon="✓"
          />
          {!idleExecKpis && (
            <>
              <KpiCard label={t("dash.kpi.active_workers")} value={loading ? "…" : s.active_workers}    sub={`${s.queue_depth ?? 0} ${t("dash.kpi.queued")}`}                                icon="⚙" />
              <KpiCard label={t("dash.kpi.total_jobs")}     value={loading ? "…" : s.total_jobs}        sub={`${s.running_jobs ?? 0} ${t("dash.kpi.running")} · ${s.queued_jobs ?? 0} ${t("dash.kpi.queued")}`} icon="◈" />
            </>
          )}
          <KpiCard label={t("dash.kpi.ui_tests")}       value={loading ? "…" : s.total_ui_tests}    sub={t("dash.kpi.in_catalog")}                                                        icon="◻" />
          <KpiCard label={t("dash.kpi.api_tests")}      value={loading ? "…" : s.total_api_tests}   sub={t("dash.kpi.in_catalog")}                                                        icon="⌥" />
          {fi && <KpiCard label={t("dash.kpi.flaky_tests")} value={fi.flaky_tests_count ?? 0} sub={`${fi.total_clusters ?? 0} ${t("dash.kpi.clusters")}`} accent={fi.flaky_tests_count > 0 ? "var(--orange)" : undefined} icon="⚠" />}
          {/* MEJORA #2 — compact CTA when no active workers and no jobs */}
          {idleExecKpis && (
            <div
              className="kpi-card dash-kpi-idle-cta"
              style={{
                gridColumn: "1 / -1",
                flexDirection: "row",
                alignItems: "center",
                justifyContent: "space-between",
                flexWrap: "wrap",
                gap: 12,
                minHeight: 72,
              }}
            >
              <span style={{ fontSize: 13, color: "var(--text-2)", fontWeight: 500 }}>{t("dash.kpi.idle_exec_title")}</span>
              <Link to="/batch" style={{ fontSize: 13, color: "var(--accent)", fontWeight: 600, textDecoration: "none", whiteSpace: "nowrap" }}>
                {t("dash.kpi.idle_exec_link")}
              </Link>
            </div>
          )}
        </div>

        {!loading && (
          <ProjectCapacitySection execStatus={execStatus} projects={projects} t={t} />
        )}

        {/* ── Visual row 1: Trend (2/3) + Coverage Donut (1/3) ─────────────── */}
        <div style={{ display: "grid", gridTemplateColumns: "minmax(0,2fr) minmax(0,1fr)", gap: 24, marginBottom: 24 }}>
          <WidgetCard title={t("dash.trends.title")} subtitle={`${t("dash.trends.subtitle")} · ${recentRuns.length}`}>
            <PassRateTrendChart runs={recentRuns} loading={loading} t={t} />
          </WidgetCard>
          <WidgetCard title={t("dash.coverage.title")}>
            <CoverageDonutChart summary={summary} loading={loading} t={t} />
          </WidgetCard>
        </div>

        {/* ── Visual row 2: Failure Distribution (1/2) + Risk Summary (1/2) ── */}
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 24, marginBottom: 28 }}>
          <WidgetCard title={t("dash.failures.title")} subtitle={t("dash.failures.subtitle")}>
            <FailureDistributionChart fi={fi} loading={loading} t={t} />
            <FailureIntelMiniPanel fi={fi} t={t} />
          </WidgetCard>
          <WidgetCard title={t("dash.risk.title")} subtitle={t("dash.risk.subtitle")}>
            <RiskSummaryCard summary={summary} fi={fi} loading={loading} t={t} />
          </WidgetCard>
        </div>

        {/* ── Bottom: runs + jobs + insights (MEJORA #6 — quick actions column removed) ─── */}
        <div style={{ display: "flex", flexDirection: "column", gap: 28, alignItems: "stretch" }}>

          <div style={{ display: "flex", flexDirection: "column", gap: 24 }}>

            {/* Recent Runs */}
            <SectionCard title={t("dash.recent_runs")} link="/runs" linkLabel={t("dash.recent_runs.link")}>
              {loading ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("dash.loading_runs")}</div>
              ) : recentRuns.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  {t("dash.no_runs")} <Link to="/catalog" style={{ color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>{t("dash.run_a_test")}</Link>
                </div>
              ) : (
                <>
                  {/* MEJORA #4 — quick client-side filters */}
                  <div
                    className="dash-recent-runs-filters"
                    role="toolbar"
                    aria-label={t("dash.runs.filter.toolbar_aria")}
                    style={{
                      padding: "12px 16px",
                      borderBottom: "1px solid var(--border)",
                      display: "flex",
                      flexWrap: "wrap",
                      gap: 8,
                      alignItems: "center",
                      minHeight: 48,
                      boxSizing: "border-box",
                    }}
                  >
                    {["all", "failed", "passed", "today"].map((key) => (
                      <button
                        key={key}
                        type="button"
                        className={
                          recentRunsFilter === key ? "dash-filter-chip dash-filter-chip-active" : "dash-filter-chip dash-filter-chip-outline"
                        }
                        aria-pressed={recentRunsFilter === key}
                        onClick={() => {
                          setRunInlineError("");
                          setRecentRunsFilter(key);
                        }}
                      >
                        {t(`dash.runs.filter.${key}`)}
                      </button>
                    ))}
                  </div>
                  {runInlineError && (
                    <div style={{ padding: "10px 16px", fontSize: 12, color: "var(--red)", borderBottom: "1px solid var(--border)" }}>
                      {runInlineError}
                    </div>
                  )}
                  {filteredRecentRuns.length === 0 ? (
                    <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("dash.runs.filter.empty")}</div>
                  ) : (
                    <table className="data-table dash-recent-runs-table">
                      <thead><tr>
                        <th>{t("dash.col.test_case")}</th>
                        <th>{t("dash.col.status")}</th>
                        <th>{t("dash.col.duration")}</th>
                        <th>{t("dash.col.executed")}</th>
                        <th className="dash-recent-actions-col">{t("dash.col.actions")}</th>
                      </tr></thead>
                      <tbody>
                        {/* MEJORA #5 — inline actions for failed-style rows (hover emphasis via CSS) */}
                        {filteredRecentRuns.slice(0, 8).map((r, i) => {
                          const showFailActions = isFailStatus(r.status);
                          const busyKey = r.run_id || r.test_id || r.test_case_id;
                          const busy = rerunBusyRunId && busyKey && rerunBusyRunId === busyKey;
                          return (
                            <tr key={r.run_id || i} className="dash-recent-runs-row">
                              <td style={{ fontWeight: 500, fontSize: 12, fontFamily: "monospace" }}>
                                {r.test_id || r.test_case_id || "—"}
                              </td>
                              <td><span className={`badge ${statusClass(r.status)}`}>{r.status}</span></td>
                              <td style={{ fontSize: 12, color: "var(--text-2)" }}>{fmtMs(r.duration_ms)}</td>
                              <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>
                                {fmtDate(r.started_at || r.executed_at)}
                              </td>
                              <td className="dash-recent-actions-cell" style={{ textAlign: "right", whiteSpace: "nowrap" }}>
                                {showFailActions ? (
                                  <span className="dash-recent-runs-actions">
                                    <button
                                      type="button"
                                      className="btn btn-ghost btn-sm"
                                      style={{
                                        padding: "2px 8px",
                                        fontSize: 11,
                                        minHeight: 28,
                                        opacity: busy ? 0.65 : 1,
                                      }}
                                      aria-busy={busy ? true : undefined}
                                      disabled={busy || !(r.test_id || r.test_case_id)}
                                      onClick={() => handleDashboardRerun(r)}
                                    >
                                      {busy ? t("dash.rerun.running") : t("dash.rerun.cta")}
                                    </button>
                                    <button
                                      type="button"
                                      className="btn btn-ghost btn-sm"
                                      style={{ padding: "2px 8px", fontSize: 11, minHeight: 28, marginLeft: 4 }}
                                      disabled={!r.run_id}
                                      onClick={() => openRunEvidence(r)}
                                    >
                                      {t("dash.runs.view_run")}
                                    </button>
                                  </span>
                                ) : null}
                              </td>
                            </tr>
                          );
                        })}
                      </tbody>
                    </table>
                  )}
                </>
              )}
            </SectionCard>

            {/* Recent Jobs */}
            <SectionCard title={t("dash.recent_jobs")} link="/batch" linkLabel={t("dash.recent_jobs.link")}>
              {loading ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("dash.loading_jobs")}</div>
              ) : recentJobs.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  {t("dash.no_jobs")} <Link to="/batch" style={{ color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>{t("dash.run_batch")}</Link>
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

          {/* Failure Intelligence summary — full width after MEJORA #6 */}
          {fi && (
            <div className="card">
              <div className="section-title">{t("dash.insights_summary")}</div>
              <div style={{ display: "flex", flexDirection: "column", gap: 10, marginTop: 6 }}>
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
                <Link to="/insights" style={{ fontSize: 12, color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>
                  {t("dash.view_insights")}
                </Link>
              </div>
            </div>
          )}

        </div>
      </div>

      {/* ── Run Analytics ────────────────────────────────────────────────── */}
      <div style={{ marginTop: 32, padding: "0 40px 40px" }}>
        <div className="card" style={{ padding: "20px 24px" }}>
          <div style={{ marginBottom: 18, display: "flex", alignItems: "baseline", justifyContent: "space-between", gap: 16, flexWrap: "wrap" }}>
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
              <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(120px, 1fr))", gap: 14, marginBottom: 24 }}>
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
                    <div style={{ fontSize: 18, fontWeight: 600, color: accent || "var(--text-1)", lineHeight: 1.1, marginBottom: 4 }}>
                      {value ?? "—"}
                    </div>
                    <div style={{ fontSize: 10, fontWeight: 400, color: "var(--text-4)", lineHeight: 1.2 }}>{label}</div>
                  </div>
                ))}
              </div>

              {/* Two-column: top failures + 7-day trend */}
              <div style={{ display: "grid", gridTemplateColumns: "minmax(0,3fr) minmax(0,2fr)", gap: 24 }}>

                {/* Top Failing Tests */}
                <div>
                  <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-4)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 10 }}>
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
                              <span style={{ color: "var(--red)", fontWeight: 600 }}>{tf.failed_runs}</span>
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
                  <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-4)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 10 }}>
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
  );
}

function Row({ label, value, accent }) {
  return (
    <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", fontSize: 13 }}>
      <span style={{ color: "var(--text-3)", fontWeight: 400 }}>{label}</span>
      <span style={{ fontWeight: 600, color: accent || "var(--text-1)" }}>{value}</span>
    </div>
  );
}
