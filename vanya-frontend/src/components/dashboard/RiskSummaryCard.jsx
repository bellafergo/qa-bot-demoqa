import React from "react";
import { Link } from "react-router-dom";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";
import { SkeletonCard } from "../ui/Skeleton.jsx";
import {
  resolveHistoryCapabilityState,
  CAPABILITY_STATE_I18N_KEYS,
  DEFAULT_MIN_RUNS_FOR_TRENDS,
} from "../../utils/capabilityStateViewUtils.js";

export default function RiskSummaryCard({ summary, fi, loading, sectionError, t }) {
  if (loading && !summary) {
    return <SkeletonCard lines={4} />;
  }
  if (sectionError && !summary) {
    return (
      <div style={{ padding: "12px 0", fontSize: 12, color: "var(--red-text)" }}>
        {sectionError}
      </div>
    );
  }
  if (!summary) {
    return (
      <CapabilityStateCard
        state={resolveHistoryCapabilityState({
          runCount: 0,
          minRuns: DEFAULT_MIN_RUNS_FOR_TRENDS,
          title: t(CAPABILITY_STATE_I18N_KEYS.riskAssessmentTitle),
          t,
        })}
      />
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
