// src/components/ProjectHealthStrip.jsx
import React from "react";
import { Link } from "react-router-dom";
import InitializeProjectPanel from "./InitializeProjectPanel.jsx";

function fmtDate(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short", day: "numeric", hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

function healthVariant(summary, passRateValid, passRateNum) {
  const runs = summary?.total_runs ?? 0;
  if (runs === 0) return "empty";
  if (passRateValid && passRateNum < 60 && runs >= 3) return "danger";
  if (passRateValid && passRateNum >= 80) return "good";
  return "neutral";
}

function memoryLabel(t, hasKnowledge, loading) {
  if (loading) return "…";
  if (hasKnowledge === true) return t("health.memory.ready");
  if (hasKnowledge === false) return t("health.memory.pending");
  return t("health.memory.unknown");
}

function executionLabel(t, runs, loading) {
  if (loading) return "…";
  if (runs > 0) return t("health.execution.active");
  return t("health.execution.none");
}

function generalHealthLabel(t, variant, loading) {
  if (loading) return "…";
  if (variant === "empty") return t("health.pending");
  if (variant === "good") return t("health.good");
  if (variant === "danger") return t("health.at_risk");
  return t("health.ok");
}

function riskLabel(t, variant, riskLevel, loading) {
  if (loading) return "…";
  if (variant === "empty") return t("health.risk_not_evaluated");
  if (riskLevel === "elevated") return t("health.elevated");
  if (riskLevel === "watch") return t("health.watch");
  return t("health.stable");
}

function ExecutiveStatusGrid({ cells, loading }) {
  return (
    <div
      style={{
        display: "grid",
        gridTemplateColumns: "repeat(auto-fit, minmax(130px, 1fr))",
        gap: 16,
        flex: 1,
      }}
    >
      {cells.map((c) => (
        <div key={c.label}>
          <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
            {c.label}
          </div>
          <div
            style={{
              fontSize: 14,
              fontWeight: 600,
              color: c.accent || "var(--text-1)",
              marginTop: 4,
              lineHeight: 1.35,
            }}
          >
            {loading ? "…" : c.value}
          </div>
        </div>
      ))}
    </div>
  );
}

export default function ProjectHealthStrip({
  t,
  projectId,
  projectName,
  summary,
  fi,
  hasKnowledge,
  loading,
  passRateValid,
  passRateNum,
  lastRefresh,
  onInitialized,
}) {
  const s = summary || {};
  const runs = s.total_runs ?? 0;
  const variant = healthVariant(s, passRateValid, passRateNum);
  const riskLevel =
    (fi?.recurrent_regressions_count ?? 0) > 0
      ? "elevated"
      : s.pass_rate != null && s.pass_rate < 70
        ? "watch"
        : "stable";

  let recommendation = t("health.rec.stable");
  if (variant === "empty") recommendation = t("health.rec.init_full");
  else if (riskLevel === "elevated") recommendation = t("health.rec.regressions");
  else if (passRateValid && passRateNum < 60) recommendation = t("health.rec.quality");

  const cells = [
    {
      label: t("health.general"),
      value: generalHealthLabel(t, variant, loading),
      accent: variant === "empty" ? "var(--orange-text)" : undefined,
    },
    {
      label: t("health.memory_status"),
      value: memoryLabel(t, hasKnowledge, loading),
    },
    {
      label: t("health.execution_status"),
      value: executionLabel(t, runs, loading),
    },
    {
      label: t("health.risk"),
      value: riskLabel(t, variant, riskLevel, loading),
      accent: variant === "empty" ? "var(--text-3)" : undefined,
    },
  ];

  if (!projectId) {
    return (
      <div className="card dash-health-strip dash-health-strip--empty" style={{ padding: "16px 20px", marginBottom: 20 }}>
        <p style={{ margin: 0, fontSize: 13, color: "var(--text-2)" }}>{t("health.no_project")}</p>
        <Link to="/projects?new=1" className="btn btn-primary btn-sm" style={{ marginTop: 12 }}>{t("health.create_project")}</Link>
      </div>
    );
  }

  const showInitPanel = variant === "empty" && !loading;

  return (
    <div style={{ marginBottom: 20 }}>
      <div className="card dash-health-strip" style={{ padding: "16px 20px", marginBottom: showInitPanel ? 14 : 0 }}>
        <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.07em", marginBottom: 12 }}>
          {t("health.executive_title")}
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 16, flexWrap: "wrap", alignItems: "flex-start" }}>
          <ExecutiveStatusGrid cells={cells} loading={loading} />
          <div style={{ minWidth: 220, maxWidth: 300 }}>
            <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase" }}>{t("health.recommendation")}</div>
            <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 4, lineHeight: 1.5 }}>{loading ? "…" : recommendation}</div>
            {lastRefresh ? (
              <div style={{ fontSize: 10, color: "var(--text-4)", marginTop: 6 }}>
                {t("dash.refreshed")}: {fmtDate(lastRefresh.toISOString?.() || lastRefresh)}
              </div>
            ) : null}
          </div>
        </div>
      </div>

      {showInitPanel ? (
        <InitializeProjectPanel
          projectId={projectId}
          projectName={projectName}
          onInitialized={onInitialized}
        />
      ) : null}
    </div>
  );
}
