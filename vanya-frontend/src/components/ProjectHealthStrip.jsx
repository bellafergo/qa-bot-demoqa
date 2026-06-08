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

export default function ProjectHealthStrip({
  t,
  projectId,
  projectName,
  summary,
  fi,
  loading,
  passRateValid,
  passRateNum,
  lastRefresh,
  onInitialized,
}) {
  const s = summary || {};
  const runs = s.total_runs ?? 0;
  const variant = healthVariant(s, passRateValid, passRateNum);
  const riskLevel = (fi?.recurrent_regressions_count ?? 0) > 0 ? "elevated" : (s.pass_rate != null && s.pass_rate < 70 ? "watch" : "stable");
  const pendingCritical = Math.max(0, (s.total_test_cases ?? 0) - runs);
  const lastRun = s.last_run_at;

  let recommendation = t("health.rec.stable");
  if (variant === "empty") recommendation = t("health.rec.init");
  else if (riskLevel === "elevated") recommendation = t("health.rec.regressions");
  else if (passRateValid && passRateNum < 60) recommendation = t("health.rec.quality");

  if (!projectId) {
    return (
      <div className="card dash-health-strip dash-health-strip--empty" style={{ padding: "16px 20px", marginBottom: 20 }}>
        <p style={{ margin: 0, fontSize: 13, color: "var(--text-2)" }}>{t("health.no_project")}</p>
        <Link to="/projects?new=1" className="btn btn-primary btn-sm" style={{ marginTop: 12 }}>{t("health.create_project")}</Link>
      </div>
    );
  }

  if (variant === "empty" && !loading) {
    return (
      <div style={{ marginBottom: 20 }}>
        <InitializeProjectPanel
          projectId={projectId}
          projectName={projectName}
          onDone={onInitialized}
        />
      </div>
    );
  }

  const cells = [
    { label: t("health.general"), value: variant === "good" ? t("health.good") : variant === "danger" ? t("health.at_risk") : t("health.ok") },
    { label: t("health.risk"), value: riskLevel === "elevated" ? t("health.elevated") : riskLevel === "watch" ? t("health.watch") : t("health.stable") },
    { label: t("health.last_run"), value: fmtDate(lastRun) },
    { label: t("health.pending"), value: pendingCritical > 0 ? String(pendingCritical) : "0" },
    { label: t("health.status"), value: runs > 0 ? t("health.active") : t("health.new") },
  ];

  return (
    <div className="card dash-health-strip" style={{ padding: "14px 20px", marginBottom: 20 }}>
      <div style={{ display: "flex", justifyContent: "space-between", gap: 16, flexWrap: "wrap", alignItems: "flex-start" }}>
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(120px, 1fr))", gap: 16, flex: 1 }}>
          {cells.map((c) => (
            <div key={c.label}>
              <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{c.label}</div>
              <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginTop: 4 }}>{loading ? "…" : c.value}</div>
            </div>
          ))}
        </div>
        <div style={{ minWidth: 200, maxWidth: 280 }}>
          <div style={{ fontSize: 10, color: "var(--text-3)", textTransform: "uppercase" }}>{t("health.recommendation")}</div>
          <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 4, lineHeight: 1.45 }}>{recommendation}</div>
          {lastRefresh ? (
            <div style={{ fontSize: 10, color: "var(--text-4)", marginTop: 6 }}>
              {t("dash.refreshed")}: {fmtDate(lastRefresh.toISOString?.() || lastRefresh)}
            </div>
          ) : null}
        </div>
      </div>
    </div>
  );
}
