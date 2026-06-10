import React from "react";
import MultiEnvironmentIntelligenceView from "../incident/MultiEnvironmentIntelligenceView.jsx";

export default function ReleaseReadinessView({ vm }) {
  if (!vm?.show) return null;

  if (vm.empty) {
    return (
      <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {vm.emptyMessage}
      </p>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
        <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)" }}>
          {vm.overallStatusLabel}:
        </span>
        <span className={vm.overallStatusBadgeClass}>{vm.overallStatusText}</span>
      </div>

      {vm.summary ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.summaryLabel}
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
            {vm.summary}
          </p>
        </div>
      ) : null}

      {vm.deploymentRisk ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.deploymentRiskLabel}
          </div>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 6 }}>
            <span className={vm.deploymentRiskBadgeClass}>
              {String(vm.deploymentRisk.risk_level || "unknown").toUpperCase()}
            </span>
            <span style={{ fontSize: 13, color: "var(--text-2)" }}>
              {vm.deploymentRisk.risk_score}/100
            </span>
          </div>
          {vm.deploymentRisk.summary ? (
            <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
              {vm.deploymentRisk.summary}
            </p>
          ) : null}
        </div>
      ) : null}

      {vm.data_gaps?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.dataGapsLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.5 }}>
            {vm.data_gaps.map((gap) => (
              <li key={gap}>{gap}</li>
            ))}
          </ul>
        </div>
      ) : null}

      <div>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
          {vm.scmConnectionsLabel}
        </div>
        <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
          {vm.scm.map((item) => (
            <li
              key={item.provider}
              style={{
                marginBottom: 8,
                padding: "10px 12px",
                background: "var(--bg-3, rgba(255,255,255,0.03))",
                borderRadius: 8,
                fontSize: 13,
              }}
            >
              <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 4 }}>
                <strong style={{ color: "var(--text-1)" }}>{item.title}</strong>
                <span className={item.badgeClass}>{item.label}</span>
              </div>
              {item.detail ? (
                <div style={{ color: "var(--text-3)", fontSize: 12 }}>{item.detail}</div>
              ) : null}
            </li>
          ))}
        </ul>
      </div>

      {vm.integrations?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.integrationReadinessLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none", display: "flex", flexWrap: "wrap", gap: 8 }}>
            {vm.integrations.map((item) => (
              <li key={item.connector_id} className={item.badgeClass} style={{ fontSize: 12, padding: "4px 8px" }}>
                {item.label}
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {vm.decisionCenterVm?.show && !vm.decisionCenterVm.empty ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.decisionCenterVm.title}
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
            {vm.decisionCenterVm.center?.executive_summary}
          </p>
        </div>
      ) : null}

      {vm.qualityHealthVm?.show && !vm.qualityHealthVm.empty ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.qualityHealthVm.title}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)" }}>
            {vm.qualityHealthVm.report?.overall_score ?? "—"}
            {" "}
            — {vm.qualityHealthVm.report?.overallStatusLabel || "—"}
          </div>
        </div>
      ) : null}

      {vm.multiEnvironmentVm?.show && !vm.multiEnvironmentVm.empty ? (
        <MultiEnvironmentIntelligenceView vm={vm.multiEnvironmentVm} />
      ) : null}
    </div>
  );
}
