import React from "react";
import ProjectReadinessBadge from "./ProjectReadinessBadge.jsx";

export default function OnboardingOperationalCard({ operational, projectName, expanded, onToggleDetails }) {
  if (!operational) return null;

  return (
    <div>
      <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap", alignItems: "flex-start" }}>
        <div style={{ flex: "1 1 280px" }}>
          <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 6 }}>
            <span style={{ fontSize: 18, lineHeight: 1, color: "#22c55e" }} aria-hidden>
              ✓
            </span>
            <div style={{ fontSize: 16, fontWeight: 700, color: "var(--text-1)" }}>{operational.title}</div>
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, maxWidth: 560 }}>{operational.subtitle}</div>
        </div>
        <button type="button" className="btn btn-secondary btn-sm" onClick={onToggleDetails}>
          {expanded ? operational.hideSetupDetailsLabel : operational.viewSetupDetailsLabel}
        </button>
      </div>

      <dl
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(auto-fit, minmax(160px, 1fr))",
          gap: "12px 20px",
          margin: "16px 0 0",
          padding: "14px 16px",
          background: "var(--bg-2)",
          borderRadius: 10,
          border: "1px solid var(--border, rgba(255,255,255,0.08))",
        }}
      >
        <div>
          <dt style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>{operational.projectLabel}</dt>
          <dd style={{ margin: 0, fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>{projectName || "—"}</dd>
        </div>
        <div>
          <dt style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>{operational.stepsCompletedLabel}</dt>
          <dd style={{ margin: 0, fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>{operational.stepsCompletedText}</dd>
        </div>
        <div>
          <dt style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>{operational.readinessLabel}</dt>
          <dd style={{ margin: 0 }}>
            <ProjectReadinessBadge label={operational.readinessLevelLabel} badgeClass={operational.readinessBadgeClass} />
          </dd>
        </div>
        <div>
          <dt style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>{operational.completionDateLabel}</dt>
          <dd style={{ margin: 0, fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>{operational.completionDateText}</dd>
        </div>
      </dl>
    </div>
  );
}
