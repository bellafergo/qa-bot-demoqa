import React from "react";
import ProjectReadinessBadge from "./ProjectReadinessBadge.jsx";

export default function OnboardingProgressCard({ checklist, labels, showCoreSetupComplete = false }) {
  if (!checklist) return null;

  const pct = Math.max(0, Math.min(100, Number(checklist.overall_completion) || 0));

  return (
    <div
      style={{
        padding: "16px 18px",
        background: "var(--bg-2)",
        borderRadius: 10,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        marginBottom: 14,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap", alignItems: "center", marginBottom: 12 }}>
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.projectReadinessLabel}
          </div>
          <div style={{ display: "flex", gap: 10, alignItems: "baseline", flexWrap: "wrap" }}>
            <span style={{ fontSize: 32, fontWeight: 800, color: "var(--text-1)", lineHeight: 1 }}>
              {pct}%
            </span>
            <ProjectReadinessBadge label={checklist.readinessLabel} badgeClass={checklist.readinessBadgeClass} />
            {showCoreSetupComplete && labels.coreSetupCompleteLabel ? (
              <span className="badge badge-green" style={{ fontSize: 11 }}>
                {labels.coreSetupCompleteLabel}
              </span>
            ) : null}
          </div>
          {showCoreSetupComplete && labels.platformOperationalNote ? (
            <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.5, marginTop: 8, maxWidth: 420 }}>
              {labels.platformOperationalNote}
            </div>
          ) : null}
        </div>
        <div style={{ minWidth: 180 }}>
          <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 4 }}>{labels.completionLabel}</div>
          <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)" }}>{checklist.completedLabel}</div>
        </div>
      </div>

      <div
        style={{
          height: 8,
          borderRadius: 999,
          background: "var(--bg-3, rgba(255,255,255,0.06))",
          overflow: "hidden",
          marginBottom: 12,
        }}
      >
        <div
          style={{
            width: `${pct}%`,
            height: "100%",
            background: "linear-gradient(90deg, #3b82f6, #22c55e)",
            transition: "width 0.3s ease",
          }}
        />
      </div>

      <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.5 }}>
        <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{labels.nextRecommendedStepLabel}: </span>
        {checklist.next_recommended_step || "—"}
      </div>
    </div>
  );
}
