import React from "react";

export default function BusinessRiskCard({ risk }) {
  if (!risk) return null;

  return (
    <div
      style={{
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ fontSize: 14, color: "var(--text-1)" }}>{risk.capability}</strong>
        <span className={risk.severityBadgeClass}>{risk.severity}</span>
        <span className={risk.confidenceBadgeClass}>{risk.confidenceLabel}</span>
      </div>
      <p style={{ fontSize: 13, color: "var(--text-2)", margin: "0 0 8px", lineHeight: 1.5 }}>
        {risk.summary}
      </p>
      {risk.evidence?.length ? (
        <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
          {risk.evidence.map((item) => (
            <li key={item}>{item}</li>
          ))}
        </ul>
      ) : null}
    </div>
  );
}
