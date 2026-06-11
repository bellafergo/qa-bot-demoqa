import React from "react";

export default function BusinessRiskSignalCard({ signal }) {
  if (!signal) return null;

  return (
    <li
      style={{
        padding: "8px 10px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 6,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 12,
        color: "var(--text-2)",
        lineHeight: 1.4,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
        <strong style={{ color: "var(--text-1)" }}>{signal.title}</strong>
        <span className={signal.severityBadgeClass}>{signal.severity}</span>
        {signal.capability ? (
          <span style={{ color: "var(--text-3)" }}>{signal.capability}</span>
        ) : null}
      </div>
    </li>
  );
}
