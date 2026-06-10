import React from "react";

export default function EnvironmentComparisonCard({ comparison, labels }) {
  if (!comparison) return null;

  const deltaColor = comparison.risk_delta < 0 ? "#ef4444" : comparison.risk_delta > 0 ? "#22c55e" : "var(--text-2)";

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ fontWeight: 600, color: "var(--text-1)", marginBottom: 8 }}>{comparison.routeLabel}</div>
      <div style={{ marginBottom: 6 }}>
        <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{labels.riskDeltaLabel}: </span>
        <span style={{ fontWeight: 700, color: deltaColor }}>{comparison.risk_delta > 0 ? "+" : ""}{comparison.risk_delta}</span>
      </div>
      <div style={{ color: "var(--text-2)" }}>{comparison.status_delta}</div>
      {comparison.summary ? (
        <div style={{ color: "var(--text-3)", fontSize: 12, marginTop: 4 }}>{comparison.summary}</div>
      ) : null}
    </li>
  );
}
