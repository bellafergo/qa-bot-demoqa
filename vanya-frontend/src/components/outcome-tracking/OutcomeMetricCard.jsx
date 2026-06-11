import React from "react";

export default function OutcomeMetricCard({ metric }) {
  if (!metric) return null;

  return (
    <div
      style={{
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        minHeight: 88,
      }}
    >
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
        {metric.title}
      </div>
      <div style={{ fontSize: 24, fontWeight: 700, color: "var(--text-1)", lineHeight: 1.2 }}>
        {metric.displayValue}
      </div>
    </div>
  );
}
