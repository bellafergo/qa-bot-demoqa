import React from "react";

export default function ImpactMetricCard({ metric }) {
  if (!metric) return null;

  return (
    <div
      style={{
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        minHeight: 96,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, marginBottom: 6 }}>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)" }}>
          {metric.title}
        </div>
        <span className={metric.directionBadgeClass} title={metric.directionLabel}>
          {metric.directionIndicator} {metric.directionLabel}
        </span>
      </div>
      <div style={{ fontSize: 22, fontWeight: 700, color: "var(--text-1)", lineHeight: 1.2 }}>
        {metric.currentDisplay}
      </div>
      <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 6, lineHeight: 1.4 }}>
        {metric.previousLabel}: {metric.previousDisplay}
        {metric.hasDelta ? (
          <span style={{ marginLeft: 8, color: metric.deltaColor }}>
            ({metric.deltaDisplay})
          </span>
        ) : null}
      </div>
    </div>
  );
}
