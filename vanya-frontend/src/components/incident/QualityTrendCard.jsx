import React from "react";

export default function QualityTrendCard({ trend, labels }) {
  if (!trend) return null;

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: `1px solid ${trend.trendColor}`,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)" }}>{trend.scope_name}</strong>
        <span className={trend.trendBadgeClass}>{trend.trendDirectionLabel}</span>
        <span style={{ fontWeight: 700, color: trend.trendColor }}>
          {labels.scoreChangeLabel}: {trend.scoreChangeText}
        </span>
      </div>
      <div style={{ display: "flex", gap: 12, alignItems: "center", flexWrap: "wrap" }}>
        {trend.sparklinePath ? (
          <svg width="120" height="36" viewBox="0 0 120 36" aria-hidden="true">
            <path d={trend.sparklinePath} fill="none" stroke={trend.trendColor} strokeWidth="2" />
          </svg>
        ) : null}
        <div style={{ fontSize: 12, color: "var(--text-2)" }}>
          <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{labels.historicalPointsLabel}: </span>
          {trend.pointsText}
        </div>
      </div>
    </li>
  );
}
