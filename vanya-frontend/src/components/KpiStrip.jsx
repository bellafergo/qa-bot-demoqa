// src/components/KpiStrip.jsx
/** Compact KPI row for Runs, Evidence, Knowledge pages. */
import React from "react";

export default function KpiStrip({ items = [], loading = false }) {
  if (!items.length && !loading) return null;
  return (
    <div
      className="kpi-grid"
      style={{
        marginBottom: 20,
        gridTemplateColumns: "repeat(auto-fit, minmax(130px, 1fr))",
      }}
    >
      {items.map(({ key, label, value, accent }) => (
        <div key={key || label} className="card" style={{ padding: "14px 16px" }}>
          <div style={{ fontSize: 11, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em" }}>
            {label}
          </div>
          <div
            style={{
              marginTop: 6,
              fontSize: 22,
              fontWeight: 700,
              color: accent || "var(--text-1)",
              fontVariantNumeric: "tabular-nums",
            }}
          >
            {loading ? "…" : value ?? "—"}
          </div>
        </div>
      ))}
    </div>
  );
}
