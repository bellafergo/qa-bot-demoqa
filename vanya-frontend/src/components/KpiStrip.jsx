// src/components/KpiStrip.jsx
/** Compact KPI row for Runs, Evidence, Knowledge pages. */
import React from "react";

export default function KpiStrip({ items = [], loading = false, loadingLabel = "…" }) {
  if (!items.length && !loading) return null;
  return (
    <div
      className="kpi-grid"
      style={{
        marginBottom: 20,
        gridTemplateColumns: "repeat(auto-fit, minmax(130px, 1fr))",
      }}
    >
      {items.map(({ key, label, value, accent, hint }) => (
        <div key={key || label} className="card" style={{ padding: "14px 16px" }}>
          <div style={{ fontSize: 11, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.05em" }}>
            {label}
          </div>
          <div
            style={{
              marginTop: 6,
              fontSize: loading ? 13 : 22,
              fontWeight: loading ? 500 : 700,
              color: loading ? "var(--text-2)" : (accent || "var(--text-1)"),
              fontVariantNumeric: "tabular-nums",
              lineHeight: loading ? 1.4 : 1.2,
            }}
            title={hint || undefined}
          >
            {loading ? loadingLabel : value ?? "—"}
          </div>
          {hint && !loading ? (
            <div style={{ fontSize: 10, color: "var(--text-4)", marginTop: 4, lineHeight: 1.35 }}>{hint}</div>
          ) : null}
        </div>
      ))}
    </div>
  );
}
