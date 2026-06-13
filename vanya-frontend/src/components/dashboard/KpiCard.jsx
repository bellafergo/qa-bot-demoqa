import React from "react";

/** `valueExtra` supports MEJORA #1 (benchmark under pass rate) without changing other KPI cards. */
export default function KpiCard({ label, value, sub, accent, icon, valueExtra }) {
  return (
    <div className="kpi-card">
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start" }}>
        <div className="kpi-label">{label}</div>
        {icon && <span style={{ fontSize: 16, opacity: 0.55 }}>{icon}</span>}
      </div>
      <div className="kpi-value" style={accent ? { color: accent } : {}}>
        {value ?? "—"}
      </div>
      {valueExtra}
      {sub && <div className="kpi-sub">{sub}</div>}
    </div>
  );
}
