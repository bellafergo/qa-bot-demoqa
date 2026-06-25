import React from "react";
import { Skeleton } from "../ui/Skeleton.jsx";

/** `valueExtra` supports MEJORA #1 (benchmark under pass rate) without changing other KPI cards. */
export default function KpiCard({ label, value, sub, accent, icon, valueExtra, loading = false }) {
  if (loading) {
    return (
      <div className="kpi-card" aria-hidden>
        <Skeleton width="55%" height={12} style={{ marginBottom: 14 }} />
        <Skeleton width="42%" height={28} style={{ marginBottom: 10 }} />
        <Skeleton width="70%" height={11} />
      </div>
    );
  }
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
