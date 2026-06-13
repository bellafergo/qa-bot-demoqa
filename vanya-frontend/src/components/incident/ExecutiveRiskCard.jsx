import React from "react";
import IncidentInsightTracePanel from "../incidents/IncidentInsightTracePanel.jsx";

function severityBadgeClass(severity) {
  const key = String(severity || "").toUpperCase();
  if (["CRITICAL", "BROKEN", "RED", "HIGH"].includes(key)) return "badge badge-red";
  if (["WARNING", "ORANGE", "YELLOW", "DEGRADED", "MEDIUM"].includes(key)) return "badge badge-orange";
  if (["ONLINE", "GREEN", "LOW", "HEALTHY"].includes(key)) return "badge badge-green";
  return "badge badge-gray";
}

export default function ExecutiveRiskCard({ risk, trace, severity }) {
  return (
    <li
      style={{
        marginBottom: 8,
        padding: "10px 12px",
        borderRadius: 8,
        background: "var(--bg-2)",
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 13,
        color: "var(--text-2)",
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "flex-start", justifyContent: "space-between" }}>
        <span style={{ color: "var(--text-1)", fontWeight: 600 }}>{risk}</span>
        {severity ? (
          <span className={severityBadgeClass(severity)} style={{ fontSize: 10, flexShrink: 0 }}>
            {severity}
          </span>
        ) : null}
      </div>
      {trace ? <IncidentInsightTracePanel trace={trace} /> : null}
    </li>
  );
}
