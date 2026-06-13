import React, { useState } from "react";
import ExplainabilityTracePanel from "../explainability/ExplainabilityTracePanel.jsx";

export default function BusinessRiskCard({ risk }) {
  const [traceOpen, setTraceOpen] = useState(false);
  if (!risk) return null;

  return (
    <div
      style={{
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ fontSize: 14, color: "var(--text-1)" }}>{risk.capability}</strong>
        <span className={risk.severityBadgeClass}>{risk.severity}</span>
        <span className={risk.confidenceBadgeClass}>{risk.confidenceLabel}</span>
      </div>
      <p style={{ fontSize: 13, color: "var(--text-2)", margin: "0 0 8px", lineHeight: 1.5 }}>
        {risk.summary}
      </p>
      {risk.showTrace ? (
        <>
          <button
            type="button"
            onClick={() => setTraceOpen((open) => !open)}
            style={{
              display: "flex",
              alignItems: "center",
              gap: 6,
              background: "none",
              border: "none",
              padding: 0,
              cursor: "pointer",
              fontSize: 11,
              fontWeight: 600,
              color: "var(--text-3)",
              textTransform: "uppercase",
              letterSpacing: "0.05em",
              marginBottom: traceOpen ? 0 : 4,
            }}
          >
            <span>{traceOpen ? "▾" : "▸"}</span>
            <span>{traceOpen ? risk.traceToggleHideLabel : risk.traceToggleShowLabel}</span>
          </button>
          {traceOpen ? <ExplainabilityTracePanel trace={risk.trace} /> : null}
        </>
      ) : null}
    </div>
  );
}
