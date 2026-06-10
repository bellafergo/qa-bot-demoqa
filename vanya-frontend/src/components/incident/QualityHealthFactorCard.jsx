import React from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";

export default function QualityHealthFactorCard({ factor }) {
  if (!factor) return null;

  return (
    <li
      style={{
        marginBottom: 6,
        padding: "8px 10px",
        background: "var(--bg-2)",
        borderRadius: 6,
        fontSize: 12,
        lineHeight: 1.5,
      }}
    >
      <div style={{ color: "var(--text-1)", fontWeight: 600 }}>{factor.title}</div>
      {factor.description ? (
        <div style={{ color: "var(--text-3)", marginTop: 2 }}>{factor.description}</div>
      ) : null}
      {factor.drilldownItem ? (
        <div style={{ marginTop: 6 }}>
          <EvidenceCorrelationDrilldownCell item={factor.drilldownItem} />
        </div>
      ) : null}
    </li>
  );
}
