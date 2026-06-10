import React from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";

export default function DependencyNodeCard({ node, riskLevelLabel, previewLabel, onPreview }) {
  return (
    <li
      style={{
        marginBottom: 8,
        padding: "10px 12px",
        borderRadius: 8,
        border: `2px solid ${node.riskColor}`,
        background: "var(--bg-2)",
        fontSize: 12,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 4 }}>
        <strong style={{ color: "var(--text-1)" }}>{node.name}</strong>
        <span className="badge badge-gray">{node.node_type}</span>
        <span className={node.riskBadgeClass}>
          {riskLevelLabel}: {node.risk_level}
        </span>
      </div>
      {node.description ? (
        <div style={{ color: "var(--text-2)", marginBottom: 6 }}>{node.description}</div>
      ) : null}
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
        {node.drilldownItem ? <EvidenceCorrelationDrilldownCell item={node.drilldownItem} /> : null}
        <button type="button" className="btn btn-secondary btn-sm" onClick={() => onPreview(node)}>
          {previewLabel}
        </button>
      </div>
    </li>
  );
}
