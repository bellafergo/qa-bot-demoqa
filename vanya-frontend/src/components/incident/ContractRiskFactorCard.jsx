import React from "react";

export default function ContractRiskFactorCard({ factor, severityLabel }) {
  return (
    <li
      style={{
        marginBottom: 8,
        padding: "10px 12px",
        borderRadius: 8,
        background: "var(--bg-1, rgba(0,0,0,0.12))",
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 12,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 4 }}>
        <strong style={{ color: "var(--text-1)" }}>{factor.title}</strong>
        <span className="badge badge-orange">
          {severityLabel}: {factor.severity}
        </span>
        {factor.weight > 0 ? (
          <span className="badge badge-blue">+{factor.weight}</span>
        ) : null}
      </div>
      <div style={{ color: "var(--text-2)" }}>{factor.description}</div>
    </li>
  );
}
