import React from "react";

export default function ExecutiveRiskCard({ risk }) {
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
      {risk}
    </li>
  );
}
