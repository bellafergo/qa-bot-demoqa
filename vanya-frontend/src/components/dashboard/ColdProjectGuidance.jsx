import React from "react";
import { Link } from "react-router-dom";

export default function ColdProjectGuidance({ vm }) {
  if (!vm?.show) return null;

  return (
    <div
      style={{
        padding: "14px 16px",
        borderRadius: 8,
        border: "1px dashed var(--border, rgba(255,255,255,0.12))",
        background: "var(--bg-3, rgba(255,255,255,0.02))",
        display: "flex",
        flexDirection: "column",
        gap: 10,
      }}
    >
      <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
        {vm.message}
      </p>
      <div style={{ display: "flex", gap: 12, flexWrap: "wrap" }}>
        {vm.actions.map((action) => (
          <Link
            key={action.path}
            to={action.path}
            style={{ fontSize: 13, color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}
          >
            {action.label} →
          </Link>
        ))}
      </div>
    </div>
  );
}
