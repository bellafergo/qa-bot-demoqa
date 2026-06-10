import React from "react";

export default function EnterpriseModuleCard({ module }) {
  if (!module) return null;

  return (
    <li
      style={{
        marginBottom: 6,
        padding: "8px 10px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 6,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 12,
        lineHeight: 1.45,
        display: "flex",
        gap: 8,
        flexWrap: "wrap",
        alignItems: "center",
      }}
    >
      <strong style={{ color: "var(--text-1)" }}>{module.module_name}</strong>
      {module.category ? <span style={{ color: "var(--text-3)" }}>{module.category}</span> : null}
      <span className={module.criticalityBadgeClass}>{module.criticality}</span>
    </li>
  );
}
