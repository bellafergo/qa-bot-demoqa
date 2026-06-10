import React from "react";
import EnterpriseModuleCard from "./EnterpriseModuleCard.jsx";

export default function EnterpriseSystemConnectorCard({ connector, labels }) {
  if (!connector) return null;

  return (
    <li
      style={{
        marginBottom: 12,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)" }}>{connector.system_name}</strong>
        <span className="badge badge-blue">{connector.systemTypeLabel}</span>
        <span className="badge badge-gray">{connector.environment}</span>
        <span className={connector.statusBadgeClass}>{connector.status}</span>
      </div>
      {connector.modules?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {labels?.modulesLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {connector.modules.map((module) => (
              <EnterpriseModuleCard key={module.module_id} module={module} />
            ))}
          </ul>
        </div>
      ) : null}
    </li>
  );
}
