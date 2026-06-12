import React from "react";

export default function ServiceNowCorrelationCard({ vm }) {
  if (!vm) return null;

  return (
    <li
      style={{
        listStyle: "none",
        padding: "10px 12px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-2)",
        marginBottom: 8,
        fontSize: 12,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
        <span style={{ fontFamily: "monospace", fontWeight: 600 }}>{vm.entityId}</span>
        <span className={vm.confidenceBadgeClass}>{vm.confidence}</span>
      </div>
      <div style={{ color: "var(--text-2)", marginBottom: 4 }}>
        {vm.capabilityLabel}: {vm.capability}
      </div>
      <div style={{ color: "var(--text-3)", fontSize: 11 }}>{vm.reason}</div>
    </li>
  );
}
