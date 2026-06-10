import React from "react";

export default function SecurityProviderCard({ provider }) {
  if (!provider) return null;

  return (
    <li
      style={{
        marginBottom: 8,
        padding: "10px 12px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
        display: "flex",
        justifyContent: "space-between",
        gap: 12,
        alignItems: "center",
        flexWrap: "wrap",
      }}
    >
      <div>
        <div style={{ fontWeight: 600, color: "var(--text-1)" }}>{provider.provider_name}</div>
        <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 2 }}>{provider.provider_type}</div>
      </div>
      <span className={provider.statusBadgeClass}>{provider.statusLabel}</span>
    </li>
  );
}
