import React from "react";

export default function RBACReadinessCard({ vm }) {
  if (!vm) return null;

  return (
    <div
      style={{
        padding: "14px 16px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "grid", gap: 10 }}>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.statusLabel}</span>
          <span className={vm.statusBadgeClass}>{vm.statusText}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.defaultRolesLabel}</span>
          <span style={{ color: "var(--text-1)", fontWeight: 600 }}>{vm.defaultRolesCount}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.permissionsLabel}</span>
          <span style={{ color: "var(--text-1)", fontWeight: 600 }}>{vm.permissionsCount}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.enforcementLabel}</span>
          <span className={vm.enforcementBadgeClass}>{vm.enforcementText}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.readinessLabel}</span>
          <span className={vm.readinessBadgeClass}>{vm.readinessScore}%</span>
        </div>
      </div>
      {vm.summary ? (
        <p style={{ margin: "12px 0 0", fontSize: 12, color: "var(--text-2)" }}>{vm.summary}</p>
      ) : null}
    </div>
  );
}
