import React from "react";

export default function SecurityReadinessCard({ vm }) {
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
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.authenticationLabel}</span>
          <span className="badge badge-blue">{vm.authenticationMethod}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.ssoReadinessLabel}</span>
          <span className={vm.ssoReadinessBadgeClass}>{vm.ssoReadinessText}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.rbacLabel}</span>
          <span className={vm.rbacBadgeClass}>{vm.rbacText}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.auditLabel}</span>
          <span className={vm.auditBadgeClass}>{vm.auditText}</span>
        </div>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap" }}>
          <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.securityScoreLabel}</span>
          <span className={vm.securityScoreBadgeClass}>{vm.securityScore}/100</span>
        </div>
      </div>
      {vm.summary ? (
        <p style={{ margin: "12px 0 0", fontSize: 12, color: "var(--text-2)" }}>{vm.summary}</p>
      ) : null}
    </div>
  );
}
