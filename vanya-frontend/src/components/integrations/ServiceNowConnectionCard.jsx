import React from "react";

export default function ServiceNowConnectionCard({ vm }) {
  if (!vm) return null;

  return (
    <div
      style={{
        padding: "14px 16px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
        marginBottom: 14,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap", marginBottom: 10 }}>
        <span style={{ fontWeight: 600 }}>{vm.title}</span>
        <span className={vm.connectedBadgeClass}>{vm.connectedLabel}</span>
      </div>

      {vm.showEmptyConnection ? (
        <p style={{ margin: 0, color: "var(--text-2)", fontSize: 12 }}>{vm.emptyConnectionText}</p>
      ) : (
        <>
          <div style={{ display: "grid", gap: 8, marginBottom: 12 }}>
            <div style={{ display: "flex", justifyContent: "space-between", gap: 12 }}>
              <span style={{ color: "var(--text-3)" }}>{vm.instanceUrlLabel}</span>
              <span style={{ fontFamily: "monospace", fontSize: 12 }}>{vm.instanceUrl}</span>
            </div>
            <div style={{ display: "flex", justifyContent: "space-between", gap: 12 }}>
              <span style={{ color: "var(--text-3)" }}>{vm.usernameLabel}</span>
              <span style={{ fontSize: 12 }}>{vm.username}</span>
            </div>
            {vm.lastSync ? (
              <div style={{ display: "flex", justifyContent: "space-between", gap: 12 }}>
                <span style={{ color: "var(--text-3)" }}>{vm.lastSyncLabel}</span>
                <span style={{ fontSize: 12 }}>{new Date(vm.lastSync).toLocaleString()}</span>
              </div>
            ) : null}
          </div>
          <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(90px, 1fr))", gap: 10 }}>
            {vm.counts.map((item) => (
              <div key={item.label} style={{ textAlign: "center", padding: "8px 6px", borderRadius: 6, background: "var(--bg-2)" }}>
                <div style={{ fontSize: 18, fontWeight: 700 }}>{item.value}</div>
                <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{item.label}</div>
              </div>
            ))}
          </div>
        </>
      )}

      {vm.readOnlyNote ? (
        <p style={{ margin: "12px 0 0", fontSize: 11, color: "var(--text-3)" }}>{vm.readOnlyNote}</p>
      ) : null}
    </div>
  );
}
