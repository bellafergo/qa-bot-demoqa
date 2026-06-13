import React from "react";

export default function DashboardCommandHeader({ vm }) {
  if (!vm) return null;

  return (
    <div
      className="card"
      style={{
        padding: "14px 20px",
        marginBottom: 16,
        display: "grid",
        gridTemplateColumns: "repeat(auto-fit, minmax(160px, 1fr))",
        gap: "12px 20px",
      }}
    >
      <div>
        <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
          {vm.projectLabel}
        </div>
        <div style={{ fontSize: 14, fontWeight: 700, color: "var(--text-1)", marginTop: 4 }}>{vm.projectName}</div>
      </div>
      <div>
        <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
          {vm.environmentLabel}
        </div>
        <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginTop: 4 }}>{vm.environment}</div>
      </div>
      <div>
        <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
          {vm.lastUpdateLabel}
        </div>
        <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginTop: 4 }}>{vm.lastUpdate}</div>
      </div>
      <div>
        <div style={{ fontSize: 10, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
          {vm.globalStatusLabel}
        </div>
        <div style={{ fontSize: 14, fontWeight: 700, color: "var(--text-1)", marginTop: 4 }}>{vm.globalStatus}</div>
      </div>
    </div>
  );
}
