import React from "react";

export default function QMetryTestCaseCard({ vm }) {
  if (!vm) return null;

  return (
    <div
      style={{
        padding: "10px 12px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-2)",
        fontSize: 12,
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap" }}>
        <span style={{ fontFamily: "monospace", fontWeight: 600 }}>{vm.testCaseId}</span>
        <span className="badge badge-blue">{vm.status}</span>
      </div>
      <div style={{ marginTop: 6, fontWeight: 500 }}>{vm.name}</div>
      <div style={{ marginTop: 8, color: "var(--text-3)" }}>{vm.priority}</div>
    </div>
  );
}
