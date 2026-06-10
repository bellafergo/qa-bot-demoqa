import React from "react";

export default function JiraProjectCard({ vm }) {
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
      <div style={{ fontWeight: 600, fontSize: 13 }}>{vm.projectName}</div>
      <div style={{ color: "var(--text-3)", marginTop: 4, fontFamily: "monospace" }}>{vm.subtitle}</div>
    </div>
  );
}
