import React from "react";

export default function JiraIssueCard({ vm }) {
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
        <span style={{ fontFamily: "monospace", fontWeight: 600 }}>{vm.issueKey}</span>
        <span className="badge badge-blue">{vm.issueType}</span>
      </div>
      <div style={{ marginTop: 6, fontWeight: 500 }}>{vm.summary}</div>
      <div style={{ display: "flex", gap: 12, flexWrap: "wrap", marginTop: 8, color: "var(--text-3)" }}>
        <span>{vm.status}</span>
        <span>{vm.assignee}</span>
        <span>{vm.priority}</span>
      </div>
    </div>
  );
}
