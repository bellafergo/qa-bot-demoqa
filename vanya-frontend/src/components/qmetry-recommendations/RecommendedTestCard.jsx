import React from "react";

export default function RecommendedTestCard({ test }) {
  if (!test) return null;

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
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
        <span style={{ fontWeight: 600, fontSize: 13 }}>{test.testCaseName}</span>
        <span className={test.priorityBadgeClass}>{test.priorityLabel}</span>
      </div>
      <div style={{ color: "var(--text-3)", marginBottom: 4 }}>{test.testCaseId}</div>
      <div style={{ color: "var(--text-2)", lineHeight: 1.45 }}>
        <span style={{ fontWeight: 600 }}>{test.reasonLabel}:</span> {test.reason}
      </div>
    </div>
  );
}
