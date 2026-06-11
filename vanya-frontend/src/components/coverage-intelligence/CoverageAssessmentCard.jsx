import React from "react";

export default function CoverageAssessmentCard({ assessment }) {
  if (!assessment) return null;

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
        <span style={{ fontWeight: 600, fontSize: 13 }}>{assessment.capability}</span>
        <span className={assessment.statusBadgeClass}>{assessment.statusLabel}</span>
      </div>
      <div style={{ color: "var(--text-3)" }}>
        {assessment.matchedTestsLabel}: {assessment.matchedTests}
      </div>
    </div>
  );
}
