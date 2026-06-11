import React from "react";
import RecommendedTestCard from "./RecommendedTestCard.jsx";

export default function RecommendationGroupCard({ group }) {
  if (!group) return null;

  return (
    <div
      style={{
        padding: "12px 14px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-1)",
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
        <span style={{ fontWeight: 600, fontSize: 14 }}>{group.capability}</span>
        <span className={group.topPriorityBadgeClass}>{group.topPriorityLabel}</span>
      </div>
      <div style={{ display: "grid", gap: 8 }}>
        {group.tests.map((test) => (
          <RecommendedTestCard key={test.testCaseId} test={test} />
        ))}
      </div>
    </div>
  );
}
