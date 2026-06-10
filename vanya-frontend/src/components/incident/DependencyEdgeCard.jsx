import React from "react";

export default function DependencyEdgeCard({ edge }) {
  return (
    <li
      style={{
        marginBottom: 6,
        padding: "8px 10px",
        borderRadius: 6,
        background: "var(--bg-1, rgba(0,0,0,0.12))",
        fontSize: 12,
        color: "var(--text-2)",
      }}
    >
      <span style={{ color: "var(--text-1)", fontWeight: 600 }}>{edge.sourceName}</span>
      {" "}
      <span style={{ color: "var(--text-3)" }}>{edge.relationship_type}</span>
      {" "}
      <span style={{ color: "var(--text-1)", fontWeight: 600 }}>{edge.targetName}</span>
    </li>
  );
}
