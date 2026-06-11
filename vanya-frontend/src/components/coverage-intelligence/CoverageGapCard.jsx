import React from "react";

export default function CoverageGapCard({ gap }) {
  if (!gap) return null;

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
        <span style={{ fontWeight: 600 }}>{gap.capability}</span>
        <span className={gap.severityBadgeClass}>{gap.severityLabel}</span>
      </div>
      {gap.module ? (
        <div style={{ color: "var(--text-3)", marginBottom: 4, fontFamily: "monospace", fontSize: 11 }}>
          {gap.module}
        </div>
      ) : null}
      <div style={{ color: "var(--text-2)", lineHeight: 1.4 }}>{gap.reason}</div>
    </div>
  );
}
