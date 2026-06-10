import React from "react";

export default function DegradationSignalCard({ signal, labels }) {
  if (!signal) return null;

  return (
    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.5 }}>
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
        <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{labels?.scoreLabel}: </span>
        <span style={{ fontWeight: 700, color: "var(--text-1)" }}>{labels?.scoreRangeText}</span>
        <span style={{ color: labels?.statusColor }} aria-hidden="true">{labels?.trendIndicator}</span>
        <span className={labels?.severityBadgeClass}>{signal.severity}</span>
      </div>
      {signal.summary ? (
        <div style={{ marginTop: 6 }}>{signal.summary}</div>
      ) : null}
    </div>
  );
}
