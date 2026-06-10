import React from "react";
import QualityHealthFactorCard from "./QualityHealthFactorCard.jsx";

export default function QualityHealthScoreCard({ score, labels, compact = false }) {
  if (!score) return null;

  return (
    <li
      style={{
        marginBottom: 10,
        padding: compact ? "10px 12px" : "14px 16px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: `1px solid ${score.statusColor}`,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)", fontSize: compact ? 13 : 14 }}>{score.scope_name}</strong>
        <span style={{ fontWeight: 700, color: score.statusColor }}>
          {score.score} / 100
        </span>
        <span className={score.statusBadgeClass}>{score.statusLabel}</span>
        <span className="badge badge-gray">
          {labels.trendLabel}: {score.trendLabel}
        </span>
      </div>
      {score.factors?.length ? (
        <div>
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.contributingFactorsLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {score.factors.map((factor) => (
              <QualityHealthFactorCard key={factor.factor_id} factor={factor} />
            ))}
          </ul>
        </div>
      ) : null}
    </li>
  );
}
