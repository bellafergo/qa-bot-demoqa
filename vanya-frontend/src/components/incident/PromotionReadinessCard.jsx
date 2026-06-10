import React from "react";

export default function PromotionReadinessCard({ readiness, labels }) {
  if (!readiness) return null;

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)" }}>{readiness.routeLabel}</strong>
        <span className={readiness.statusBadgeClass}>{readiness.readiness_status}</span>
      </div>
      <div style={{ marginBottom: 8 }}>
        <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{labels.readinessScoreLabel}: </span>
        <span style={{ fontWeight: 700, color: "var(--text-1)" }}>{readiness.readiness_score}</span>
      </div>
      {readiness.blockers?.length ? (
        <div style={{ marginBottom: 8 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.blockersLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18 }}>
            {readiness.blockers.map((blocker) => (
              <li key={blocker} style={{ color: "var(--text-2)" }}>{blocker}</li>
            ))}
          </ul>
        </div>
      ) : null}
      {readiness.warnings?.length ? (
        <div style={{ marginBottom: 8 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.warningsLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18 }}>
            {readiness.warnings.map((warning) => (
              <li key={warning} style={{ color: "var(--text-2)" }}>{warning}</li>
            ))}
          </ul>
        </div>
      ) : null}
      {readiness.recommended_validations?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels.recommendedValidationsLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18 }}>
            {readiness.recommended_validations.map((validation) => (
              <li key={validation} style={{ color: "var(--text-2)" }}>{validation}</li>
            ))}
          </ul>
        </div>
      ) : null}
    </li>
  );
}
