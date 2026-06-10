import React from "react";
import DegradationSignalCard from "./DegradationSignalCard.jsx";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";

export default function DegradationAssessmentCard({ assessment, labels }) {
  if (!assessment) return null;
  const signal = assessment.signals?.[0];

  return (
    <li
      style={{
        marginBottom: 10,
        padding: "12px 14px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: `1px solid ${assessment.statusColor}`,
        fontSize: 13,
        lineHeight: 1.5,
      }}
    >
      <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center", marginBottom: 8 }}>
        <strong style={{ color: "var(--text-1)" }}>{assessment.scope_name}</strong>
        <span className={assessment.statusBadgeClass}>{assessment.statusLabel}</span>
        <span className={assessment.projectionBadgeClass}>
          {labels?.riskProjectionLabel}: {assessment.projectionLabel}
        </span>
        <span className="badge badge-blue">
          {labels?.confidenceLabel}: {assessment.confidenceText}
        </span>
      </div>

      <DegradationSignalCard
        signal={signal}
        labels={{
          scoreLabel: labels?.scoreLabel,
          scoreRangeText: assessment.scoreRangeText,
          trendIndicator: assessment.trendIndicator,
          statusColor: assessment.statusColor,
          severityBadgeClass:
            signal?.severity === "CRITICAL" || signal?.severity === "HIGH"
              ? "badge badge-red"
              : signal?.severity === "MEDIUM"
                ? "badge badge-orange"
                : "badge badge-gray",
        }}
      />

      {assessment.recommendedAttention?.length ? (
        <div style={{ marginTop: 10 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {labels?.recommendedAttentionLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, color: "var(--text-2)", fontSize: 12 }}>
            {assessment.recommendedAttention.map((item) => (
              <li key={item.label}>{item.label}</li>
            ))}
          </ul>
        </div>
      ) : null}

      {assessment.drilldownItems?.length ? (
        <div style={{ display: "grid", gap: 8, marginTop: 10 }}>
          {assessment.drilldownItems.map((item) => (
            <EvidenceCorrelationDrilldownCell key={`${item.related_entity_type}:${item.related_entity_id}`} item={item} />
          ))}
        </div>
      ) : null}
    </li>
  );
}
