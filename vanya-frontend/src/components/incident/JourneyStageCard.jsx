import React from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";

export default function JourneyStageCard({ stage, stageStatusLabel, isLast }) {
  return (
    <div style={{ display: "flex", alignItems: "stretch", gap: 0, flex: "1 1 0", minWidth: 100 }}>
      <div
        style={{
          flex: 1,
          padding: "10px 12px",
          borderRadius: 8,
          border: `2px solid ${stage.statusColor}`,
          background: "var(--bg-2)",
          fontSize: 12,
          lineHeight: 1.45,
        }}
      >
        <div style={{ fontWeight: 600, color: "var(--text-1)", marginBottom: 4 }}>{stage.name}</div>
        <div style={{ marginBottom: 6 }}>
          <span className={stage.statusBadgeClass}>
            {stageStatusLabel}: {stage.status}
          </span>
        </div>
        {stage.drilldownItems?.length ? (
          <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
            {stage.drilldownItems.map((item) => (
              <EvidenceCorrelationDrilldownCell
                key={`${item.related_entity_type}:${item.related_entity_id}`}
                item={item}
              />
            ))}
          </div>
        ) : null}
      </div>
      {!isLast ? (
        <div
          aria-hidden="true"
          style={{
            display: "flex",
            alignItems: "center",
            padding: "0 4px",
            color: "var(--text-3)",
            fontSize: 16,
            fontWeight: 600,
          }}
        >
          ↓
        </div>
      ) : null}
    </div>
  );
}
