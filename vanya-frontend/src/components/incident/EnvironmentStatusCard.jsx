import React from "react";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";

export default function EnvironmentStatusCard({ environment, labels }) {
  if (!environment) return null;

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
        <strong style={{ color: "var(--text-1)", fontSize: 14 }}>{environment.name}</strong>
        <span className={environment.statusBadgeClass}>
          {labels.statusLabel}: {environment.status}
        </span>
        <span className="badge badge-blue">
          {labels.signalsLabel}: {environment.signalCount}
        </span>
      </div>
      {environment.signals?.length ? (
        <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
          {environment.signals.map((signal) => (
            <li
              key={signal.signal_id}
              style={{
                marginBottom: 8,
                padding: "8px 10px",
                background: "var(--bg-2)",
                borderRadius: 6,
              }}
            >
              <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 4 }}>
                <span className={signal.severityBadgeClass}>{signal.severity}</span>
                <span className="badge badge-gray">{signal.signal_type}</span>
              </div>
              <div style={{ color: "var(--text-1)", fontWeight: 600, marginBottom: 2 }}>{signal.title}</div>
              {signal.description ? (
                <div style={{ color: "var(--text-3)", fontSize: 12, marginBottom: signal.drilldownItem ? 6 : 0 }}>
                  {signal.description}
                </div>
              ) : null}
              {signal.drilldownItem ? (
                <EvidenceCorrelationDrilldownCell item={signal.drilldownItem} />
              ) : null}
            </li>
          ))}
        </ul>
      ) : null}
    </li>
  );
}
