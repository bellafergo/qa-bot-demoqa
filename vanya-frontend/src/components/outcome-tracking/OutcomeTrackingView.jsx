import React from "react";
import OutcomeMetricCard from "./OutcomeMetricCard.jsx";

export default function OutcomeTrackingView({ vm }) {
  if (!vm?.show) return null;

  if (vm.empty) {
    return (
      <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {vm.emptyMessage}
      </p>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(auto-fit, minmax(180px, 1fr))",
          gap: 12,
        }}
      >
        {vm.metrics.map((metric) => (
          <OutcomeMetricCard key={metric.metric_id} metric={metric} />
        ))}
      </div>

      <div>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
          {vm.executiveSummaryLabel}
        </div>
        <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.55 }}>
          {vm.executiveSummary}
        </p>
      </div>
    </div>
  );
}
