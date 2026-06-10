import React from "react";
import ValueMetricCard from "./ValueMetricCard.jsx";

function MetricGrid({ title, metrics }) {
  if (!metrics?.length) return null;
  return (
    <div>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
        {title}
      </div>
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(auto-fit, minmax(180px, 1fr))",
          gap: 12,
        }}
      >
        {metrics.map((metric) => (
          <ValueMetricCard key={metric.metric_id} metric={metric} />
        ))}
      </div>
    </div>
  );
}

export default function ValueDashboardView({ vm }) {
  if (!vm?.show) return null;

  if (vm.empty) {
    return (
      <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {vm.emptyMessage}
      </p>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>
      {vm.topMetrics?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
            {vm.topMetricsLabel}
          </div>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
            {vm.topMetrics.map((metric) => (
              <span
                key={metric.metric_id}
                className="badge badge-blue"
                style={{ fontSize: 12, padding: "6px 10px" }}
                title={metric.description}
              >
                {metric.title}: {metric.displayValue}
              </span>
            ))}
          </div>
        </div>
      ) : null}

      <MetricGrid title={vm.activityLabel} metrics={vm.activityMetrics} />
      <MetricGrid title={vm.riskLabel} metrics={vm.riskMetrics} />
      <MetricGrid title={vm.qualityLabel} metrics={vm.qualityMetrics} />
      <MetricGrid title={vm.operationalLabel} metrics={vm.operationalMetrics} />
    </div>
  );
}
