import React from "react";
import ImpactMetricCard from "./ImpactMetricCard.jsx";

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
          gridTemplateColumns: "repeat(auto-fit, minmax(200px, 1fr))",
          gap: 12,
        }}
      >
        {metrics.map((metric) => (
          <ImpactMetricCard key={metric.metric_id} metric={metric} />
        ))}
      </div>
    </div>
  );
}

function HighlightList({ title, items, emptyMessage }) {
  return (
    <div>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
        {title}
      </div>
      {items?.length ? (
        <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.5 }}>
          {items.map((item) => (
            <li key={item.metric_id}>
              <span className={item.directionBadgeClass} style={{ marginRight: 6 }}>
                {item.directionIndicator}
              </span>
              {item.title}: {item.currentDisplay} ({item.deltaDisplay})
            </li>
          ))}
        </ul>
      ) : (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{emptyMessage}</p>
      )}
    </div>
  );
}

export default function ExecutiveImpactView({ vm }) {
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
      <MetricGrid title={vm.qualityLabel} metrics={vm.qualityMetrics} />
      <MetricGrid title={vm.riskLabel} metrics={vm.riskMetrics} />
      <MetricGrid title={vm.operationsLabel} metrics={vm.operationsMetrics} />

      <div style={{ display: "grid", gap: 16, gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))" }}>
        <HighlightList
          title={vm.topImprovementsLabel}
          items={vm.topImprovements}
          emptyMessage={vm.noImprovementsMessage}
        />
        <HighlightList
          title={vm.topConcernsLabel}
          items={vm.topConcerns}
          emptyMessage={vm.noConcernsMessage}
        />
      </div>
    </div>
  );
}
