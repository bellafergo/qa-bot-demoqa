import React from "react";

export default function BrowserWatchSummaryHeader({ vm }) {
  if (!vm) return null;

  const cards = [
    { label: vm.totalLabel, value: vm.total, tone: "neutral" },
    { label: vm.healthyLabel, value: vm.healthy, tone: "healthy" },
    { label: vm.warningsLabel, value: vm.warnings, tone: "warning" },
    { label: vm.criticalLabel, value: vm.critical, tone: "critical" },
  ];

  const border = {
    healthy: "#22c55e",
    warning: "#f59e0b",
    critical: "#ef4444",
    neutral: "var(--border)",
  };

  return (
    <div
      style={{
        display: "grid",
        gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))",
        gap: 12,
        marginBottom: 16,
      }}
    >
      {cards.map((card) => (
        <div
          key={card.label}
          className="card"
          style={{
            padding: "12px 14px",
            borderLeft: `3px solid ${border[card.tone] || border.neutral}`,
          }}
        >
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)" }}>{card.label}</div>
          <div style={{ fontSize: 22, fontWeight: 800, color: "var(--text-1)", marginTop: 4, lineHeight: 1 }}>{card.value}</div>
        </div>
      ))}
    </div>
  );
}
