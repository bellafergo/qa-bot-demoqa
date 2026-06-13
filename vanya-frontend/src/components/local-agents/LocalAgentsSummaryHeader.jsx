import React from "react";

export default function LocalAgentsSummaryHeader({ metrics, labels }) {
  if (!metrics || !labels) return null;

  const cards = [
    { label: labels.totalAgents, value: metrics.totalAgents, tone: "neutral" },
    { label: labels.online, value: metrics.online, tone: "healthy" },
    { label: labels.offline, value: metrics.offline, tone: "neutral" },
    { label: labels.dbConnections, value: metrics.dbConnections, tone: "neutral" },
    { label: labels.capabilities, value: metrics.capabilities, tone: "neutral" },
  ];

  const border = {
    healthy: "#22c55e",
    neutral: "var(--border)",
  };

  return (
    <div
      style={{
        display: "grid",
        gridTemplateColumns: "repeat(auto-fit, minmax(130px, 1fr))",
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
          <div style={{ fontSize: 22, fontWeight: 800, color: "var(--text-1)", marginTop: 4 }}>{card.value}</div>
        </div>
      ))}
    </div>
  );
}
