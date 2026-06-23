import React from "react";

export default function DashboardMoreIntelligenceSection({ title, summary, children }) {
  const items = React.Children.toArray(children).filter(Boolean);
  if (!items.length) return null;

  return (
    <details
      className="card"
      style={{
        padding: "16px 20px",
        marginBottom: 20,
      }}
    >
      <summary
        style={{
          cursor: "pointer",
          fontSize: 13,
          fontWeight: 600,
          color: "var(--text-2)",
          listStyle: "none",
          display: "flex",
          alignItems: "center",
          gap: 8,
        }}
      >
        <span style={{ fontSize: 10, color: "var(--text-3)" }}>▸</span>
        {title}
        {summary ? (
          <span style={{ fontSize: 12, fontWeight: 400, color: "var(--text-3)", marginLeft: 4 }}>
            — {summary}
          </span>
        ) : null}
      </summary>
      <div style={{ marginTop: 16, display: "flex", flexDirection: "column", gap: 16 }}>
        {items}
      </div>
    </details>
  );
}
