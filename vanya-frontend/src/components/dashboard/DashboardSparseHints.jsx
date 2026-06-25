import React from "react";
import { Link } from "react-router-dom";

export default function DashboardSparseHints({ vm }) {
  if (!vm?.show) return null;

  return (
    <div
      style={{
        display: "grid",
        gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))",
        gap: 12,
        marginBottom: 24,
      }}
    >
      {vm.cards.map((card) => (
        <div
          key={card.title}
          className="card"
          style={{ padding: "16px 18px", display: "flex", flexDirection: "column", gap: 8 }}
        >
          <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>{card.title}</div>
          <p style={{ fontSize: 12, color: "var(--text-3)", margin: 0, lineHeight: 1.55 }}>{card.message}</p>
          {card.path && card.cta ? (
            <Link
              to={card.path}
              style={{ fontSize: 12, color: "var(--accent)", fontWeight: 600, textDecoration: "none", marginTop: 4 }}
            >
              {card.cta} →
            </Link>
          ) : null}
        </div>
      ))}
    </div>
  );
}
