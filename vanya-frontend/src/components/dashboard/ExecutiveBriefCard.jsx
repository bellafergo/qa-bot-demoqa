import React from "react";
import { Link } from "react-router-dom";

export default function ExecutiveBriefCard({ vm, compact = false }) {
  if (!vm?.show) return null;

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: compact ? 12 : 16 }}>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(180px, 1fr))", gap: 12 }}>
        <div
          style={{
            padding: "12px 14px",
            borderRadius: 8,
            border: "1px solid var(--border, rgba(255,255,255,0.08))",
            background: "var(--bg-3, rgba(255,255,255,0.03))",
          }}
        >
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {vm.releaseStatusLabel}
          </div>
          <span className={vm.releaseStatusBadgeClass}>{vm.releaseStatus}</span>
        </div>

        <div
          style={{
            padding: "12px 14px",
            borderRadius: 8,
            border: "1px solid var(--border, rgba(255,255,255,0.08))",
            background: "var(--bg-3, rgba(255,255,255,0.03))",
          }}
        >
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {vm.trendLabel}
          </div>
          <span className={vm.trendBadgeClass}>{vm.trendDirection}</span>
        </div>

        <div
          style={{
            padding: "12px 14px",
            borderRadius: 8,
            border: "1px solid var(--border, rgba(255,255,255,0.08))",
            background: "var(--bg-3, rgba(255,255,255,0.03))",
          }}
        >
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {vm.actionLabel}
          </div>
          <Link
            to={vm.action.path}
            style={{ fontSize: 13, color: "var(--accent)", fontWeight: 600, textDecoration: "none" }}
          >
            {vm.action.label} →
          </Link>
        </div>
      </div>

      {!compact ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.topRisksLabel}
          </div>
          {vm.topRisks?.length ? (
            <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.5 }}>
              {vm.topRisks.map((risk) => (
                <li key={risk}>{risk}</li>
              ))}
            </ul>
          ) : (
            <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{vm.noRisksMessage}</p>
          )}
        </div>
      ) : null}
    </div>
  );
}
