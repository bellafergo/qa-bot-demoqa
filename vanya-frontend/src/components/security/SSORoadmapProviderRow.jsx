import React from "react";

export default function SSORoadmapProviderRow({ provider }) {
  if (!provider) return null;

  return (
    <li
      style={{
        marginBottom: 8,
        padding: "12px 14px",
        borderRadius: 8,
        border: "1px solid var(--border)",
        background: "var(--bg-2)",
        listStyle: "none",
        display: "flex",
        justifyContent: "space-between",
        gap: 12,
        alignItems: "center",
        flexWrap: "wrap",
      }}
    >
      <div>
        <div style={{ fontWeight: 600, fontSize: 14, color: "var(--text-1)" }}>{provider.providerLabel}</div>
        <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4 }}>
          {provider.targetQuarterLabel}: {provider.targetQuarter}
        </div>
      </div>
      <span className={provider.statusBadgeClass}>{provider.statusLabel}</span>
    </li>
  );
}

export function EnterpriseAvailableTodayCard({ title, description, badgeLabel }) {
  return (
    <div
      className="card"
      style={{
        padding: "16px 18px",
        border: "1px solid rgba(34,197,94,0.25)",
        background: "rgba(34,197,94,0.06)",
      }}
    >
      <div style={{ display: "flex", gap: 12, alignItems: "flex-start", flexWrap: "wrap" }}>
        <div style={{ fontSize: 20, lineHeight: 1 }} aria-hidden>✓</div>
        <div style={{ flex: "1 1 240px", minWidth: 0 }}>
          <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
            <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text-1)" }}>{title}</div>
            <span className="badge badge-green">{badgeLabel}</span>
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, margin: 0 }}>{description}</p>
        </div>
      </div>
    </div>
  );
}
