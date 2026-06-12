import React from "react";

export default function PlatformHealthCard({ title, area, integrationSummaryLabels }) {
  if (!area) return null;

  return (
    <div
      style={{
        padding: "14px 16px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      <div style={{ display: "flex", justifyContent: "space-between", gap: 12, alignItems: "flex-start", marginBottom: 8 }}>
        <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text-1)" }}>{title}</div>
        <span className={area.statusBadgeClass}>{area.statusLabel}</span>
      </div>

      {area.summary ? (
        <p style={{ fontSize: 12, color: "var(--text-2)", margin: "0 0 10px", lineHeight: 1.5 }}>
          {area.summary}
        </p>
      ) : null}

      {area.integrationSummary ? (
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
          <span className="badge badge-green" style={{ fontSize: 11 }}>
            {integrationSummaryLabels?.healthy}: {area.integrationSummary.healthy}
          </span>
          <span className="badge badge-orange" style={{ fontSize: 11 }}>
            {integrationSummaryLabels?.degraded}: {area.integrationSummary.degraded}
          </span>
          <span className="badge badge-red" style={{ fontSize: 11 }}>
            {integrationSummaryLabels?.disconnected}: {area.integrationSummary.disconnected}
          </span>
        </div>
      ) : null}

      {area.metrics?.length ? (
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(160px, 1fr))", gap: 8 }}>
          {area.metrics.map((metric) => (
            <div
              key={metric.metric_name}
              style={{
                padding: "10px 12px",
                borderRadius: 6,
                border: "1px solid var(--border, rgba(255,255,255,0.06))",
                background: "var(--bg-2, rgba(255,255,255,0.02))",
              }}
            >
              <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 4 }}>{metric.metric_name}</div>
              <div style={{ fontSize: 18, fontWeight: 700, color: "var(--text-1)", lineHeight: 1.2 }}>{metric.value}</div>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4, lineHeight: 1.4 }}>{metric.summary}</div>
            </div>
          ))}
        </div>
      ) : null}
    </div>
  );
}
