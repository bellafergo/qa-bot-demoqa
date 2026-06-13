import React from "react";

export default function CommandCenterView({ vm }) {
  if (!vm?.kpis?.length) return null;

  return (
    <div style={{ marginBottom: 16 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10, textTransform: "uppercase", letterSpacing: "0.06em" }}>
        {vm.title}
      </div>
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "repeat(auto-fit, minmax(160px, 1fr))",
          gap: 12,
        }}
        className="command-center-kpi-grid"
      >
        {vm.kpis.map((kpi) => (
          <div
            key={kpi.id}
            className="card"
            style={{
              padding: "14px 16px",
              borderLeft: `3px solid ${kpi.colors.border}`,
              minHeight: 88,
              display: "flex",
              flexDirection: "column",
              justifyContent: "space-between",
            }}
          >
            <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
              <span
                aria-hidden
                style={{
                  width: 8,
                  height: 8,
                  borderRadius: "50%",
                  background: kpi.colors.dot,
                  flexShrink: 0,
                }}
              />
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", lineHeight: 1.3 }}>{kpi.label}</div>
            </div>
            <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text-1)", marginTop: 10, lineHeight: 1.3 }}>
              {kpi.value}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}
