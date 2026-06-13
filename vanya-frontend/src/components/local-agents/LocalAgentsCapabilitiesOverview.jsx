import React from "react";

export default function LocalAgentsCapabilitiesOverview({ title, items }) {
  if (!items?.length) return null;

  return (
    <div style={{ marginTop: 20 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10, textTransform: "uppercase", letterSpacing: "0.06em" }}>
        {title}
      </div>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(200px, 1fr))", gap: 12 }}>
        {items.map((item) => (
          <div key={item.capabilityId} className="card" style={{ padding: "12px 14px" }}>
            <div style={{ fontSize: 14, fontWeight: 700, color: "var(--text-1)" }}>{item.label}</div>
            <div style={{ fontSize: 12, color: "var(--text-3)", marginTop: 6 }}>
              {item.agentCount} {item.agentCount === 1 ? "agent" : "agents"}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}
