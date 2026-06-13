import React from "react";
import ExecutiveRiskCard from "../incident/ExecutiveRiskCard.jsx";

function RiskGroup({ label, items }) {
  if (!items?.length) return null;

  return (
    <div style={{ marginBottom: 14 }}>
      <div style={{ fontSize: 12, fontWeight: 700, color: "var(--text-2)", marginBottom: 8, textTransform: "uppercase", letterSpacing: "0.04em" }}>
        {label}
      </div>
      <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
        {items.map((item) => (
          <ExecutiveRiskCard key={item.title} risk={item.title} trace={item.trace} severity={item.severity} />
        ))}
      </ul>
    </div>
  );
}

export default function IncidentRiskGrouping({ vm }) {
  if (!vm?.hasItems) return null;

  return (
    <div style={{ marginBottom: 16 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
        {vm.title}
      </div>
      <RiskGroup label={vm.technicalRisk.label} items={vm.technicalRisk.items} />
      <RiskGroup label={vm.operationalRisk.label} items={vm.operationalRisk.items} />
      <RiskGroup label={vm.executiveRisk.label} items={vm.executiveRisk.items} />
    </div>
  );
}
