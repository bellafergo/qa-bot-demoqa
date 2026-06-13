import React from "react";
import ServiceNowCorrelationCard from "./ServiceNowCorrelationCard.jsx";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";

function CorrelationSection({ label, items }) {
  if (!items?.length) return null;
  return (
    <div style={{ marginBottom: 14 }}>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{label}</div>
      <ul style={{ margin: 0, padding: 0 }}>
        {items.map((item) => (
          <ServiceNowCorrelationCard key={`${item.entityType}-${item.entityId}`} vm={item} />
        ))}
      </ul>
    </div>
  );
}

export default function ServiceNowIntelligenceView({ vm }) {
  if (!vm?.show) return null;

  if (!vm.showContent && vm.capabilityState) {
    return (
      <div>
        <CapabilityStateCard state={vm.capabilityState} />
        <p style={{ margin: "12px 0 0", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, fontStyle: "italic" }}>
          {vm.readOnlyNote}
        </p>
      </div>
    );
  }

  return (
    <div
      style={{
        padding: "16px 18px",
        background: "var(--bg-2)",
        borderRadius: 8,
        border: "1px solid var(--border, rgba(255,255,255,0.08))",
      }}
    >
      <>
          {vm.executiveSummary ? (
            <p style={{ margin: "0 0 14px", fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
              <span style={{ fontWeight: 600, color: "var(--text-3)" }}>{vm.executiveSummaryLabel}: </span>
              {vm.executiveSummary}
            </p>
          ) : null}

          {vm.showOperationalRisks ? (
            <div style={{ marginBottom: 14 }}>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
                {vm.operationalRisksLabel}
              </div>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
                {vm.operationalRisks.map((risk) => (
                  <li key={risk}>{risk}</li>
                ))}
              </ul>
            </div>
          ) : null}

          <CorrelationSection label={vm.incidentsLabel} items={vm.incidents} />
          <CorrelationSection label={vm.changesLabel} items={vm.changes} />
          <CorrelationSection label={vm.servicesLabel} items={vm.services} />
          <CorrelationSection label={vm.cmdbLabel} items={vm.cmdb} />
      </>
      <p style={{ margin: "12px 0 0", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
