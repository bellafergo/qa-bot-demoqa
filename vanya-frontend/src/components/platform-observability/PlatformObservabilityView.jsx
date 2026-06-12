import React from "react";
import PlatformHealthCard from "./PlatformHealthCard.jsx";

export default function PlatformObservabilityView({ vm }) {
  if (!vm?.show) return null;

  if (vm.empty) {
    return (
      <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {vm.emptyMessage}
      </p>
    );
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>
      {vm.executiveSummary ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.executiveSummaryLabel}
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
            {vm.executiveSummary}
          </p>
        </div>
      ) : null}

      {vm.healthSections?.length ? (
        <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
          {vm.healthSections.map((section) => (
            <PlatformHealthCard
              key={section.key}
              title={section.title}
              area={section.area}
              integrationSummaryLabels={
                section.key === "integration" ? vm.integrationSummaryLabels : null
              }
            />
          ))}
        </div>
      ) : null}

      {vm.topPlatformRisks?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.topPlatformRisksLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.5 }}>
            {vm.topPlatformRisks.map((risk) => (
              <li key={risk}>{risk}</li>
            ))}
          </ul>
        </div>
      ) : null}
    </div>
  );
}
