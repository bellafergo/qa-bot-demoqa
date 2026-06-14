import React from "react";

export default function ExecutiveRiskBriefCard({ vm }) {
  if (!vm?.show) return null;

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 14 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 12, flexWrap: "wrap" }}>
        <div>
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
            {vm.primaryRiskLabel}
          </div>
          <div style={{ fontSize: 22, fontWeight: 700, color: "var(--text-1)", marginTop: 4 }}>
            {vm.module}
          </div>
        </div>
        <div>
          <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.confidenceLabel}
          </div>
          <span className={vm.confidenceBadgeClass}>{vm.confidenceText}</span>
        </div>
      </div>

      {vm.evidence?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {vm.evidenceLabel}
          </div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
            {vm.evidence.map((line) => (
              <li key={line}>{line}</li>
            ))}
          </ul>
        </div>
      ) : null}

      {vm.impact ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.impactLabel}
          </div>
          <p style={{ margin: 0, fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>{vm.impact}</p>
        </div>
      ) : null}

      {vm.recommendation ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
            {vm.recommendationLabel}
          </div>
          <p style={{ margin: 0, fontSize: 13, color: "var(--text-1)", lineHeight: 1.6, fontWeight: 600 }}>
            {vm.recommendation}
          </p>
        </div>
      ) : null}

      <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: 0, fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
