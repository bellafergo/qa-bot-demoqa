import React from "react";
import BusinessRiskCard from "./BusinessRiskCard.jsx";
import BusinessRiskSignalCard from "./BusinessRiskSignalCard.jsx";

export default function BusinessRiskView({ vm }) {
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
      <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
        <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)" }}>
          {vm.overallRiskLabel}:
        </span>
        <span className={vm.overallSeverityBadgeClass}>{vm.overallBusinessRisk}</span>
      </div>

      {vm.executiveSummary ? (
        <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
          {vm.executiveSummary}
        </p>
      ) : null}

      {vm.topCapabilities?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.capabilitiesLabel}
          </div>
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
            {vm.topCapabilities.map((cap) => (
              <span key={cap} className="badge badge-orange" style={{ fontSize: 12 }}>
                {cap}
              </span>
            ))}
          </div>
        </div>
      ) : null}

      {vm.businessRisks?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
            {vm.risksLabel}
          </div>
          <div style={{ display: "grid", gap: 12, gridTemplateColumns: "repeat(auto-fit, minmax(260px, 1fr))" }}>
            {vm.businessRisks.map((risk) => (
              <BusinessRiskCard key={risk.risk_id} risk={risk} />
            ))}
          </div>
        </div>
      ) : null}

      {vm.signals?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.evidenceLabel}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none", display: "grid", gap: 8 }}>
            {vm.signals.map((signal) => (
              <BusinessRiskSignalCard key={signal.signal_id} signal={signal} />
            ))}
          </ul>
        </div>
      ) : null}
    </div>
  );
}
