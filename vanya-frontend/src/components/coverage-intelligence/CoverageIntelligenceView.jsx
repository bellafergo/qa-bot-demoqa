import React from "react";
import CoverageAssessmentCard from "./CoverageAssessmentCard.jsx";
import CoverageGapCard from "./CoverageGapCard.jsx";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";

export default function CoverageIntelligenceView({ vm }) {
  if (!vm?.show) return null;

  if (!vm.showContent && vm.capabilityState) {
    return <CapabilityStateCard state={vm.capabilityState} />;
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(120px, 1fr))", gap: 10 }}>
        <div style={{ textAlign: "center", padding: "8px 6px", borderRadius: 6, background: "var(--bg-2)" }}>
          <div style={{ fontSize: 18, fontWeight: 700 }}>{vm.totalTests}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{vm.totalTestsLabel}</div>
        </div>
        <div style={{ textAlign: "center", padding: "8px 6px", borderRadius: 6, background: "var(--bg-2)" }}>
          <div style={{ fontSize: 18, fontWeight: 700 }}>{vm.totalMatches}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{vm.totalMatchesLabel}</div>
        </div>
      </div>

      {vm.assessments?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.byCapabilityLabel}
          </div>
          <div style={{ display: "grid", gap: 8, gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))" }}>
            {vm.assessments.map((a) => (
              <CoverageAssessmentCard key={a.capability} assessment={a} />
            ))}
          </div>
        </div>
      ) : null}

      {vm.gaps?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.gapsLabel}
          </div>
          {vm.criticalGaps?.length ? (
            <div style={{ marginBottom: 10 }}>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>{vm.criticalGapsLabel}</div>
              <div style={{ display: "grid", gap: 8 }}>
                {vm.criticalGaps.map((g) => (
                  <CoverageGapCard key={`critical-${g.capability}`} gap={g} />
                ))}
              </div>
            </div>
          ) : null}
          {vm.mediumGaps?.length ? (
            <div>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>{vm.mediumGapsLabel}</div>
              <div style={{ display: "grid", gap: 8 }}>
                {vm.mediumGaps.map((g) => (
                  <CoverageGapCard key={`medium-${g.capability}`} gap={g} />
                ))}
              </div>
            </div>
          ) : null}
        </div>
      ) : null}

      {vm.executiveSummary ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
            {vm.executiveSummaryLabel}
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", margin: 0, lineHeight: 1.5 }}>
            {vm.executiveSummary}
          </p>
        </div>
      ) : null}
    </div>
  );
}
