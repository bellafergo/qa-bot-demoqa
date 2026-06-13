import React from "react";
import RecommendationGroupCard from "./RecommendationGroupCard.jsx";
import RecommendedTestCard from "./RecommendedTestCard.jsx";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";

function PrioritySection({ label, tests }) {
  if (!tests?.length) return null;
  return (
    <div>
      <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
        {label} ({tests.length})
      </div>
      <div style={{ display: "grid", gap: 8, gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))" }}>
        {tests.map((test) => (
          <RecommendedTestCard key={`${test.priority}-${test.testCaseId}`} test={test} />
        ))}
      </div>
    </div>
  );
}

export default function QMetryRecommendationView({ vm }) {
  if (!vm?.show) return null;

  if (!vm.showContent && vm.capabilityState) {
    return <CapabilityStateCard state={vm.capabilityState} />;
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
      <div style={{ textAlign: "center", padding: "8px 6px", borderRadius: 6, background: "var(--bg-2)", maxWidth: 180 }}>
        <div style={{ fontSize: 18, fontWeight: 700 }}>{vm.totalRecommendations}</div>
        <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{vm.totalRecommendationsLabel}</div>
      </div>

      <PrioritySection label={vm.priorityCriticalLabel} tests={vm.criticalTests} />
      <PrioritySection label={vm.priorityHighLabel} tests={vm.highTests} />
      <PrioritySection label={vm.priorityMediumLabel} tests={vm.mediumTests} />

      {vm.groups?.length ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {vm.byCapabilityLabel}
          </div>
          <div style={{ display: "grid", gap: 10 }}>
            {vm.groups.map((group) => (
              <RecommendationGroupCard key={group.capability} group={group} />
            ))}
          </div>
        </div>
      ) : null}

      {vm.executiveSummary ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.5 }}>
          <span style={{ fontWeight: 600 }}>{vm.executiveSummaryLabel}: </span>
          {vm.executiveSummary}
        </div>
      ) : null}
    </div>
  );
}
