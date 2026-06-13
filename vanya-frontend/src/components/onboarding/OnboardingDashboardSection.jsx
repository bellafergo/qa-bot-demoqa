import React, { useState } from "react";
import OnboardingChecklistView from "./OnboardingChecklistView.jsx";
import OnboardingOperationalCard from "./OnboardingOperationalCard.jsx";

export default function OnboardingDashboardSection({ vm, projectName }) {
  const [setupDetailsExpanded, setSetupDetailsExpanded] = useState(false);

  if (!vm?.show) return null;

  if (!vm.isComplete) {
    return (
      <div className="card" style={{ padding: "20px 24px", marginBottom: 20 }}>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>{vm.title}</div>
        <OnboardingChecklistView vm={vm} />
        <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
          {vm.readOnlyNote}
        </p>
      </div>
    );
  }

  return (
    <div className="card" style={{ padding: "20px 24px", marginBottom: 20 }}>
      <OnboardingOperationalCard
        operational={vm.operational}
        projectName={projectName}
        expanded={setupDetailsExpanded}
        onToggleDetails={() => setSetupDetailsExpanded((prev) => !prev)}
      />
      {setupDetailsExpanded ? (
        <div style={{ marginTop: 18, paddingTop: 18, borderTop: "1px solid var(--border)" }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>{vm.title}</div>
          <OnboardingChecklistView vm={vm} />
          <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
            {vm.readOnlyNote}
          </p>
        </div>
      ) : null}
    </div>
  );
}
