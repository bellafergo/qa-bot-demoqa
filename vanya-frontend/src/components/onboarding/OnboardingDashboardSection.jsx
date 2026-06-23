import React, { useState } from "react";
import OnboardingChecklistView from "./OnboardingChecklistView.jsx";
import OnboardingOperationalCard from "./OnboardingOperationalCard.jsx";

export default function OnboardingDashboardSection({
  vm,
  projectName,
  collapsedByDefault = false,
}) {
  const [setupDetailsExpanded, setSetupDetailsExpanded] = useState(false);
  const [checklistExpanded, setChecklistExpanded] = useState(!collapsedByDefault);

  if (!vm?.show) return null;

  if (!vm.isComplete) {
    if (collapsedByDefault && !checklistExpanded) {
      const checklist = vm.checklist;
      const pct = Math.max(0, Math.min(100, Number(checklist?.overall_completion) || 0));

      return (
        <div className="card" style={{ padding: "16px 20px", marginBottom: 20 }}>
          <div style={{ display: "flex", justifyContent: "space-between", gap: 12, flexWrap: "wrap", alignItems: "center" }}>
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 4 }}>
                {vm.title}
              </div>
              <div style={{ display: "flex", gap: 10, alignItems: "center", flexWrap: "wrap" }}>
                <span style={{ fontSize: 22, fontWeight: 800, color: "var(--text-1)" }}>{pct}%</span>
                {vm.coreSetupComplete && vm.coreSetupCompleteLabel ? (
                  <span className="badge badge-green" style={{ fontSize: 11 }}>
                    {vm.coreSetupCompleteLabel}
                  </span>
                ) : null}
              </div>
              {vm.coreSetupComplete && vm.platformOperationalNote ? (
                <p style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.5, margin: "8px 0 0", maxWidth: 520 }}>
                  {vm.platformOperationalNote}
                </p>
              ) : null}
            </div>
            <button
              type="button"
              className="btn btn-ghost btn-sm"
              onClick={() => setChecklistExpanded(true)}
            >
              {vm.expandChecklistLabel}
            </button>
          </div>
        </div>
      );
    }

    return (
      <div className="card" style={{ padding: "20px 24px", marginBottom: 20 }}>
        <div style={{ display: "flex", justifyContent: "space-between", gap: 12, alignItems: "flex-start", marginBottom: collapsedByDefault ? 10 : 0 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)" }}>{vm.title}</div>
          {collapsedByDefault ? (
            <button
              type="button"
              className="btn btn-ghost btn-sm"
              onClick={() => setChecklistExpanded(false)}
            >
              {vm.collapseChecklistLabel}
            </button>
          ) : null}
        </div>
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
