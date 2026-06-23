import React from "react";
import OnboardingProgressCard from "./OnboardingProgressCard.jsx";
import OnboardingStepCard from "./OnboardingStepCard.jsx";

export default function OnboardingChecklistView({ vm }) {
  if (!vm?.checklist) return null;
  const checklist = vm.checklist;

  const labels = {
    projectReadinessLabel: vm.projectReadinessLabel,
    completionLabel: vm.completionLabel,
    nextRecommendedStepLabel: vm.nextRecommendedStepLabel,
    coreSetupCompleteLabel: vm.coreSetupCompleteLabel,
    platformOperationalNote: vm.platformOperationalNote,
  };

  return (
    <>
      <OnboardingProgressCard checklist={checklist} labels={labels} showCoreSetupComplete={vm.coreSetupComplete} />
      <div>
        <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
          {vm.checklistLabel}
        </div>
        <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
          {checklist.steps.map((step) => (
            <OnboardingStepCard key={step.step_id} step={step} />
          ))}
        </ul>
      </div>
    </>
  );
}
