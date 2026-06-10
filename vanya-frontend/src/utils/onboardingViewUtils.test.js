import { describe, it, expect } from "vitest";
import {
  ONBOARDING_I18N_KEYS,
  buildOnboardingViewModel,
  navigationForStep,
  readinessBadgeClass,
  stepStatusIcon,
  stepStatusLabelKey,
} from "./onboardingViewUtils.js";

const t = (key) => key;

const sampleChecklist = {
  project_id: "demo",
  overall_completion: 72,
  readiness_level: "PARTIALLY_READY",
  completed_steps: 5,
  total_steps: 8,
  next_recommended_step: "Configure Database Validation",
  steps: [
    {
      step_id: "connect_repository",
      title: "Connect Repository",
      description: "Link GitHub",
      category: "repository",
      status: "COMPLETED",
      priority: 1,
      completion_percentage: 100,
      recommended_next_action: "",
    },
    {
      step_id: "configure_database_validation",
      title: "Configure Database Validation",
      description: "Add connectors",
      category: "database_validation",
      status: "NOT_STARTED",
      priority: 6,
      completion_percentage: 0,
      recommended_next_action: "Add connector",
    },
  ],
};

describe("onboardingViewUtils", () => {
  it("renders onboarding view model", () => {
    const vm = buildOnboardingViewModel(sampleChecklist, t);
    expect(vm.show).toBe(true);
    expect(vm.checklist.overall_completion).toBe(72);
    expect(vm.checklist.readinessLabel).toBe(ONBOARDING_I18N_KEYS.readinessPartiallyReady);
  });

  it("calculates progress display", () => {
    const vm = buildOnboardingViewModel(sampleChecklist, t);
    expect(vm.checklist.completedLabel).toContain("5 / 8");
  });

  it("renders readiness badge class", () => {
    expect(readinessBadgeClass("PARTIALLY_READY")).toContain("badge-orange");
    expect(readinessBadgeClass("FULLY_OPERATIONAL")).toContain("badge-green");
  });

  it("renders step status labels and icons", () => {
    expect(stepStatusLabelKey("COMPLETED")).toBe(ONBOARDING_I18N_KEYS.completed);
    expect(stepStatusIcon("COMPLETED")).toBe("✓");
    expect(stepStatusIcon("NOT_STARTED")).toBe("○");
  });

  it("shows next recommended step", () => {
    const vm = buildOnboardingViewModel(sampleChecklist, t);
    expect(vm.checklist.next_recommended_step).toBe("Configure Database Validation");
  });

  it("maps navigation actions by category", () => {
    const nav = navigationForStep({ category: "database_validation" });
    expect(nav.path).toBe("/local-agents");
    const vm = buildOnboardingViewModel(sampleChecklist, t);
    expect(vm.checklist.steps[1].navigation.label).toBe(ONBOARDING_I18N_KEYS.goToDatabaseValidation);
  });

  it("exposes translation keys", () => {
    expect(ONBOARDING_I18N_KEYS.title).toBe("onboarding.title");
    expect(ONBOARDING_I18N_KEYS.projectReadiness).toBe("onboarding.project_readiness");
  });
});
