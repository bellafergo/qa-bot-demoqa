import { describe, it, expect } from "vitest";
import {
  ONBOARDING_I18N_KEYS,
  buildOnboardingViewModel,
  isOnboardingComplete,
  navigationForStep,
  readinessBadgeClass,
  resolveOnboardingCompletionDate,
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
    expect(ONBOARDING_I18N_KEYS.operationalTitle).toBe("onboarding.operational.title");
    expect(ONBOARDING_I18N_KEYS.viewSetupDetails).toBe("onboarding.operational.view_setup_details");
  });

  it("detects incomplete onboarding at 80%", () => {
    expect(isOnboardingComplete({ overall_completion: 80, completed_steps: 6, total_steps: 8 })).toBe(false);
    const vm = buildOnboardingViewModel(
      { ...sampleChecklist, overall_completion: 80, completed_steps: 6, total_steps: 8 },
      t,
    );
    expect(vm.isComplete).toBe(false);
    expect(vm.operational).toBeNull();
  });

  it("detects complete onboarding at 100%", () => {
    const complete = {
      ...sampleChecklist,
      overall_completion: 100,
      completed_steps: 8,
      total_steps: 8,
      readiness_level: "FULLY_OPERATIONAL",
    };
    expect(isOnboardingComplete(complete)).toBe(true);
    const vm = buildOnboardingViewModel(complete, t);
    expect(vm.isComplete).toBe(true);
    expect(vm.operational.title).toBe(ONBOARDING_I18N_KEYS.operationalTitle);
    expect(vm.operational.stepsCompletedText).toBe("8/8");
    expect(vm.operational.readinessLevelLabel).toBe(ONBOARDING_I18N_KEYS.readinessFullyOperational);
  });

  it("detects completion when completed_steps equals total_steps", () => {
    expect(isOnboardingComplete({ overall_completion: 99, completed_steps: 8, total_steps: 8 })).toBe(true);
  });

  it("formats completion date when available", () => {
    const formatted = resolveOnboardingCompletionDate({ completion_date: "2026-06-12T10:00:00Z" });
    expect(formatted).toBeTruthy();
    expect(resolveOnboardingCompletionDate({})).toBeNull();
    const vm = buildOnboardingViewModel(
      {
        ...sampleChecklist,
        overall_completion: 100,
        completed_steps: 8,
        total_steps: 8,
        completion_date: "2026-06-12T10:00:00Z",
      },
      t,
    );
    expect(vm.operational.completionDateText).not.toBe("—");
  });
});
