import { describe, it, expect } from "vitest";
import {
  ONBOARDING_I18N_KEYS,
  buildOnboardingViewModel,
  isCoreOnboardingComplete,
  isOnboardingComplete,
  isOptionalOnboardingStep,
  localizeOnboardingStep,
  navigationForStep,
  readinessBadgeClass,
  resolveOnboardingCompletionDate,
  shouldCollapseOnboardingDashboard,
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
    const incompleteNav = navigationForStep({ category: "database_validation", status: "NOT_STARTED" });
    expect(incompleteNav.path).toBe("/local-agents");
    const vm = buildOnboardingViewModel(sampleChecklist, t, { projectId: "demo" });
    expect(vm.checklist.steps[1].navigation.label).toBe(ONBOARDING_I18N_KEYS.goToDatabaseValidation);
  });

  it("shows management navigation for completed steps", () => {
    const vm = buildOnboardingViewModel(sampleChecklist, t, { projectId: "demo" });
    expect(vm.checklist.steps[0].navigation.path).toBe("/integrations");
    expect(vm.checklist.steps[0].navigation.label).toBe(ONBOARDING_I18N_KEYS.manageIntegration);
  });

  it("routes testing NOT_STARTED to generate", () => {
    const nav = navigationForStep({ category: "testing", status: "NOT_STARTED" });
    expect(nav.path).toBe("/generate");
    expect(nav.labelKey).toBe(ONBOARDING_I18N_KEYS.goToGenerate);
  });

  it("routes testing COMPLETED to catalog with manage label", () => {
    const nav = navigationForStep({ category: "testing", status: "COMPLETED" });
    expect(nav.path).toBe("/catalog");
    expect(nav.labelKey).toBe(ONBOARDING_I18N_KEYS.openCatalog);
  });

  it("routes completed environments to project edit deep link", () => {
    const nav = navigationForStep(
      { category: "environments", status: "COMPLETED" },
      { projectId: "proj-42" },
    );
    expect(nav.path).toBe("/projects?edit=proj-42");
    expect(nav.labelKey).toBe(ONBOARDING_I18N_KEYS.manageEnvironments);
  });

  it("routes completed database validation to hash anchor", () => {
    const nav = navigationForStep({ category: "database_validation", status: "COMPLETED" });
    expect(nav.path).toBe("/local-agents#database-connections");
    expect(nav.labelKey).toBe(ONBOARDING_I18N_KEYS.manageDatabaseValidation);
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

  it("localizes contract intelligence step and marks it optional", () => {
    const step = localizeOnboardingStep(
      {
        step_id: "configure_contract_intelligence",
        title: "Generate Contract Intelligence",
        description: "Backend description",
        category: "contract_intelligence",
        status: "NOT_STARTED",
        completion_percentage: 0,
      },
      t,
    );
    expect(isOptionalOnboardingStep(step)).toBe(true);
    expect(step.title).toBe(ONBOARDING_I18N_KEYS.contractIntelligenceTitle);
    expect(step.optionalLabel).toBe(ONBOARDING_I18N_KEYS.optionalCapability);
    expect(step.guidanceText).toBe(ONBOARDING_I18N_KEYS.contractIntelligenceGuidanceNotStarted);
  });

  it("routes incomplete contract intelligence to incidents with generate label", () => {
    const nav = navigationForStep({ category: "contract_intelligence", status: "NOT_STARTED" });
    expect(nav.path).toBe("/incidents");
    expect(nav.labelKey).toBe(ONBOARDING_I18N_KEYS.generateContractIntelligence);
  });

  it("shows core setup complete when only optional step remains", () => {
    const checklist = {
      ...sampleChecklist,
      overall_completion: 88,
      completed_steps: 7,
      total_steps: 8,
      next_recommended_step: "Generate Contract Intelligence",
      steps: [
        { step_id: "connect_repository", status: "COMPLETED" },
        { step_id: "import_tests", status: "COMPLETED" },
        { step_id: "configure_browser_monitoring", status: "COMPLETED" },
        { step_id: "configure_environments", status: "COMPLETED" },
        { step_id: "configure_local_agent", status: "COMPLETED" },
        { step_id: "configure_database_validation", status: "COMPLETED" },
        { step_id: "configure_contract_intelligence", status: "NOT_STARTED", category: "contract_intelligence" },
        { step_id: "generate_executive_report", status: "COMPLETED" },
      ],
    };
    expect(isCoreOnboardingComplete(checklist)).toBe(true);
    const vm = buildOnboardingViewModel(checklist, t);
    expect(vm.coreSetupComplete).toBe(true);
    expect(vm.coreSetupCompleteLabel).toBe(ONBOARDING_I18N_KEYS.coreSetupComplete);
    expect(vm.checklist.next_recommended_step).toBe(ONBOARDING_I18N_KEYS.contractIntelligenceTitle);
  });

  it("collapses onboarding dashboard when core setup complete or pct >= 88", () => {
    const checklist = {
      overall_completion: 96,
      steps: [
        { step_id: "connect_repository", status: "COMPLETED" },
        { step_id: "import_tests", status: "COMPLETED" },
        { step_id: "configure_browser_monitoring", status: "COMPLETED" },
        { step_id: "configure_environments", status: "COMPLETED" },
        { step_id: "configure_local_agent", status: "COMPLETED" },
        { step_id: "configure_database_validation", status: "COMPLETED" },
        { step_id: "configure_contract_intelligence", status: "NOT_STARTED" },
        { step_id: "generate_executive_report", status: "COMPLETED" },
      ],
    };
    const vm = buildOnboardingViewModel(checklist, t);
    expect(shouldCollapseOnboardingDashboard(checklist, vm)).toBe(true);
    expect(shouldCollapseOnboardingDashboard({ overall_completion: 50, steps: [] }, { coreSetupComplete: false })).toBe(false);
  });
});
