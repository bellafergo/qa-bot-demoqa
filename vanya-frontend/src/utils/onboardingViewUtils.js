/** View helpers for Guided Onboarding (ENT-03A). */

export const ONBOARDING_I18N_KEYS = {
  title: "onboarding.title",
  projectReadiness: "onboarding.project_readiness",
  completion: "onboarding.completion",
  nextRecommendedStep: "onboarding.next_recommended_step",
  completed: "onboarding.completed",
  inProgress: "onboarding.in_progress",
  notStarted: "onboarding.not_started",
  blocked: "onboarding.blocked",
  checklist: "onboarding.checklist",
  readinessNotReady: "onboarding.readiness.not_ready",
  readinessEarlySetup: "onboarding.readiness.early_setup",
  readinessPartiallyReady: "onboarding.readiness.partially_ready",
  readinessReady: "onboarding.readiness.ready",
  readinessFullyOperational: "onboarding.readiness.fully_operational",
  goToRepositories: "onboarding.action.go_repositories",
  goToBrowserWatches: "onboarding.action.go_browser_watches",
  goToLocalAgents: "onboarding.action.go_local_agents",
  goToDatabaseValidation: "onboarding.action.go_database_validation",
  goToCatalog: "onboarding.action.go_catalog",
  goToGenerate: "onboarding.action.go_generate",
  goToIncidents: "onboarding.action.go_incidents",
  goToProjects: "onboarding.action.go_projects",
  manageIntegration: "onboarding.action.manage_integration",
  openCatalog: "onboarding.action.open_catalog",
  manageBrowserWatch: "onboarding.action.manage_browser_watch",
  manageEnvironments: "onboarding.action.manage_environments",
  manageLocalAgents: "onboarding.action.manage_local_agents",
  manageDatabaseValidation: "onboarding.action.manage_database_validation",
  viewContractIntelligence: "onboarding.action.view_contract_intelligence",
  generateContractIntelligence: "onboarding.action.generate_contract_intelligence",
  viewExecutiveReports: "onboarding.action.view_executive_reports",
  optionalCapability: "onboarding.step.optional_capability",
  coreSetupComplete: "onboarding.core_setup_complete",
  platformOperationalNote: "onboarding.platform_operational_note",
  contractIntelligenceTitle: "onboarding.step.contract_intelligence.title",
  contractIntelligenceDescription: "onboarding.step.contract_intelligence.description",
  contractIntelligenceGuidanceNotStarted: "onboarding.step.contract_intelligence.guidance_not_started",
  contractIntelligenceGuidanceInProgress: "onboarding.step.contract_intelligence.guidance_in_progress",
  contractIntelligenceGuidanceCompleted: "onboarding.step.contract_intelligence.guidance_completed",
  readOnlyNote: "onboarding.read_only_note",
  operationalTitle: "onboarding.operational.title",
  operationalSubtitle: "onboarding.operational.subtitle",
  operationalCompletionDate: "onboarding.operational.completion_date",
  operationalStepsCompleted: "onboarding.operational.steps_completed",
  operationalReadiness: "onboarding.operational.readiness",
  operationalProject: "onboarding.operational.project",
  viewSetupDetails: "onboarding.operational.view_setup_details",
  hideSetupDetails: "onboarding.operational.hide_setup_details",
  expandChecklist: "onboarding.dashboard.expand_checklist",
  collapseChecklist: "onboarding.dashboard.collapse_checklist",
};

export const CONTRACT_INTELLIGENCE_STEP_ID = "configure_contract_intelligence";

/** Core setup steps — contract intelligence is optional/advanced. */
export const CORE_ONBOARDING_STEP_IDS = new Set([
  "connect_repository",
  "import_tests",
  "configure_browser_monitoring",
  "configure_environments",
  "configure_local_agent",
  "configure_database_validation",
  "generate_executive_report",
]);

/** True when onboarding checklist is fully complete (frontend-only UX gate). */
export function isOnboardingComplete(checklist) {
  if (!checklist) return false;
  const total = Number(checklist.total_steps) || 0;
  const completed = Number(checklist.completed_steps) || 0;
  const pct = Number(checklist.overall_completion) || 0;
  if (total > 0 && completed >= total) return true;
  return pct >= 100;
}

function formatCompletionDate(iso) {
  if (!iso) return null;
  try {
    return new Date(iso).toLocaleDateString(undefined, {
      year: "numeric",
      month: "short",
      day: "numeric",
    });
  } catch {
    return null;
  }
}

export function resolveOnboardingCompletionDate(checklist) {
  const raw = checklist?.completion_date || checklist?.completed_at || null;
  return formatCompletionDate(raw);
}

const READINESS_LABEL_KEY = {
  NOT_READY: ONBOARDING_I18N_KEYS.readinessNotReady,
  EARLY_SETUP: ONBOARDING_I18N_KEYS.readinessEarlySetup,
  PARTIALLY_READY: ONBOARDING_I18N_KEYS.readinessPartiallyReady,
  READY: ONBOARDING_I18N_KEYS.readinessReady,
  FULLY_OPERATIONAL: ONBOARDING_I18N_KEYS.readinessFullyOperational,
};

const STATUS_LABEL_KEY = {
  COMPLETED: ONBOARDING_I18N_KEYS.completed,
  IN_PROGRESS: ONBOARDING_I18N_KEYS.inProgress,
  NOT_STARTED: ONBOARDING_I18N_KEYS.notStarted,
  BLOCKED: ONBOARDING_I18N_KEYS.blocked,
};

const STATUS_BADGE = {
  COMPLETED: "badge badge-green",
  IN_PROGRESS: "badge badge-orange",
  NOT_STARTED: "badge badge-gray",
  BLOCKED: "badge badge-red",
};

const READINESS_BADGE = {
  NOT_READY: "badge badge-gray",
  EARLY_SETUP: "badge badge-orange",
  PARTIALLY_READY: "badge badge-orange",
  READY: "badge badge-blue",
  FULLY_OPERATIONAL: "badge badge-green",
};

const INCOMPLETE_NAVIGATION_BY_CATEGORY = {
  repository: { path: "/integrations", labelKey: ONBOARDING_I18N_KEYS.goToRepositories },
  testing: { path: "/catalog", labelKey: ONBOARDING_I18N_KEYS.goToCatalog },
  browser_monitoring: { path: "/browser-watch", labelKey: ONBOARDING_I18N_KEYS.goToBrowserWatches },
  environments: { path: "/projects", labelKey: ONBOARDING_I18N_KEYS.goToProjects },
  agents: { path: "/local-agents", labelKey: ONBOARDING_I18N_KEYS.goToLocalAgents },
  database_validation: { path: "/local-agents", labelKey: ONBOARDING_I18N_KEYS.goToDatabaseValidation },
  contract_intelligence: { path: "/incidents", labelKey: ONBOARDING_I18N_KEYS.generateContractIntelligence },
  reporting: { path: "/incidents", labelKey: ONBOARDING_I18N_KEYS.goToIncidents },
};

const COMPLETED_NAVIGATION_BY_CATEGORY = {
  repository: { path: "/integrations", labelKey: ONBOARDING_I18N_KEYS.manageIntegration },
  testing: { path: "/catalog", labelKey: ONBOARDING_I18N_KEYS.openCatalog },
  browser_monitoring: { path: "/browser-watch", labelKey: ONBOARDING_I18N_KEYS.manageBrowserWatch },
  environments: { path: "/projects", labelKey: ONBOARDING_I18N_KEYS.manageEnvironments },
  agents: { path: "/local-agents", labelKey: ONBOARDING_I18N_KEYS.manageLocalAgents },
  database_validation: { path: "/local-agents#database-connections", labelKey: ONBOARDING_I18N_KEYS.manageDatabaseValidation },
  contract_intelligence: { path: "/incidents", labelKey: ONBOARDING_I18N_KEYS.viewContractIntelligence },
  reporting: { path: "/incidents", labelKey: ONBOARDING_I18N_KEYS.viewExecutiveReports },
};

export function readinessLabelKey(level) {
  return READINESS_LABEL_KEY[String(level || "NOT_READY").toUpperCase()] || READINESS_LABEL_KEY.NOT_READY;
}

export function readinessBadgeClass(level) {
  return READINESS_BADGE[String(level || "NOT_READY").toUpperCase()] || "badge badge-gray";
}

export function stepStatusLabelKey(status) {
  return STATUS_LABEL_KEY[String(status || "NOT_STARTED").toUpperCase()] || STATUS_LABEL_KEY.NOT_STARTED;
}

export function stepStatusBadgeClass(status) {
  return STATUS_BADGE[String(status || "NOT_STARTED").toUpperCase()] || "badge badge-gray";
}

export function stepStatusIcon(status) {
  const key = String(status || "NOT_STARTED").toUpperCase();
  if (key === "COMPLETED") return "✓";
  if (key === "IN_PROGRESS") return "◐";
  if (key === "BLOCKED") return "✕";
  return "○";
}

export function navigationForStep(step, options = {}) {
  const category = String(step?.category || "");
  const status = String(step?.status || "NOT_STARTED").toUpperCase();
  const projectId = String(options.projectId || step?.project_id || "").trim();

  if (category === "testing") {
    if (status === "NOT_STARTED") {
      return { path: "/generate", labelKey: ONBOARDING_I18N_KEYS.goToGenerate };
    }
    if (status === "COMPLETED") {
      return { path: "/catalog", labelKey: ONBOARDING_I18N_KEYS.openCatalog };
    }
    return INCOMPLETE_NAVIGATION_BY_CATEGORY.testing;
  }

  if (category === "environments" && status === "COMPLETED") {
    const path = projectId
      ? `/projects?edit=${encodeURIComponent(projectId)}`
      : COMPLETED_NAVIGATION_BY_CATEGORY.environments.path;
    return { path, labelKey: ONBOARDING_I18N_KEYS.manageEnvironments };
  }

  const navMap = status === "COMPLETED"
    ? COMPLETED_NAVIGATION_BY_CATEGORY
    : INCOMPLETE_NAVIGATION_BY_CATEGORY;

  return navMap[category] || null;
}

export function isOptionalOnboardingStep(step) {
  return String(step?.step_id || "") === CONTRACT_INTELLIGENCE_STEP_ID;
}

export function isCoreOnboardingComplete(checklist) {
  const steps = checklist?.steps || [];
  if (!steps.length) return false;
  const coreSteps = steps.filter((step) => CORE_ONBOARDING_STEP_IDS.has(step.step_id));
  if (!coreSteps.length) return false;
  return coreSteps.every((step) => String(step.status || "").toUpperCase() === "COMPLETED");
}

/** Dashboard fold: collapse onboarding checklist when setup is nearly complete. */
export const ONBOARDING_DASHBOARD_COLLAPSE_THRESHOLD = 88;

export function shouldCollapseOnboardingDashboard(checklist, onboardingVm) {
  if (!checklist) return false;
  if (onboardingVm?.isComplete) return true;
  const pct = Number(checklist.overall_completion) || 0;
  if (pct >= ONBOARDING_DASHBOARD_COLLAPSE_THRESHOLD) return true;
  return Boolean(onboardingVm?.coreSetupComplete);
}

function contractIntelligenceGuidanceKey(status) {
  const key = String(status || "NOT_STARTED").toUpperCase();
  if (key === "IN_PROGRESS") return ONBOARDING_I18N_KEYS.contractIntelligenceGuidanceInProgress;
  if (key === "COMPLETED") return ONBOARDING_I18N_KEYS.contractIntelligenceGuidanceCompleted;
  return ONBOARDING_I18N_KEYS.contractIntelligenceGuidanceNotStarted;
}

export function localizeOnboardingStep(step, t) {
  if (!isOptionalOnboardingStep(step)) return step;

  const status = String(step.status || "NOT_STARTED").toUpperCase();
  const guidanceKey = contractIntelligenceGuidanceKey(status);

  return {
    ...step,
    title: t(ONBOARDING_I18N_KEYS.contractIntelligenceTitle),
    description: t(ONBOARDING_I18N_KEYS.contractIntelligenceDescription),
    guidanceText: t(guidanceKey),
    isOptional: true,
    optionalLabel: t(ONBOARDING_I18N_KEYS.optionalCapability),
  };
}

function localizeNextRecommendedStep(title, t) {
  const normalized = String(title || "").trim().toLowerCase();
  if (
    normalized === "configure contract intelligence"
    || normalized === "generate contract intelligence"
  ) {
    return t(ONBOARDING_I18N_KEYS.contractIntelligenceTitle);
  }
  return title;
}

export function buildOnboardingViewModel(checklist, t, options = {}) {
  const projectId = String(options.projectId || checklist?.project_id || "").trim();

  const steps = (checklist?.steps || []).map((step) => {
    const localized = localizeOnboardingStep(step, t);
    const nav = navigationForStep(localized, { projectId });
    return {
      ...localized,
      statusLabel: t(stepStatusLabelKey(localized.status)),
      statusBadgeClass: stepStatusBadgeClass(localized.status),
      statusIcon: stepStatusIcon(localized.status),
      navigation: nav
        ? {
            path: nav.path,
            label: t(nav.labelKey),
          }
        : null,
    };
  });

  const isComplete = isOnboardingComplete(checklist);
  const coreSetupComplete = isCoreOnboardingComplete(checklist) && !isComplete;
  const completionDateText = resolveOnboardingCompletionDate(checklist);

  return {
    show: Boolean(checklist),
    isComplete,
    title: t(ONBOARDING_I18N_KEYS.title),
    projectReadinessLabel: t(ONBOARDING_I18N_KEYS.projectReadiness),
    completionLabel: t(ONBOARDING_I18N_KEYS.completion),
    nextRecommendedStepLabel: t(ONBOARDING_I18N_KEYS.nextRecommendedStep),
    checklistLabel: t(ONBOARDING_I18N_KEYS.checklist),
    readOnlyNote: t(ONBOARDING_I18N_KEYS.readOnlyNote),
    coreSetupComplete,
    coreSetupCompleteLabel: coreSetupComplete ? t(ONBOARDING_I18N_KEYS.coreSetupComplete) : "",
    platformOperationalNote: coreSetupComplete ? t(ONBOARDING_I18N_KEYS.platformOperationalNote) : "",
    expandChecklistLabel: t(ONBOARDING_I18N_KEYS.expandChecklist),
    collapseChecklistLabel: t(ONBOARDING_I18N_KEYS.collapseChecklist),
    operational: isComplete && checklist
      ? {
          title: t(ONBOARDING_I18N_KEYS.operationalTitle),
          subtitle: t(ONBOARDING_I18N_KEYS.operationalSubtitle),
          completionDateLabel: t(ONBOARDING_I18N_KEYS.operationalCompletionDate),
          completionDateText: completionDateText || "—",
          stepsCompletedLabel: t(ONBOARDING_I18N_KEYS.operationalStepsCompleted),
          stepsCompletedText: `${checklist.completed_steps}/${checklist.total_steps}`,
          readinessLabel: t(ONBOARDING_I18N_KEYS.operationalReadiness),
          readinessLevelLabel: t(readinessLabelKey(checklist.readiness_level)),
          readinessBadgeClass: readinessBadgeClass(checklist.readiness_level),
          projectLabel: t(ONBOARDING_I18N_KEYS.operationalProject),
          viewSetupDetailsLabel: t(ONBOARDING_I18N_KEYS.viewSetupDetails),
          hideSetupDetailsLabel: t(ONBOARDING_I18N_KEYS.hideSetupDetails),
        }
      : null,
    checklist: checklist
      ? {
          ...checklist,
          readinessLabel: t(readinessLabelKey(checklist.readiness_level)),
          readinessBadgeClass: readinessBadgeClass(checklist.readiness_level),
          completedLabel: `${checklist.completed_steps} / ${checklist.total_steps} ${t(ONBOARDING_I18N_KEYS.completed)}`,
          next_recommended_step: localizeNextRecommendedStep(checklist.next_recommended_step, t),
          steps,
        }
      : null,
  };
}
