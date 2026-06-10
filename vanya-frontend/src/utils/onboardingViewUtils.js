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
  goToIncidents: "onboarding.action.go_incidents",
  goToProjects: "onboarding.action.go_projects",
  readOnlyNote: "onboarding.read_only_note",
};

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

const NAVIGATION_BY_CATEGORY = {
  repository: { path: "/integrations", labelKey: ONBOARDING_I18N_KEYS.goToRepositories },
  testing: { path: "/catalog", labelKey: ONBOARDING_I18N_KEYS.goToCatalog },
  browser_monitoring: { path: "/browser-watch", labelKey: ONBOARDING_I18N_KEYS.goToBrowserWatches },
  environments: { path: "/projects", labelKey: ONBOARDING_I18N_KEYS.goToProjects },
  agents: { path: "/local-agents", labelKey: ONBOARDING_I18N_KEYS.goToLocalAgents },
  database_validation: { path: "/local-agents", labelKey: ONBOARDING_I18N_KEYS.goToDatabaseValidation },
  contract_intelligence: { path: "/incidents", labelKey: ONBOARDING_I18N_KEYS.goToIncidents },
  reporting: { path: "/incidents", labelKey: ONBOARDING_I18N_KEYS.goToIncidents },
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

export function navigationForStep(step) {
  return NAVIGATION_BY_CATEGORY[String(step?.category || "")] || null;
}

export function buildOnboardingViewModel(checklist, t) {
  const steps = (checklist?.steps || []).map((step) => {
    const nav = navigationForStep(step);
    return {
      ...step,
      statusLabel: t(stepStatusLabelKey(step.status)),
      statusBadgeClass: stepStatusBadgeClass(step.status),
      statusIcon: stepStatusIcon(step.status),
      navigation: nav
        ? {
            path: nav.path,
            label: t(nav.labelKey),
          }
        : null,
    };
  });

  return {
    show: Boolean(checklist),
    title: t(ONBOARDING_I18N_KEYS.title),
    projectReadinessLabel: t(ONBOARDING_I18N_KEYS.projectReadiness),
    completionLabel: t(ONBOARDING_I18N_KEYS.completion),
    nextRecommendedStepLabel: t(ONBOARDING_I18N_KEYS.nextRecommendedStep),
    checklistLabel: t(ONBOARDING_I18N_KEYS.checklist),
    readOnlyNote: t(ONBOARDING_I18N_KEYS.readOnlyNote),
    checklist: checklist
      ? {
          ...checklist,
          readinessLabel: t(readinessLabelKey(checklist.readiness_level)),
          readinessBadgeClass: readinessBadgeClass(checklist.readiness_level),
          completedLabel: `${checklist.completed_steps} / ${checklist.total_steps} ${t(ONBOARDING_I18N_KEYS.completed)}`,
          steps,
        }
      : null,
  };
}
