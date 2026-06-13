/** UX-01 — Consistent capability availability states (presentation only). */

export const CAPABILITY_STATE = {
  AVAILABLE: "AVAILABLE",
  INTEGRATION_REQUIRED: "INTEGRATION_REQUIRED",
  INSUFFICIENT_HISTORY: "INSUFFICIENT_HISTORY",
};

export const CAPABILITY_STATE_I18N_KEYS = {
  integrationRequiredTitle: "capability_state.integration_required_title",
  integrationRequiredDesc: "capability_state.integration_required_desc",
  integrationBenefits: "capability_state.integration_benefits",
  connectCta: "capability_state.connect_cta",
  insufficientHistoryTitle: "capability_state.insufficient_history_title",
  insufficientHistoryDesc: "capability_state.insufficient_history_desc",
  insufficientHistoryStatus: "capability_state.insufficient_history_status",
  runFirstTestCta: "capability_state.run_first_test_cta",
  investigateCta: "capability_state.investigate_cta",
  qmetryTitle: "capability_state.qmetry.title",
  qmetryIntegration: "capability_state.qmetry.integration",
  qmetryBenefit1: "capability_state.qmetry.benefit_1",
  qmetryBenefit2: "capability_state.qmetry.benefit_2",
  qmetryBenefit3: "capability_state.qmetry.benefit_3",
  qmetryBenefit4: "capability_state.qmetry.benefit_4",
  servicenowTitle: "capability_state.servicenow.title",
  servicenowIntegration: "capability_state.servicenow.integration",
  servicenowBenefit1: "capability_state.servicenow.benefit_1",
  servicenowBenefit2: "capability_state.servicenow.benefit_2",
  servicenowBenefit3: "capability_state.servicenow.benefit_3",
  jiraTitle: "capability_state.jira.title",
  jiraIntegration: "capability_state.jira.integration",
  jiraBenefit1: "capability_state.jira.benefit_1",
  jiraBenefit2: "capability_state.jira.benefit_2",
  jiraBenefit3: "capability_state.jira.benefit_3",
  trendsTitle: "capability_state.trends.title",
  failuresTitle: "capability_state.failures.title",
  coverageDistributionTitle: "capability_state.coverage_distribution.title",
  riskAssessmentTitle: "capability_state.risk_assessment.title",
  executiveImpactTitle: "capability_state.executive_impact.title",
  businessRiskTitle: "capability_state.business_risk.title",
};

export const DEFAULT_MIN_RUNS_FOR_TRENDS = 2;
export const DEFAULT_MIN_RUNS_FOR_FAILURE_INTEL = 5;

const INTEGRATION_CTA = { path: "/integrations", labelKey: CAPABILITY_STATE_I18N_KEYS.connectCta };
const RUNS_CTA = { path: "/runs", labelKey: CAPABILITY_STATE_I18N_KEYS.runFirstTestCta };
const INCIDENTS_CTA = { path: "/incidents", labelKey: CAPABILITY_STATE_I18N_KEYS.investigateCta };

export function isCapabilityAvailable(state) {
  return state?.state === CAPABILITY_STATE.AVAILABLE;
}

export function buildIntegrationRequiredState({
  capabilityTitle,
  integrationName,
  benefits = [],
  cta = INTEGRATION_CTA,
  t,
}) {
  return {
    state: CAPABILITY_STATE.INTEGRATION_REQUIRED,
    icon: "🔒",
    title: capabilityTitle,
    description: t(CAPABILITY_STATE_I18N_KEYS.integrationRequiredDesc, { integration: integrationName }),
    benefits,
    statusLabel: null,
    statusValue: null,
    cta: {
      path: cta.path,
      label: t(cta.labelKey, { integration: integrationName }),
    },
  };
}

export function buildInsufficientHistoryState({
  capabilityTitle,
  minRuns,
  currentRuns = 0,
  cta = RUNS_CTA,
  t,
}) {
  return {
    state: CAPABILITY_STATE.INSUFFICIENT_HISTORY,
    icon: "⏳",
    title: capabilityTitle,
    description: t(CAPABILITY_STATE_I18N_KEYS.insufficientHistoryDesc, { count: minRuns }),
    benefits: [],
    statusLabel: t(CAPABILITY_STATE_I18N_KEYS.insufficientHistoryStatus),
    statusValue: t("capability_state.runs_registered", { count: currentRuns }),
    cta: {
      path: cta.path,
      label: t(cta.labelKey),
    },
  };
}

export function qmetryBenefits(t) {
  return [
    t(CAPABILITY_STATE_I18N_KEYS.qmetryBenefit1),
    t(CAPABILITY_STATE_I18N_KEYS.qmetryBenefit2),
    t(CAPABILITY_STATE_I18N_KEYS.qmetryBenefit3),
    t(CAPABILITY_STATE_I18N_KEYS.qmetryBenefit4),
  ];
}

export function servicenowBenefits(t) {
  return [
    t(CAPABILITY_STATE_I18N_KEYS.servicenowBenefit1),
    t(CAPABILITY_STATE_I18N_KEYS.servicenowBenefit2),
    t(CAPABILITY_STATE_I18N_KEYS.servicenowBenefit3),
  ];
}

export function jiraBenefits(t) {
  return [
    t(CAPABILITY_STATE_I18N_KEYS.jiraBenefit1),
    t(CAPABILITY_STATE_I18N_KEYS.jiraBenefit2),
    t(CAPABILITY_STATE_I18N_KEYS.jiraBenefit3),
  ];
}

export function resolveQMetryCapabilityState({
  connected,
  totalTestCases = 0,
  totalMatches = 0,
  totalRecommendations = 0,
  title,
  t,
}) {
  const capabilityTitle = title || t(CAPABILITY_STATE_I18N_KEYS.qmetryTitle);
  const integrationName = t(CAPABILITY_STATE_I18N_KEYS.qmetryIntegration);

  if (!connected) {
    return buildIntegrationRequiredState({
      capabilityTitle,
      integrationName,
      benefits: qmetryBenefits(t),
      t,
    });
  }

  const hasData = totalTestCases > 0 || totalMatches > 0 || totalRecommendations > 0;
  if (!hasData) {
    return buildInsufficientHistoryState({
      capabilityTitle,
      minRuns: DEFAULT_MIN_RUNS_FOR_FAILURE_INTEL,
      currentRuns: 0,
      cta: INTEGRATION_CTA,
      t,
    });
  }

  return { state: CAPABILITY_STATE.AVAILABLE };
}

export function resolveServiceNowCapabilityState({ connected, correlationCount = 0, title, t }) {
  const capabilityTitle = title || t(CAPABILITY_STATE_I18N_KEYS.servicenowTitle);
  const integrationName = t(CAPABILITY_STATE_I18N_KEYS.servicenowIntegration);

  if (!connected) {
    return buildIntegrationRequiredState({
      capabilityTitle,
      integrationName,
      benefits: servicenowBenefits(t),
      t,
    });
  }

  if (correlationCount === 0) {
    return buildInsufficientHistoryState({
      capabilityTitle,
      minRuns: DEFAULT_MIN_RUNS_FOR_FAILURE_INTEL,
      currentRuns: 0,
      cta: INCIDENTS_CTA,
      t,
    });
  }

  return { state: CAPABILITY_STATE.AVAILABLE };
}

export function resolveJiraCapabilityState({ connected, issueCount = 0, title, t }) {
  const capabilityTitle = title || t(CAPABILITY_STATE_I18N_KEYS.jiraTitle);
  const integrationName = t(CAPABILITY_STATE_I18N_KEYS.jiraIntegration);

  if (!connected) {
    return buildIntegrationRequiredState({
      capabilityTitle,
      integrationName,
      benefits: jiraBenefits(t),
      t,
    });
  }

  if (issueCount === 0) {
    return buildInsufficientHistoryState({
      capabilityTitle,
      minRuns: 1,
      currentRuns: 0,
      cta: INTEGRATION_CTA,
      t,
    });
  }

  return { state: CAPABILITY_STATE.AVAILABLE };
}

export function resolveHistoryCapabilityState({
  runCount = 0,
  minRuns = DEFAULT_MIN_RUNS_FOR_TRENDS,
  title,
  cta = RUNS_CTA,
  t,
}) {
  const capabilityTitle = title || t(CAPABILITY_STATE_I18N_KEYS.trendsTitle);

  if (runCount < minRuns) {
    return buildInsufficientHistoryState({
      capabilityTitle,
      minRuns,
      currentRuns: runCount,
      cta,
      t,
    });
  }

  return { state: CAPABILITY_STATE.AVAILABLE };
}

export function attachCapabilityPresentation(vm, capabilityState) {
  const available = isCapabilityAvailable(capabilityState);
  return {
    ...vm,
    capabilityState,
    empty: !available,
    showContent: available,
  };
}
