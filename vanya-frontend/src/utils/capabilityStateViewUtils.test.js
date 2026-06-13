import { describe, it, expect } from "vitest";
import {
  CAPABILITY_STATE,
  CAPABILITY_STATE_I18N_KEYS,
  DEFAULT_MIN_RUNS_FOR_TRENDS,
  DEFAULT_MIN_RUNS_FOR_FAILURE_INTEL,
  attachCapabilityPresentation,
  buildIntegrationRequiredState,
  buildInsufficientHistoryState,
  isCapabilityAvailable,
  resolveHistoryCapabilityState,
  resolveJiraCapabilityState,
  resolveQMetryCapabilityState,
  resolveServiceNowCapabilityState,
  buildQMetryIntegrationGroupState,
  isCapabilityGated,
  shouldConsolidateQMetryIntegration,
} from "./capabilityStateViewUtils.js";

const t = (key, vars) => {
  if (vars) {
    return `${key}:${JSON.stringify(vars)}`;
  }
  return key;
};

describe("capabilityStateViewUtils", () => {
  it("identifies available state", () => {
    expect(isCapabilityAvailable({ state: CAPABILITY_STATE.AVAILABLE })).toBe(true);
    expect(isCapabilityAvailable({ state: CAPABILITY_STATE.INTEGRATION_REQUIRED })).toBe(false);
  });

  it("builds integration required state with benefits and CTA", () => {
    const state = buildIntegrationRequiredState({
      capabilityTitle: "Coverage",
      integrationName: "QMetry",
      benefits: ["benefit-1"],
      t,
    });
    expect(state.state).toBe(CAPABILITY_STATE.INTEGRATION_REQUIRED);
    expect(state.icon).toBe("🔒");
    expect(state.title).toBe("Coverage");
    expect(state.benefits).toEqual(["benefit-1"]);
    expect(state.cta.path).toBe("/integrations");
  });

  it("builds insufficient history state with run count", () => {
    const state = buildInsufficientHistoryState({
      capabilityTitle: "Trends",
      minRuns: 5,
      currentRuns: 1,
      t,
    });
    expect(state.state).toBe(CAPABILITY_STATE.INSUFFICIENT_HISTORY);
    expect(state.icon).toBe("⏳");
    expect(state.statusLabel).toBe(CAPABILITY_STATE_I18N_KEYS.insufficientHistoryStatus);
    expect(state.cta.path).toBe("/runs");
  });

  it("resolves QMetry integration required when disconnected", () => {
    const state = resolveQMetryCapabilityState({ connected: false, t });
    expect(state.state).toBe(CAPABILITY_STATE.INTEGRATION_REQUIRED);
    expect(state.benefits).toHaveLength(4);
  });

  it("resolves QMetry insufficient history when connected without data", () => {
    const state = resolveQMetryCapabilityState({
      connected: true,
      totalTestCases: 0,
      totalMatches: 0,
      totalRecommendations: 0,
      t,
    });
    expect(state.state).toBe(CAPABILITY_STATE.INSUFFICIENT_HISTORY);
  });

  it("resolves QMetry available when data exists", () => {
    const state = resolveQMetryCapabilityState({
      connected: true,
      totalTestCases: 3,
      t,
    });
    expect(state.state).toBe(CAPABILITY_STATE.AVAILABLE);
  });

  it("resolves ServiceNow integration required when disconnected", () => {
    const state = resolveServiceNowCapabilityState({ connected: false, t });
    expect(state.state).toBe(CAPABILITY_STATE.INTEGRATION_REQUIRED);
  });

  it("resolves ServiceNow insufficient history when connected without correlations", () => {
    const state = resolveServiceNowCapabilityState({ connected: true, correlationCount: 0, t });
    expect(state.state).toBe(CAPABILITY_STATE.INSUFFICIENT_HISTORY);
    expect(state.cta.path).toBe("/incidents");
  });

  it("resolves ServiceNow available when correlations exist", () => {
    const state = resolveServiceNowCapabilityState({ connected: true, correlationCount: 2, t });
    expect(state.state).toBe(CAPABILITY_STATE.AVAILABLE);
  });

  it("resolves Jira integration required when disconnected", () => {
    const state = resolveJiraCapabilityState({ connected: false, t });
    expect(state.state).toBe(CAPABILITY_STATE.INTEGRATION_REQUIRED);
    expect(state.benefits).toHaveLength(4);
  });

  it("resolves Jira insufficient history when connected without correlations", () => {
    const state = resolveJiraCapabilityState({
      connected: true,
      totalIssues: 2,
      correlatedIssues: 0,
      t,
    });
    expect(state.state).toBe(CAPABILITY_STATE.INSUFFICIENT_HISTORY);
    expect(state.cta.path).toBe("/runs");
    expect(state.description).toBe(CAPABILITY_STATE_I18N_KEYS.jiraInsufficientHistoryDesc);
  });

  it("resolves history capability insufficient below minimum runs", () => {
    const state = resolveHistoryCapabilityState({
      runCount: 1,
      minRuns: DEFAULT_MIN_RUNS_FOR_TRENDS,
      t,
    });
    expect(state.state).toBe(CAPABILITY_STATE.INSUFFICIENT_HISTORY);
  });

  it("resolves history capability available at minimum runs", () => {
    const state = resolveHistoryCapabilityState({
      runCount: DEFAULT_MIN_RUNS_FOR_TRENDS,
      minRuns: DEFAULT_MIN_RUNS_FOR_TRENDS,
      t,
    });
    expect(state.state).toBe(CAPABILITY_STATE.AVAILABLE);
  });

  it("uses failure intel minimum for QMetry empty connected state", () => {
    const state = resolveQMetryCapabilityState({
      connected: true,
      totalRecommendations: 0,
      t,
    });
    expect(state.description).toContain(String(DEFAULT_MIN_RUNS_FOR_FAILURE_INTEL));
  });

  it("attaches capability presentation flags", () => {
    const gated = attachCapabilityPresentation({ title: "X" }, {
      state: CAPABILITY_STATE.INTEGRATION_REQUIRED,
      icon: "🔒",
      title: "X",
      description: "desc",
      benefits: [],
      cta: { path: "/integrations", label: "Connect" },
    });
    expect(gated.empty).toBe(true);
    expect(gated.showContent).toBe(false);
    expect(gated.capabilityState.state).toBe(CAPABILITY_STATE.INTEGRATION_REQUIRED);

    const available = attachCapabilityPresentation({ title: "Y" }, { state: CAPABILITY_STATE.AVAILABLE });
    expect(available.empty).toBe(false);
    expect(available.showContent).toBe(true);
  });

  it("builds consolidated QMetry integration group state", () => {
    const state = buildQMetryIntegrationGroupState(t);
    expect(state.state).toBe(CAPABILITY_STATE.INTEGRATION_REQUIRED);
    expect(state.title).toBe(CAPABILITY_STATE_I18N_KEYS.qmetryGroupTitle);
    expect(state.benefits).toHaveLength(4);
    expect(state.description).toBeNull();
    expect(state.benefitsLabel).toBe(CAPABILITY_STATE_I18N_KEYS.qmetryGroupUnlocks);
  });

  it("detects QMetry consolidation and gated sections", () => {
    const gatedVm = {
      show: true,
      showContent: false,
      capabilityState: { state: CAPABILITY_STATE.INTEGRATION_REQUIRED },
    };
    const availableVm = {
      show: true,
      showContent: true,
      capabilityState: { state: CAPABILITY_STATE.AVAILABLE },
    };

    expect(shouldConsolidateQMetryIntegration(gatedVm, gatedVm)).toBe(true);
    expect(shouldConsolidateQMetryIntegration(gatedVm, availableVm)).toBe(false);
    expect(isCapabilityGated(gatedVm)).toBe(true);
    expect(isCapabilityGated(availableVm)).toBe(false);
  });
});
