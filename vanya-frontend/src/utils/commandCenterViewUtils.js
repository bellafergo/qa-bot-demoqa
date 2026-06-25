/** UX-02 — Command Center view model (operational dashboard, frontend-only). */

import { isOnboardingComplete } from "./onboardingViewUtils.js";

export const COMMAND_CENTER_I18N_KEYS = {
  title: "command_center.title",
  headerProject: "command_center.header.project",
  headerEnvironment: "command_center.header.environment",
  headerLastUpdate: "command_center.header.last_update",
  headerGlobalStatus: "command_center.header.global_status",
  systemHealth: "command_center.kpi.system_health",
  memoryCoverage: "command_center.kpi.memory_coverage",
  executionReadiness: "command_center.kpi.execution_readiness",
  currentRisk: "command_center.kpi.current_risk",
  nextActionTitle: "command_center.next_action.title",
  executiveSummary: "command_center.executive_summary",
  actionBuildMemory: "command_center.action.build_memory",
  actionRunSmokeTests: "command_center.action.run_smoke_tests",
  actionInvestigateRisk: "command_center.action.investigate_risk",
  actionNoAction: "command_center.action.no_action_required",
  statusHealthy: "command_center.status.healthy",
  statusPending: "command_center.status.pending",
  statusNoRuns: "command_center.status.no_runs",
  statusNotEvaluated: "command_center.status.not_evaluated",
  statusAtRisk: "command_center.status.at_risk",
  statusActive: "command_center.status.active",
  statusStable: "command_center.status.stable",
  statusWatch: "command_center.status.watch",
  statusElevated: "command_center.status.elevated",
};

const KPI_STATUS_TONE = {
  healthy: { border: "#22c55e", dot: "#22c55e" },
  pending: { border: "#f59e0b", dot: "#f59e0b" },
  neutral: { border: "#94a3b8", dot: "#94a3b8" },
  danger: { border: "#ef4444", dot: "#ef4444" },
};

/** Shared health context (mirrors ProjectHealthStrip heuristics). */
export function computeCommandCenterHealthContext({
  summary,
  fi,
  hasKnowledge,
  passRateValid,
  passRateNum,
}) {
  const runs = summary?.total_runs ?? 0;
  let variant = "neutral";
  if (runs === 0) variant = "empty";
  else if (passRateValid && passRateNum < 60 && runs >= 3) variant = "danger";
  else if (passRateValid && passRateNum >= 80) variant = "good";

  const riskLevel =
    (fi?.recurrent_regressions_count ?? 0) > 0
      ? "elevated"
      : summary?.pass_rate != null && summary.pass_rate < 70
        ? "watch"
        : runs === 0
          ? "unknown"
          : "stable";

  return { runs, variant, riskLevel };
}

function resolveEnvironmentLabel(project, t) {
  const envs = project?.settings?.environments;
  if (Array.isArray(envs) && envs.length > 0) {
    const name = envs[0]?.name || envs[0]?.label;
    if (name) return String(name);
  }
  return t("common.production");
}

function resolveGlobalStatus(onboardingChecklist, healthVariant, t) {
  if (onboardingChecklist && isOnboardingComplete(onboardingChecklist)) {
    const level = String(onboardingChecklist.readiness_level || "").toUpperCase();
    if (level === "FULLY_OPERATIONAL") return t("onboarding.readiness.fully_operational");
    if (level === "READY") return t("onboarding.readiness.ready");
  }
  if (healthVariant === "good") return t(COMMAND_CENTER_I18N_KEYS.statusHealthy);
  if (healthVariant === "empty") return t(COMMAND_CENTER_I18N_KEYS.statusPending);
  if (healthVariant === "danger") return t(COMMAND_CENTER_I18N_KEYS.statusAtRisk);
  return t("health.ok");
}

export function buildCommandCenterHeaderViewModel({
  project,
  onboardingChecklist,
  summary,
  fi,
  passRateValid,
  passRateNum,
  lastRefresh,
  t,
  formatTimestamp,
}) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  const { variant } = computeCommandCenterHealthContext({
    summary,
    fi,
    hasKnowledge: null,
    passRateValid,
    passRateNum,
  });

  return {
    projectLabel: t(COMMAND_CENTER_I18N_KEYS.headerProject),
    projectName: project?.name || "—",
    environmentLabel: t(COMMAND_CENTER_I18N_KEYS.headerEnvironment),
    environment: resolveEnvironmentLabel(project, t),
    lastUpdateLabel: t(COMMAND_CENTER_I18N_KEYS.headerLastUpdate),
    lastUpdate: lastRefresh ? fmt(lastRefresh.toISOString?.() || lastRefresh) : "—",
    globalStatusLabel: t(COMMAND_CENTER_I18N_KEYS.headerGlobalStatus),
    globalStatus: resolveGlobalStatus(onboardingChecklist, variant, t),
  };
}

export function buildCommandCenterViewModel({
  summary,
  fi,
  hasKnowledge,
  passRateValid,
  passRateNum,
  loading,
  t,
}) {
  const { runs, variant, riskLevel } = computeCommandCenterHealthContext({
    summary,
    fi,
    hasKnowledge,
    passRateValid,
    passRateNum,
  });

  const systemHealthValue =
    loading
      ? "…"
      : variant === "good"
        ? t(COMMAND_CENTER_I18N_KEYS.statusHealthy)
        : variant === "empty"
          ? t(COMMAND_CENTER_I18N_KEYS.statusPending)
          : variant === "danger"
            ? t(COMMAND_CENTER_I18N_KEYS.statusAtRisk)
            : t("health.ok");

  const memoryValue = loading
    ? "…"
    : hasKnowledge === true
      ? t("health.memory.ready")
      : hasKnowledge === false
        ? t(COMMAND_CENTER_I18N_KEYS.statusPending)
        : t("health.memory.unknown");

  const executionValue = loading
    ? "…"
    : runs > 0
      ? t(COMMAND_CENTER_I18N_KEYS.statusActive)
      : t(COMMAND_CENTER_I18N_KEYS.statusNoRuns);

  const riskValue = loading
    ? "…"
    : riskLevel === "elevated"
      ? t(COMMAND_CENTER_I18N_KEYS.statusElevated)
      : riskLevel === "watch"
        ? t(COMMAND_CENTER_I18N_KEYS.statusWatch)
        : riskLevel === "unknown"
          ? t(COMMAND_CENTER_I18N_KEYS.statusNotEvaluated)
          : t(COMMAND_CENTER_I18N_KEYS.statusStable);

  const kpis = [
    {
      id: "system_health",
      label: t(COMMAND_CENTER_I18N_KEYS.systemHealth),
      value: systemHealthValue,
      tone:
        variant === "good"
          ? "healthy"
          : variant === "empty"
            ? "pending"
            : variant === "danger"
              ? "danger"
              : "neutral",
    },
    {
      id: "memory_coverage",
      label: t(COMMAND_CENTER_I18N_KEYS.memoryCoverage),
      value: memoryValue,
      tone: hasKnowledge === true ? "healthy" : hasKnowledge === false ? "pending" : "neutral",
    },
    {
      id: "execution_readiness",
      label: t(COMMAND_CENTER_I18N_KEYS.executionReadiness),
      value: executionValue,
      tone: runs > 0 ? "healthy" : "neutral",
    },
    {
      id: "current_risk",
      label: t(COMMAND_CENTER_I18N_KEYS.currentRisk),
      value: riskValue,
      tone:
        riskLevel === "elevated"
          ? "danger"
          : riskLevel === "watch"
            ? "pending"
            : riskLevel === "unknown"
              ? "neutral"
              : "healthy",
    },
  ].map((kpi) => ({
    ...kpi,
    colors: KPI_STATUS_TONE[kpi.tone] || KPI_STATUS_TONE.neutral,
  }));

  return {
    title: t(COMMAND_CENTER_I18N_KEYS.title),
    loading: Boolean(loading),
    kpis: loading
      ? [{ id: "loading-1" }, { id: "loading-2" }, { id: "loading-3" }, { id: "loading-4" }]
      : kpis,
  };
}

/** Operational next action — no onboarding language when project is complete. */
export function buildNextRecommendedActionViewModel({
  hasKnowledge,
  summary,
  fi,
  passRateValid,
  passRateNum,
  businessRiskVm,
  loading,
  t,
}) {
  const { runs, riskLevel } = computeCommandCenterHealthContext({
    summary,
    fi,
    hasKnowledge,
    passRateValid,
    passRateNum,
  });

  const criticalRisk =
    String(businessRiskVm?.overallBusinessRisk || "").toUpperCase() === "CRITICAL"
    || riskLevel === "elevated";

  let action = null;
  let message = t(COMMAND_CENTER_I18N_KEYS.actionNoAction);

  if (!loading) {
    if (criticalRisk) {
      action = { label: t(COMMAND_CENTER_I18N_KEYS.actionInvestigateRisk), path: "/incidents" };
      message = action.label;
    } else if (hasKnowledge === false) {
      action = { label: t(COMMAND_CENTER_I18N_KEYS.actionBuildMemory), path: "/knowledge" };
      message = action.label;
    } else if (runs === 0) {
      action = { label: t(COMMAND_CENTER_I18N_KEYS.actionRunSmokeTests), path: "/batch" };
      message = action.label;
    }
  }

  return {
    title: t(COMMAND_CENTER_I18N_KEYS.nextActionTitle),
    message: loading ? "…" : message,
    action,
    noActionRequired: !loading && !action,
  };
}
