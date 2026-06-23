/** V1.1 — Executive Brief rollup from existing dashboard intelligence (no new scoring). */

import { localizeBackendMessage } from "./localizeBackendMessage.js";

export const EXECUTIVE_BRIEF_I18N_KEYS = {
  title: "executive_brief.title",
  releaseStatus: "executive_brief.release_status",
  topRisks: "executive_brief.top_risks",
  trendDirection: "executive_brief.trend_direction",
  recommendedAction: "executive_brief.recommended_action",
  empty: "executive_brief.empty",
  integrationRequired: "executive_brief.integration_required",
  insufficientHistory: "executive_brief.insufficient_history",
  readOnlyNote: "executive_brief.read_only_note",
  noRisks: "executive_brief.no_risks",
  actionInvestigate: "executive_brief.action.investigate",
  actionIntegrations: "executive_brief.action.integrations",
  actionReviewReadiness: "executive_brief.action.review_readiness",
  actionMonitor: "executive_brief.action.monitor",
};

const TREND_LABEL_KEY = {
  IMPROVING: "executive_impact.direction.improving",
  STABLE: "executive_impact.direction.stable",
  DEGRADING: "executive_impact.direction.degrading",
  UNKNOWN: "executive_impact.direction.unknown",
};

const TREND_BADGE = {
  IMPROVING: "badge badge-green",
  STABLE: "badge badge-blue",
  DEGRADING: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

function findIncompleteIntegrationStep(onboardingVm) {
  return (onboardingVm?.checklist?.steps || []).find(
    (step) => step.status !== "COMPLETED"
      && step.navigation?.path === "/integrations",
  );
}

function resolveBriefReleaseStatus({
  hasRelease,
  releaseReadinessVm,
  onboardingVm,
  totalRuns,
  t,
}) {
  if (hasRelease) {
    return {
      text: releaseReadinessVm.overallStatusText,
      badgeClass: releaseReadinessVm.overallStatusBadgeClass,
    };
  }

  if (findIncompleteIntegrationStep(onboardingVm)) {
    return {
      text: t(EXECUTIVE_BRIEF_I18N_KEYS.integrationRequired),
      badgeClass: "badge badge-orange",
    };
  }

  if ((totalRuns ?? 0) === 0 || releaseReadinessVm?.empty) {
    return {
      text: t(EXECUTIVE_BRIEF_I18N_KEYS.insufficientHistory),
      badgeClass: "badge badge-gray",
    };
  }

  return {
    text: t(EXECUTIVE_BRIEF_I18N_KEYS.insufficientHistory),
    badgeClass: "badge badge-gray",
  };
}

function resolveBriefTrend({
  trend,
  totalRuns,
  executiveImpactVm,
  t,
}) {
  if (trend !== "UNKNOWN") {
    return {
      text: t(TREND_LABEL_KEY[trend] || TREND_LABEL_KEY.UNKNOWN),
      badgeClass: TREND_BADGE[trend] || TREND_BADGE.UNKNOWN,
    };
  }

  if (
    (totalRuns ?? 0) === 0
    || executiveImpactVm?.capabilityState?.state === "INSUFFICIENT_HISTORY"
    || executiveImpactVm?.hasSufficientHistory === false
  ) {
    return {
      text: t(EXECUTIVE_BRIEF_I18N_KEYS.insufficientHistory),
      badgeClass: "badge badge-gray",
    };
  }

  return {
    text: t(EXECUTIVE_BRIEF_I18N_KEYS.insufficientHistory),
    badgeClass: "badge badge-gray",
  };
}

function collectTopRisks({ businessRiskVm, platformObservabilityVm, releaseReadinessVm, t }) {
  const risks = [];
  const seen = new Set();

  const add = (text) => {
    const localized = t ? localizeBackendMessage(text, t) : text;
    const key = String(localized || "").trim().toLowerCase();
    if (!key || seen.has(key)) return;
    seen.add(key);
    risks.push(localized);
  };

  (businessRiskVm?.businessRisks || []).slice(0, 2).forEach((r) => add(r.title || r.capability));
  (platformObservabilityVm?.topPlatformRisks || []).forEach(add);
  (releaseReadinessVm?.data_gaps || []).slice(0, 2).forEach(add);

  return risks.slice(0, 3);
}

function resolveTrend(executiveImpactVm, valueDashboardVm) {
  const impactMetric = executiveImpactVm?.qualityMetrics?.[0];
  if (impactMetric?.direction && impactMetric.direction !== "UNKNOWN") {
    return String(impactMetric.direction).toUpperCase();
  }
  const trend = String(valueDashboardVm?.qualityTrend || "UNKNOWN").toUpperCase();
  if (trend === "IMPROVING" || trend === "DEGRADING" || trend === "STABLE") return trend;
  return "UNKNOWN";
}

function resolveRecommendedAction({
  releaseReadinessVm,
  onboardingVm,
  businessRiskVm,
  hasKnowledge,
  totalRuns,
  fi,
  passRateValid,
  passRateNum,
  t,
}) {
  if (onboardingVm?.isComplete) {
    const { riskLevel } = computeOperationalRisk({
      fi,
      releaseReadinessVm,
      businessRiskVm,
      passRateValid,
      passRateNum,
      totalRuns,
    });
    const criticalRisk =
      String(businessRiskVm?.overallBusinessRisk || "").toUpperCase() === "CRITICAL"
      || riskLevel === "elevated";
    if (criticalRisk) {
      return {
        label: t("command_center.action.investigate_risk"),
        path: "/incidents",
      };
    }
    if (hasKnowledge === false) {
      return {
        label: t("command_center.action.build_memory"),
        path: "/knowledge",
      };
    }
    if ((totalRuns ?? 0) === 0) {
      return {
        label: t("command_center.action.run_smoke_tests"),
        path: "/batch",
      };
    }
    return {
      label: t(EXECUTIVE_BRIEF_I18N_KEYS.actionMonitor),
      path: "/dashboard",
    };
  }

  const incompleteStep = (onboardingVm?.checklist?.steps || []).find(
    (step) => step.status !== "COMPLETED" && step.navigation,
  );
  if (incompleteStep?.navigation) {
    return {
      label: incompleteStep.navigation.label,
      path: incompleteStep.navigation.path,
    };
  }

  const status = String(releaseReadinessVm?.overallStatusText || "UNKNOWN").toUpperCase();
  if (status === "UNKNOWN" || releaseReadinessVm?.empty) {
    return {
      label: t(EXECUTIVE_BRIEF_I18N_KEYS.actionInvestigate),
      path: "/incidents",
    };
  }
  if (status === "BLOCKED" || (businessRiskVm?.overallBusinessRisk === "CRITICAL")) {
    return {
      label: t(EXECUTIVE_BRIEF_I18N_KEYS.actionReviewReadiness),
      path: "/dashboard",
    };
  }
  return {
    label: t(EXECUTIVE_BRIEF_I18N_KEYS.actionMonitor),
    path: "/dashboard",
  };
}

function computeOperationalRisk({ fi, businessRiskVm, passRateValid, passRateNum, totalRuns }) {
  const riskLevel =
    (fi?.recurrent_regressions_count ?? 0) > 0
      ? "elevated"
      : passRateValid && passRateNum < 70
        ? "watch"
        : (totalRuns ?? 0) === 0
          ? "unknown"
          : "stable";
  return { riskLevel };
}

export function buildExecutiveBriefViewModel({
  releaseReadinessVm,
  businessRiskVm,
  executiveImpactVm,
  valueDashboardVm,
  platformObservabilityVm,
  onboardingVm,
  hasKnowledge,
  totalRuns,
  fi,
  passRateValid,
  passRateNum,
  t,
}) {
  const hasRelease = releaseReadinessVm?.show && !releaseReadinessVm?.empty;
  const topRisks = collectTopRisks({ businessRiskVm, platformObservabilityVm, releaseReadinessVm, t });
  const trend = resolveTrend(executiveImpactVm, valueDashboardVm);
  const releaseStatus = resolveBriefReleaseStatus({
    hasRelease,
    releaseReadinessVm,
    onboardingVm,
    totalRuns,
    t,
  });
  const trendDisplay = resolveBriefTrend({
    trend,
    totalRuns,
    executiveImpactVm,
    t,
  });
  const action = resolveRecommendedAction({
    releaseReadinessVm,
    onboardingVm,
    businessRiskVm,
    hasKnowledge,
    totalRuns,
    fi,
    passRateValid,
    passRateNum,
    t,
  });

  const show = Boolean(releaseReadinessVm || businessRiskVm || executiveImpactVm || valueDashboardVm);
  const operationalMode = Boolean(onboardingVm?.isComplete);

  return {
    show,
    operationalMode,
    title: operationalMode
      ? t("command_center.executive_summary")
      : t(EXECUTIVE_BRIEF_I18N_KEYS.title),
    readOnlyNote: t(EXECUTIVE_BRIEF_I18N_KEYS.readOnlyNote),
    releaseStatusLabel: t(EXECUTIVE_BRIEF_I18N_KEYS.releaseStatus),
    releaseStatus: releaseStatus.text,
    releaseStatusBadgeClass: releaseStatus.badgeClass,
    topRisksLabel: t(EXECUTIVE_BRIEF_I18N_KEYS.topRisks),
    topRisks,
    noRisksMessage: t(EXECUTIVE_BRIEF_I18N_KEYS.noRisks),
    trendLabel: t(EXECUTIVE_BRIEF_I18N_KEYS.trendDirection),
    trendDirection: trendDisplay.text,
    trendBadgeClass: trendDisplay.badgeClass,
    actionLabel: t(EXECUTIVE_BRIEF_I18N_KEYS.recommendedAction),
    action,
  };
}
