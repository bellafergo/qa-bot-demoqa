/** Executive Dashboard V2 — narrative view model (frontend-only, no new scoring). */

import { localizeBackendMessage } from "./localizeBackendMessage.js";
import { isInsufficientEvidence } from "./qaInvestigationReportLayoutUtils.js";

export const DASHBOARD_V2_I18N_KEYS = {
  executiveDecision: "dash.v2.executive_decision",
  releaseDecision: "dash.v2.release_decision",
  deploymentRisk: "dash.v2.deployment_risk",
  confidence: "dash.v2.confidence",
  topRecommendation: "dash.v2.top_recommendation",
  whatChanged: "dash.v2.what_changed",
  topRisks: "dash.v2.top_risks",
  topChanges: "dash.v2.top_changes",
  topModules: "dash.v2.top_modules",
  recommendedActions: "dash.v2.recommended_actions",
  systemHealth: "dash.v2.system_health",
  explore: "dash.v2.explore",
  exploreSummary: "dash.v2.explore_summary",
  moreDetails: "dash.v2.more_details",
  decisionGo: "dash.v2.decision.go",
  decisionCaution: "dash.v2.decision.caution",
  decisionStop: "dash.v2.decision.stop",
  decisionPending: "dash.v2.decision.pending",
  riskLow: "dash.v2.risk.low",
  riskMedium: "dash.v2.risk.medium",
  riskHigh: "dash.v2.risk.high",
  riskUnknown: "dash.v2.risk.unknown",
  confidenceInsufficient: "dash.v2.confidence.insufficient",
  cautionDegrading: "dash.v2.caution_degrading",
  cautionInsufficientSignals: "dash.v2.caution_insufficient_signals",
  noRisks: "dash.v2.no_risks",
  noChanges: "dash.v2.no_changes",
  noModules: "dash.v2.no_modules",
  why: "dash.v2.why",
  impact: "dash.v2.impact",
  timeEstimate: "dash.v2.time_estimate",
  healthQualityTrend: "dash.v2.health.quality_trend",
  healthEarlyDegradation: "dash.v2.health.early_degradation",
  healthPlatform: "dash.v2.health.platform",
  healthRelease: "dash.v2.health.release",
  actionSmoke: "dash.v2.action.smoke",
  actionSmokeWhy: "dash.v2.action.smoke_why",
  actionSmokeImpact: "dash.v2.action.smoke_impact",
  actionSmokeTime: "dash.v2.action.smoke_time",
  actionPr: "dash.v2.action.pr",
  actionPrWhy: "dash.v2.action.pr_why",
  actionPrImpact: "dash.v2.action.pr_impact",
  actionPrTime: "dash.v2.action.pr_time",
  actionWatch: "dash.v2.action.watch",
  actionWatchWhy: "dash.v2.action.watch_why",
  actionWatchImpact: "dash.v2.action.watch_impact",
  actionWatchTime: "dash.v2.action.watch_time",
};

const DECISION_BADGE = {
  GO: "badge badge-green",
  CAUTION: "badge badge-orange",
  STOP: "badge badge-red",
  PENDING: "badge badge-gray",
};

const RISK_LEVEL_KEY = {
  LOW: DASHBOARD_V2_I18N_KEYS.riskLow,
  MEDIUM: DASHBOARD_V2_I18N_KEYS.riskMedium,
  HIGH: DASHBOARD_V2_I18N_KEYS.riskHigh,
};

const DECISION_LABEL_KEY = {
  GO: DASHBOARD_V2_I18N_KEYS.decisionGo,
  CAUTION: DASHBOARD_V2_I18N_KEYS.decisionCaution,
  STOP: DASHBOARD_V2_I18N_KEYS.decisionStop,
  PENDING: DASHBOARD_V2_I18N_KEYS.decisionPending,
};

function mapOverallStatusToDecision(status) {
  const s = String(status || "UNKNOWN").toUpperCase();
  if (s === "GO") return "GO";
  if (s === "CAUTION") return "CAUTION";
  if (s === "BLOCKED") return "STOP";
  return "PENDING";
}

function isDegradingTrend(executiveBriefVm, earlyDegradationVm) {
  const trend = String(executiveBriefVm?.trendDirection || "").toLowerCase();
  if (trend.includes("degrad") || trend.includes("degradando")) return true;
  const edStatus = String(
    earlyDegradationVm?.report?.overall_status || earlyDegradationVm?.overall_status || "",
  ).toUpperCase();
  return edStatus && edStatus !== "STABLE";
}

function resolveExecutiveDecision({
  releaseReadinessVm,
  executiveBriefVm,
  earlyDegradationVm,
  totalRuns,
  t,
}) {
  const hasRelease = releaseReadinessVm?.show && !releaseReadinessVm?.empty;
  let decision = hasRelease
    ? mapOverallStatusToDecision(releaseReadinessVm.overallStatus)
    : "PENDING";
  let cautionReason = null;

  if (decision === "GO" && isDegradingTrend(executiveBriefVm, earlyDegradationVm)) {
    decision = "CAUTION";
    cautionReason = t(DASHBOARD_V2_I18N_KEYS.cautionDegrading);
  }

  if (decision === "PENDING" && (totalRuns ?? 0) === 0) {
    cautionReason = t(DASHBOARD_V2_I18N_KEYS.cautionInsufficientSignals);
  }

  if (decision === "GO" && !hasRelease && (totalRuns ?? 0) < 5) {
    decision = "CAUTION";
    cautionReason = t(DASHBOARD_V2_I18N_KEYS.cautionInsufficientSignals);
  }

  return {
    decision,
    decisionLabel: t(DECISION_LABEL_KEY[decision] || DECISION_LABEL_KEY.PENDING),
    decisionBadgeClass: DECISION_BADGE[decision] || DECISION_BADGE.PENDING,
    cautionReason,
  };
}

function resolveDeploymentRisk(releaseReadinessVm, t) {
  const level = String(releaseReadinessVm?.deploymentRisk?.risk_level || "UNKNOWN").toUpperCase();
  const labelKey = RISK_LEVEL_KEY[level] || DASHBOARD_V2_I18N_KEYS.riskUnknown;
  return {
    level: ["LOW", "MEDIUM", "HIGH"].includes(level) ? level : "UNKNOWN",
    label: t(labelKey),
    badgeClass: level === "HIGH" || level === "CRITICAL"
      ? "badge badge-red"
      : level === "MEDIUM"
        ? "badge badge-orange"
        : level === "LOW"
          ? "badge badge-green"
          : "badge badge-gray",
  };
}

function resolveConfidenceDisplay({ executiveRiskBriefVm, releaseReadinessVm, t }) {
  const deploymentConf = releaseReadinessVm?.deploymentRisk?.confidence;
  if (executiveRiskBriefVm?.show && executiveRiskBriefVm?.hasRisk) {
    return {
      show: true,
      label: executiveRiskBriefVm.confidenceText,
      badgeClass: executiveRiskBriefVm.confidenceBadgeClass,
    };
  }
  if (deploymentConf != null && !isInsufficientEvidence(deploymentConf)) {
    const pct = Math.round(Number(deploymentConf) * 100);
    return {
      show: true,
      label: `${pct}%`,
      badgeClass: pct >= 70 ? "badge badge-green" : pct >= 40 ? "badge badge-orange" : "badge badge-gray",
    };
  }
  return {
    show: false,
    label: t(DASHBOARD_V2_I18N_KEYS.confidenceInsufficient),
    badgeClass: "badge badge-gray",
  };
}

function collectTopChanges({ earlyDegradationVm, executiveImpactVm, releaseReadinessVm, t }) {
  const changes = [];
  const seen = new Set();
  const add = (text) => {
    const localized = localizeBackendMessage(text, t);
    const key = String(localized || "").trim().toLowerCase();
    if (!key || seen.has(key)) return;
    seen.add(key);
    changes.push(localized);
  };

  (earlyDegradationVm?.report?.assessments || []).slice(0, 3).forEach((assessment) => {
    add(assessment.scope_name || assessment.recommendedAttention);
  });
  (executiveImpactVm?.topConcerns || []).slice(0, 2).forEach(add);
  if (releaseReadinessVm?.summary) add(releaseReadinessVm.summary);

  return changes.slice(0, 3);
}

function collectTopModules({ businessRiskVm, executiveRiskBriefVm, t }) {
  const modules = [];
  const seen = new Set();
  const add = (text) => {
    const localized = localizeBackendMessage(text, t);
    const key = String(localized || "").trim().toLowerCase();
    if (!key || seen.has(key)) return;
    seen.add(key);
    modules.push(localized);
  };

  if (executiveRiskBriefVm?.module && executiveRiskBriefVm.hasRisk) {
    add(executiveRiskBriefVm.module);
  }
  (businessRiskVm?.businessRisks || []).forEach((r) => add(r.title || r.capability));
  (businessRiskVm?.signals || []).slice(0, 2).forEach((s) => add(s.capability || s.impacted_capability));

  return modules.slice(0, 3);
}

function buildRecommendedActions({
  executiveBriefVm,
  nextRecommendedActionVm,
  executiveRiskBriefVm,
  releaseReadinessVm,
  totalRuns,
  t,
}) {
  const actions = [];
  const seen = new Set();

  const push = (action) => {
    const key = `${action.path}:${action.title}`;
    if (seen.has(key)) return;
    seen.add(key);
    actions.push(action);
  };

  if (executiveBriefVm?.action?.path) {
    push({
      title: executiveBriefVm.action.label,
      path: executiveBriefVm.action.path,
      why: releaseReadinessVm?.summary || t(DASHBOARD_V2_I18N_KEYS.actionSmokeWhy),
      impact: executiveRiskBriefVm?.impact || t(DASHBOARD_V2_I18N_KEYS.actionSmokeImpact),
      timeEstimate: t(DASHBOARD_V2_I18N_KEYS.actionSmokeTime),
    });
  }

  if (nextRecommendedActionVm?.action?.path && actions.length < 3) {
    push({
      title: nextRecommendedActionVm.action.label,
      path: nextRecommendedActionVm.action.path,
      why: nextRecommendedActionVm.message || t(DASHBOARD_V2_I18N_KEYS.actionPrWhy),
      impact: t(DASHBOARD_V2_I18N_KEYS.actionPrImpact),
      timeEstimate: t(DASHBOARD_V2_I18N_KEYS.actionPrTime),
    });
  }

  if (executiveRiskBriefVm?.recommendation && actions.length < 3) {
    push({
      title: executiveRiskBriefVm.recommendation,
      path: "/incidents",
      why: executiveRiskBriefVm.primaryRiskLabel,
      impact: executiveRiskBriefVm.impact || t(DASHBOARD_V2_I18N_KEYS.actionWatchImpact),
      timeEstimate: t(DASHBOARD_V2_I18N_KEYS.actionWatchTime),
    });
  }

  if ((totalRuns ?? 0) > 0 && actions.length < 3) {
    push({
      title: t(DASHBOARD_V2_I18N_KEYS.actionSmoke),
      path: "/batch",
      why: t(DASHBOARD_V2_I18N_KEYS.actionSmokeWhy),
      impact: t(DASHBOARD_V2_I18N_KEYS.actionSmokeImpact),
      timeEstimate: t(DASHBOARD_V2_I18N_KEYS.actionSmokeTime),
    });
  }

  if (actions.length < 3) {
    push({
      title: t(DASHBOARD_V2_I18N_KEYS.actionPr),
      path: "/pr-analysis",
      why: t(DASHBOARD_V2_I18N_KEYS.actionPrWhy),
      impact: t(DASHBOARD_V2_I18N_KEYS.actionPrImpact),
      timeEstimate: t(DASHBOARD_V2_I18N_KEYS.actionPrTime),
    });
  }

  if (actions.length < 3) {
    push({
      title: t(DASHBOARD_V2_I18N_KEYS.actionWatch),
      path: "/browser-watch",
      why: t(DASHBOARD_V2_I18N_KEYS.actionWatchWhy),
      impact: t(DASHBOARD_V2_I18N_KEYS.actionWatchImpact),
      timeEstimate: t(DASHBOARD_V2_I18N_KEYS.actionWatchTime),
    });
  }

  return actions.slice(0, 3);
}

function buildSystemHealthSummary({
  qualityTrendVm,
  earlyDegradationVm,
  platformObservabilityVm,
  releaseReadinessVm,
  executiveBriefVm,
  t,
}) {
  return {
    qualityTrend: {
      show: qualityTrendVm?.show && !qualityTrendVm?.empty,
      label: t(DASHBOARD_V2_I18N_KEYS.healthQualityTrend),
      value: executiveBriefVm?.trendDirection || "—",
      badgeClass: executiveBriefVm?.trendBadgeClass || "badge badge-gray",
    },
    earlyDegradation: {
      show: earlyDegradationVm?.show && !earlyDegradationVm?.empty,
      label: t(DASHBOARD_V2_I18N_KEYS.healthEarlyDegradation),
      value: earlyDegradationVm?.report?.overallStatusText || "—",
      badgeClass: earlyDegradationVm?.report?.overallStatusBadgeClass || "badge badge-gray",
    },
    platform: {
      show: platformObservabilityVm?.show && !platformObservabilityVm?.empty,
      label: t(DASHBOARD_V2_I18N_KEYS.healthPlatform),
      value: (platformObservabilityVm?.executiveSummary || "").slice(0, 80) || "—",
      badgeClass: "badge badge-blue",
    },
    release: {
      show: releaseReadinessVm?.show && !releaseReadinessVm?.empty,
      label: t(DASHBOARD_V2_I18N_KEYS.healthRelease),
      value: releaseReadinessVm?.overallStatusText || "—",
      badgeClass: releaseReadinessVm?.overallStatusBadgeClass || "badge badge-gray",
      summary: releaseReadinessVm?.summary || "",
    },
  };
}

export function buildExecutiveDashboardV2ViewModel({
  releaseReadinessVm,
  executiveBriefVm,
  earlyDegradationVm,
  qualityTrendVm,
  platformObservabilityVm,
  businessRiskVm,
  executiveRiskBriefVm,
  executiveImpactVm,
  nextRecommendedActionVm,
  totalRuns,
  loading,
  t,
}) {
  const decision = resolveExecutiveDecision({
    releaseReadinessVm,
    executiveBriefVm,
    earlyDegradationVm,
    totalRuns,
    t,
  });

  const deploymentRisk = resolveDeploymentRisk(releaseReadinessVm, t);
  const confidence = resolveConfidenceDisplay({ executiveRiskBriefVm, releaseReadinessVm, t });

  const topRisks = (executiveBriefVm?.topRisks || []).slice(0, 3);
  const topChanges = collectTopChanges({
    earlyDegradationVm,
    executiveImpactVm,
    releaseReadinessVm,
    t,
  });
  const topModules = collectTopModules({ businessRiskVm, executiveRiskBriefVm, t });

  const recommendedActions = buildRecommendedActions({
    executiveBriefVm,
    nextRecommendedActionVm,
    executiveRiskBriefVm,
    releaseReadinessVm,
    totalRuns,
    t,
  });

  const topRecommendation = executiveBriefVm?.action
    ? {
        label: executiveBriefVm.action.label,
        path: executiveBriefVm.action.path,
        context: decision.cautionReason || releaseReadinessVm?.summary || "",
      }
    : null;

  return {
    loading: Boolean(loading),
    block1: {
      title: t(DASHBOARD_V2_I18N_KEYS.executiveDecision),
      releaseDecisionLabel: t(DASHBOARD_V2_I18N_KEYS.releaseDecision),
      ...decision,
      deploymentRiskLabel: t(DASHBOARD_V2_I18N_KEYS.deploymentRisk),
      deploymentRisk,
      confidenceLabel: t(DASHBOARD_V2_I18N_KEYS.confidence),
      confidence,
      topRecommendationLabel: t(DASHBOARD_V2_I18N_KEYS.topRecommendation),
      topRecommendation,
    },
    block2: {
      title: t(DASHBOARD_V2_I18N_KEYS.whatChanged),
      topRisksLabel: t(DASHBOARD_V2_I18N_KEYS.topRisks),
      topRisks,
      noRisks: t(DASHBOARD_V2_I18N_KEYS.noRisks),
      topChangesLabel: t(DASHBOARD_V2_I18N_KEYS.topChanges),
      topChanges,
      noChanges: t(DASHBOARD_V2_I18N_KEYS.noChanges),
      topModulesLabel: t(DASHBOARD_V2_I18N_KEYS.topModules),
      topModules,
      noModules: t(DASHBOARD_V2_I18N_KEYS.noModules),
    },
    block3: {
      title: t(DASHBOARD_V2_I18N_KEYS.recommendedActions),
      whyLabel: t(DASHBOARD_V2_I18N_KEYS.why),
      impactLabel: t(DASHBOARD_V2_I18N_KEYS.impact),
      timeLabel: t(DASHBOARD_V2_I18N_KEYS.timeEstimate),
      actions: recommendedActions,
    },
    block4: {
      title: t(DASHBOARD_V2_I18N_KEYS.systemHealth),
      moreDetailsLabel: t(DASHBOARD_V2_I18N_KEYS.moreDetails),
      summary: buildSystemHealthSummary({
        qualityTrendVm,
        earlyDegradationVm,
        platformObservabilityVm,
        releaseReadinessVm,
        executiveBriefVm,
        t,
      }),
    },
    block5: {
      title: t(DASHBOARD_V2_I18N_KEYS.explore),
      summary: t(DASHBOARD_V2_I18N_KEYS.exploreSummary),
    },
  };
}
