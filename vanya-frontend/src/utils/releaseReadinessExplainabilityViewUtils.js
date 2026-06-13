/** Explainability view helpers for Release Readiness (UX-03). */

import { EVIDENCE_TRACE_I18N_KEYS, formatTraceConfidence } from "./incidentEvidenceTraceabilityViewUtils.js";

export const RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS = {
  whyTitle: "explainability.release_readiness.why_title",
  whyPrefix: "explainability.release_readiness.why_prefix",
  toggleShow: "explainability.toggle_show",
  toggleHide: "explainability.toggle_hide",
  deploymentRiskBullet: "explainability.release_readiness.bullet.deployment_risk",
  promotionBlockedBullet: "explainability.release_readiness.bullet.promotion_blocked",
  riskSignalsBullet: "explainability.release_readiness.bullet.risk_signals",
  jiraBlockersBullet: "explainability.release_readiness.bullet.jira_blockers",
  sourceJiraBlockers: "explainability.release_readiness.source.jira_blockers",
  sourcePromotionBlockers: "explainability.release_readiness.source.promotion_blockers",
  sourceDeploymentRisk: "explainability.release_readiness.source.deployment_risk",
  sourceJourneyFailures: "explainability.release_readiness.source.journey_failures",
  sourceContractDrift: "explainability.release_readiness.source.contract_drift",
  sourceEnvSignals: "explainability.release_readiness.source.env_signals",
  sourceDataGaps: "explainability.release_readiness.source.data_gaps",
  sourceDecisionTakeaways: "explainability.release_readiness.source.decision_takeaways",
  sourceOther: "explainability.release_readiness.source.other",
};

const BLOCKED_STATUSES = new Set(["BLOCKED", "CAUTION"]);
const BROKEN_JOURNEY_STATUSES = new Set(["BROKEN", "FAILED", "INCONSISTENT"]);
const CRITICAL_CONTRACT_LEVELS = new Set(["CRITICAL", "HIGH"]);

function releaseConfidence(vm) {
  return vm?.decisionCenterVm?.center?.confidence
    ?? vm?.deploymentRisk?.confidence
    ?? vm?.multiEnvironmentVm?.report?.confidence;
}

function countBlockedPromotions(vm) {
  return (vm?.multiEnvironmentVm?.report?.promotion_readiness || [])
    .filter((item) => String(item.readiness_status || "").toUpperCase() === "BLOCKED")
    .length;
}

function collectPromotionBlockers(vm) {
  const blockers = [];
  for (const promo of vm?.multiEnvironmentVm?.report?.promotion_readiness || []) {
    if (String(promo.readiness_status || "").toUpperCase() !== "BLOCKED") continue;
    for (const blocker of promo.blockers || []) {
      const text = String(blocker || "").trim();
      if (text) blockers.push(text);
    }
  }
  return blockers;
}

function countBrokenJourneys(view) {
  let count = 0;
  for (const result of view?.data_journey_validation?.results || []) {
    if (BROKEN_JOURNEY_STATUSES.has(String(result.status || "").toUpperCase())) {
      count += 1;
    }
  }
  return { count };
}

function countCriticalContracts(view) {
  return (view?.contract_risk_assessment?.assessments || []).filter(
    (assessment) => CRITICAL_CONTRACT_LEVELS.has(String(assessment.overall_risk_level || "").toUpperCase()),
  ).length;
}

export function shouldShowReleaseReadinessTrace(vm) {
  if (!vm || vm.empty) return false;
  const status = String(vm.overallStatus || "UNKNOWN").toUpperCase();
  return BLOCKED_STATUSES.has(status);
}

export function buildReleaseReadinessWhyExplanation(vm, view, t) {
  const bullets = [];
  const deploymentLevel = String(vm?.deploymentRisk?.risk_level || "").toUpperCase();
  if (deploymentLevel) {
    bullets.push(t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.deploymentRiskBullet, { level: deploymentLevel }));
  }

  const blockedPromotions = countBlockedPromotions(vm);
  if (blockedPromotions > 0) {
    bullets.push(t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.promotionBlockedBullet, { count: blockedPromotions }));
  }

  const signalCount = (vm?.multiEnvironmentVm?.report?.signals || []).length;
  if (signalCount > 0) {
    bullets.push(t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.riskSignalsBullet, { count: signalCount }));
  }

  if (vm?.jiraIntelVm?.blockerCount > 0) {
    bullets.push(t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.jiraBlockersBullet, { count: vm.jiraIntelVm.blockerCount }));
  }

  const unique = [...new Set(bullets.filter(Boolean))];
  const status = String(vm?.overallStatus || "UNKNOWN").toUpperCase();

  return {
    prefix: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.whyPrefix, { status }),
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.whyTitle),
    show: true,
  };
}

export function buildReleaseReadinessEvidenceSummary(vm, view, t) {
  const bullets = [];
  for (const gap of vm?.data_gaps || []) {
    const text = String(gap || "").trim();
    if (text) bullets.push(text);
  }
  for (const blocker of vm?.compactJiraBlockers || []) {
    const text = [blocker.issueKey, blocker.summary].filter(Boolean).join(" — ");
    if (text) bullets.push(text);
  }
  for (const blocker of collectPromotionBlockers(vm)) {
    bullets.push(blocker);
  }
  for (const factor of vm?.deploymentRisk?.contributing_factors || []) {
    const text = String(factor.title || factor.description || "").trim();
    if (text) bullets.push(text);
  }
  const unique = [...new Set(bullets)].slice(0, 6);

  return {
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSummary),
  };
}

export function buildReleaseReadinessContributors(vm, view, t) {
  const confidenceText = formatTraceConfidence(releaseConfidence(vm));
  const contributors = [];

  for (const factor of vm?.deploymentRisk?.contributing_factors || []) {
    const title = String(factor.title || factor.description || "").trim();
    if (title) contributors.push({ title, confidenceText });
  }
  for (const takeaway of vm?.decisionCenterVm?.center?.key_takeaways || []) {
    const title = String(takeaway.title || "").trim();
    if (title) contributors.push({ title, confidenceText });
  }

  return {
    contributors: contributors.slice(0, 5),
    empty: contributors.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.rootCauseContributors),
    confidenceLabel: t(EVIDENCE_TRACE_I18N_KEYS.confidence),
  };
}

export function buildReleaseReadinessEvidenceSources(vm, view, t) {
  const brokenJourneys = countBrokenJourneys(view);
  const criticalContracts = countCriticalContracts(view);
  const promotionBlockers = collectPromotionBlockers(vm).length;
  const envSignals = (vm?.multiEnvironmentVm?.report?.signals || []).length;
  const deploymentFactors = (vm?.deploymentRisk?.contributing_factors || []).length;
  const decisionTakeaways = (vm?.decisionCenterVm?.center?.key_takeaways || []).length;

  const sources = [
    {
      key: "jira_blockers",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourceJiraBlockers),
      count: vm?.jiraIntelVm?.blockerCount || 0,
    },
    {
      key: "promotion_blockers",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourcePromotionBlockers),
      count: promotionBlockers,
    },
    {
      key: "deployment_risk",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourceDeploymentRisk),
      count: deploymentFactors,
    },
    {
      key: "journey_failures",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourceJourneyFailures),
      count: brokenJourneys.count,
    },
    {
      key: "contract_drift",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourceContractDrift),
      count: criticalContracts,
    },
    {
      key: "env_signals",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourceEnvSignals),
      count: envSignals,
    },
    {
      key: "data_gaps",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourceDataGaps),
      count: (vm?.data_gaps || []).length,
    },
    {
      key: "decision_takeaways",
      label: t(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.sourceDecisionTakeaways),
      count: decisionTakeaways,
    },
  ].filter((source) => source.count > 0);

  return {
    sources,
    empty: sources.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSources),
  };
}

export function buildReleaseReadinessTraceViewModel(vm, view, t) {
  const show = shouldShowReleaseReadinessTrace(vm);
  return {
    show,
    whyExplanation: buildReleaseReadinessWhyExplanation(vm, view, t),
    evidenceSummary: buildReleaseReadinessEvidenceSummary(vm, view, t),
    rootCauseContributors: buildReleaseReadinessContributors(vm, view, t),
    evidenceSources: buildReleaseReadinessEvidenceSources(vm, view, t),
  };
}
