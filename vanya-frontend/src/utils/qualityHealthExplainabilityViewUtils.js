/** Explainability view helpers for Quality Health (UX-03). */

import { EVIDENCE_TRACE_I18N_KEYS, formatTraceConfidence } from "./incidentEvidenceTraceabilityViewUtils.js";

export const QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS = {
  whyTitle: "explainability.quality_health.why_title",
  whyPrefix: "explainability.quality_health.why_prefix",
  toggleShow: "explainability.toggle_show",
  toggleHide: "explainability.toggle_hide",
  sourceContractRisk: "explainability.quality_health.source.contract_risk",
  sourceDeploymentRisk: "explainability.quality_health.source.deployment_risk",
  sourceJourney: "explainability.quality_health.source.journey",
  sourceEnvironment: "explainability.quality_health.source.environment",
  sourceDependency: "explainability.quality_health.source.dependency",
  sourceHistorical: "explainability.quality_health.source.historical",
  sourceImpact: "explainability.quality_health.source.impact",
  sourceDecision: "explainability.quality_health.source.decision",
  sourceRecommendedTest: "explainability.quality_health.source.recommended_test",
  sourceRecommendedAction: "explainability.quality_health.source.recommended_action",
  sourceOther: "explainability.quality_health.source.other",
};

const LOW_STATUSES = new Set(["ATTENTION", "HIGH_RISK"]);

const FACTOR_SOURCE_LABEL_KEY = {
  contract_risk: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceContractRisk,
  deployment_risk: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceDeploymentRisk,
  journey: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceJourney,
  environment: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceEnvironment,
  dependency: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceDependency,
  historical: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceHistorical,
  impact: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceImpact,
  decision: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceDecision,
  recommended_test: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceRecommendedTest,
  recommended_action: QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceRecommendedAction,
};

function collectFactors(report) {
  const factors = [];
  for (const score of report?.scores || []) {
    for (const factor of score.contributing_factors || []) {
      factors.push({
        ...factor,
        scope_name: score.scope_name,
        scope_type: score.scope_type,
      });
    }
  }
  return factors;
}

function topContributingFactors(report, limit = 5) {
  return collectFactors(report)
    .sort((a, b) => (Number(b.impact) || 0) - (Number(a.impact) || 0))
    .slice(0, limit);
}

export function shouldShowQualityHealthTrace(report) {
  if (!report) return false;
  const status = String(report.overall_status || "").toUpperCase();
  const score = Number(report.overall_score);
  return LOW_STATUSES.has(status) || (Number.isFinite(score) && score < 75);
}

export function buildQualityHealthWhyExplanation(report, t) {
  const factors = topContributingFactors(report, 5);
  const bullets = factors.map((factor) => String(factor.title || "").trim()).filter(Boolean);
  const unique = [...new Set(bullets)];
  const score = report?.overall_score ?? "—";
  const status = String(report?.overall_status || "").toUpperCase();

  return {
    prefix: t(QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.whyPrefix, { score, status }),
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.whyTitle),
    show: true,
  };
}

export function buildQualityHealthEvidenceSummary(report, t) {
  const factors = topContributingFactors(report, 6);
  const bullets = factors
    .map((factor) => String(factor.description || factor.title || "").trim())
    .filter(Boolean);
  const unique = [...new Set(bullets)];

  return {
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSummary),
  };
}

export function buildQualityHealthContributors(report, t) {
  const factors = topContributingFactors(report, 5);
  const confidenceText = formatTraceConfidence(report?.confidence);
  const contributors = factors.map((factor) => ({
    title: factor.title,
    confidenceText,
  }));

  return {
    contributors,
    empty: contributors.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.rootCauseContributors),
    confidenceLabel: t(EVIDENCE_TRACE_I18N_KEYS.confidence),
  };
}

export function buildQualityHealthEvidenceSources(report, t) {
  const counts = new Map();
  for (const factor of collectFactors(report)) {
    const key = String(factor.related_entity_type || "other").toLowerCase();
    counts.set(key, (counts.get(key) || 0) + 1);
  }

  const sources = [...counts.entries()].map(([key, count]) => ({
    key,
    label: t(FACTOR_SOURCE_LABEL_KEY[key] || QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.sourceOther),
    count,
  }));

  return {
    sources,
    empty: sources.every((s) => s.count === 0),
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSources),
  };
}

export function buildQualityHealthTraceViewModel(report, t) {
  const show = shouldShowQualityHealthTrace(report);
  return {
    show,
    whyExplanation: buildQualityHealthWhyExplanation(report, t),
    evidenceSummary: buildQualityHealthEvidenceSummary(report, t),
    rootCauseContributors: buildQualityHealthContributors(report, t),
    evidenceSources: buildQualityHealthEvidenceSources(report, t),
  };
}
