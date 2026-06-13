/** Explainability view helpers for Business Risk (UX-03). */

import { EVIDENCE_TRACE_I18N_KEYS } from "./incidentEvidenceTraceabilityViewUtils.js";

export const BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS = {
  whyPrefix: "explainability.business_risk.why_prefix",
  toggleShow: "explainability.toggle_show",
  toggleHide: "explainability.toggle_hide",
  sourceJiraBlockers: "explainability.business_risk.source.jira_blockers",
  sourceBrokenJourneys: "explainability.business_risk.source.broken_journeys",
  sourceCriticalContracts: "explainability.business_risk.source.critical_contracts",
  sourceEnvironmentIssues: "explainability.business_risk.source.environment_issues",
  sourceReleaseBlocked: "explainability.business_risk.source.release_blocked",
  sourceDeploymentRisk: "explainability.business_risk.source.deployment_risk",
  sourceCriticalDependencies: "explainability.business_risk.source.critical_dependencies",
  sourceDecisionInsights: "explainability.business_risk.source.decision_insights",
  sourceServicenowOperational: "explainability.business_risk.source.servicenow_operational",
  sourceOther: "explainability.business_risk.source.other",
};

const HIGH_SEVERITIES = new Set(["HIGH", "CRITICAL"]);

const CONFIDENCE_LABEL_KEY = {
  LOW: "business_risk_overview.confidence.low",
  MEDIUM: "business_risk_overview.confidence.medium",
  HIGH: "business_risk_overview.confidence.high",
};

const SIGNAL_SOURCE_LABEL_KEY = {
  jira_blockers: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceJiraBlockers,
  broken_journeys: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceBrokenJourneys,
  critical_contracts: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceCriticalContracts,
  environment_issues: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceEnvironmentIssues,
  release_blocked: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceReleaseBlocked,
  deployment_risk: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceDeploymentRisk,
  critical_dependencies: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceCriticalDependencies,
  decision_insights: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceDecisionInsights,
  servicenow_operational: BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceServicenowOperational,
};

function parseSignalSourceKey(signalId) {
  const parts = String(signalId || "").split(":");
  if (parts.length < 3) return "other";
  return parts[2] || "other";
}

function enumConfidenceText(confidence, t) {
  const key = String(confidence || "LOW").toUpperCase();
  return t(CONFIDENCE_LABEL_KEY[key] || CONFIDENCE_LABEL_KEY.LOW);
}

export function filterSignalsForRisk(risk, signals) {
  const capability = String(risk?.capability || "").trim().toLowerCase();
  if (!capability) return [];
  return (signals || []).filter(
    (signal) => String(signal?.impacted_capability || "").trim().toLowerCase() === capability,
  );
}

export function shouldShowBusinessRiskTrace(risk, signals) {
  if (!risk) return false;
  const severity = String(risk.severity || "").toUpperCase();
  const matching = filterSignalsForRisk(risk, signals);
  return HIGH_SEVERITIES.has(severity) || (risk.evidence || []).length > 0 || matching.length > 0;
}

export function buildBusinessRiskWhyExplanation(risk, signals, t) {
  const matching = filterSignalsForRisk(risk, signals);
  const bullets = [];
  for (const item of risk?.evidence || []) {
    const text = String(item || "").trim();
    if (text) bullets.push(text);
  }
  for (const signal of matching) {
    const title = String(signal?.title || "").trim();
    if (title && !bullets.some((b) => b.toLowerCase().includes(title.toLowerCase()))) {
      bullets.push(title);
    }
  }
  const unique = [...new Set(bullets)].slice(0, 6);
  const capability = risk?.capability || "Capability";
  const severity = String(risk?.severity || "UNKNOWN").toUpperCase();

  return {
    prefix: t(BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.whyPrefix, { capability, severity }),
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.whyExplanation),
    show: true,
  };
}

export function buildBusinessRiskEvidenceSummary(risk, signals, t) {
  const matching = filterSignalsForRisk(risk, signals);
  const bullets = [];
  for (const item of risk?.evidence || []) {
    const text = String(item || "").trim();
    if (text) bullets.push(text);
  }
  for (const signal of matching) {
    const count = Number(signal?.evidence_count) || 0;
    const title = String(signal?.title || "").trim();
    if (count > 0 && title) {
      bullets.push(`${count} ${title.toLowerCase()}`);
    }
  }
  const unique = [...new Set(bullets)].slice(0, 6);

  return {
    bullets: unique,
    empty: unique.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSummary),
  };
}

export function buildBusinessRiskContributors(risk, signals, t) {
  const matching = filterSignalsForRisk(risk, signals);
  const contributors = matching.map((signal) => ({
    title: signal.title,
    confidenceText: enumConfidenceText(signal.confidence, t),
  }));

  return {
    contributors: contributors.slice(0, 5),
    empty: contributors.length === 0,
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.rootCauseContributors),
    confidenceLabel: t(EVIDENCE_TRACE_I18N_KEYS.confidence),
  };
}

export function buildBusinessRiskEvidenceSources(risk, signals, t) {
  const matching = filterSignalsForRisk(risk, signals);
  const counts = new Map();

  for (const signal of matching) {
    const key = parseSignalSourceKey(signal.signal_id);
    const count = Number(signal.evidence_count) || 1;
    counts.set(key, (counts.get(key) || 0) + count);
  }

  const sources = [...counts.entries()].map(([key, count]) => ({
    key,
    label: t(SIGNAL_SOURCE_LABEL_KEY[key] || BUSINESS_RISK_EXPLAINABILITY_I18N_KEYS.sourceOther),
    count,
  }));

  return {
    sources,
    empty: sources.every((s) => s.count === 0),
    emptyMessage: t(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence),
    title: t(EVIDENCE_TRACE_I18N_KEYS.evidenceSources),
  };
}

export function buildBusinessRiskTraceViewModel(risk, signals, t) {
  const show = shouldShowBusinessRiskTrace(risk, signals);
  return {
    show,
    whyExplanation: buildBusinessRiskWhyExplanation(risk, signals, t),
    evidenceSummary: buildBusinessRiskEvidenceSummary(risk, signals, t),
    rootCauseContributors: buildBusinessRiskContributors(risk, signals, t),
    evidenceSources: buildBusinessRiskEvidenceSources(risk, signals, t),
  };
}
