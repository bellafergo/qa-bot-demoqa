/** View helpers for Executive Impact Metrics (ROI-01B). */

import {
  attachCapabilityPresentation,
  buildInsufficientHistoryState,
  CAPABILITY_STATE_I18N_KEYS,
} from "./capabilityStateViewUtils.js";

export const EXECUTIVE_IMPACT_I18N_KEYS = {
  title: "executive_impact.title",
  empty: "executive_impact.empty",
  insufficientHistory: "executive_impact.insufficient_history",
  loadError: "executive_impact.load_error",
  readOnlyNote: "executive_impact.read_only_note",
  quality: "executive_impact.quality",
  risk: "executive_impact.risk",
  operations: "executive_impact.operations",
  topImprovements: "executive_impact.top_improvements",
  topConcerns: "executive_impact.top_concerns",
  noImprovements: "executive_impact.no_improvements",
  noConcerns: "executive_impact.no_concerns",
  previous: "executive_impact.previous",
  directionImproving: "executive_impact.direction.improving",
  directionStable: "executive_impact.direction.stable",
  directionDegrading: "executive_impact.direction.degrading",
  directionUnknown: "executive_impact.direction.unknown",
};

const DIRECTION_INDICATOR = {
  IMPROVING: "↑",
  STABLE: "→",
  DEGRADING: "↓",
  UNKNOWN: "—",
};

const DIRECTION_BADGE = {
  IMPROVING: "badge badge-green",
  STABLE: "badge badge-blue",
  DEGRADING: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

const DIRECTION_LABEL_KEY = {
  IMPROVING: EXECUTIVE_IMPACT_I18N_KEYS.directionImproving,
  STABLE: EXECUTIVE_IMPACT_I18N_KEYS.directionStable,
  DEGRADING: EXECUTIVE_IMPACT_I18N_KEYS.directionDegrading,
  UNKNOWN: EXECUTIVE_IMPACT_I18N_KEYS.directionUnknown,
};

const DELTA_COLOR = {
  IMPROVING: "var(--success, #22c55e)",
  STABLE: "var(--text-3)",
  DEGRADING: "var(--danger, #ef4444)",
  UNKNOWN: "var(--text-3)",
};

function formatValue(value) {
  if (value == null || value === "") return "0";
  return String(value);
}

function formatDelta(delta) {
  const num = Number(delta);
  if (Number.isNaN(num)) return "0";
  if (num > 0) return `+${num}`;
  return String(num);
}

export function mapImpactMetric(metric, t) {
  if (!metric) return null;
  const direction = String(metric.direction || "UNKNOWN").toUpperCase();
  const hasDelta = direction !== "UNKNOWN" && metric.delta !== 0;
  return {
    ...metric,
    currentDisplay: formatValue(metric.current_value),
    previousDisplay: formatValue(metric.previous_value),
    previousLabel: t(EXECUTIVE_IMPACT_I18N_KEYS.previous),
    directionIndicator: DIRECTION_INDICATOR[direction] || DIRECTION_INDICATOR.UNKNOWN,
    directionBadgeClass: DIRECTION_BADGE[direction] || DIRECTION_BADGE.UNKNOWN,
    directionLabel: t(DIRECTION_LABEL_KEY[direction] || DIRECTION_LABEL_KEY.UNKNOWN),
    deltaDisplay: formatDelta(metric.delta),
    deltaColor: DELTA_COLOR[direction] || DELTA_COLOR.UNKNOWN,
    hasDelta,
  };
}

export function hasExecutiveImpactSection(report) {
  return report != null;
}

export function isExecutiveImpactEmpty(report) {
  if (!report) return true;
  if (!report.has_sufficient_history) return true;
  return false;
}

export function buildExecutiveImpactViewModel(report, t, { loadError = "" } = {}) {
  const show = Boolean(loadError) || report != null;

  const map = (metric) => mapImpactMetric(metric, t);

  const qualityMetrics = [
    map(report?.quality_health_trend),
    map(report?.degraded_environment_trend),
    map(report?.impacted_journey_trend),
  ].filter(Boolean);

  const riskMetrics = [
    map(report?.blocked_release_trend),
    map(report?.critical_risk_trend),
  ].filter(Boolean);

  const operationsMetrics = [
    map(report?.recommendation_trend),
    map(report?.approval_trend),
    map(report?.validation_trend),
  ].filter(Boolean);

  const topImprovements = (report?.top_improvements || []).map(map).filter(Boolean);
  const topConcerns = (report?.top_concerns || []).map(map).filter(Boolean);

  let capabilityState = { state: "AVAILABLE" };
  if (!loadError && report && !report.has_sufficient_history) {
    capabilityState = buildInsufficientHistoryState({
      capabilityTitle: t(CAPABILITY_STATE_I18N_KEYS.executiveImpactTitle),
      minRuns: 2,
      currentRuns: 0,
      t,
    });
  }

  return attachCapabilityPresentation({
    show,
    loadError: Boolean(loadError),
    title: t(EXECUTIVE_IMPACT_I18N_KEYS.title),
    readOnlyNote: t(EXECUTIVE_IMPACT_I18N_KEYS.readOnlyNote),
    qualityLabel: t(EXECUTIVE_IMPACT_I18N_KEYS.quality),
    riskLabel: t(EXECUTIVE_IMPACT_I18N_KEYS.risk),
    operationsLabel: t(EXECUTIVE_IMPACT_I18N_KEYS.operations),
    topImprovementsLabel: t(EXECUTIVE_IMPACT_I18N_KEYS.topImprovements),
    topConcernsLabel: t(EXECUTIVE_IMPACT_I18N_KEYS.topConcerns),
    noImprovementsMessage: t(EXECUTIVE_IMPACT_I18N_KEYS.noImprovements),
    noConcernsMessage: t(EXECUTIVE_IMPACT_I18N_KEYS.noConcerns),
    qualityMetrics,
    riskMetrics,
    operationsMetrics,
    topImprovements,
    topConcerns,
    hasSufficientHistory: Boolean(report?.has_sufficient_history),
    generated_at: report?.generated_at || null,
  }, capabilityState);
}
