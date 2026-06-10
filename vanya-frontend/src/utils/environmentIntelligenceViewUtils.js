/** View helpers for Multi-Environment Intelligence (ENT-01A). */

export const ENVIRONMENT_INTELLIGENCE_I18N_KEYS = {
  title: "incident.qa.multi_environment",
  empty: "incident.qa.multi_environment_empty",
  environment: "incident.qa.multi_environment_environment",
  status: "incident.qa.multi_environment_status",
  signals: "incident.qa.multi_environment_signals",
  comparisons: "incident.qa.multi_environment_comparisons",
  promotionReadiness: "incident.qa.multi_environment_promotion_readiness",
  readinessScore: "incident.qa.multi_environment_readiness_score",
  blockers: "incident.qa.multi_environment_blockers",
  warnings: "incident.qa.multi_environment_warnings",
  recommendedValidations: "incident.qa.multi_environment_recommended_validations",
  riskDelta: "incident.qa.multi_environment_risk_delta",
  summary: "incident.qa.multi_environment_summary",
  confidence: "incident.qa.multi_environment_confidence",
  readOnlyNote: "incident.qa.multi_environment_read_only_note",
};

const STATUS_BADGE = {
  HEALTHY: "badge badge-green",
  DEGRADED: "badge badge-orange",
  BROKEN: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

const READINESS_BADGE = {
  READY: "badge badge-green",
  CAUTION: "badge badge-orange",
  BLOCKED: "badge badge-red",
  UNKNOWN: "badge badge-gray",
};

const SEVERITY_BADGE = {
  CRITICAL: "badge badge-red",
  HIGH: "badge badge-orange",
  MEDIUM: "badge badge-orange",
  LOW: "badge badge-blue",
};

export function hasMultiEnvironmentSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "multi_environment");
}

export function isMultiEnvironmentEmpty(report) {
  return report?.multi_environment == null;
}

export function environmentStatusBadgeClass(status) {
  return STATUS_BADGE[String(status || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

export function readinessStatusBadgeClass(status) {
  return READINESS_BADGE[String(status || "UNKNOWN").toUpperCase()] || "badge badge-gray";
}

export function signalSeverityBadgeClass(severity) {
  return SEVERITY_BADGE[String(severity || "LOW").toUpperCase()] || "badge badge-gray";
}

export function formatEnvironmentConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function buildSignalDrilldownItem(signal) {
  if (!signal?.related_entity_type || !signal?.related_entity_id) return null;
  return {
    source: "multi_environment",
    related_entity_type: signal.related_entity_type,
    related_entity_id: signal.related_entity_id,
    reason: signal.description,
    detail: signal.title,
    title: signal.title,
  };
}

function profileNameById(environments, environmentId) {
  const profile = (environments || []).find((e) => e.environment_id === environmentId);
  return profile?.name || environmentId;
}

export function buildMultiEnvironmentIntelligenceViewModel(report, t) {
  const me = report?.multi_environment ?? null;
  const environments = me?.environments || [];
  const signals = me?.signals || [];

  const signalCountByEnv = new Map();
  for (const signal of signals) {
    signalCountByEnv.set(signal.environment_id, (signalCountByEnv.get(signal.environment_id) || 0) + 1);
  }

  return {
    show: hasMultiEnvironmentSection(report),
    empty: isMultiEnvironmentEmpty(report),
    emptyMessage: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.empty),
    title: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.title),
    environmentLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.environment),
    statusLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.status),
    signalsLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.signals),
    comparisonsLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.comparisons),
    promotionReadinessLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.promotionReadiness),
    readinessScoreLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.readinessScore),
    blockersLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.blockers),
    warningsLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.warnings),
    recommendedValidationsLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.recommendedValidations),
    riskDeltaLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.riskDelta),
    summaryLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.summary),
    confidenceLabel: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.confidence),
    readOnlyNote: t(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.readOnlyNote),
    report: me
      ? {
          ...me,
          confidenceText: formatEnvironmentConfidence(me.confidence),
          environments: environments.map((env) => ({
            ...env,
            statusBadgeClass: environmentStatusBadgeClass(env.status),
            signalCount: signalCountByEnv.get(env.environment_id) || 0,
            signals: signals
              .filter((s) => s.environment_id === env.environment_id)
              .map((signal) => ({
                ...signal,
                severityBadgeClass: signalSeverityBadgeClass(signal.severity),
                drilldownItem: buildSignalDrilldownItem(signal),
              })),
          })),
          comparisons: (me.comparisons || []).map((comparison) => ({
            ...comparison,
            sourceName: profileNameById(environments, comparison.source_environment_id),
            targetName: profileNameById(environments, comparison.target_environment_id),
            routeLabel: `${profileNameById(environments, comparison.source_environment_id)} → ${profileNameById(environments, comparison.target_environment_id)}`,
          })),
          promotion_readiness: (me.promotion_readiness || []).map((item) => ({
            ...item,
            sourceName: profileNameById(environments, item.source_environment_id),
            targetName: profileNameById(environments, item.target_environment_id),
            routeLabel: `${profileNameById(environments, item.source_environment_id)} → ${profileNameById(environments, item.target_environment_id)}`,
            statusBadgeClass: readinessStatusBadgeClass(item.readiness_status),
          })),
        }
      : null,
  };
}
