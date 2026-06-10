/** View helpers for Deployment Risk Assessment (II-05A). */

export const DEPLOYMENT_RISK_I18N_KEYS = {
  title: "incident.qa.deployment_risk",
  riskScore: "incident.qa.deployment_risk_score",
  riskLevel: "incident.qa.deployment_risk_level",
  contributingFactors: "incident.qa.deployment_risk_factors",
  confidence: "incident.qa.deployment_risk_confidence",
  empty: "incident.qa.deployment_risk_empty",
  weight: "incident.qa.deployment_risk_weight",
  levelLow: "incident.qa.deployment_risk_level_low",
  levelMedium: "incident.qa.deployment_risk_level_medium",
  levelHigh: "incident.qa.deployment_risk_level_high",
  levelCritical: "incident.qa.deployment_risk_level_critical",
};

export function hasDeploymentRiskSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "deployment_risk_assessment");
}

export function isDeploymentRiskEmpty(report) {
  return report?.deployment_risk_assessment == null;
}

export function getDeploymentRiskLevelLabelKey(level) {
  const v = String(level || "low").toLowerCase();
  if (v === "critical") return DEPLOYMENT_RISK_I18N_KEYS.levelCritical;
  if (v === "high") return DEPLOYMENT_RISK_I18N_KEYS.levelHigh;
  if (v === "medium") return DEPLOYMENT_RISK_I18N_KEYS.levelMedium;
  return DEPLOYMENT_RISK_I18N_KEYS.levelLow;
}

export function getDeploymentRiskLevelBadgeClass(level) {
  const v = String(level || "low").toLowerCase();
  if (v === "critical") return "badge badge-red";
  if (v === "high") return "badge badge-red";
  if (v === "medium") return "badge badge-orange";
  return "badge badge-gray";
}

export function formatDeploymentRiskConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function formatFactorWeight(weight) {
  const n = Number(weight);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function buildDeploymentRiskDrilldownItem(factor) {
  if (!factor?.related_entity_type || !factor?.related_entity_id) return null;
  const et = String(factor.related_entity_type);
  if (et === "hypothesis") return null;
  return {
    source: et,
    related_entity_type: et,
    related_entity_id: factor.related_entity_id,
    reason: factor.description,
    detail: factor.title,
    title: factor.title,
  };
}

export function buildDeploymentRiskViewModel(report, t) {
  const assessment = report?.deployment_risk_assessment ?? null;
  return {
    show: hasDeploymentRiskSection(report),
    empty: isDeploymentRiskEmpty(report),
    emptyMessage: t(DEPLOYMENT_RISK_I18N_KEYS.empty),
    title: t(DEPLOYMENT_RISK_I18N_KEYS.title),
    riskScoreLabel: t(DEPLOYMENT_RISK_I18N_KEYS.riskScore),
    riskLevelLabel: t(DEPLOYMENT_RISK_I18N_KEYS.riskLevel),
    factorsLabel: t(DEPLOYMENT_RISK_I18N_KEYS.contributingFactors),
    confidenceLabel: t(DEPLOYMENT_RISK_I18N_KEYS.confidence),
    weightLabel: t(DEPLOYMENT_RISK_I18N_KEYS.weight),
    assessment: assessment
      ? {
          ...assessment,
          riskLevelLabel: t(getDeploymentRiskLevelLabelKey(assessment.risk_level)),
          riskLevelBadgeClass: getDeploymentRiskLevelBadgeClass(assessment.risk_level),
          confidenceText: formatDeploymentRiskConfidence(assessment.confidence),
          factors: (assessment.contributing_factors || []).map((factor) => ({
            ...factor,
            weightText: formatFactorWeight(factor.weight),
            drilldownItem: buildDeploymentRiskDrilldownItem(factor),
          })),
        }
      : null,
  };
}
