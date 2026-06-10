/** View helpers for Contract Risk Assessment (INT-02B). */

export const CONTRACT_RISK_ASSESSMENT_I18N_KEYS = {
  title: "incident.qa.contract_risk_assessment",
  empty: "incident.qa.contract_risk_assessment_empty",
  riskScore: "incident.qa.contract_risk_assessment_risk_score",
  riskLevel: "incident.qa.contract_risk_assessment_risk_level",
  confidence: "incident.qa.contract_risk_assessment_confidence",
  affectedJourneys: "incident.qa.contract_risk_assessment_affected_journeys",
  affectedModules: "incident.qa.contract_risk_assessment_affected_modules",
  affectedTests: "incident.qa.contract_risk_assessment_affected_tests",
  riskFactors: "incident.qa.contract_risk_assessment_risk_factors",
  preview: "incident.qa.contract_risk_assessment_preview",
  previewSubtitle: "incident.qa.contract_risk_assessment_preview_subtitle",
  changeDetails: "incident.qa.contract_risk_assessment_change_details",
  businessImpact: "incident.qa.contract_risk_assessment_business_impact",
  readOnlyNote: "incident.qa.contract_risk_assessment_read_only_note",
};

const RISK_BADGE = {
  CRITICAL: "badge badge-red",
  HIGH: "badge badge-red",
  MEDIUM: "badge badge-orange",
  LOW: "badge badge-blue",
};

export function hasContractRiskAssessmentSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "contract_risk_assessment");
}

export function isContractRiskAssessmentEmpty(report) {
  return report?.contract_risk_assessment == null;
}

export function formatRiskConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function riskLevelBadgeClass(riskLevel) {
  const key = String(riskLevel || "LOW").toUpperCase();
  return RISK_BADGE[key] || "badge badge-gray";
}

export function findContract(report, contractId) {
  return (report?.api_contract_intelligence?.contracts || []).find((c) => c.contract_id === contractId) || null;
}

export function findContractChanges(report, contractId) {
  const assessments = report?.api_contract_intelligence?.risk_assessments || [];
  const assessment = assessments.find((a) => a.assessment_id === `assessment:${contractId}` || a.assessment_id?.endsWith(contractId));
  return assessment?.changes || [];
}

export function buildContractDrilldownItem(contract) {
  if (!contract?.contract_id) return null;
  return {
    source: "contract_risk_assessment",
    related_entity_type: "api_contract",
    related_entity_id: contract.contract_id,
    reason: contract.service_name,
    detail: `${contract.method} ${contract.endpoint}`,
    title: contract.service_name,
  };
}

export function buildJourneyDrilldownItem(journeyName, report) {
  const journey = (report?.data_journey_validation?.journeys || []).find(
    (j) => j.name === journeyName || j.business_area?.toLowerCase() === journeyName.toLowerCase(),
  );
  if (!journey?.journey_id) {
    return {
      source: "contract_risk_assessment",
      related_entity_type: "data_journey",
      related_entity_id: `journey:${journeyName.toLowerCase().replace(/\s+/g, "_")}`,
      reason: journeyName,
      detail: "Data journey correlation",
      title: journeyName,
    };
  }
  return {
    source: "contract_risk_assessment",
    related_entity_type: "data_journey",
    related_entity_id: journey.journey_id,
    reason: journey.name,
    detail: journey.description || journeyName,
    title: journey.name,
  };
}

export function buildTestDrilldownItem(testName, report) {
  const rec = (report?.test_recommendations?.recommendations || []).find((r) => r.test_name === testName);
  if (!rec) {
    return {
      source: "contract_risk_assessment",
      related_entity_type: "recommended_test",
      related_entity_id: testName,
      reason: testName,
      detail: "Recommended test correlation",
      title: testName,
    };
  }
  return {
    source: "contract_risk_assessment",
    related_entity_type: "recommended_test",
    related_entity_id: rec.recommendation_id,
    reason: rec.test_name,
    detail: rec.reason,
    title: rec.test_name,
  };
}

export function buildImpactDrilldownItem(moduleName) {
  return {
    source: "contract_risk_assessment",
    related_entity_type: "impact_map",
    related_entity_id: moduleName,
    reason: moduleName,
    detail: "Impacted area correlation",
    title: moduleName,
  };
}

export function buildContractRiskPreviewPayload(assessment, contract, changes, t) {
  return {
    title: contract?.service_name || assessment.contract_id,
    riskScore: assessment.risk_score,
    riskLevel: assessment.overall_risk_level,
    confidence: formatRiskConfidence(assessment.confidence),
    summary: assessment.summary,
    affectedJourneys: assessment.affected_journeys || [],
    affectedModules: assessment.affected_modules || [],
    affectedTests: assessment.affected_tests || [],
    factors: assessment.factors || [],
    changes,
    readOnlyNote: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.readOnlyNote),
    labels: {
      riskScore: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.riskScore),
      riskLevel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.riskLevel),
      confidence: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.confidence),
      affectedJourneys: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.affectedJourneys),
      affectedModules: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.affectedModules),
      affectedTests: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.affectedTests),
      riskFactors: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.riskFactors),
      changeDetails: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.changeDetails),
      businessImpact: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.businessImpact),
    },
  };
}

export function buildContractRiskAssessmentViewModel(report, t) {
  const cra = report?.contract_risk_assessment ?? null;
  const assessments = (cra?.assessments || []).map((assessment) => {
    const contract = findContract(report, assessment.contract_id);
    const changes = findContractChanges(report, assessment.contract_id);
    return {
      ...assessment,
      contract,
      changes,
      riskBadgeClass: riskLevelBadgeClass(assessment.overall_risk_level),
      confidenceText: formatRiskConfidence(assessment.confidence),
      contractDrilldown: buildContractDrilldownItem(contract),
      journeyDrilldowns: (assessment.affected_journeys || []).map((name) => buildJourneyDrilldownItem(name, report)),
      moduleDrilldowns: (assessment.affected_modules || []).map((name) => buildImpactDrilldownItem(name)),
      testDrilldowns: (assessment.affected_tests || []).map((name) => buildTestDrilldownItem(name, report)),
      previewPayload: buildContractRiskPreviewPayload(assessment, contract, changes, t),
    };
  });

  return {
    show: hasContractRiskAssessmentSection(report),
    empty: isContractRiskAssessmentEmpty(report),
    emptyMessage: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.empty),
    title: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.title),
    riskScoreLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.riskScore),
    riskLevelLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.riskLevel),
    confidenceLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.confidence),
    affectedJourneysLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.affectedJourneys),
    affectedModulesLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.affectedModules),
    affectedTestsLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.affectedTests),
    riskFactorsLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.riskFactors),
    previewLabel: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.preview),
    readOnlyNote: t(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.readOnlyNote),
    report: cra
      ? {
          ...cra,
          confidenceText: formatRiskConfidence(cra.confidence),
          assessments,
        }
      : null,
  };
}
