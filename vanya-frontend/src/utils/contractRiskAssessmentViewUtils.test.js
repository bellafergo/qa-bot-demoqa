import { describe, it, expect } from "vitest";
import {
  CONTRACT_RISK_ASSESSMENT_I18N_KEYS,
  buildContractDrilldownItem,
  buildContractRiskAssessmentViewModel,
  buildContractRiskPreviewPayload,
  buildJourneyDrilldownItem,
  buildTestDrilldownItem,
  formatRiskConfidence,
  hasContractRiskAssessmentSection,
  isContractRiskAssessmentEmpty,
  riskLevelBadgeClass,
} from "./contractRiskAssessmentViewUtils.js";

const t = (key) => key;

const sampleReport = {
  contract_risk_assessment: {
    summary: "1 contract(s) assessed; 1 CRITICAL business risk profile(s).",
    confidence: 0.88,
    assessments: [
      {
        assessment_id: "business_risk_assessment:contract:payments_api_post_payments_vv2",
        contract_id: "contract:payments_api_post_payments_vv2",
        overall_risk_level: "CRITICAL",
        risk_score: 92,
        confidence: 0.88,
        summary: "The Payments API contract removed required field 'amount'.",
        affected_journeys: ["Checkout", "Payments"],
        affected_modules: ["Payments", "Orders"],
        affected_tests: ["Payments Regression", "Checkout Smoke"],
        factors: [
          {
            factor_id: "factor:contract:payments_api_post_payments_vv2:change_removed_field_amount",
            title: "Contract change: removed field",
            description: "Removed field 'amount'.",
            severity: "HIGH",
            weight: 50,
          },
        ],
      },
    ],
  },
  api_contract_intelligence: {
    contracts: [
      {
        contract_id: "contract:payments_api_post_payments_vv2",
        service_name: "Payments API",
        endpoint: "/payments",
        method: "POST",
        version: "v2",
      },
    ],
    risk_assessments: [
      {
        assessment_id: "assessment:contract:payments_api_post_payments_vv2",
        risk_level: "HIGH",
        confidence: 0.85,
        summary: "Changes detected",
        changes: [
          {
            change_id: "change:contract:payments_api_post_payments_vv2:removed_field:amount",
            severity: "HIGH",
            change_type: "removed_field",
            field_name: "amount",
            description: "Removed field 'amount'.",
          },
        ],
      },
    ],
  },
  data_journey_validation: {
    journeys: [
      {
        journey_id: "journey:checkout",
        name: "Checkout Journey",
        business_area: "checkout",
        stages: [],
      },
    ],
    results: [],
    summary: "",
    confidence: 0.7,
  },
  test_recommendations: {
    recommendations: [
      {
        recommendation_id: "rec-1",
        test_name: "Payments Regression",
        reason: "payments contract",
        priority: 10,
        confidence: 0.8,
      },
    ],
    summary: "",
    recommendation_confidence: 0.8,
  },
};

describe("contractRiskAssessmentViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasContractRiskAssessmentSection({ contract_risk_assessment: null })).toBe(true);
    expect(hasContractRiskAssessmentSection({})).toBe(false);
    expect(isContractRiskAssessmentEmpty({ contract_risk_assessment: null })).toBe(true);
  });

  it("renders assessments", () => {
    const vm = buildContractRiskAssessmentViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.report.assessments[0].risk_score).toBe(92);
    expect(vm.report.assessments[0].contract.service_name).toBe("Payments API");
  });

  it("renders empty state via i18n key", () => {
    const vm = buildContractRiskAssessmentViewModel({ contract_risk_assessment: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.contract_risk_assessment_empty");
  });

  it("maps risk badges", () => {
    expect(riskLevelBadgeClass("CRITICAL")).toBe("badge badge-red");
    expect(riskLevelBadgeClass("LOW")).toBe("badge badge-blue");
    expect(formatRiskConfidence(0.88)).toBe("88%");
  });

  it("shows affected journeys and tests", () => {
    const vm = buildContractRiskAssessmentViewModel(sampleReport, t);
    const assessment = vm.report.assessments[0];
    expect(assessment.affected_journeys).toContain("Checkout");
    expect(assessment.affected_tests).toContain("Payments Regression");
  });

  it("builds preview payload", () => {
    const vm = buildContractRiskAssessmentViewModel(sampleReport, t);
    const payload = buildContractRiskPreviewPayload(
      vm.report.assessments[0],
      vm.report.assessments[0].contract,
      vm.report.assessments[0].changes,
      t,
    );
    expect(payload.riskScore).toBe(92);
    expect(payload.readOnlyNote).toBe("incident.qa.contract_risk_assessment_read_only_note");
  });

  it("builds drilldown items", () => {
    const contractItem = buildContractDrilldownItem(sampleReport.api_contract_intelligence.contracts[0]);
    expect(contractItem.related_entity_type).toBe("api_contract");
    const journeyItem = buildJourneyDrilldownItem("Checkout", sampleReport);
    expect(journeyItem.related_entity_type).toBe("data_journey");
    const testItem = buildTestDrilldownItem("Payments Regression", sampleReport);
    expect(testItem.related_entity_type).toBe("recommended_test");
  });

  it("exposes translation keys", () => {
    expect(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.title).toBe("incident.qa.contract_risk_assessment");
    expect(CONTRACT_RISK_ASSESSMENT_I18N_KEYS.affectedJourneys).toBe(
      "incident.qa.contract_risk_assessment_affected_journeys",
    );
  });
});
