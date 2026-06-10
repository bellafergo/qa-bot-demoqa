import { describe, it, expect } from "vitest";
import { buildDrilldownNavigation } from "./correlationDrilldownUtils.js";
import {
  DEPLOYMENT_RISK_I18N_KEYS,
  buildDeploymentRiskDrilldownItem,
  buildDeploymentRiskViewModel,
  formatDeploymentRiskConfidence,
  formatFactorWeight,
  getDeploymentRiskLevelLabelKey,
  hasDeploymentRiskSection,
  isDeploymentRiskEmpty,
} from "./deploymentRiskAssessmentViewUtils.js";

const t = (key) => key;

describe("deploymentRiskAssessmentViewUtils", () => {
  it("detects deployment risk section and empty state", () => {
    expect(hasDeploymentRiskSection({ deployment_risk_assessment: null })).toBe(true);
    expect(hasDeploymentRiskSection({})).toBe(false);
    expect(isDeploymentRiskEmpty({ deployment_risk_assessment: null })).toBe(true);
  });

  it("renders risk score, badge, and factor list", () => {
    const vm = buildDeploymentRiskViewModel(
      {
        deployment_risk_assessment: {
          risk_score: 82,
          risk_level: "critical",
          confidence: 0.88,
          summary: "Elevated deployment risk.",
          contributing_factors: [
            {
              title: "Repeated Checkout Failures",
              description: "12 failures clustered.",
              weight: 0.24,
              related_entity_type: "failure_cluster",
              related_entity_id: "cluster_7",
            },
          ],
        },
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.assessment.risk_score).toBe(82);
    expect(vm.assessment.riskLevelLabel).toBe(DEPLOYMENT_RISK_I18N_KEYS.levelCritical);
    expect(vm.assessment.confidenceText).toBe("88%");
    expect(vm.assessment.factors[0].weightText).toBe("24%");
  });

  it("renders empty state via i18n key", () => {
    const vm = buildDeploymentRiskViewModel({ deployment_risk_assessment: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.deployment_risk_empty");
  });

  it("maps risk level label keys", () => {
    expect(getDeploymentRiskLevelLabelKey("high")).toBe(DEPLOYMENT_RISK_I18N_KEYS.levelHigh);
    expect(getDeploymentRiskLevelLabelKey("low")).toBe(DEPLOYMENT_RISK_I18N_KEYS.levelLow);
  });

  it("formats confidence and factor weight", () => {
    expect(formatDeploymentRiskConfidence(0.885)).toBe("89%");
    expect(formatFactorWeight(0.24)).toBe("24%");
  });

  it("builds drilldown item for II-02D navigation", () => {
    const item = buildDeploymentRiskDrilldownItem({
      title: "Repeated Checkout Failures",
      description: "Cluster overlap",
      related_entity_type: "failure_cluster",
      related_entity_id: "cluster_7",
    });
    const target = buildDrilldownNavigation(item);
    expect(target?.path).toContain("failure-intel");
  });

  it("exposes translation keys", () => {
    expect(DEPLOYMENT_RISK_I18N_KEYS.title).toBe("incident.qa.deployment_risk");
    expect(DEPLOYMENT_RISK_I18N_KEYS.contributingFactors).toBe("incident.qa.deployment_risk_factors");
  });
});
