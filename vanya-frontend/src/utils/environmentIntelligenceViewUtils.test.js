import { describe, it, expect } from "vitest";
import {
  ENVIRONMENT_INTELLIGENCE_I18N_KEYS,
  buildMultiEnvironmentIntelligenceViewModel,
  buildSignalDrilldownItem,
  environmentStatusBadgeClass,
  hasMultiEnvironmentSection,
  isMultiEnvironmentEmpty,
  readinessStatusBadgeClass,
} from "./environmentIntelligenceViewUtils.js";

const t = (key) => key;

const sampleReport = {
  multi_environment: {
    summary: "3 environment(s) modeled; 1 environment(s) BROKEN; 5 signal(s).",
    confidence: 0.84,
    environments: [
      { environment_id: "env:qa", name: "QA", type: "QA", status: "HEALTHY", url_label: null, is_production: false },
      { environment_id: "env:staging", name: "STAGING", type: "STAGING", status: "DEGRADED", url_label: null, is_production: false },
      { environment_id: "env:prod", name: "PROD", type: "PROD", status: "HEALTHY", url_label: null, is_production: true },
    ],
    signals: [
      {
        signal_id: "signal:env:staging:deployment_risk:primary",
        environment_id: "env:staging",
        signal_type: "deployment_risk",
        title: "Deployment risk is HIGH",
        description: "High deployment risk",
        severity: "HIGH",
        confidence: 0.85,
        related_entity_type: "deployment_risk",
        related_entity_id: "deployment_risk_assessment",
      },
      {
        signal_id: "signal:env:qa:test_recommendation:rec-1",
        environment_id: "env:qa",
        signal_type: "test_recommendation",
        title: "Recommended test: Smoke Suite",
        description: "baseline",
        severity: "LOW",
        confidence: 0.7,
        related_entity_type: "recommended_test",
        related_entity_id: "rec-1",
      },
    ],
    comparisons: [
      {
        comparison_id: "comparison:env:qa:to:env:staging",
        source_environment_id: "env:qa",
        target_environment_id: "env:staging",
        comparison_type: "QA_TO_STAGING",
        summary: "STAGING is riskier than QA (risk delta -23).",
        risk_delta: -23,
        status_delta: "STAGING is riskier than QA",
        confidence: 0.82,
      },
    ],
    promotion_readiness: [
      {
        source_environment_id: "env:staging",
        target_environment_id: "env:prod",
        readiness_status: "BLOCKED",
        readiness_score: 42,
        blockers: ["Payments contract risk is CRITICAL", "Checkout journey is BROKEN"],
        warnings: [],
        recommended_validations: ["Run Payments Regression Suite", "Validate Orders DB"],
        confidence: 0.84,
      },
    ],
  },
};

describe("environmentIntelligenceViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasMultiEnvironmentSection({ multi_environment: null })).toBe(true);
    expect(hasMultiEnvironmentSection({})).toBe(false);
    expect(isMultiEnvironmentEmpty({ multi_environment: null })).toBe(true);
  });

  it("renders multi-environment intelligence", () => {
    const vm = buildMultiEnvironmentIntelligenceViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.title);
    expect(vm.report.environments).toHaveLength(3);
  });

  it("renders empty state message", () => {
    const vm = buildMultiEnvironmentIntelligenceViewModel({ multi_environment: null }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.empty);
  });

  it("builds environment status cards with signal counts", () => {
    const vm = buildMultiEnvironmentIntelligenceViewModel(sampleReport, t);
    const qa = vm.report.environments.find((e) => e.name === "QA");
    const staging = vm.report.environments.find((e) => e.name === "STAGING");
    expect(qa.signalCount).toBe(1);
    expect(staging.signalCount).toBe(1);
    expect(environmentStatusBadgeClass("HEALTHY")).toContain("badge-green");
    expect(environmentStatusBadgeClass("DEGRADED")).toContain("badge-orange");
  });

  it("builds comparison cards", () => {
    const vm = buildMultiEnvironmentIntelligenceViewModel(sampleReport, t);
    expect(vm.report.comparisons[0].routeLabel).toBe("QA → STAGING");
    expect(vm.report.comparisons[0].risk_delta).toBe(-23);
  });

  it("builds promotion readiness cards", () => {
    const vm = buildMultiEnvironmentIntelligenceViewModel(sampleReport, t);
    const readiness = vm.report.promotion_readiness[0];
    expect(readiness.routeLabel).toBe("STAGING → PROD");
    expect(readinessStatusBadgeClass("BLOCKED")).toContain("badge-red");
    expect(readiness.readiness_score).toBe(42);
  });

  it("renders blockers", () => {
    const vm = buildMultiEnvironmentIntelligenceViewModel(sampleReport, t);
    expect(vm.report.promotion_readiness[0].blockers).toHaveLength(2);
  });

  it("renders recommended validations", () => {
    const vm = buildMultiEnvironmentIntelligenceViewModel(sampleReport, t);
    expect(vm.report.promotion_readiness[0].recommended_validations).toContain("Validate Orders DB");
  });

  it("exposes translation keys", () => {
    expect(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.title).toBe("incident.qa.multi_environment");
    expect(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.empty).toBe("incident.qa.multi_environment_empty");
    expect(ENVIRONMENT_INTELLIGENCE_I18N_KEYS.riskDelta).toBe("incident.qa.multi_environment_risk_delta");
  });

  it("builds drilldown integration for signals", () => {
    const item = buildSignalDrilldownItem(sampleReport.multi_environment.signals[0]);
    expect(item.related_entity_type).toBe("deployment_risk");
    expect(item.related_entity_id).toBe("deployment_risk_assessment");
    const vm = buildMultiEnvironmentIntelligenceViewModel(sampleReport, t);
    expect(vm.report.environments[1].signals[0].drilldownItem).not.toBeNull();
  });
});
