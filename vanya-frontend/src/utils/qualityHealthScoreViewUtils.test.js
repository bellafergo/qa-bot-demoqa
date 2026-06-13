import { describe, it, expect } from "vitest";
import {
  QUALITY_HEALTH_SCORE_I18N_KEYS,
  buildFactorDrilldownItem,
  buildQualityHealthScoreViewModel,
  hasQualityHealthSection,
  healthStatusBadgeClass,
  healthTrendLabelKey,
  isQualityHealthEmpty,
} from "./qualityHealthScoreViewUtils.js";

const t = (key) => key;

const sampleReport = {
  quality_health: {
    overall_score: 82,
    overall_status: "GOOD",
    confidence: 0.84,
    trend: "STABLE",
    summary: "Quality health score 82/100 (GOOD). Trend STABLE.",
    scores: [
      {
        score_id: "quality_health:project:overall",
        scope_type: "project",
        scope_name: "Project",
        environment: null,
        score: 82,
        status: "GOOD",
        confidence: 0.84,
        trend: "STABLE",
        contributing_factors: [],
      },
      {
        score_id: "quality_health:module:payments",
        scope_type: "module",
        scope_name: "Payments",
        environment: null,
        score: 48,
        status: "HIGH_RISK",
        confidence: 0.75,
        trend: "DEGRADING",
        contributing_factors: [
          {
            factor_id: "factor:module:contract_contract_payments_api",
            title: "Payments contract risk is CRITICAL",
            description: "Removed amount field",
            impact: 25,
            severity: "CRITICAL",
            related_entity_type: "contract_risk",
            related_entity_id: "contract:payments_api",
          },
          {
            factor_id: "factor:module:test_rec_1",
            title: "Payments Regression Suite validation recommended",
            description: "payments",
            impact: 5,
            severity: "MEDIUM",
            related_entity_type: "recommended_test",
            related_entity_id: "rec-1",
          },
        ],
      },
    ],
  },
};

describe("qualityHealthScoreViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasQualityHealthSection({ quality_health: null })).toBe(true);
    expect(hasQualityHealthSection({})).toBe(false);
    expect(isQualityHealthEmpty({ quality_health: null })).toBe(true);
  });

  it("renders quality health report", () => {
    const vm = buildQualityHealthScoreViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(QUALITY_HEALTH_SCORE_I18N_KEYS.title);
    expect(vm.report.overall_score).toBe(82);
    expect(vm.report.showTrace).toBe(false);
  });

  it("attaches explainability trace for low quality health scores", () => {
    const vm = buildQualityHealthScoreViewModel({
      quality_health: {
        overall_score: 8,
        overall_status: "HIGH_RISK",
        confidence: 0.73,
        trend: "DEGRADING",
        summary: "Quality health is critically low.",
        scores: [
          {
            score_id: "quality_health:project:overall",
            scope_type: "project",
            scope_name: "Project",
            score: 8,
            status: "HIGH_RISK",
            contributing_factors: [
              {
                factor_id: "factor:auth",
                title: "Broken Authentication Journey",
                description: "Failed validations",
                impact: 30,
                severity: "CRITICAL",
                related_entity_type: "journey",
                related_entity_id: "journey:auth",
              },
            ],
          },
        ],
      },
    }, t);
    expect(vm.report.showTrace).toBe(true);
    expect(vm.report.trace.show).toBe(true);
    expect(vm.report.trace.whyExplanation.bullets[0]).toContain("Authentication");
  });

  it("renders empty state message", () => {
    const vm = buildQualityHealthScoreViewModel({ quality_health: null }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(QUALITY_HEALTH_SCORE_I18N_KEYS.empty);
  });

  it("displays overall score and status labels", () => {
    const vm = buildQualityHealthScoreViewModel(sampleReport, t);
    expect(vm.report.overallStatusLabel).toBe(QUALITY_HEALTH_SCORE_I18N_KEYS.statusGood);
    expect(vm.report.overallTrendLabel).toBe(QUALITY_HEALTH_SCORE_I18N_KEYS.trendStable);
    expect(healthStatusBadgeClass("GOOD")).toContain("badge-blue");
  });

  it("renders trend labels", () => {
    expect(healthTrendLabelKey("DEGRADING")).toBe(QUALITY_HEALTH_SCORE_I18N_KEYS.trendDegrading);
    const vm = buildQualityHealthScoreViewModel(sampleReport, t);
    expect(vm.report.moduleScores[0].trendLabel).toBe(QUALITY_HEALTH_SCORE_I18N_KEYS.trendDegrading);
  });

  it("renders factors on scope cards", () => {
    const vm = buildQualityHealthScoreViewModel(sampleReport, t);
    expect(vm.report.moduleScores[0].factors).toHaveLength(2);
  });

  it("builds drilldown integration for factors", () => {
    const factor = sampleReport.quality_health.scores[1].contributing_factors[0];
    const item = buildFactorDrilldownItem(factor);
    expect(item.related_entity_type).toBe("contract_risk");
    const vm = buildQualityHealthScoreViewModel(sampleReport, t);
    expect(vm.report.moduleScores[0].factors[0].drilldownItem).not.toBeNull();
  });

  it("exposes translation keys", () => {
    expect(QUALITY_HEALTH_SCORE_I18N_KEYS.title).toBe("incident.qa.quality_health_score");
    expect(QUALITY_HEALTH_SCORE_I18N_KEYS.contributingFactors).toBe("incident.qa.quality_health_score_contributing_factors");
  });
});
