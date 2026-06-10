import { describe, it, expect } from "vitest";
import {
  EXECUTIVE_QUALITY_REPORT_I18N_KEYS,
  buildExecutiveQualityReportViewModel,
  formatExecutiveConfidence,
  hasExecutiveQualityReportSection,
  isExecutiveQualityReportEmpty,
  qualityScoreBand,
  riskLevelBadgeClass,
} from "./executiveQualityReportViewUtils.js";

const t = (key) => key;

const sampleReport = {
  executive_quality_report: {
    report_id: "executive_quality_report:demo:abc12345",
    generated_at: "2026-06-10T10:00:00+00:00",
    overall_quality_score: 58,
    overall_risk_level: "MEDIUM",
    confidence: 0.82,
    executive_summary: "The platform shows moderate quality risk.",
    top_risks: ["Payments contract risk is CRITICAL", "Checkout Journey is BROKEN"],
    top_recommendations: ["Execute Payments Regression Suite", "Review Checkout Journey"],
    open_incident_count: 2,
    critical_incident_count: 1,
    critical_contract_count: 1,
    broken_journey_count: 1,
    recommended_test_count: 2,
    historical_pattern_summary: "Recurring payments incidents.",
    quality_trend: "elevated",
  },
};

describe("executiveQualityReportViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasExecutiveQualityReportSection({ executive_quality_report: null })).toBe(true);
    expect(hasExecutiveQualityReportSection({})).toBe(false);
    expect(isExecutiveQualityReportEmpty({ executive_quality_report: null })).toBe(true);
  });

  it("renders executive report", () => {
    const vm = buildExecutiveQualityReportViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.report.overall_quality_score).toBe(58);
    expect(vm.report.top_risks).toHaveLength(2);
  });

  it("renders empty state via i18n key", () => {
    const vm = buildExecutiveQualityReportViewModel({ executive_quality_report: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.executive_quality_report_empty");
  });

  it("maps quality score bands", () => {
    expect(qualityScoreBand(95).labelKey).toBe("incident.qa.executive_quality_report_score_excellent");
    expect(qualityScoreBand(80).labelKey).toBe("incident.qa.executive_quality_report_score_good");
    expect(qualityScoreBand(60).labelKey).toBe("incident.qa.executive_quality_report_score_attention");
    expect(qualityScoreBand(30).labelKey).toBe("incident.qa.executive_quality_report_score_high_risk");
  });

  it("maps risk badges and confidence", () => {
    expect(riskLevelBadgeClass("CRITICAL")).toBe("badge badge-red");
    expect(formatExecutiveConfidence(0.82)).toBe("82%");
  });

  it("exposes translation keys", () => {
    expect(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.title).toBe("incident.qa.executive_quality_report");
    expect(EXECUTIVE_QUALITY_REPORT_I18N_KEYS.topRisks).toBe(
      "incident.qa.executive_quality_report_top_risks",
    );
  });
});
