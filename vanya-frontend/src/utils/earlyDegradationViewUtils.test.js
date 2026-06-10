import { describe, it, expect } from "vitest";
import {
  EARLY_DEGRADATION_I18N_KEYS,
  buildEarlyDegradationViewModel,
  buildRecommendedAttention,
  degradationStatusBadgeClass,
  degradationStatusLabelKey,
  formatDegradationConfidence,
  riskProjectionLabelKey,
} from "./earlyDegradationViewUtils.js";

const t = (key) => key;

const sampleReport = {
  quality_trends: {
    trends: [
      {
        trend_id: "quality_trend:module:payments",
        scope_type: "module",
        scope_name: "Payments",
        trend_direction: "DEGRADING",
        score_change: -25,
        confidence: 0.82,
        points: [],
      },
    ],
  },
  contract_risk_assessment: {
    assessments: [
      {
        contract_id: "payments",
        overall_risk_level: "CRITICAL",
        affected_modules: ["Payments"],
      },
    ],
  },
  executive_quality_report: {
    top_recommendations: ["Run Payments Regression Suite"],
  },
  historical_learning: {
    similar_incidents: [{ incident_id: "inc-1", title: "Payments outage", summary: "Repeated failures" }],
  },
  early_degradation: {
    overall_status: "CRITICAL_DEGRADATION",
    degrading_areas: 1,
    critical_areas: 1,
    confidence: 0.89,
    summary: "Critical degradation detected in Payments.",
    assessments: [
      {
        assessment_id: "degradation_assessment:module:payments",
        scope_type: "module",
        scope_name: "Payments",
        status: "CRITICAL_DEGRADATION",
        risk_projection: "INCIDENT_LIKELY",
        confidence: 0.89,
        signals: [
          {
            signal_id: "degradation_signal:module:payments",
            scope_type: "module",
            scope_name: "Payments",
            current_score: 57,
            previous_score: 82,
            score_delta: -25,
            degradation_velocity: 8.33,
            severity: "HIGH",
            confidence: 0.85,
            summary: "Payments quality declined from 82 to 57.",
            related_entity_type: "module",
            related_entity_id: "payments",
          },
        ],
      },
      {
        assessment_id: "degradation_assessment:module:authentication",
        scope_type: "module",
        scope_name: "Authentication",
        status: "STABLE",
        risk_projection: "LOW_RISK",
        confidence: 0.75,
        signals: [
          {
            signal_id: "degradation_signal:module:authentication",
            scope_type: "module",
            scope_name: "Authentication",
            current_score: 88,
            previous_score: 91,
            score_delta: -3,
            degradation_velocity: 1.0,
            severity: "LOW",
            confidence: 0.7,
            summary: "Authentication quality remained stable.",
            related_entity_type: "module",
            related_entity_id: "authentication",
          },
        ],
      },
    ],
  },
};

describe("earlyDegradationViewUtils", () => {
  it("renders early degradation report", () => {
    const vm = buildEarlyDegradationViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.report.overallStatusText).toBe(EARLY_DEGRADATION_I18N_KEYS.statusCriticalDegradation);
    expect(vm.report.assessments).toHaveLength(2);
  });

  it("renders empty state", () => {
    const vm = buildEarlyDegradationViewModel({ early_degradation: null }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(EARLY_DEGRADATION_I18N_KEYS.empty);
  });

  it("renders status labels", () => {
    expect(degradationStatusLabelKey("RAPID_DEGRADATION")).toBe(
      EARLY_DEGRADATION_I18N_KEYS.statusRapidDegradation,
    );
    expect(degradationStatusBadgeClass("STABLE")).toContain("badge-green");
  });

  it("renders projection labels", () => {
    expect(riskProjectionLabelKey("INCIDENT_LIKELY")).toBe(
      EARLY_DEGRADATION_I18N_KEYS.projectionIncidentLikely,
    );
  });

  it("formats confidence display", () => {
    expect(formatDegradationConfidence(0.89)).toBe("89%");
    const vm = buildEarlyDegradationViewModel(sampleReport, t);
    expect(vm.report.confidenceText).toBe("89%");
  });

  it("builds drilldown integration", () => {
    const vm = buildEarlyDegradationViewModel(sampleReport, t);
    const payments = vm.report.assessments[0];
    expect(payments.drilldownItems.length).toBeGreaterThan(0);
    expect(payments.recommendedAttention.length).toBeGreaterThan(0);
  });

  it("maps translation keys", () => {
    const vm = buildEarlyDegradationViewModel(sampleReport, t);
    expect(vm.title).toBe(EARLY_DEGRADATION_I18N_KEYS.title);
    expect(buildRecommendedAttention("Payments", sampleReport).some((i) => i.label.includes("Contract Risk"))).toBe(
      true,
    );
  });
});
