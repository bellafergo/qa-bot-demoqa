import { describe, expect, it } from "vitest";
import {
  buildQualityHealthTraceViewModel,
  shouldShowQualityHealthTrace,
} from "./qualityHealthExplainabilityViewUtils.js";
import { QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS } from "./qualityHealthExplainabilityViewUtils.js";
import { EVIDENCE_TRACE_I18N_KEYS } from "./incidentEvidenceTraceabilityViewUtils.js";

const t = (key, params) => (params ? `${key}:${JSON.stringify(params)}` : key);

const lowScoreReport = {
  overall_score: 48,
  overall_status: "HIGH_RISK",
  confidence: 0.75,
  trend: "DEGRADING",
  summary: "Quality health is degrading.",
  scores: [
    {
      score_id: "quality_health:project:overall",
      scope_type: "project",
      scope_name: "Project",
      score: 48,
      status: "HIGH_RISK",
      contributing_factors: [
        {
          factor_id: "factor:module:contract",
          title: "Payments contract risk is CRITICAL",
          description: "Removed amount field",
          impact: 25,
          severity: "CRITICAL",
          related_entity_type: "contract_risk",
          related_entity_id: "contract:payments_api",
        },
        {
          factor_id: "factor:module:deployment",
          title: "Deployment risk is HIGH",
          description: "Elevated deployment factors",
          impact: 20,
          severity: "HIGH",
          related_entity_type: "deployment_risk",
          related_entity_id: "deployment:main",
        },
      ],
    },
  ],
};

describe("qualityHealthExplainabilityViewUtils", () => {
  it("shows trace for low scores and attention statuses", () => {
    expect(shouldShowQualityHealthTrace(lowScoreReport)).toBe(true);
    expect(shouldShowQualityHealthTrace({ overall_score: 90, overall_status: "GOOD", scores: [] })).toBe(false);
    expect(shouldShowQualityHealthTrace({ overall_score: 70, overall_status: "ATTENTION", scores: [] })).toBe(true);
    expect(shouldShowQualityHealthTrace({ overall_score: 82, overall_status: "GOOD", scores: [{ contributing_factors: [{}] }] })).toBe(false);
  });

  it("builds trace from contributing factors without recalculating score", () => {
    const trace = buildQualityHealthTraceViewModel(lowScoreReport, t);
    expect(trace.show).toBe(true);
    expect(trace.whyExplanation.title).toBe(QUALITY_HEALTH_EXPLAINABILITY_I18N_KEYS.whyTitle);
    expect(trace.whyExplanation.bullets[0]).toContain("CRITICAL");
    expect(trace.rootCauseContributors.contributors).toHaveLength(2);
    expect(trace.rootCauseContributors.contributors[0].confidenceText).toBe("75%");
    expect(trace.evidenceSources.sources.some((s) => s.key === "contract_risk")).toBe(true);
  });

  it("uses empty state when no factors exist", () => {
    const trace = buildQualityHealthTraceViewModel(
      { overall_score: 8, overall_status: "HIGH_RISK", scores: [] },
      t,
    );
    expect(trace.whyExplanation.empty).toBe(true);
    expect(trace.whyExplanation.emptyMessage).toBe(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence);
  });
});
