import { describe, expect, it } from "vitest";
import {
  buildBusinessRiskTraceViewModel,
  buildBusinessRiskWhyExplanation,
  filterSignalsForRisk,
  shouldShowBusinessRiskTrace,
} from "./businessRiskExplainabilityViewUtils.js";
import { EVIDENCE_TRACE_I18N_KEYS } from "./incidentEvidenceTraceabilityViewUtils.js";

const t = (key, params) => {
  if (params) {
    return `${key}:${JSON.stringify(params)}`;
  }
  return key;
};

const sampleRisk = {
  risk_id: "risk:revenue_collection",
  capability: "Revenue Collection",
  severity: "HIGH",
  confidence: "MEDIUM",
  summary: "Revenue Collection is at high business risk supported by open Jira blockers.",
  evidence: ["open Jira blockers", "critical contract changes"],
};

const sampleSignals = [
  {
    signal_id: "signal:revenue_collection:jira_blockers:0",
    title: "Open Jira Blockers",
    severity: "HIGH",
    confidence: "MEDIUM",
    impacted_capability: "Revenue Collection",
    evidence_count: 2,
  },
  {
    signal_id: "signal:customer_purchase_flow:broken_journeys:0",
    title: "Broken Data Journeys",
    severity: "CRITICAL",
    confidence: "HIGH",
    impacted_capability: "Customer Purchase Flow",
    evidence_count: 1,
  },
];

describe("businessRiskExplainabilityViewUtils", () => {
  it("filters signals by impacted capability", () => {
    const matching = filterSignalsForRisk(sampleRisk, sampleSignals);
    expect(matching).toHaveLength(1);
    expect(matching[0].signal_id).toContain("jira_blockers");
  });

  it("shows trace for high severity or evidence", () => {
    expect(shouldShowBusinessRiskTrace(sampleRisk, sampleSignals)).toBe(true);
    expect(shouldShowBusinessRiskTrace({ severity: "LOW", evidence: [] }, [])).toBe(false);
    expect(shouldShowBusinessRiskTrace({ severity: "LOW", evidence: ["x"] }, [])).toBe(true);
  });

  it("builds why explanation from evidence and matching signals", () => {
    const why = buildBusinessRiskWhyExplanation(sampleRisk, sampleSignals, t);
    expect(why.title).toBe(EVIDENCE_TRACE_I18N_KEYS.whyExplanation);
    expect(why.bullets).toContain("open Jira blockers");
    expect(why.bullets).toContain("critical contract changes");
    expect(why.empty).toBe(false);
    expect(why.prefix).toContain("explainability.business_risk.why_prefix");
  });

  it("uses enum confidence labels in contributors, not percentages", () => {
    const trace = buildBusinessRiskTraceViewModel(sampleRisk, sampleSignals, t);
    expect(trace.show).toBe(true);
    expect(trace.rootCauseContributors.contributors).toHaveLength(1);
    expect(trace.rootCauseContributors.contributors[0].confidenceText).toBe("business_risk_overview.confidence.medium");
    expect(trace.rootCauseContributors.contributors[0].confidenceText).not.toMatch(/%/);
  });

  it("aggregates evidence sources from signal ids", () => {
    const trace = buildBusinessRiskTraceViewModel(sampleRisk, sampleSignals, t);
    expect(trace.evidenceSources.sources).toHaveLength(1);
    expect(trace.evidenceSources.sources[0].key).toBe("jira_blockers");
    expect(trace.evidenceSources.sources[0].count).toBe(2);
  });

  it("returns empty state message when no evidence exists", () => {
    const trace = buildBusinessRiskTraceViewModel(
      { capability: "Payments", severity: "CRITICAL", evidence: [] },
      [],
      t,
    );
    expect(trace.whyExplanation.empty).toBe(true);
    expect(trace.whyExplanation.emptyMessage).toBe(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence);
  });
});
