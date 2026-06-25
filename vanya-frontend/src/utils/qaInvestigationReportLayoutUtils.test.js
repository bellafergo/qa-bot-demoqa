import { describe, expect, it } from "vitest";
import {
  buildQaInvestigationReportLayoutViewModel,
  isGenericInferredModule,
  isInsufficientEvidence,
  normalizeInvestigationConfidence,
  partitionImpactMapNodes,
  resolveConfidenceTier,
  sliceTopItems,
} from "./qaInvestigationReportLayoutUtils.js";

const t = (key) => key;

describe("qaInvestigationReportLayoutUtils", () => {
  it("normalizes confidence from ratio and percentage", () => {
    expect(normalizeInvestigationConfidence(0.45)).toBe(0.45);
    expect(normalizeInvestigationConfidence(45)).toBe(0.45);
    expect(normalizeInvestigationConfidence("bad")).toBe(0);
  });

  it("resolves confidence tiers", () => {
    expect(resolveConfidenceTier(0.1)).toBe("low");
    expect(resolveConfidenceTier(0.39)).toBe("low");
    expect(resolveConfidenceTier(0.4)).toBe("medium");
    expect(resolveConfidenceTier(0.6)).toBe("medium");
    expect(resolveConfidenceTier(0.79)).toBe("medium");
    expect(resolveConfidenceTier(0.81)).toBe("high");
  });

  it("exposes confidence presentation fields", () => {
    const vm = buildQaInvestigationReportLayoutViewModel({ confidence: 0.05 }, t);
    expect(vm.confidencePctText).toBe("5%");
    expect(vm.showInsufficientEvidenceState).toBe(true);
    expect(vm.showEvidenceQualityMetric).toBe(false);
    expect(vm.confidenceBadgeClass).toBe("badge badge-orange");
    expect(vm.degradedPresentation).toBe(true);
    expect(vm.insufficientEvidenceTitle).toBe("incident.qa.insufficient_evidence_title");
  });

  it("shows evidence quality metric when confidence is sufficient", () => {
    const vm = buildQaInvestigationReportLayoutViewModel({ confidence: 0.45 }, t);
    expect(vm.showInsufficientEvidenceState).toBe(false);
    expect(vm.showEvidenceQualityMetric).toBe(true);
    expect(vm.evidenceQualityLabel).toBe("incident.qa.evidence_quality");
  });

  it("detects insufficient evidence below threshold", () => {
    expect(isInsufficientEvidence(0.19)).toBe(true);
    expect(isInsufficientEvidence(0.2)).toBe(false);
  });

  it("flags generic inferred modules", () => {
    expect(isGenericInferredModule({ title: "Self" })).toBe(true);
    expect(isGenericInferredModule({ title: "Payments API", confidence: 0.8 })).toBe(false);
    expect(isGenericInferredModule({ title: "Checkout", confidence: 0.2 })).toBe(true);
  });

  it("partitions impact map nodes into confirmed and inferred", () => {
    const nodes = [
      { title: "Checkout", confidence: 0.8 },
      { title: "Self", confidence: 0.9 },
      { title: "Auth", confidence: 0.2 },
    ];
    const { confirmed, inferred } = partitionImpactMapNodes(nodes);
    expect(confirmed.map((n) => n.title)).toEqual(["Checkout"]);
    expect(inferred.map((n) => n.title)).toEqual(["Self", "Auth"]);
  });

  it("slices top items with remainder", () => {
    const items = [1, 2, 3, 4, 5];
    expect(sliceTopItems(items, 3)).toEqual({
      top: [1, 2, 3],
      remainder: [4, 5],
      hasRemainder: true,
    });
    expect(sliceTopItems([1, 2], 3).hasRemainder).toBe(false);
  });

  it("builds layout view model with low-confidence visibility flags", () => {
    const low = buildQaInvestigationReportLayoutViewModel({ confidence: 0.1 }, t);
    expect(low.lowConfidence).toBe(true);
    expect(low.showNoisySections).toBe(false);
    expect(low.showInsufficientEvidenceState).toBe(true);
    expect(low.showLowConfidenceWarning).toBe(false);

    const mediumLow = buildQaInvestigationReportLayoutViewModel({ confidence: 0.25 }, t);
    expect(mediumLow.showInsufficientEvidenceState).toBe(false);
    expect(mediumLow.showLowConfidenceWarning).toBe(true);

    const high = buildQaInvestigationReportLayoutViewModel({ confidence: 0.85 }, t);
    expect(high.tier).toBe("high");
    expect(high.confidenceBadgeClass).toBe("badge badge-green");
    expect(high.expandOperationalAnalysis).toBe(true);
    expect(high.expandDependencyIntelligence).toBe(false);
    expect(high.expandTechnicalEvidence).toBe(false);
  });
});
