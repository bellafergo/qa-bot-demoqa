import { describe, expect, it } from "vitest";
import { hasApiStepEvidence, runSteps, shouldShowApiHttpEvidence } from "./runEvidenceUtils.js";

describe("runEvidenceUtils", () => {
  it("runSteps prefers steps then steps_result", () => {
    expect(runSteps({ steps: [{ action: "a" }] })).toEqual([{ action: "a" }]);
    expect(runSteps({ steps_result: [{ action: "b" }] })).toEqual([{ action: "b" }]);
    expect(runSteps(null)).toEqual([]);
  });

  it("hasApiStepEvidence detects structured step evidence", () => {
    expect(hasApiStepEvidence({ steps: [{ action: "x" }] })).toBe(false);
    expect(
      hasApiStepEvidence({
        steps: [{ action: "catalog_assertions", evidence: { failure: { type: "assertion_failed" } } }],
      })
    ).toBe(true);
  });

  it("shouldShowApiHttpEvidence requires API run type and step evidence", () => {
    const run = {
      steps: [{ evidence: { request: {}, response: {}, failure: {} } }],
    };
    expect(shouldShowApiHttpEvidence(run, "ui")).toBe(false);
    expect(shouldShowApiHttpEvidence(run, "api")).toBe(true);
    expect(shouldShowApiHttpEvidence({ steps: [] }, "api")).toBe(false);
  });
});
