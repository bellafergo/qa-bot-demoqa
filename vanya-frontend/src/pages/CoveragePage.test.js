import { describe, expect, it } from "vitest";
import { isCoverageInsufficientHistory } from "../utils/coveragePageViewUtils.js";

describe("isCoverageInsufficientHistory", () => {
  it("detects zero coverage with no test history", () => {
    expect(isCoverageInsufficientHistory({
      total_test_cases: 0,
      modules: [{ module: "auth", coverage_pct: 0 }],
    })).toBe(true);
  });

  it("returns false when tests exist", () => {
    expect(isCoverageInsufficientHistory({
      total_test_cases: 2,
      modules: [{ module: "auth", coverage_pct: 0 }],
    })).toBe(false);
  });
});
