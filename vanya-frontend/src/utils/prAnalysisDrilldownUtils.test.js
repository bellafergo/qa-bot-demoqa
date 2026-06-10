import { describe, it, expect } from "vitest";
import {
  findAzureDrilldownPr,
  findGithubDrilldownPr,
  parsePrAnalysisDrilldown,
  resolvePrAnalysisDrilldown,
} from "./prAnalysisDrilldownUtils";

describe("prAnalysisDrilldownUtils", () => {
  it("parses provider and pr from search params", () => {
    const params = new URLSearchParams("provider=github&pr=42");
    expect(parsePrAnalysisDrilldown(params)).toEqual({
      provider: "github",
      prNumber: "42",
    });
    expect(parsePrAnalysisDrilldown(new URLSearchParams("provider=github"))).toBeNull();
  });

  it("finds github drilldown PR without auto-analyze side effects", () => {
    const match = findGithubDrilldownPr([
      { number: 42, title: "Fix auth", branch: "feature/auth" },
    ], "42");
    expect(match?.title).toBe("Fix auth");
  });

  it("resolves github drilldown when PR exists", () => {
    const result = resolvePrAnalysisDrilldown({
      provider: "github",
      prNumber: "42",
      ghPRs: [{ number: 42, title: "Fix auth", branch: "feature/auth" }],
      azPRs: [],
    });
    expect(result.found).toBe(true);
    expect(result.formPatch).toEqual({
      pr_id: "42",
      title: "Fix auth",
      branch: "feature/auth",
    });
  });

  it("returns not found for missing PR", () => {
    const result = resolvePrAnalysisDrilldown({
      provider: "github",
      prNumber: "99",
      ghPRs: [{ number: 42, title: "Fix auth", branch: "feature/auth" }],
      azPRs: [],
    });
    expect(result.found).toBe(false);
  });

  it("finds azure drilldown PR", () => {
    const match = findAzureDrilldownPr([
      { pull_request_id: 7, title: "Azure PR", branch: "dev" },
    ], "7");
    expect(match?.title).toBe("Azure PR");
  });
});
