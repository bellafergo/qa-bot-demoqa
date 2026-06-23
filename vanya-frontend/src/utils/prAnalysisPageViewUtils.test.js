import { describe, expect, it } from "vitest";
import {
  buildPrAnalysisPageShellClassName,
  parsePrAnalysisSearch,
} from "./prAnalysisPageViewUtils.js";

describe("prAnalysisPageViewUtils", () => {
  it("parses empty search without throwing", () => {
    expect(parsePrAnalysisSearch("")).toBeNull();
    expect(parsePrAnalysisSearch("?")).toBeNull();
  });

  it("parses drilldown query from search string", () => {
    expect(parsePrAnalysisSearch("?provider=github&pr=42")).toEqual({
      provider: "github",
      prNumber: "42",
    });
  });

  it("uses page-wrap only on standalone route", () => {
    expect(buildPrAnalysisPageShellClassName()).toBe("page-wrap");
    expect(buildPrAnalysisPageShellClassName({ embedded: true })).toBe("pr-analysis-embedded");
  });
});
