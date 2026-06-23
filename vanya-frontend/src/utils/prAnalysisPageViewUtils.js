/** Safe route-state helpers for PR Analysis page (production readiness). */

import { parsePrAnalysisDrilldown } from "./prAnalysisDrilldownUtils.js";

export function parsePrAnalysisSearch(search) {
  const params = new URLSearchParams(String(search || "").replace(/^\?/, ""));
  return parsePrAnalysisDrilldown(params);
}

export function buildPrAnalysisPageShellClassName({ embedded = false } = {}) {
  return embedded ? "pr-analysis-embedded" : "page-wrap";
}
