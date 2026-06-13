import { describe, it, expect } from "vitest";
import {
  QMETRY_COVERAGE_I18N_KEYS,
  buildCoverageIntelligenceViewModel,
  buildCoverageOverviewViewModel,
  mapCoverageAssessment,
  mapCoverageGap,
} from "./qmetryCoverageViewUtils.js";

const t = (key) => key;

const sampleCoverage = {
  connected: true,
  total_test_cases: 7,
  total_matches: 5,
  executive_summary: "Revenue Collection has weak test coverage (1 matching test case).",
  coverage_assessments: [
    { capability: "Revenue Collection", matched_tests: 1, total_tests: 7, coverage_status: "WEAK" },
    { capability: "Customer Purchase Flow", matched_tests: 2, total_tests: 7, coverage_status: "MODERATE" },
    { capability: "Recruiting Operations", matched_tests: 0, total_tests: 7, coverage_status: "NONE" },
  ],
  coverage_gaps: [
    { capability: "Recruiting Operations", module: "Candidate", severity: "MEDIUM", reason: "No tests" },
    { capability: "Revenue Collection", module: "Payment", severity: "CRITICAL", reason: "Blocker overlap" },
  ],
};

describe("qmetryCoverageViewUtils", () => {
  it("maps coverage assessment cards", () => {
    const vm = mapCoverageAssessment(sampleCoverage.coverage_assessments[0], t);
    expect(vm.capability).toBe("Revenue Collection");
    expect(vm.coverageStatus).toBe("WEAK");
    expect(vm.statusLabel).toBe(QMETRY_COVERAGE_I18N_KEYS.statusWeak);
  });

  it("maps coverage gap cards", () => {
    const vm = mapCoverageGap(sampleCoverage.coverage_gaps[1], t);
    expect(vm.severity).toBe("CRITICAL");
    expect(vm.severityLabel).toBe(QMETRY_COVERAGE_I18N_KEYS.severityCritical);
  });

  it("builds incident coverage view model", () => {
    const vm = buildCoverageIntelligenceViewModel({ coverage_intelligence: sampleCoverage }, t);
    expect(vm.show).toBe(true);
    expect(vm.assessments).toHaveLength(3);
    expect(vm.criticalGaps).toHaveLength(1);
    expect(vm.mediumGaps).toHaveLength(1);
  });

  it("shows empty connection state", () => {
    const vm = buildCoverageOverviewViewModel({ connected: false }, t);
    expect(vm.empty).toBe(true);
    expect(vm.showContent).toBe(false);
    expect(vm.capabilityState.state).toBe("INTEGRATION_REQUIRED");
  });

  it("shows available state when connected with test cases but no matches", () => {
    const vm = buildCoverageOverviewViewModel(
      { connected: true, total_test_cases: 3, total_matches: 0 },
      t,
    );
    expect(vm.showContent).toBe(true);
    expect(vm.capabilityState.state).toBe("AVAILABLE");
  });

  it("shows insufficient history when connected without test data", () => {
    const vm = buildCoverageOverviewViewModel(
      { connected: true, total_test_cases: 0, total_matches: 0 },
      t,
    );
    expect(vm.capabilityState.state).toBe("INSUFFICIENT_HISTORY");
  });

  it("renders dashboard overview", () => {
    const vm = buildCoverageOverviewViewModel(sampleCoverage, t);
    expect(vm.show).toBe(true);
    expect(vm.totalMatches).toBe("5");
    expect(vm.executiveSummary).toContain("Revenue Collection");
  });
});
