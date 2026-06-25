import { describe, it, expect } from "vitest";
import {
  isLegacyIncidentReport,
  isV13BReport,
  getEngineVersion,
  isAnalyzeOnly,
  hasBrowserProbeEvidence,
  shouldShowEvidenceEmptyState,
  shouldShowBlastRadiusEmptyState,
  shouldShowRecommendedTestsEmptyState,
  shouldShowTemporalEmptyState,
  buildIncidentReportViewModel,
  isEvidenceCorrelationEmpty,
  correlationReasonText,
} from "./incidentReportViewUtils";

const t = (key) => key;

const v13bSparseReport = {
  meta: { engine_version: "incident-v1.3b", analyze_only: true },
  evidence_strength: { evidence: [], inference: [{ label: "Topic keywords" }], assumptions: [] },
  impacted_modules_ranked: [],
  recommended_tests_v2: [],
  temporal_correlation: { signal: "none", reason: "Insufficient temporal events" },
  timeline: [{ event_type: "incident_reported", title: "Incident reported" }],
};

const v13bWithBrowserProbe = {
  ...v13bSparseReport,
  evidence_strength: {
    evidence: [{ label: "Browser Probe", detail: "Passive browser observation completed successfully." }],
    inference: [],
    assumptions: [],
  },
};

const legacyReport = {
  meta: { engine_version: "incident-v1.3", analyze_only: true },
  hypotheses: [{ statement: "test", confidence: 0.5 }],
  impacted_modules: ["auth"],
  recommended_tests: ["AUTH-001"],
};

describe("isLegacyIncidentReport", () => {
  it("detects pre-v1.3B reports missing new fields", () => {
    expect(isLegacyIncidentReport(legacyReport)).toBe(true);
  });

  it("does not flag v1.3B sparse reports", () => {
    expect(isLegacyIncidentReport(v13bSparseReport)).toBe(false);
  });
});

describe("buildIncidentReportViewModel — empty states", () => {
  it("shows empty state flags for sparse v1.3B report", () => {
    const vm = buildIncidentReportViewModel(v13bSparseReport, t);
    expect(vm.v13b).toBe(true);
    expect(vm.showLegacyBanner).toBe(false);
    expect(vm.evidenceStrength.show).toBe(true);
    expect(vm.evidenceStrength.evidenceEmpty).toBe(true);
    expect(vm.evidenceStrength.evidenceEmptyMessage).toBe("incident.qa.evidence_empty");
    expect(vm.blastRadius.show).toBe(true);
    expect(vm.blastRadius.empty).toBe(true);
    expect(vm.blastRadius.emptyMessage).toBe("incident.qa.blast_radius_empty");
    expect(vm.recommendedTests.show).toBe(true);
    expect(vm.recommendedTests.empty).toBe(true);
    expect(vm.recommendedTests.emptyMessage).toBe("incident.qa.recommended_tests_empty");
    expect(vm.temporal.showEmpty).toBe(true);
    expect(vm.temporal.emptyMessage).toBe("incident.qa.temporal_empty");
  });
});

describe("buildIncidentReportViewModel — engine meta", () => {
  it("exposes engine version and analyze_only", () => {
    const vm = buildIncidentReportViewModel(v13bSparseReport, t);
    expect(vm.engineVersion).toBe("incident-v1.3b");
    expect(vm.analyzeOnly).toBe(true);
    expect(vm.showEngineMeta).toBe(false);
  });
});

describe("buildIncidentReportViewModel — legacy banner", () => {
  it("shows legacy banner for old reports", () => {
    const vm = buildIncidentReportViewModel(legacyReport, t);
    expect(vm.showLegacyBanner).toBe(true);
    expect(vm.legacyBannerMessage).toBe("incident.qa.legacy_banner");
    expect(vm.evidenceStrength.show).toBe(false);
    expect(vm.blastRadius.show).toBe(false);
    expect(vm.recommendedTests.show).toBe(false);
  });
});

describe("browser probe evidence", () => {
  it("detects Browser Probe in evidence bucket", () => {
    expect(hasBrowserProbeEvidence(v13bWithBrowserProbe.evidence_strength)).toBe(true);
    const vm = buildIncidentReportViewModel(v13bWithBrowserProbe, t);
    expect(vm.evidenceStrength.hasBrowserProbe).toBe(true);
    expect(vm.evidenceStrength.evidenceEmpty).toBe(false);
  });
});

describe("helper predicates", () => {
  it("shouldShowEvidenceEmptyState when evidence list is empty", () => {
    expect(shouldShowEvidenceEmptyState({ evidence: [] })).toBe(true);
    expect(shouldShowEvidenceEmptyState({ evidence: [{ label: "x" }] })).toBe(false);
  });

  it("shouldShowTemporalEmptyState when signal is none", () => {
    expect(shouldShowTemporalEmptyState({ signal: "none" })).toBe(true);
    expect(shouldShowTemporalEmptyState({ signal: "strong" })).toBe(false);
  });

  it("isV13BReport when evidence_strength exists", () => {
    expect(isV13BReport(v13bSparseReport)).toBe(true);
    expect(isV13BReport(legacyReport)).toBe(false);
  });

  it("getEngineVersion and isAnalyzeOnly", () => {
    expect(getEngineVersion(v13bSparseReport)).toBe("incident-v1.3b");
    expect(isAnalyzeOnly(v13bSparseReport)).toBe(true);
  });

  it("shouldShowBlastRadiusEmptyState and recommended tests empty", () => {
    expect(shouldShowBlastRadiusEmptyState(v13bSparseReport)).toBe(true);
    expect(shouldShowRecommendedTestsEmptyState(v13bSparseReport)).toBe(true);
  });
});

describe("evidence correlation view helpers", () => {
  it("isEvidenceCorrelationEmpty when total_correlations is zero", () => {
    expect(isEvidenceCorrelationEmpty({ total_correlations: 0, evidence: [] })).toBe(true);
    expect(isEvidenceCorrelationEmpty({ total_correlations: 2, evidence: [{ source: "failed_run" }] })).toBe(false);
  });

  it("correlationReasonText returns reason or fallback", () => {
    expect(correlationReasonText(
      { reason: "Run failed 32 minutes before the incident." },
      t,
    )).toBe("Run failed 32 minutes before the incident.");
    expect(correlationReasonText({ reason: "" }, t)).toBe("incident.qa.correlation_reason_unavailable");
    expect(correlationReasonText({}, t)).toBe("incident.qa.correlation_reason_unavailable");
  });
});
