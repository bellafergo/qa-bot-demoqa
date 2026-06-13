import { describe, it, expect } from "vitest";
import {
  EVIDENCE_TRACE_I18N_KEYS,
  parseInsightText,
  isCriticalOrHighInsight,
  formatTraceConfidence,
  buildEvidenceSummary,
  buildRootCauseContributors,
  buildEvidenceSources,
  buildWhyExplanation,
  groupIncidentRisks,
  buildInsightTraceViewModel,
  buildInsightTextForJourney,
  buildInsightTextForDeployment,
  buildInsightTextForContract,
} from "./incidentEvidenceTraceabilityViewUtils.js";

const t = (key, vars) => {
  const templates = {
    "incident.qa.evidence_trace.summary_correlated_runs": "{{count}} correlated failed runs",
    "incident.qa.evidence_trace.summary_test_failures": "{{test}} failed {{count}} times",
    "incident.qa.evidence_trace.summary_module_affected": "{{module}} module affected",
    "incident.qa.evidence_trace.summary_cluster_overlap": "Failure cluster overlap detected",
    "incident.qa.evidence_trace.summary_historical_match": "Historical incident match ({{count}} similar)",
    "incident.qa.evidence_trace.why_prefix": "Vanya marked {{subject}} as {{severity}} because:",
    "incident.qa.evidence_trace.why_failures_increased": "Login-related failures increased {{pct}}%",
    "incident.qa.evidence_trace.why_dependency_map": "Authentication dependency map contains critical nodes",
    "incident.qa.evidence_trace.why_historical_incidents": "Similar incidents historically required regression validation ({{count}} matches)",
    "incident.qa.evidence_trace.why_journey_broken": "Journey validation shows broken stages with downstream impact",
    "incident.qa.evidence_trace.summary_deployment_factors": "{{count}} deployment contributing factors",
    "incident.qa.evidence_trace.summary_critical_contracts": "{{count}} critical contract assessments",
  };
  let s = templates[key] || key;
  if (vars) {
    for (const [k, v] of Object.entries(vars)) {
      s = s.split(`{{${k}}}`).join(String(v));
    }
  }
  return s;
};

const sampleReport = {
  related_runs: [
    { run_id: "r1", test_name: "TC-TOS-008", status: "failed", module: "Auth" },
    { run_id: "r2", test_name: "TC-TOS-008", status: "failed", module: "Auth" },
    { run_id: "r3", test_name: "TC-TOS-008", status: "failed", module: "Auth" },
    { run_id: "r4", test_name: "TC-TOS-008", status: "failed", module: "Auth" },
    { run_id: "r5", test_name: "TC-AUTH-001", status: "failed", module: "Auth" },
  ],
  related_prs: [{ pr_id: "101" }, { pr_id: "102" }],
  timeline: [{ event_type: "failure_cluster", title: "Auth cluster" }],
  evidence_correlation: {
    total_correlations: 8,
    evidence: [{ source: "failure_cluster", related_entity_type: "failure_cluster", related_entity_id: "c1" }],
  },
  evidence_strength: {
    evidence: [{ label: "System memory hit" }],
    inference: [{ label: "Pattern match" }],
    assumptions: [],
  },
  evidence_found: ["Auth regression pattern"],
  hypotheses: [
    { id: "h1", statement: "Login selector missing", confidence: 0.85, basis: "Auth failures" },
    { id: "h2", statement: "Auth contract changed", confidence: 0.8, basis: "Contract drift" },
    { id: "h3", statement: "Regression cluster overlap", confidence: 0.75, basis: "Cluster overlap" },
  ],
  deployment_risk_assessment: {
    risk_score: 92,
    risk_level: "critical",
    confidence: 0.9,
    summary: "Deployment should be blocked until auth validation completes.",
    contributing_factors: [
      { title: "Repeated Auth Failures", description: "Cluster overlap", weight: 0.24, related_entity_type: "failure_cluster", related_entity_id: "c1" },
    ],
  },
  contract_risk_assessment: {
    confidence: 0.88,
    summary: "Contract drift detected.",
    assessments: [
      {
        assessment_id: "a1",
        contract_id: "payments-api",
        overall_risk_level: "CRITICAL",
        risk_score: 88,
        confidence: 0.9,
        summary: "Payments contract breaking change.",
        contract: { service_name: "Payments API", contract_id: "payments-api" },
        factors: [{ factor_id: "f1", title: "Schema mismatch", description: "Response shape changed", weight: 0.3 }],
        affected_journeys: ["Checkout Journey"],
      },
    ],
  },
  data_journey_validation: {
    confidence: 0.8,
    summary: "Journey validation complete.",
    journeys: [{ journey_id: "j-auth", name: "Authentication Journey", business_area: "Auth", stages: [] }],
    results: [
      {
        journey_id: "j-auth",
        status: "BROKEN",
        completed_stages: 1,
        total_stages: 4,
        confidence: 0.82,
        summary: "Auth journey broken.",
        missing_stages: ["Login", "Session"],
        inconsistent_stages: [],
      },
    ],
  },
  enterprise_dependency_map: {
    nodes: [{ node_id: "n1", label: "Auth Gateway", risk_level: "CRITICAL" }],
    edges: [],
  },
  historical_learning: {
    similar_incidents: [
      { incident_id: "i1" },
      { incident_id: "i2" },
      { incident_id: "i3" },
      { incident_id: "i4" },
      { incident_id: "i5" },
      { incident_id: "i6" },
    ],
  },
  executive_quality_report: {
    overall_quality_score: 42,
    overall_risk_level: "CRITICAL",
    confidence: 0.86,
    executive_summary: "Critical quality degradation.",
    top_risks: [
      "Authentication Journey is BROKEN",
      "Payments contract risk is CRITICAL",
      "Deployment Risk is CRITICAL",
      "Executive Quality Risk is CRITICAL",
    ],
    historical_pattern_summary: "Similar incidents historically required Auth regression validation",
    broken_journey_count: 1,
    critical_contract_count: 1,
  },
  decision_center: {
    overall_status: "RED",
    executive_summary: "Do not promote.",
    confidence: 0.9,
    key_takeaways: [{ title: "Block release", description: "Auth validation required", priority: 95 }],
  },
  impacted_modules: ["Auth"],
};

describe("incidentEvidenceTraceabilityViewUtils", () => {
  it("parses insight text with severity", () => {
    const parsed = parseInsightText("Authentication Journey is BROKEN");
    expect(parsed.subject).toBe("Authentication Journey");
    expect(parsed.severity).toBe("BROKEN");
  });

  it("detects critical and high insights", () => {
    expect(isCriticalOrHighInsight("Deployment Risk is CRITICAL")).toBe(true);
    expect(isCriticalOrHighInsight("Contract Risk is LOW")).toBe(false);
    expect(isCriticalOrHighInsight("Authentication Journey is BROKEN")).toBe(true);
  });

  it("formats trace confidence", () => {
    expect(formatTraceConfidence(0.85)).toBe("85%");
    expect(formatTraceConfidence(82)).toBe("82%");
  });

  it("builds evidence summary bullets from correlated runs", () => {
    const summary = buildEvidenceSummary(sampleReport, "Authentication Journey is BROKEN", t);
    expect(summary.empty).toBe(false);
    expect(summary.bullets.some((b) => b.includes("5 correlated failed runs"))).toBe(true);
    expect(summary.bullets.some((b) => b.includes("TC-TOS-008 failed 4 times"))).toBe(true);
    expect(summary.bullets.some((b) => b.includes("Auth module affected"))).toBe(true);
    expect(summary.bullets.some((b) => b.includes("Historical incident match (6 similar)"))).toBe(true);
  });

  it("builds root cause contributors sorted by confidence", () => {
    const vm = buildRootCauseContributors(sampleReport, "Authentication Journey is BROKEN", t);
    expect(vm.contributors.length).toBeGreaterThan(0);
    expect(vm.contributors[0].title).toBe("Login selector missing");
    expect(vm.contributors[0].confidenceText).toBe("85%");
    expect(vm.contributors.some((c) => c.title === "Auth contract changed")).toBe(true);
  });

  it("builds evidence source counts", () => {
    const sources = buildEvidenceSources(sampleReport, "Authentication Journey is BROKEN", t);
    const runs = sources.sources.find((s) => s.key === "runs");
    const prs = sources.sources.find((s) => s.key === "prs");
    const historical = sources.sources.find((s) => s.key === "historicalIncidents");
    expect(runs.count).toBe(5);
    expect(prs.count).toBe(2);
    expect(historical.count).toBe(6);
    expect(sources.empty).toBe(false);
  });

  it("builds why explanation for critical insights", () => {
    const why = buildWhyExplanation(sampleReport, "Authentication Journey is BROKEN", t);
    expect(why.show).toBe(true);
    expect(why.prefix).toContain("Authentication Journey");
    expect(why.bullets.length).toBeGreaterThan(0);
    expect(why.bullets.some((b) => b.includes("dependency map"))).toBe(true);
  });

  it("groups incident risks into technical operational executive buckets", () => {
    const grouped = groupIncidentRisks(sampleReport, t);
    expect(grouped.hasItems).toBe(true);
    expect(grouped.technicalRisk.items.some((i) => i.title.toLowerCase().includes("contract"))).toBe(true);
    expect(grouped.technicalRisk.items.some((i) => i.title.toLowerCase().includes("deployment"))).toBe(true);
    expect(grouped.operationalRisk.items.some((i) => i.title.includes("Authentication Journey"))).toBe(true);
    expect(grouped.executiveRisk.items.length).toBeGreaterThan(0);
  });

  it("attaches trace view model to grouped risk items", () => {
    const grouped = groupIncidentRisks(sampleReport, t);
    const journeyRisk = grouped.operationalRisk.items.find((i) => i.title.includes("Authentication Journey"));
    expect(journeyRisk.trace.show).toBe(true);
    expect(journeyRisk.trace.evidenceSummary.bullets.length).toBeGreaterThan(0);
  });

  it("builds full insight trace view model", () => {
    const trace = buildInsightTraceViewModel(sampleReport, "Deployment Risk is CRITICAL", t);
    expect(trace.show).toBe(true);
    expect(trace.evidenceSummary.title).toBe(EVIDENCE_TRACE_I18N_KEYS.evidenceSummary);
    expect(trace.rootCauseContributors.title).toBe(EVIDENCE_TRACE_I18N_KEYS.rootCauseContributors);
    expect(trace.evidenceSources.title).toBe(EVIDENCE_TRACE_I18N_KEYS.evidenceSources);
    expect(trace.whyExplanation.show).toBe(true);
  });

  it("returns empty state message when no evidence exists", () => {
    const sparse = { related_runs: [], related_prs: [], hypotheses: [] };
    const summary = buildEvidenceSummary(sparse, "Unknown Risk is CRITICAL", t);
    const contributors = buildRootCauseContributors(sparse, "Unknown Risk is CRITICAL", t);
    const sources = buildEvidenceSources(sparse, "Unknown Risk is CRITICAL", t);
    expect(summary.empty).toBe(true);
    expect(summary.emptyMessage).toBe(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence);
    expect(contributors.empty).toBe(true);
    expect(sources.empty).toBe(true);
  });

  it("does not show why explanation for low severity insights", () => {
    const why = buildWhyExplanation(sampleReport, "Minor drift is LOW", t);
    expect(why.show).toBe(false);
  });

  it("builds journey insight helper text", () => {
    expect(buildInsightTextForJourney({ name: "Authentication Journey", status: "BROKEN" })).toBe(
      "Authentication Journey is BROKEN",
    );
  });

  it("builds deployment insight helper text", () => {
    expect(buildInsightTextForDeployment({ risk_level: "critical" })).toBe("Deployment Risk is CRITICAL");
  });

  it("builds contract insight helper text", () => {
    expect(
      buildInsightTextForContract({
        contract: { service_name: "Payments API" },
        overall_risk_level: "CRITICAL",
      }),
    ).toBe("Payments API contract risk is CRITICAL");
  });

  it("includes deployment factors in evidence summary for deployment insights", () => {
    const summary = buildEvidenceSummary(sampleReport, "Deployment Risk is CRITICAL", t);
    expect(summary.bullets.some((b) => b.includes("deployment contributing factors"))).toBe(true);
  });

  it("includes critical contract count for contract insights", () => {
    const summary = buildEvidenceSummary(sampleReport, "Payments contract risk is CRITICAL", t);
    expect(summary.bullets.some((b) => b.includes("critical contract assessments"))).toBe(true);
  });

  it("deduplicates root cause contributors by title", () => {
    const report = {
      ...sampleReport,
      hypotheses: [
        { statement: "Login selector missing", confidence: 0.85 },
        { statement: "login selector missing", confidence: 0.7 },
      ],
    };
    const vm = buildRootCauseContributors(report, "Authentication Journey is BROKEN", t);
    const titles = vm.contributors.map((c) => c.title.toLowerCase());
    expect(new Set(titles).size).toBe(titles.length);
  });

  it("limits evidence summary bullets to readable executive set", () => {
    const summary = buildEvidenceSummary(sampleReport, "Authentication Journey is BROKEN", t);
    expect(summary.bullets.length).toBeLessThanOrEqual(6);
  });

  it("groups dependency map nodes under technical risk", () => {
    const grouped = groupIncidentRisks(sampleReport, t);
    expect(grouped.technicalRisk.items.some((i) => i.title.includes("Auth Gateway"))).toBe(true);
  });

  it("places decision center red status in executive risk bucket", () => {
    const grouped = groupIncidentRisks(sampleReport, t);
    expect(grouped.executiveRisk.items.some((i) => i.title.includes("Decision Center"))).toBe(true);
  });

  it("exposes i18n keys for evidence trace sections", () => {
    expect(EVIDENCE_TRACE_I18N_KEYS.evidenceSummary).toBe("incident.qa.evidence_trace.evidence_summary");
    expect(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence).toBe("incident.qa.evidence_trace.no_supporting_evidence");
  });
});
