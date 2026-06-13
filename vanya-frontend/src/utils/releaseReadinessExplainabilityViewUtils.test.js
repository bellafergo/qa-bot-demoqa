import { describe, expect, it } from "vitest";
import {
  buildReleaseReadinessTraceViewModel,
  shouldShowReleaseReadinessTrace,
} from "./releaseReadinessExplainabilityViewUtils.js";
import { RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS } from "./releaseReadinessExplainabilityViewUtils.js";
import { buildReleaseReadinessViewModel } from "./releaseReadinessViewUtils.js";
import { EVIDENCE_TRACE_I18N_KEYS } from "./incidentEvidenceTraceabilityViewUtils.js";

const t = (key, params) => (params ? `${key}:${JSON.stringify(params)}` : key);

const blockedVmBase = {
  empty: false,
  overallStatus: "BLOCKED",
  deploymentRisk: {
    risk_level: "CRITICAL",
    risk_score: 88,
    confidence: 0.73,
    summary: "Elevated deployment risk.",
    contributing_factors: [
      { title: "Recent production incidents", description: "3 incidents in 7 days", weight: 0.4 },
    ],
  },
  decisionCenterVm: {
    center: {
      confidence: 0.73,
      key_takeaways: [{ title: "Validate authentication journey before release", priority: 90 }],
    },
  },
  multiEnvironmentVm: {
    report: {
      signals: Array.from({ length: 18 }, (_, i) => ({ signal_id: `s${i}`, title: `Signal ${i}` })),
      promotion_readiness: [
        {
          readiness_status: "BLOCKED",
          blockers: ["Authentication journey failing in staging"],
        },
      ],
    },
  },
  jiraIntelVm: { blockerCount: 2 },
  compactJiraBlockers: [
    { issueKey: "AUTH-228", summary: "production login failures" },
  ],
  data_gaps: ["Contract drift detected in payments API"],
};

const blockedView = {
  contract_risk_assessment: {
    assessments: [{ contract_id: "payments_api", overall_risk_level: "CRITICAL" }],
  },
  data_journey_validation: {
    results: [{ journey_id: "auth", status: "BROKEN" }],
    journeys: [{ journey_id: "auth", name: "Authentication" }],
  },
};

describe("releaseReadinessExplainabilityViewUtils", () => {
  it("shows trace only for blocked or caution statuses", () => {
    expect(shouldShowReleaseReadinessTrace({ empty: false, overallStatus: "BLOCKED" })).toBe(true);
    expect(shouldShowReleaseReadinessTrace({ empty: false, overallStatus: "CAUTION" })).toBe(true);
    expect(shouldShowReleaseReadinessTrace({ empty: false, overallStatus: "GO" })).toBe(false);
    expect(shouldShowReleaseReadinessTrace({ empty: true, overallStatus: "BLOCKED" })).toBe(false);
  });

  it("builds blocked release trace from existing vm slices", () => {
    const trace = buildReleaseReadinessTraceViewModel(blockedVmBase, blockedView, t);
    expect(trace.show).toBe(true);
    expect(trace.whyExplanation.title).toBe(RELEASE_READINESS_EXPLAINABILITY_I18N_KEYS.whyTitle);
    expect(trace.whyExplanation.bullets.some((b) => b.includes("CRITICAL"))).toBe(true);
    expect(trace.whyExplanation.bullets.some((b) => b.includes("18"))).toBe(true);
    expect(trace.rootCauseContributors.contributors[0].confidenceText).toBe("73%");
    expect(trace.evidenceSources.sources.some((s) => s.key === "contract_drift")).toBe(true);
    expect(trace.evidenceSources.sources.some((s) => s.key === "journey_failures")).toBe(true);
  });

  it("uses empty state when no evidence exists", () => {
    const trace = buildReleaseReadinessTraceViewModel(
      { empty: false, overallStatus: "CAUTION" },
      {},
      t,
    );
    expect(trace.whyExplanation.empty).toBe(true);
    expect(trace.whyExplanation.emptyMessage).toBe(EVIDENCE_TRACE_I18N_KEYS.noSupportingEvidence);
  });

  it("integrates trace into release readiness view model", () => {
    const vm = buildReleaseReadinessViewModel({
      release_readiness: {
        overall_status: "BLOCKED",
        summary: "Release blocked.",
        deployment_risk_assessment: blockedVmBase.deploymentRisk,
        decision_center: {
          overall_status: "RED",
          executive_summary: "Blocked.",
          confidence: 0.73,
          key_takeaways: blockedVmBase.decisionCenterVm.center.key_takeaways,
        },
        multi_environment: blockedVmBase.multiEnvironmentVm.report,
        data_gaps: blockedVmBase.data_gaps,
        jira_issue_intelligence: {
          connected: true,
          blocker_count: 2,
          top_blockers: [
            {
              issue_key: "AUTH-228",
              summary: "production login failures",
              is_blocker: true,
            },
          ],
          issue_correlations: [],
          data_gaps: [],
        },
        contract_risk_assessment: blockedView.contract_risk_assessment,
        data_journey_validation: blockedView.data_journey_validation,
      },
    }, t);
    expect(vm.showTrace).toBe(true);
    expect(vm.trace.show).toBe(true);
    expect(vm.overallStatus).toBe("BLOCKED");
  });
});
