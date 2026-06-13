import { describe, expect, it } from "vitest";
import {
  RELEASE_READINESS_I18N_KEYS,
  buildReleaseReadinessViewModel,
  isReleaseReadinessEmpty,
  overallStatusBadgeClass,
  overallStatusLabelKey,
} from "./releaseReadinessViewUtils.js";

const t = (key) => key;

const sampleView = {
  project_id: "demo",
  generated_at: "2026-06-10T08:00:00+00:00",
  source_incident_report_id: "rep-1",
  overall_status: "CAUTION",
  summary: "Validation recommended before release.",
  data_gaps: ["quality_trends unavailable."],
  deployment_risk_assessment: {
    risk_score: 78,
    risk_level: "high",
    confidence: 0.75,
    summary: "Elevated deployment risk.",
    contributing_factors: [],
  },
  decision_center: {
    overall_status: "ORANGE",
    executive_summary: "Decision center summary.",
    confidence: 0.8,
    top_risk_level: "HIGH",
    top_risk_score: 78,
    key_takeaways: [],
  },
  quality_health: {
    overall_score: 82,
    overall_status: "WARNING",
    trend: "DEGRADING",
    summary: "Quality degrading.",
    scores: [],
  },
  github: {
    project_id: "demo",
    connected: true,
    enabled: true,
    full_name: "org/repo",
    validation_message: "Connected.",
  },
  azure_devops: {
    project_id: "demo",
    connected: false,
    enabled: false,
  },
  integration_readiness: {
    slack: { ready: true, enabled: true, health: "ok" },
    email: { ready: false, enabled: false, health: "unconfigured" },
  },
};

describe("releaseReadinessViewUtils", () => {
  it("maps overall status labels and badges", () => {
    expect(overallStatusLabelKey("GO")).toBe(RELEASE_READINESS_I18N_KEYS.statusGo);
    expect(overallStatusLabelKey("BLOCKED")).toBe(RELEASE_READINESS_I18N_KEYS.statusBlocked);
    expect(overallStatusBadgeClass("CAUTION")).toBe("badge badge-orange");
  });

  it("detects empty release readiness", () => {
    expect(isReleaseReadinessEmpty({ release_readiness: null })).toBe(true);
    expect(isReleaseReadinessEmpty({ release_readiness: { data_gaps: ["missing"] } })).toBe(false);
    expect(isReleaseReadinessEmpty({ release_readiness: sampleView })).toBe(false);
  });

  it("builds dashboard view model from compositor payload", () => {
    const vm = buildReleaseReadinessViewModel({ release_readiness: sampleView }, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.overallStatusText).toBe(RELEASE_READINESS_I18N_KEYS.statusCaution);
    expect(vm.summary).toBe("Validation recommended before release.");
    expect(vm.data_gaps).toHaveLength(1);
    expect(vm.scm).toHaveLength(2);
    expect(vm.scm[0].badgeClass).toBe("badge badge-green");
    expect(vm.integrations).toHaveLength(2);
    expect(vm.deploymentRisk.risk_level).toBe("high");
    expect(vm.decisionCenterVm.empty).toBe(false);
    expect(vm.qualityHealthVm.empty).toBe(false);
    expect(vm.showTrace).toBe(true);
    expect(vm.overallStatus).toBe("CAUTION");
  });

  it("builds compact Jira blocker slice when blockers exist", () => {
    const vm = buildReleaseReadinessViewModel({
      release_readiness: {
        ...sampleView,
        jira_issue_intelligence: {
          connected: true,
          total_issues: 4,
          correlated_issues: 2,
          blocker_count: 2,
          top_blockers: [
            {
              issue_key: "PAY-451",
              issue_type: "Bug",
              status: "Open",
              priority: "Blocker",
              summary: "timeout in staging",
              related_module: "Payments",
              is_blocker: true,
            },
            {
              issue_key: "AUTH-228",
              issue_type: "Bug",
              status: "Open",
              priority: "Blocker",
              summary: "production login failures",
              related_module: "Authentication",
              is_blocker: true,
            },
          ],
          issue_correlations: [],
          summary: "2 blockers detected.",
          data_gaps: [],
        },
      },
    }, t);
    expect(vm.showJiraBlockers).toBe(true);
    expect(vm.jiraIntelVm.blockerCount).toBe(2);
    expect(vm.compactJiraBlockers).toHaveLength(2);
    expect(vm.compactJiraBlockers[0].issueKey).toBe("PAY-451");
    expect(vm.jiraBlockersLabel).toBe(RELEASE_READINESS_I18N_KEYS.jiraBlockers);
  });

  it("hides Jira blocker slice when no blockers", () => {
    const vm = buildReleaseReadinessViewModel({
      release_readiness: {
        ...sampleView,
        jira_issue_intelligence: {
          connected: true,
          total_issues: 2,
          correlated_issues: 0,
          blocker_count: 0,
          top_blockers: [],
          issue_correlations: [],
          summary: "No blockers.",
          data_gaps: [],
        },
      },
    }, t);
    expect(vm.showJiraBlockers).toBe(false);
  });
});
