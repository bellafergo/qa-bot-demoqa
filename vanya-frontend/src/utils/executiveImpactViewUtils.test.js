import { describe, expect, it } from "vitest";
import {
  EXECUTIVE_IMPACT_I18N_KEYS,
  buildExecutiveImpactViewModel,
  hasExecutiveImpactSection,
  isExecutiveImpactEmpty,
  mapImpactMetric,
} from "./executiveImpactViewUtils.js";

const t = (key) => key;

const sampleReport = {
  generated_at: "2026-06-10T08:00:00+00:00",
  has_sufficient_history: true,
  quality_health_trend: {
    metric_id: "quality_health",
    title: "Quality Health",
    current_value: 82,
    previous_value: 70,
    delta: 12,
    direction: "IMPROVING",
  },
  degraded_environment_trend: {
    metric_id: "degraded_environments",
    title: "Environment Stability",
    current_value: 1,
    previous_value: 2,
    delta: -1,
    direction: "IMPROVING",
  },
  impacted_journey_trend: {
    metric_id: "impacted_journeys",
    title: "Journey Stability",
    current_value: 0,
    previous_value: 0,
    delta: 0,
    direction: "STABLE",
  },
  blocked_release_trend: {
    metric_id: "blocked_releases",
    title: "Blocked Releases",
    current_value: 0,
    previous_value: 1,
    delta: -1,
    direction: "IMPROVING",
  },
  critical_risk_trend: {
    metric_id: "critical_risks",
    title: "Critical Risks",
    current_value: 1,
    previous_value: 2,
    delta: -1,
    direction: "IMPROVING",
  },
  jira_blocker_trend: {
    metric_id: "jira_blockers",
    title: "Jira Blockers",
    current_value: 2,
    previous_value: 5,
    delta: -3,
    direction: "IMPROVING",
  },
  recommendation_trend: {
    metric_id: "recommendations",
    title: "Recommendations",
    current_value: 4,
    previous_value: 2,
    delta: 2,
    direction: "IMPROVING",
  },
  approval_trend: {
    metric_id: "approvals",
    title: "Approvals",
    current_value: 1,
    previous_value: 1,
    delta: 0,
    direction: "STABLE",
  },
  validation_trend: {
    metric_id: "validations",
    title: "Validations",
    current_value: 3,
    previous_value: 1,
    delta: 2,
    direction: "IMPROVING",
  },
  top_improvements: [
    {
      metric_id: "jira_blockers",
      title: "Jira Blockers",
      current_value: 2,
      previous_value: 5,
      delta: -3,
      direction: "IMPROVING",
    },
  ],
  top_concerns: [],
};

describe("executiveImpactViewUtils", () => {
  it("detects section and insufficient history", () => {
    expect(hasExecutiveImpactSection(sampleReport)).toBe(true);
    expect(hasExecutiveImpactSection(null)).toBe(false);
    expect(isExecutiveImpactEmpty(sampleReport)).toBe(false);
    expect(isExecutiveImpactEmpty({ ...sampleReport, has_sufficient_history: false })).toBe(true);
  });

  it("maps trend indicators", () => {
    const improving = mapImpactMetric(sampleReport.jira_blocker_trend, t);
    expect(improving.directionIndicator).toBe("↑");
    expect(improving.directionLabel).toBe(EXECUTIVE_IMPACT_I18N_KEYS.directionImproving);
    expect(improving.deltaDisplay).toBe("-3");
    expect(improving.directionBadgeClass).toContain("badge-green");

    const stable = mapImpactMetric(sampleReport.approval_trend, t);
    expect(stable.directionIndicator).toBe("→");
    expect(stable.directionLabel).toBe(EXECUTIVE_IMPACT_I18N_KEYS.directionStable);
  });

  it("builds executive impact view model", () => {
    const vm = buildExecutiveImpactViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(EXECUTIVE_IMPACT_I18N_KEYS.title);
    expect(vm.qualityMetrics).toHaveLength(3);
    expect(vm.riskMetrics).toHaveLength(3);
    expect(vm.operationsMetrics).toHaveLength(3);
    expect(vm.topImprovements).toHaveLength(1);
    expect(vm.topImprovements[0].directionIndicator).toBe("↑");
  });

  it("renders insufficient history empty state", () => {
    const vm = buildExecutiveImpactViewModel({
      generated_at: "2026-06-10T08:00:00+00:00",
      has_sufficient_history: false,
      quality_health_trend: {
        metric_id: "quality_health",
        title: "Quality Health",
        current_value: 80,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      degraded_environment_trend: {
        metric_id: "degraded_environments",
        title: "Environment Stability",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      impacted_journey_trend: {
        metric_id: "impacted_journeys",
        title: "Journey Stability",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      blocked_release_trend: {
        metric_id: "blocked_releases",
        title: "Blocked Releases",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      critical_risk_trend: {
        metric_id: "critical_risks",
        title: "Critical Risks",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      jira_blocker_trend: {
        metric_id: "jira_blockers",
        title: "Jira Blockers",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      recommendation_trend: {
        metric_id: "recommendations",
        title: "Recommendations",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      approval_trend: {
        metric_id: "approvals",
        title: "Approvals",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      validation_trend: {
        metric_id: "validations",
        title: "Validations",
        current_value: 0,
        previous_value: 0,
        delta: 0,
        direction: "UNKNOWN",
      },
      top_improvements: [],
      top_concerns: [],
    }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(EXECUTIVE_IMPACT_I18N_KEYS.insufficientHistory);
  });
});
