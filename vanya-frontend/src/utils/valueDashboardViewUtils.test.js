import { describe, expect, it } from "vitest";
import {
  VALUE_DASHBOARD_I18N_KEYS,
  buildValueDashboardViewModel,
  hasValueDashboardSection,
  isValueDashboardEmpty,
} from "./valueDashboardViewUtils.js";

const t = (key) => key;

const sampleDashboard = {
  generated_at: "2026-06-10T08:00:00+00:00",
  incidents_investigated: 3,
  executive_reports_generated: 3,
  release_readiness_reports: 3,
  scheduled_reports_generated: 4,
  blocked_releases: 1,
  critical_risks_identified: 2,
  jira_blockers_detected: 2,
  degradation_events_detected: 1,
  quality_health_score: 68,
  quality_trend: "DEGRADING",
  degraded_environments: 1,
  impacted_journeys: 1,
  recommendations_generated: 6,
  approvals_requested: 2,
  validations_planned: 4,
  correlated_jira_issues: 5,
  top_value_metrics: [
    {
      metric_id: "incidents_investigated",
      title: "Incidents Investigated",
      value: 3,
      description: "Incident investigations completed by Vanya.",
    },
  ],
};

describe("valueDashboardViewUtils", () => {
  it("detects section and empty states", () => {
    expect(hasValueDashboardSection(sampleDashboard)).toBe(true);
    expect(hasValueDashboardSection(null)).toBe(false);
    expect(isValueDashboardEmpty(sampleDashboard)).toBe(false);
    expect(isValueDashboardEmpty({
      generated_at: "2026-06-10T08:00:00+00:00",
      incidents_investigated: 0,
      executive_reports_generated: 0,
      release_readiness_reports: 0,
      scheduled_reports_generated: 0,
      blocked_releases: 0,
      critical_risks_identified: 0,
      jira_blockers_detected: 0,
      degradation_events_detected: 0,
      quality_health_score: 0,
      quality_trend: "UNKNOWN",
      degraded_environments: 0,
      impacted_journeys: 0,
      recommendations_generated: 0,
      approvals_requested: 0,
      validations_planned: 0,
      correlated_jira_issues: 0,
      top_value_metrics: [],
    })).toBe(true);
  });

  it("builds metric groups and top highlights", () => {
    const vm = buildValueDashboardViewModel(sampleDashboard, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(VALUE_DASHBOARD_I18N_KEYS.title);
    expect(vm.activityMetrics).toHaveLength(4);
    expect(vm.riskMetrics).toHaveLength(4);
    expect(vm.qualityMetrics).toHaveLength(4);
    expect(vm.operationalMetrics).toHaveLength(4);
    expect(vm.activityMetrics[0].displayValue).toBe("3");
    expect(vm.riskMetrics[2].displayValue).toBe("2");
    expect(vm.qualityMetrics[1].displayValue).toBe("DEGRADING");
    expect(vm.topMetrics).toHaveLength(1);
    expect(vm.topMetrics[0].displayValue).toBe("3");
  });

  it("renders empty message when no intelligence exists", () => {
    const vm = buildValueDashboardViewModel({
      generated_at: "2026-06-10T08:00:00+00:00",
      quality_trend: "UNKNOWN",
      top_value_metrics: [],
    }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(VALUE_DASHBOARD_I18N_KEYS.empty);
  });
});
