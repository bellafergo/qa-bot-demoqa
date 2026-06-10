import { describe, it, expect } from "vitest";
import {
  SCHEDULED_REPORT_I18N_KEYS,
  buildScheduledReportViewModel,
  frequencyLabelKey,
  reportTypeLabelKey,
  riskLevelBadgeClass,
  trendBadgeClass,
} from "./scheduledReportViewUtils.js";

const t = (key) => key;

const sampleCenter = {
  total_schedules: 4,
  schedules: [
    {
      schedule_id: "exec_schedule:quality_brief",
      name: "Weekly Quality Brief",
      frequency: "WEEKLY",
      enabled: true,
      report_type: "QUALITY_BRIEF",
      recipients_count: 3,
      next_run_preview: "Monday 8:00 AM",
    },
    {
      schedule_id: "exec_schedule:executive_summary",
      name: "Executive Summary",
      frequency: "WEEKLY",
      enabled: true,
      report_type: "EXECUTIVE_SUMMARY",
      recipients_count: 5,
      next_run_preview: "Monday 8:00 AM",
    },
  ],
  latest_preview: {
    preview_id: "exec_preview:demo:quality_brief",
    title: "Weekly Quality Brief",
    generated_at: "2026-06-04T08:00:00+00:00",
    quality_score: 82,
    quality_trend: "DEGRADING",
    risk_level: "HIGH",
    executive_summary: "Payments and checkout require executive attention.",
    top_risks: ["Payments contract risk is CRITICAL", "Checkout journey is BROKEN"],
    top_recommendations: ["Run Payments Regression Suite", "Validate Orders Database"],
    incident_count: 2,
    critical_contract_count: 1,
    broken_journey_count: 1,
  },
};

describe("scheduledReportViewUtils", () => {
  it("renders executive report center", () => {
    const vm = buildScheduledReportViewModel(sampleCenter, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(SCHEDULED_REPORT_I18N_KEYS.title);
    expect(vm.schedules).toHaveLength(2);
    expect(vm.preview.quality_score).toBe(82);
  });

  it("renders empty state", () => {
    const vm = buildScheduledReportViewModel({ schedules: [], total_schedules: 0 }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(SCHEDULED_REPORT_I18N_KEYS.empty);
  });

  it("renders schedule cards", () => {
    const vm = buildScheduledReportViewModel(sampleCenter, t);
    expect(vm.schedules[0].reportTypeLabel).toBe(SCHEDULED_REPORT_I18N_KEYS.qualityBrief);
    expect(vm.schedules[0].frequencyLabel).toBe(SCHEDULED_REPORT_I18N_KEYS.frequencyWeekly);
    expect(vm.schedules[0].recipientsText).toContain("3");
  });

  it("renders preview cards", () => {
    const vm = buildScheduledReportViewModel(sampleCenter, t);
    expect(vm.preview.top_risks).toHaveLength(2);
    expect(vm.preview.top_recommendations).toHaveLength(2);
    expect(vm.preview.trendBadgeClass).toContain("badge-red");
  });

  it("exposes disabled send button labels", () => {
    const vm = buildScheduledReportViewModel(sampleCenter, t);
    expect(vm.sendReportLabel).toBe(SCHEDULED_REPORT_I18N_KEYS.sendReport);
    expect(vm.sendDisabledNote).toBe(SCHEDULED_REPORT_I18N_KEYS.sendDisabledNote);
  });

  it("maps translation keys", () => {
    expect(reportTypeLabelKey("RELEASE_READINESS")).toBe(SCHEDULED_REPORT_I18N_KEYS.releaseReadiness);
    expect(frequencyLabelKey("MONTHLY")).toBe(SCHEDULED_REPORT_I18N_KEYS.frequencyMonthly);
    expect(riskLevelBadgeClass("HIGH")).toContain("badge-red");
    expect(trendBadgeClass("IMPROVING")).toContain("badge-green");
  });

  it("maps Jira blocker KPI on preview", () => {
    const vm = buildScheduledReportViewModel({
      ...sampleCenter,
      latest_preview: {
        ...sampleCenter.latest_preview,
        jira_blocker_count: 2,
        jira_blocker_keys: ["PAY-451", "AUTH-228"],
      },
    }, t);
    expect(vm.preview.showJiraKpi).toBe(true);
    expect(vm.preview.jiraBlockerCount).toBe(2);
    expect(vm.preview.jiraBlockerKeys).toEqual(["PAY-451", "AUTH-228"]);
    expect(vm.preview.jiraBlockersLabel).toBe(SCHEDULED_REPORT_I18N_KEYS.jiraBlockers);
  });

  it("hides Jira KPI when blocker count is zero", () => {
    const vm = buildScheduledReportViewModel(sampleCenter, t);
    expect(vm.preview.showJiraKpi).toBe(false);
  });
});
