import { describe, expect, it } from "vitest";
import {
  OUTCOME_TRACKING_I18N_KEYS,
  buildOutcomeTrackingViewModel,
  hasOutcomeTrackingSection,
  isOutcomeTrackingEmpty,
  mapOutcomeMetric,
} from "./outcomeTrackingViewUtils.js";

const t = (key) => key;

describe("outcomeTrackingViewUtils", () => {
  const sampleReport = {
    generated_at: "2026-06-10T08:00:00+00:00",
    blockers_identified: 14,
    releases_blocked: 2,
    recommendations_generated: 28,
    incidents_investigated: 6,
    executive_reports_sent: 8,
    executive_summary:
      "During the measured period, Vanya identified 14 blockers, generated 28 test recommendations, and delivered 8 executive reports.",
  };

  it("detects section presence", () => {
    expect(hasOutcomeTrackingSection(sampleReport)).toBe(true);
    expect(hasOutcomeTrackingSection(null)).toBe(false);
  });

  it("detects empty state", () => {
    expect(isOutcomeTrackingEmpty(sampleReport)).toBe(false);
    expect(isOutcomeTrackingEmpty({
      blockers_identified: 0,
      releases_blocked: 0,
      recommendations_generated: 0,
      incidents_investigated: 0,
      executive_reports_sent: 0,
    })).toBe(true);
  });

  it("maps metric cards", () => {
    const metric = mapOutcomeMetric({ metric_name: "blockers_identified", value: 14 }, t);
    expect(metric.title).toBe(OUTCOME_TRACKING_I18N_KEYS.blockersIdentified);
    expect(metric.displayValue).toBe("14");
  });

  it("builds view model with metrics and summary", () => {
    const vm = buildOutcomeTrackingViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.title).toBe(OUTCOME_TRACKING_I18N_KEYS.title);
    expect(vm.metrics).toHaveLength(5);
    expect(vm.executiveSummary).toContain("14 blockers");
    expect(vm.executiveSummaryLabel).toBe(OUTCOME_TRACKING_I18N_KEYS.executiveSummary);
  });

  it("renders empty state message", () => {
    const vm = buildOutcomeTrackingViewModel({
      blockers_identified: 0,
      releases_blocked: 0,
      recommendations_generated: 0,
      incidents_investigated: 0,
      executive_reports_sent: 0,
      executive_summary: "No measurable outcomes available yet.",
    }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(OUTCOME_TRACKING_I18N_KEYS.empty);
    expect(vm.executiveSummary).toBe(OUTCOME_TRACKING_I18N_KEYS.empty);
  });

  it("hides privileged metric values as zero when empty", () => {
    const vm = buildOutcomeTrackingViewModel({
      blockers_identified: 0,
      releases_blocked: 0,
      recommendations_generated: 0,
      incidents_investigated: 0,
      executive_reports_sent: 0,
    }, t);
    expect(vm.metrics.every((metric) => metric.displayValue === "0")).toBe(true);
  });
});
