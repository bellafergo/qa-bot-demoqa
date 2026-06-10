import { describe, it, expect } from "vitest";
import {
  QUALITY_TREND_I18N_KEYS,
  buildQualityTrendViewModel,
  buildSparklinePath,
  formatScoreChange,
  trendDirectionBadgeClass,
  trendDirectionColor,
  trendDirectionLabelKey,
} from "./qualityTrendViewUtils.js";

const t = (key) => key;

const sampleReport = {
  historical_learning: {
    similar_incidents: [{ incident_id: "inc-1", title: "Payments issue", summary: "Repeated failures" }],
    pattern_summary: "Repeated payments incidents",
    confidence: 0.75,
  },
  quality_trends: {
    overall_trend: "DEGRADING",
    confidence: 0.82,
    summary: "Overall quality trend is DEGRADING.",
    trends: [
      {
        trend_id: "quality_trend:project:project",
        scope_type: "project",
        scope_name: "Project",
        trend_direction: "DEGRADING",
        score_change: -9,
        confidence: 0.82,
        points: [
          { timestamp: "2026-06-01T10:00:00+00:00", score: 91, status: "EXCELLENT" },
          { timestamp: "2026-06-02T10:00:00+00:00", score: 88, status: "GOOD" },
          { timestamp: "2026-06-03T10:00:00+00:00", score: 84, status: "GOOD" },
          { timestamp: "2026-06-04T10:00:00+00:00", score: 82, status: "GOOD" },
        ],
      },
      {
        trend_id: "quality_trend:module:payments",
        scope_type: "module",
        scope_name: "Payments",
        trend_direction: "IMPROVING",
        score_change: 12,
        confidence: 0.78,
        points: [
          { timestamp: "2026-06-01T10:00:00+00:00", score: 58, status: "ATTENTION" },
          { timestamp: "2026-06-04T10:00:00+00:00", score: 70, status: "ATTENTION" },
        ],
      },
    ],
  },
};

describe("qualityTrendViewUtils", () => {
  it("renders quality trend report", () => {
    const vm = buildQualityTrendViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.report.overallTrendLabel).toBe(QUALITY_TREND_I18N_KEYS.degrading);
  });

  it("renders empty state", () => {
    const vm = buildQualityTrendViewModel({ quality_trends: null }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(QUALITY_TREND_I18N_KEYS.empty);
  });

  it("renders trend labels and colors", () => {
    expect(trendDirectionLabelKey("IMPROVING")).toBe(QUALITY_TREND_I18N_KEYS.improving);
    expect(trendDirectionColor("DEGRADING")).toBe("#ef4444");
    expect(trendDirectionBadgeClass("STABLE")).toContain("badge-blue");
  });

  it("formats score changes", () => {
    expect(formatScoreChange(12)).toBe("+12");
    expect(formatScoreChange(-9)).toBe("-9");
    const vm = buildQualityTrendViewModel(sampleReport, t);
    expect(vm.report.trends[0].scoreChangeText).toBe("-9");
  });

  it("builds sparkline path", () => {
    const path = buildSparklinePath(sampleReport.quality_trends.trends[0].points);
    expect(path.startsWith("M")).toBe(true);
    expect(path.includes("L")).toBe(true);
  });

  it("builds drilldown integration", () => {
    const vm = buildQualityTrendViewModel(sampleReport, t);
    expect(vm.drilldownItem.related_entity_type).toBe("historical_learning");
  });

  it("exposes translation keys", () => {
    expect(QUALITY_TREND_I18N_KEYS.title).toBe("incident.qa.quality_trends");
    expect(QUALITY_TREND_I18N_KEYS.empty).toBe("incident.qa.quality_trends_empty");
  });
});
