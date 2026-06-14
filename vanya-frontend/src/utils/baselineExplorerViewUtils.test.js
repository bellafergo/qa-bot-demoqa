import { describe, expect, it } from "vitest";
import {
  buildBaselineHistoryRow,
  buildDiffHighlights,
  canCompareBaseline,
  filterBaselineSetEvents,
  formatBaselineDate,
  resolveBaselineHealthBucket,
  shortInspectionId,
} from "./baselineExplorerViewUtils.js";

const t = (key) => key;

describe("baselineExplorerViewUtils", () => {
  const now = new Date("2026-06-12T12:00:00Z").getTime();

  it("classifies baseline health by age", () => {
    expect(resolveBaselineHealthBucket("2026-06-10T12:00:00Z", now)).toBe("fresh");
    expect(resolveBaselineHealthBucket("2026-05-20T12:00:00Z", now)).toBe("aging");
    expect(resolveBaselineHealthBucket("2026-04-01T12:00:00Z", now)).toBe("stale");
    expect(resolveBaselineHealthBucket(null, now)).toBe("none");
  });

  it("filters baseline_set events only", () => {
    const events = [
      { event_id: "1", event_type: "baseline_set", target_inspection_id: "a" },
      { event_id: "2", event_type: "diff_generated", target_inspection_id: "b" },
      { event_id: "3", event_type: "baseline_set", target_inspection_id: "c" },
    ];
    expect(filterBaselineSetEvents(events)).toHaveLength(2);
    expect(filterBaselineSetEvents(events)[0].event_id).toBe("1");
  });

  it("builds history rows from events", () => {
    const row = buildBaselineHistoryRow({
      event_id: "e1",
      created_at: "2026-06-14T12:28:00Z",
      target_inspection_id: "499c1051",
    });
    expect(row).toEqual({
      eventId: "e1",
      createdAt: "2026-06-14T12:28:00Z",
      inspectionId: "499c1051",
    });
  });

  it("shortens inspection ids", () => {
    expect(shortInspectionId("abc")).toBe("abc");
    expect(shortInspectionId("499c1051-aaaa-bbbb-cccc")).toBe("499c1051…");
    expect(shortInspectionId("_")).toBe("");
  });

  it("formats baseline dates", () => {
    const out = formatBaselineDate("2026-06-14T12:28:00Z");
    expect(out).not.toBe("—");
  });

  it("detects when baseline compare is possible", () => {
    expect(canCompareBaseline({ baseline_inspection_id: "a", last_inspection_id: "b" })).toBe(true);
    expect(canCompareBaseline({ baseline_inspection_id: "a", last_inspection_id: "a" })).toBe(false);
    expect(canCompareBaseline({ baseline_inspection_id: "", last_inspection_id: "b" })).toBe(false);
  });

  it("builds diff highlight rows", () => {
    const rows = buildDiffHighlights(
      {
        visual_hash_changed: true,
        visual_change_level: "medium",
        changes: {
          title_changed: true,
          counts_delta: { selector_candidates_count: 2 },
          console_errors_delta: 1,
          network_errors_delta: -1,
          primary_actions_changed: ["Save"],
        },
      },
      t,
    );
    expect(rows.some((r) => r.label === "watch.baseline.diff.visual_hash")).toBe(true);
    expect(rows.some((r) => r.label === "watch.baseline.diff.selectors")).toBe(true);
    expect(rows.some((r) => r.label === "watch.baseline.diff.navigation")).toBe(true);
  });
});
