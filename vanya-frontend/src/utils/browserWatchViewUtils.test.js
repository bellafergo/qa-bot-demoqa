import { describe, expect, it } from "vitest";
import {
  buildWatchListItemViewModel,
  buildWatchSummaryStats,
  formatRelativeTime,
  resolveWatchHealthBucket,
  resolveWatchName,
  truncateWatchUrl,
} from "./browserWatchViewUtils.js";

const t = (key) => key;

describe("browserWatchViewUtils", () => {
  it("classifies healthy, warning, and critical watches", () => {
    expect(resolveWatchHealthBucket({ enabled: true, current_status: "healthy", last_run_at: "2026-01-01" })).toBe("healthy");
    expect(resolveWatchHealthBucket({ enabled: true, current_status: "changed", last_run_at: "2026-01-01" })).toBe("warning");
    expect(resolveWatchHealthBucket({ enabled: true, current_status: "failed", last_run_at: "2026-01-01" })).toBe("critical");
    expect(resolveWatchHealthBucket({ enabled: true })).toBe("never_run");
  });

  it("builds summary stats", () => {
    const stats = buildWatchSummaryStats([
      { enabled: true, current_status: "healthy", last_run_at: "t" },
      { enabled: true, current_status: "changed", last_run_at: "t" },
      { enabled: true, current_status: "failed", last_run_at: "t" },
    ]);
    expect(stats).toEqual({ total: 3, healthy: 1, warnings: 1, critical: 1 });
  });

  it("truncates long URLs without vertical breaks", () => {
    const url = "https://zuperio-talent-os.vercel.app/dashboard/settings/profile";
    const out = truncateWatchUrl(url, 40);
    expect(out).toContain("…");
    expect(out.length).toBeLessThanOrEqual(40);
    expect(out).not.toMatch(/\n/);
  });

  it("resolves watch name from hostname", () => {
    expect(resolveWatchName({ url: "https://zuperio-talent-os.vercel.app/path" })).toBe("zuperio-talent-os.vercel.app");
  });

  it("formats relative time", () => {
    const now = new Date("2026-06-12T10:02:00Z").getTime();
    expect(formatRelativeTime("2026-06-12T10:00:00Z", t, now)).toBe("time.relative.minutes_ago");
  });

  it("flags watches that need attention", () => {
    const item = buildWatchListItemViewModel(
      { watch_id: "w1", url: "https://app.example.com", current_status: "failed", last_run_at: "t", enabled: true },
      t,
    );
    expect(item.needsAttention).toBe(true);
    expect(item.statusLabel).toBe("watch.enterprise.status.critical");
  });
});
