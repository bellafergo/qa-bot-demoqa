import { describe, expect, it } from "vitest";
import { formatRelativeTime } from "./relativeTimeUtils.js";

const t = (key, vars) => {
  if (key === "time.relative.minutes_ago") return `${vars?.count} min ago`;
  if (key === "time.relative.no_executions") return "No runs yet";
  return key;
};

describe("relativeTimeUtils", () => {
  it("formats minutes ago with locale function", () => {
    const now = new Date("2026-06-12T10:02:00Z").getTime();
    expect(formatRelativeTime("2026-06-12T10:00:00Z", t, now)).toBe("2 min ago");
  });

  it("returns no executions label when iso is missing", () => {
    expect(formatRelativeTime(null, t)).toBe("No runs yet");
  });
});
