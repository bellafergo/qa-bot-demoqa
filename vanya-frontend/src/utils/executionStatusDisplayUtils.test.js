import { describe, expect, it } from "vitest";
import { formatExecutionStatus } from "./executionStatusDisplayUtils.js";

const t = (key) => key;

describe("executionStatusDisplayUtils", () => {
  it("maps known statuses to i18n keys", () => {
    expect(formatExecutionStatus("passed", t)).toBe("status.execution.passed");
    expect(formatExecutionStatus("FAILED", t)).toBe("status.execution.failed");
    expect(formatExecutionStatus("running", t)).toBe("status.execution.running");
  });

  it("falls back to unknown for unrecognized statuses", () => {
    expect(formatExecutionStatus("weird", t)).toBe("status.execution.unknown");
  });
});
