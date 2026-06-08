import { describe, expect, it } from "vitest";
import {
  INIT_CHECKLIST_KEYS,
  INIT_STEP_ORDER,
  isSmokeQueued,
  sortInitSteps,
  stepStatusBadgeClass,
  stepStatusIcon,
} from "./initStepUtils.js";

describe("initStepUtils", () => {
  it("orders steps catalog → readiness", () => {
    const steps = sortInitSteps([
      { step: "readiness", status: "ok" },
      { step: "catalog", status: "ok" },
      { step: "smoke_run", status: "failed" },
      { step: "knowledge", status: "ok" },
    ]);
    expect(steps.map((s) => s.step)).toEqual(INIT_STEP_ORDER);
  });

  it("maps step status icons and badge classes", () => {
    expect(stepStatusIcon("ok")).toBe("✓");
    expect(stepStatusIcon("failed")).toBe("✕");
    expect(stepStatusBadgeClass("partial")).toBe("badge-orange");
    expect(stepStatusBadgeClass("skipped")).toBe("badge-gray");
  });

  it("detects queued smoke when job_id present", () => {
    expect(
      isSmokeQueued({
        job_id: "job-1",
        steps: [{ step: "smoke_run", status: "ok", details: { job_id: "job-1" } }],
      }),
    ).toBe(true);
    expect(
      isSmokeQueued({
        steps: [{ step: "smoke_run", status: "failed" }],
      }),
    ).toBe(false);
  });

  it("exposes checklist keys for recommended panel", () => {
    expect(INIT_CHECKLIST_KEYS).toHaveLength(4);
  });
});
