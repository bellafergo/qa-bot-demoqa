import { describe, it, expect, vi } from "vitest";
import { verifyDrilldownEntity } from "./correlationDrilldownVerify";

describe("verifyDrilldownEntity", () => {
  it("uses GET-only APIs and never writes", async () => {
    const getTestRun = vi.fn().mockResolvedValue({ run_id: "RUN-1" });
    const getBrowserInspectionWatch = vi.fn();
    const getClusters = vi.fn();

    const ok = await verifyDrilldownEntity("run", "RUN-1", "demo", {
      getTestRun,
      getBrowserInspectionWatch,
      getClusters,
    });

    expect(ok).toBe(true);
    expect(getTestRun).toHaveBeenCalledWith("RUN-1");
    expect(getBrowserInspectionWatch).not.toHaveBeenCalled();
    expect(getClusters).not.toHaveBeenCalled();
  });

  it("returns false when run entity is unavailable", async () => {
    const getTestRun = vi.fn().mockRejectedValue(new Error("404"));
    const ok = await verifyDrilldownEntity("run", "missing", "demo", {
      getTestRun,
      getBrowserInspectionWatch: vi.fn(),
      getClusters: vi.fn(),
    });
    expect(ok).toBe(false);
  });

  it("returns false when browser watch entity is unavailable", async () => {
    const getBrowserInspectionWatch = vi.fn().mockRejectedValue(new Error("404"));
    const ok = await verifyDrilldownEntity("browser_watch", "W-1", "demo", {
      getTestRun: vi.fn(),
      getBrowserInspectionWatch,
      getClusters: vi.fn(),
    });
    expect(ok).toBe(false);
  });

  it("checks failure cluster via clusters GET", async () => {
    const getClusters = vi.fn().mockResolvedValue([
      { cluster_id: "cluster-auth-1", module: "auth" },
    ]);
    const ok = await verifyDrilldownEntity("failure_cluster", "cluster-auth-1", "demo", {
      getTestRun: vi.fn(),
      getBrowserInspectionWatch: vi.fn(),
      getClusters,
    });
    expect(ok).toBe(true);
    expect(getClusters).toHaveBeenCalledWith({ project_id: "demo" });
  });

  it("allows pr_analysis navigation without stored-report GET", async () => {
    const ok = await verifyDrilldownEntity("pr_analysis", "github:42", "demo", {
      getTestRun: vi.fn(),
      getBrowserInspectionWatch: vi.fn(),
      getClusters: vi.fn(),
    });
    expect(ok).toBe(true);
  });
});
