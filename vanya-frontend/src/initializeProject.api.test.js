import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

describe("initializeProject API contract", () => {
  beforeEach(() => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => ({
        ok: true,
        status: 200,
        headers: { get: () => "application/json" },
        text: async () =>
          JSON.stringify({
            ok: true,
            project_id: "demo",
            steps: [],
            message: "ok",
          }),
      })),
    );
  });

  afterEach(() => {
    vi.unstubAllGlobals();
    vi.resetModules();
  });

  it("POSTs defaults run_smoke and refresh_knowledge true", async () => {
    const { initializeProject } = await import("./api.js");
    await initializeProject("demo-proj");

    expect(fetch).toHaveBeenCalledTimes(1);
    const [url, opts] = fetch.mock.calls[0];
    expect(String(url)).toContain("/projects/demo-proj/initialize");
    expect(opts.method).toBe("POST");
    expect(JSON.parse(opts.body)).toEqual({
      run_smoke: true,
      refresh_knowledge: true,
    });
  });

  it("honours explicit false flags in body", async () => {
    const { initializeProject } = await import("./api.js");
    await initializeProject("demo-proj", { run_smoke: false, refresh_knowledge: false });

    const [, opts] = fetch.mock.calls[0];
    expect(JSON.parse(opts.body)).toEqual({
      run_smoke: false,
      refresh_knowledge: false,
    });
  });
});
