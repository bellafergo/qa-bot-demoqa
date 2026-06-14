import { describe, expect, it } from "vitest";
import { buildMemoryExplorerViewModel } from "./memoryExplorerViewUtils.js";

const t = (key) => key;

const sampleExplorer = {
  project_id: "demo",
  default_expanded_module: "AUTH",
  modules: [
    {
      module: "AUTH",
      summary: "AUTH-related routes, APIs, tests and failures.",
      counts: { routes: 1, apis: 1, tests: 1, failure_clusters: 1 },
      routes: [{ url: "/login", source: "repository" }],
      apis: [{ method: "GET", url: "/api/auth/session" }],
      tests: [{ test_case_id: "TC-TOS-009", name: "Login" }],
      failure_clusters: [{ cluster_id: "CL-1", category: "AUTH_ISSUE", occurrences: 2, confidence: "high" }],
    },
  ],
};

describe("memoryExplorerViewUtils", () => {
  it("maps payload to accordion VM with counts", () => {
    const vm = buildMemoryExplorerViewModel(sampleExplorer, t);
    expect(vm.show).toBe(true);
    expect(vm.modules).toHaveLength(1);
    expect(vm.modules[0].counts.routes).toBe(1);
    expect(vm.defaultExpanded).toBe("AUTH");
    expect(vm.modules[0].failure_clusters[0].confidenceBadgeClass).toContain("badge-red");
  });

  it("returns empty state when explorer is null", () => {
    const vm = buildMemoryExplorerViewModel(null, t);
    expect(vm.show).toBe(false);
    expect(vm.modules).toHaveLength(0);
  });
});
