import { describe, expect, it } from "vitest";
import {
  DEMO_ENTERPRISE_PROJECT,
  filterPresentationProjects,
  hasDemoEnterpriseProject,
  isJunkDemoProject,
} from "./projectDemoReadinessUtils.js";

describe("projectDemoReadinessUtils", () => {
  it("flags junk demo project names", () => {
    expect(isJunkDemoProject({ id: "test", name: "test" })).toBe(true);
    expect(isJunkDemoProject({ id: "t", name: "t" })).toBe(true);
    expect(isJunkDemoProject({ id: "demo", name: "Demo", description: "demo" })).toBe(true);
  });

  it("keeps professional demo enterprise project", () => {
    expect(isJunkDemoProject(DEMO_ENTERPRISE_PROJECT)).toBe(false);
  });

  it("filters junk projects from presentation list", () => {
    const list = filterPresentationProjects([
      { id: "test", name: "test" },
      DEMO_ENTERPRISE_PROJECT,
    ]);
    expect(list).toHaveLength(1);
    expect(list[0].id).toBe(DEMO_ENTERPRISE_PROJECT.id);
  });

  it("detects demo enterprise project presence", () => {
    expect(hasDemoEnterpriseProject([DEMO_ENTERPRISE_PROJECT])).toBe(true);
    expect(hasDemoEnterpriseProject([{ id: "acme", name: "Acme" }])).toBe(false);
  });
});
