import { describe, expect, it } from "vitest";
import {
  computeMemoryDepth,
  formatMemoryDepthLabel,
  formatSourcesLabel,
} from "./knowledgeDepthUtils.js";

const t = (key) => key;

describe("knowledgeDepthUtils", () => {
  it("returns low depth for shallow catalog-only memory", () => {
    const depth = computeMemoryDepth({
      modules: Array.from({ length: 7 }, (_, i) => ({ name: `Mod${i}` })),
      routes: [{ url: "/dashboard" }],
      apis: [],
      related_tests: Array.from({ length: 16 }, (_, i) => ({ test_case_id: `T${i}` })),
      metadata: { reconstruction_sources: ["catalog"] },
    });
    expect(depth.score).toBeLessThan(70);
    expect(depth.label).not.toBe("high");
    expect(depth.repositoryIndexed).toBe(false);
  });

  it("returns higher depth when repository is indexed with architecture signals", () => {
    const depth = computeMemoryDepth({
      modules: [
        { name: "Candidates", entity_type: "prisma_model", fields: ["id"] },
        { name: "Vacancies", entity_type: "prisma_model", fields: ["id"] },
      ],
      routes: Array.from({ length: 20 }, (_, i) => ({ url: `/route-${i}` })),
      apis: Array.from({ length: 15 }, (_, i) => ({ url: `/api/x-${i}`, method: "GET" })),
      related_tests: Array.from({ length: 10 }, (_, i) => ({ test_case_id: `T${i}` })),
      metadata: {
        repository_indexed: true,
        reconstruction_sources: ["catalog", "repository"],
        repository_files_scanned: 120,
      },
    });
    expect(depth.score).toBeGreaterThanOrEqual(50);
    expect(depth.label).toMatch(/high|medium/);
    expect(depth.repositoryIndexed).toBe(true);
    expect(depth.sources).toContain("repository");
  });

  it("formats labels via i18n keys", () => {
    expect(formatMemoryDepthLabel("high", t)).toBe("knowledge.kpi.memory_depth.high");
    expect(formatSourcesLabel(["catalog", "repository"], t)).toContain("knowledge.sources.catalog");
  });
});
