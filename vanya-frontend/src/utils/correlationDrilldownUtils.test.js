import { describe, it, expect } from "vitest";
import {
  buildDrilldownModalPayload,
  buildDrilldownNavigation,
  getDrilldownActionLabelKey,
  hasDrilldownMetadata,
  isNonNavigableDrilldownType,
  parsePrAnalysisEntityId,
  shouldShowDrilldownUnavailable,
} from "./correlationDrilldownUtils";

describe("correlationDrilldownUtils", () => {
  it("builds run navigation to /runs with state", () => {
    const target = buildDrilldownNavigation({
      related_entity_type: "run",
      related_entity_id: "RUN-123",
    });
    expect(target).toEqual({
      kind: "navigate",
      path: "/runs",
      state: { tab: 0, run_id: "RUN-123" },
    });
  });

  it("builds pr_analysis navigation from provider:pr_number", () => {
    const target = buildDrilldownNavigation({
      related_entity_type: "pr_analysis",
      related_entity_id: "github:42",
    });
    expect(target?.kind).toBe("navigate");
    expect(target?.path).toBe("/pr-analysis?provider=github&pr=42");
  });

  it("builds browser_watch navigation", () => {
    const target = buildDrilldownNavigation({
      related_entity_type: "browser_watch",
      related_entity_id: "AUTH_LOGIN",
    });
    expect(target?.path).toBe("/browser-watch?watch=AUTH_LOGIN");
  });

  it("builds failure_cluster navigation", () => {
    const target = buildDrilldownNavigation({
      related_entity_type: "failure_cluster",
      related_entity_id: "cluster-auth-1",
    });
    expect(target?.path).toBe("/failure-intel?tab=clusters&cluster=cluster-auth-1");
  });

  it("returns null when related_entity_id is missing", () => {
    expect(buildDrilldownNavigation({
      related_entity_type: "run",
      related_entity_id: "",
    })).toBeNull();
    expect(shouldShowDrilldownUnavailable({
      related_entity_type: "run",
      related_entity_id: "",
    })).toBe(true);
  });

  it("marks memory_hint and browser_probe as non-navigable", () => {
    expect(isNonNavigableDrilldownType("memory_hint")).toBe(true);
    expect(isNonNavigableDrilldownType("browser_probe")).toBe(true);
    expect(buildDrilldownNavigation({
      related_entity_type: "memory_hint",
      related_entity_id: "hint-0",
    })).toBeNull();
  });

  it("parses pr analysis entity id", () => {
    expect(parsePrAnalysisEntityId("github:42")).toEqual({
      provider: "github",
      prNumber: "42",
    });
    expect(parsePrAnalysisEntityId("azure:12345")).toEqual({
      provider: "azure",
      prNumber: "12345",
    });
    expect(parsePrAnalysisEntityId("invalid")).toBeNull();
  });

  it("builds browser watch modal fallback payload", () => {
    const payload = buildDrilldownModalPayload({
      source: "browser_watch",
      related_entity_type: "browser_watch",
      related_entity_id: "AUTH_LOGIN",
      reason: "Browser Watch alert detected on the same route referenced by the incident.",
    });
    expect(payload.titleKey).toBe("incident.qa.drilldown.modal.browser_watch_title");
    expect(payload.fields[0].value).toBe("AUTH_LOGIN");
    expect(payload.fields[1].value).toBe("browser_watch");
  });

  it("builds failure cluster modal fallback payload with module", () => {
    const payload = buildDrilldownModalPayload({
      source: "failure_cluster",
      related_entity_type: "failure_cluster",
      related_entity_id: "cluster-auth-1",
      detail: "Failure cluster overlaps incident modules (auth, 4 failure(s), timeout)",
      reason: "Failure cluster overlaps incident modules and occurred within the investigation window.",
    });
    expect(payload.fields.find((f) => f.labelKey.includes("cluster_id"))?.value).toBe("cluster-auth-1");
    expect(payload.fields.find((f) => f.labelKey.includes("module"))?.value).toBe("auth");
  });

  it("uses action label keys per entity type", () => {
    expect(getDrilldownActionLabelKey("run")).toBe("incident.qa.drilldown.view_run");
    expect(getDrilldownActionLabelKey("pr_analysis")).toBe("incident.qa.drilldown.view_pr_analysis");
    expect(hasDrilldownMetadata({
      related_entity_type: "run",
      related_entity_id: "RUN-1",
    })).toBe(true);
  });
});
