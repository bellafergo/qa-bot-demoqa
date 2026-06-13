import { describe, it, expect } from "vitest";
import {
  hasProjectIncidentHistoryItems,
  normalizeProjectIncidentHistory,
} from "./incidentHistoryViewUtils.js";

describe("normalizeProjectIncidentHistory", () => {
  it("returns items from paginated response shape", () => {
    const items = [
      { id: "1dd7e960-8e94-49fc-8e5a-ca3b085dad8e", description: "Login failure" },
    ];
    expect(normalizeProjectIncidentHistory({ items, total: 1 })).toEqual(items);
  });

  it("returns empty array when items is missing", () => {
    expect(normalizeProjectIncidentHistory({})).toEqual([]);
    expect(normalizeProjectIncidentHistory(null)).toEqual([]);
    expect(normalizeProjectIncidentHistory(undefined)).toEqual([]);
  });

  it("accepts a bare array for backward compatibility", () => {
    const items = [{ id: "a", description: "x" }];
    expect(normalizeProjectIncidentHistory(items)).toEqual(items);
  });
});

describe("hasProjectIncidentHistoryItems", () => {
  it("is true only when items has entries", () => {
    expect(hasProjectIncidentHistoryItems([{ id: "a" }])).toBe(true);
    expect(hasProjectIncidentHistoryItems([])).toBe(false);
    expect(hasProjectIncidentHistoryItems(null)).toBe(false);
  });
});
