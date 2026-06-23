import { describe, expect, it } from "vitest";
import { humanizePermission } from "./permissionLabelUtils.js";

const t = (key) => key;

describe("humanizePermission", () => {
  it("maps known permission ids to i18n keys", () => {
    expect(humanizePermission("VIEW_DASHBOARD", t)).toBe("permissions.label.view_dashboard");
    expect(humanizePermission("SEND_REPORTS", t)).toBe("permissions.label.send_reports");
  });

  it("returns empty string for blank ids", () => {
    expect(humanizePermission("", t)).toBe("");
  });
});
