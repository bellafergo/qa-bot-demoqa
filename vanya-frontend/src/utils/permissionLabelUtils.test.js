import { describe, expect, it } from "vitest";
import { humanizePermission, fallbackHumanizePermission } from "./permissionLabelUtils.js";

const t = (key) => key;

describe("humanizePermission", () => {
  it("maps known permission ids to i18n keys", () => {
    expect(humanizePermission("VIEW_DASHBOARD", t)).toBe("permissions.label.view_dashboard");
    expect(humanizePermission("SEND_REPORTS", t)).toBe("permissions.label.send_reports");
    expect(humanizePermission("APPROVE_ACTIONS", t)).toBe("permissions.label.approve_actions");
  });

  it("returns empty string for blank ids", () => {
    expect(humanizePermission("", t)).toBe("");
  });

  it("humanizes unknown underscore permissions as fallback", () => {
    expect(humanizePermission("CUSTOM_VIEW_THING", t)).toBe("Custom View Thing");
    expect(fallbackHumanizePermission("VIEW_SOMETHING_NEW")).toBe("View Something New");
  });
});
