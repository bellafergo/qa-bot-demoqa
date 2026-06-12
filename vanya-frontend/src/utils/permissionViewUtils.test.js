import { describe, expect, it } from "vitest";
import {
  PERMISSION_I18N_KEYS,
  buildCurrentPermissionsViewModel,
  hasPermission,
} from "./permissionViewUtils.js";

const t = (key) => key;

describe("permissionViewUtils", () => {
  it("checks permission with ADMIN override", () => {
    expect(hasPermission(["VIEW_DASHBOARD"], "VIEW_DASHBOARD")).toBe(true);
    expect(hasPermission(["VIEW_DASHBOARD"], "MANAGE_SECURITY")).toBe(false);
    expect(hasPermission(["ADMIN"], "MANAGE_SECURITY")).toBe(true);
  });

  it("builds current permissions view model", () => {
    const vm = buildCurrentPermissionsViewModel({
      me: {
        user_id: "user-1",
        email: "qa@corp.com",
        role_name: "VIEWER",
        permissions: ["VIEW_DASHBOARD", "VIEW_INCIDENTS", "VIEW_REPORTS"],
        enforcement_enabled: true,
      },
      t,
    });

    expect(vm.title).toBe(PERMISSION_I18N_KEYS.title);
    expect(vm.roleName).toBe("VIEWER");
    expect(vm.email).toBe("qa@corp.com");
    expect(vm.enforcementEnabled).toBe(true);
    expect(vm.enforcementText).toBe(PERMISSION_I18N_KEYS.enforcementOn);
    expect(vm.permissions).toHaveLength(3);
    expect(vm.empty).toBe(false);
  });

  it("handles empty permissions", () => {
    const vm = buildCurrentPermissionsViewModel({
      me: { permissions: [], role_name: "VIEWER", enforcement_enabled: false },
      t,
    });
    expect(vm.empty).toBe(true);
    expect(vm.enforcementText).toBe(PERMISSION_I18N_KEYS.enforcementOff);
  });

  it("exposes audit denied message key", () => {
    const vm = buildCurrentPermissionsViewModel({ me: { permissions: [] }, t });
    expect(vm.deniedManageSecurityMessage).toBe(PERMISSION_I18N_KEYS.deniedManageSecurity);
  });
});
