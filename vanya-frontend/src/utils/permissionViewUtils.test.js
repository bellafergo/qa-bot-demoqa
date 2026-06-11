import { describe, expect, it } from "vitest";
import {
  PERMISSION_I18N_KEYS,
  buildCurrentPermissionsViewModel,
  hasPermission,
} from "./permissionViewUtils.js";

const t = (key) => key;

const ROLE_PERMISSIONS = {
  VIEWER: ["VIEW_DASHBOARD", "VIEW_INCIDENTS", "VIEW_REPORTS"],
  QA_ENGINEER: ["VIEW_DASHBOARD", "VIEW_INCIDENTS", "VIEW_REPORTS", "VIEW_RELEASE_INTELLIGENCE"],
  QA_MANAGER: [
    "VIEW_DASHBOARD",
    "VIEW_INCIDENTS",
    "VIEW_REPORTS",
    "VIEW_RELEASE_INTELLIGENCE",
    "APPROVE_ACTIONS",
    "SEND_REPORTS",
  ],
  RELEASE_MANAGER: [
    "VIEW_DASHBOARD",
    "VIEW_INCIDENTS",
    "VIEW_REPORTS",
    "VIEW_RELEASE_INTELLIGENCE",
    "APPROVE_ACTIONS",
    "SEND_REPORTS",
    "MANAGE_INTEGRATIONS",
  ],
  ADMIN: ["ADMIN", "MANAGE_SECURITY", "MANAGE_INTEGRATIONS", "SEND_REPORTS"],
};

describe("permissionViewUtils", () => {
  it("grants admin all permissions", () => {
    expect(hasPermission(ROLE_PERMISSIONS.ADMIN, "MANAGE_SECURITY")).toBe(true);
    expect(hasPermission(ROLE_PERMISSIONS.ADMIN, "UNKNOWN_PERM")).toBe(true);
  });

  it("viewer can view reports but not send", () => {
    expect(hasPermission(ROLE_PERMISSIONS.VIEWER, "VIEW_REPORTS")).toBe(true);
    expect(hasPermission(ROLE_PERMISSIONS.VIEWER, "SEND_REPORTS")).toBe(false);
    expect(hasPermission(ROLE_PERMISSIONS.VIEWER, "MANAGE_INTEGRATIONS")).toBe(false);
  });

  it("qa engineer can access incidents and release intelligence", () => {
    expect(hasPermission(ROLE_PERMISSIONS.QA_ENGINEER, "VIEW_INCIDENTS")).toBe(true);
    expect(hasPermission(ROLE_PERMISSIONS.QA_ENGINEER, "VIEW_RELEASE_INTELLIGENCE")).toBe(true);
    expect(hasPermission(ROLE_PERMISSIONS.QA_ENGINEER, "MANAGE_SECURITY")).toBe(false);
  });

  it("release manager can manage integrations and send reports", () => {
    expect(hasPermission(ROLE_PERMISSIONS.RELEASE_MANAGER, "MANAGE_INTEGRATIONS")).toBe(true);
    expect(hasPermission(ROLE_PERMISSIONS.RELEASE_MANAGER, "SEND_REPORTS")).toBe(true);
    expect(hasPermission(ROLE_PERMISSIONS.RELEASE_MANAGER, "MANAGE_SECURITY")).toBe(false);
  });

  it("builds current permissions view model", () => {
    const vm = buildCurrentPermissionsViewModel({
      me: { permissions: ROLE_PERMISSIONS.VIEWER, role_name: "VIEWER", user_id: "u-1" },
      t,
    });
    expect(vm.title).toBe(PERMISSION_I18N_KEYS.title);
    expect(vm.roleName).toBe("VIEWER");
    expect(vm.deniedMessage).toBe(PERMISSION_I18N_KEYS.denied);
    expect(vm.empty).toBe(false);
  });

  it("exposes denied empty state message", () => {
    const vm = buildCurrentPermissionsViewModel({ me: { permissions: [] }, t });
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe(PERMISSION_I18N_KEYS.empty);
    expect(vm.deniedMessage).toBe(PERMISSION_I18N_KEYS.denied);
  });

  it("hides privileged actions when permission missing", () => {
    const canSend = hasPermission(ROLE_PERMISSIONS.VIEWER, "SEND_REPORTS");
    const canManageSecurity = hasPermission(ROLE_PERMISSIONS.QA_ENGINEER, "MANAGE_SECURITY");
    expect(canSend).toBe(false);
    expect(canManageSecurity).toBe(false);
  });
});
