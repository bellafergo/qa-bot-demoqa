import { describe, expect, it } from "vitest";
import {
  RBAC_I18N_KEYS,
  buildRbacViewModel,
  roleLabelKey,
} from "./rbacViewUtils.js";

const t = (key) => key;

describe("rbacViewUtils", () => {
  const rbac = {
    role_count: 5,
    permission_count: 9,
    default_roles_ready: true,
    enforcement_enabled: false,
    readiness_score: 50,
    summary: "RBAC foundation ready.",
  };

  const roles = {
    roles: [
      { role_id: "viewer", role_name: "VIEWER", description: "Read-only" },
      { role_id: "admin", role_name: "ADMIN", description: "Full access" },
    ],
    total: 2,
  };

  const permissions = {
    permissions: [
      { permission_id: "VIEW_DASHBOARD", permission_name: "View Dashboard", description: "" },
    ],
    total: 1,
  };

  it("maps role label keys", () => {
    expect(roleLabelKey("ADMIN")).toBe(RBAC_I18N_KEYS.roleAdmin);
  });

  it("builds rbac view model", () => {
    const vm = buildRbacViewModel({ rbac, roles, permissions, t });
    expect(vm.empty).toBe(false);
    expect(vm.statusText).toBe(RBAC_I18N_KEYS.configured);
    expect(vm.defaultRolesCount).toBe(5);
    expect(vm.permissionsCount).toBe(9);
    expect(vm.enforcementText).toBe(RBAC_I18N_KEYS.enforcementDisabled);
    expect(vm.readinessScore).toBe(50);
    expect(vm.readOnlyNote).toBe(RBAC_I18N_KEYS.readOnlyNote);
    expect(vm.roles).toHaveLength(2);
    expect(vm.permissions).toHaveLength(1);
  });

  it("uses updated enforcement-active read only note", () => {
    const vm = buildRbacViewModel({ rbac, roles, permissions, t: (key) => key });
    expect(vm.readOnlyNote).toBe("rbac.read_only_note");
  });

  it("handles empty roles", () => {
    const vm = buildRbacViewModel({ rbac, roles: { roles: [] }, permissions, t });
    expect(vm.empty).toBe(true);
  });
});
