/** View helpers for RBAC permission enforcement (SEC-01F alignment). */

export const PERMISSION_I18N_KEYS = {
  title: "permissions.current.title",
  subtitle: "permissions.current.subtitle",
  role: "permissions.current.role",
  email: "permissions.current.email",
  enforcement: "permissions.current.enforcement",
  enforcementOn: "permissions.current.enforcement_on",
  enforcementOff: "permissions.current.enforcement_off",
  empty: "permissions.current.empty",
  denied: "permissions.denied",
  deniedManageSecurity: "permissions.denied_manage_security",
  readOnlyNote: "permissions.read_only_note",
};

export function hasPermission(permissions, permissionId) {
  const list = Array.isArray(permissions) ? permissions : [];
  if (list.includes("ADMIN")) return true;
  return list.includes(String(permissionId || "").toUpperCase());
}

export function buildCurrentPermissionsViewModel({ me, t }) {
  const permissions = me?.permissions || [];
  const enforcementEnabled = Boolean(me?.enforcement_enabled);

  return {
    show: true,
    title: t(PERMISSION_I18N_KEYS.title),
    subtitle: t(PERMISSION_I18N_KEYS.subtitle),
    readOnlyNote: t(PERMISSION_I18N_KEYS.readOnlyNote),
    roleLabel: t(PERMISSION_I18N_KEYS.role),
    emailLabel: t(PERMISSION_I18N_KEYS.email),
    enforcementLabel: t(PERMISSION_I18N_KEYS.enforcement),
    enforcementText: enforcementEnabled
      ? t(PERMISSION_I18N_KEYS.enforcementOn)
      : t(PERMISSION_I18N_KEYS.enforcementOff),
    enforcementBadgeClass: enforcementEnabled ? "badge badge-green" : "badge badge-gray",
    roleName: me?.role_name || "VIEWER",
    userId: me?.user_id || "anonymous",
    email: me?.email || "",
    enforcementEnabled,
    permissions,
    empty: permissions.length === 0,
    emptyMessage: t(PERMISSION_I18N_KEYS.empty),
    deniedMessage: t(PERMISSION_I18N_KEYS.denied),
    deniedManageSecurityMessage: t(PERMISSION_I18N_KEYS.deniedManageSecurity),
  };
}
