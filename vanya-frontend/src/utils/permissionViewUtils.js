/** View helpers for RBAC permission enforcement (SEC-01F). */

export const PERMISSION_I18N_KEYS = {
  title: "permissions.current.title",
  subtitle: "permissions.current.subtitle",
  role: "permissions.current.role",
  empty: "permissions.current.empty",
  denied: "permissions.denied",
  readOnlyNote: "permissions.read_only_note",
};

export function hasPermission(permissions, permissionId) {
  const list = Array.isArray(permissions) ? permissions : [];
  if (list.includes("ADMIN")) return true;
  return list.includes(String(permissionId || "").toUpperCase());
}

export function buildCurrentPermissionsViewModel({ me, t }) {
  const permissions = me?.permissions || [];
  return {
    show: true,
    title: t(PERMISSION_I18N_KEYS.title),
    subtitle: t(PERMISSION_I18N_KEYS.subtitle),
    readOnlyNote: t(PERMISSION_I18N_KEYS.readOnlyNote),
    roleLabel: t(PERMISSION_I18N_KEYS.role),
    roleName: me?.role_name || "VIEWER",
    userId: me?.user_id || "anonymous",
    permissions,
    empty: permissions.length === 0,
    emptyMessage: t(PERMISSION_I18N_KEYS.empty),
    deniedMessage: t(PERMISSION_I18N_KEYS.denied),
  };
}
