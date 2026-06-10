/** View helpers for RBAC foundation (SEC-01B). */

export const RBAC_I18N_KEYS = {
  title: "rbac.title",
  empty: "rbac.empty",
  status: "rbac.status",
  configured: "rbac.configured",
  notConfigured: "rbac.not_configured",
  defaultRoles: "rbac.default_roles",
  permissions: "rbac.permissions",
  enforcement: "rbac.enforcement",
  enforcementEnabled: "rbac.enforcement_enabled",
  enforcementDisabled: "rbac.enforcement_disabled",
  readiness: "rbac.readiness",
  rolesTitle: "rbac.roles_title",
  permissionsTitle: "rbac.permissions_title",
  readOnlyNote: "rbac.read_only_note",
  roleViewer: "rbac.role.viewer",
  roleQaEngineer: "rbac.role.qa_engineer",
  roleQaManager: "rbac.role.qa_manager",
  roleReleaseManager: "rbac.role.release_manager",
  roleAdmin: "rbac.role.admin",
};

const ROLE_LABEL_KEY = {
  VIEWER: RBAC_I18N_KEYS.roleViewer,
  QA_ENGINEER: RBAC_I18N_KEYS.roleQaEngineer,
  QA_MANAGER: RBAC_I18N_KEYS.roleQaManager,
  RELEASE_MANAGER: RBAC_I18N_KEYS.roleReleaseManager,
  ADMIN: RBAC_I18N_KEYS.roleAdmin,
};

export function roleLabelKey(roleName) {
  return ROLE_LABEL_KEY[String(roleName || "").toUpperCase()] || roleName;
}

export function buildRbacViewModel({ rbac, roles, permissions, t }) {
  const report = rbac || {};
  const roleList = roles?.roles || [];
  const permissionList = permissions?.permissions || [];
  const empty = roleList.length === 0;

  return {
    show: true,
    empty,
    emptyMessage: t(RBAC_I18N_KEYS.empty),
    title: t(RBAC_I18N_KEYS.title),
    readOnlyNote: t(RBAC_I18N_KEYS.readOnlyNote),
    statusLabel: t(RBAC_I18N_KEYS.status),
    statusText: report.default_roles_ready
      ? t(RBAC_I18N_KEYS.configured)
      : t(RBAC_I18N_KEYS.notConfigured),
    statusBadgeClass: report.default_roles_ready ? "badge badge-green" : "badge badge-orange",
    defaultRolesLabel: t(RBAC_I18N_KEYS.defaultRoles),
    defaultRolesCount: report.role_count ?? roleList.length,
    permissionsLabel: t(RBAC_I18N_KEYS.permissions),
    permissionsCount: report.permission_count ?? permissionList.length,
    enforcementLabel: t(RBAC_I18N_KEYS.enforcement),
    enforcementText: report.enforcement_enabled
      ? t(RBAC_I18N_KEYS.enforcementEnabled)
      : t(RBAC_I18N_KEYS.enforcementDisabled),
    enforcementBadgeClass: report.enforcement_enabled ? "badge badge-green" : "badge badge-gray",
    readinessLabel: t(RBAC_I18N_KEYS.readiness),
    readinessScore: report.readiness_score ?? 0,
    readinessBadgeClass: (report.readiness_score ?? 0) >= 50 ? "badge badge-blue" : "badge badge-orange",
    summary: report.summary || "",
    rolesTitle: t(RBAC_I18N_KEYS.rolesTitle),
    permissionsTitle: t(RBAC_I18N_KEYS.permissionsTitle),
    roles: roleList.map((role) => ({
      ...role,
      displayName: t(roleLabelKey(role.role_name)),
    })),
    permissions: permissionList,
  };
}
