/** Human-readable permission labels for customer-facing UI. */

export const PERMISSION_LABEL_KEYS = {
  VIEW_DASHBOARD: "permissions.label.view_dashboard",
  VIEW_INCIDENTS: "permissions.label.view_incidents",
  VIEW_RELEASE_INTELLIGENCE: "permissions.label.view_release_intelligence",
  VIEW_REPORTS: "permissions.label.view_reports",
  SEND_REPORTS: "permissions.label.send_reports",
  MANAGE_INTEGRATIONS: "permissions.label.manage_integrations",
  MANAGE_SECURITY: "permissions.label.manage_security",
  ADMIN: "permissions.label.admin",
};

export function humanizePermission(permissionId, t) {
  const id = String(permissionId || "").trim().toUpperCase();
  if (!id) return "";
  const key = PERMISSION_LABEL_KEYS[id];
  return key ? t(key) : id;
}
