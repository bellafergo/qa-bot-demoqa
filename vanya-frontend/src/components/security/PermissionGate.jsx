import React from "react";
import { useRbac } from "../../auth/RbacContext.jsx";
import { useLang } from "../../i18n/LangContext";
import { hasPermission as checkPermission } from "../../utils/permissionViewUtils.js";

export default function PermissionGate({
  permission,
  permissions: permissionsProp,
  children,
  fallback = null,
  showDenied = false,
  deniedMessageKey = "permissions.denied",
}) {
  const { permissions: contextPermissions, enforcementEnabled } = useRbac();
  const { t } = useLang();
  const permissions = permissionsProp ?? contextPermissions;
  const allowed = !enforcementEnabled || checkPermission(permissions, permission);

  if (allowed) {
    return <>{children}</>;
  }

  if (showDenied) {
    return (
      <p style={{ fontSize: 12, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {t(deniedMessageKey)}
      </p>
    );
  }

  return fallback;
}
