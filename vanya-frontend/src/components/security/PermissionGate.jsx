import React from "react";
import { usePermissions } from "../../context/PermissionsContext.jsx";
import { hasPermission } from "../../utils/permissionViewUtils.js";
import { useLang } from "../../i18n/LangContext";

export default function PermissionGate({
  permission,
  permissions: permissionsProp,
  children,
  fallback = null,
  showDenied = false,
}) {
  const { permissions: contextPermissions } = usePermissions();
  const { t } = useLang();
  const permissions = permissionsProp ?? contextPermissions;
  const allowed = hasPermission(permissions, permission);

  if (allowed) {
    return <>{children}</>;
  }

  if (showDenied) {
    return (
      <p style={{ fontSize: 12, color: "var(--text-3)", margin: 0, lineHeight: 1.5 }}>
        {t("permissions.denied")}
      </p>
    );
  }

  return fallback;
}
