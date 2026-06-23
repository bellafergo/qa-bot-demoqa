import React from "react";
import { humanizePermission } from "../../utils/permissionLabelUtils.js";
import { useLang } from "../../i18n/LangContext";

export default function PermissionCard({ permission }) {
  const { t } = useLang();
  if (!permission) return null;

  const label = humanizePermission(permission.permission_id, t)
    || permission.permission_name
    || permission.permission_id;

  return (
    <li
      style={{
        marginBottom: 8,
        padding: "10px 12px",
        background: "var(--bg-3, rgba(255,255,255,0.03))",
        borderRadius: 8,
        fontSize: 13,
      }}
    >
      <div style={{ fontWeight: 600, color: "var(--text-1)" }}>{label}</div>
      {permission.description ? (
        <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 4 }}>{permission.description}</div>
      ) : null}
    </li>
  );
}
