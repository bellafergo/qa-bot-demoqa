import React, { useMemo } from "react";
import { useLang } from "../../i18n/LangContext";
import { useRbac } from "../../auth/RbacContext.jsx";
import { useAuth } from "../../auth/AuthContext.jsx";
import { buildCurrentPermissionsViewModel } from "../../utils/permissionViewUtils.js";
import { humanizePermission } from "../../utils/permissionLabelUtils.js";
import { roleLabelKey } from "../../utils/rbacViewUtils.js";

export default function CurrentPermissionsSection() {
  const { t } = useLang();
  const { user } = useAuth();
  const {
    permissions,
    roleName,
    email,
    enforcementEnabled,
    loading,
  } = useRbac();

  const vm = useMemo(
    () => buildCurrentPermissionsViewModel({
      me: {
        permissions,
        role_name: roleName,
        user_id: user?.id || null,
        email: email || user?.email || "",
        enforcement_enabled: enforcementEnabled,
      },
      t,
    }),
    [permissions, roleName, user, email, enforcementEnabled, t],
  );

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{vm.title}</div>
      <p style={{ margin: "0 0 14px", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
        {vm.subtitle}
      </p>

      {loading ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>…</div>
      ) : vm.empty ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{vm.emptyMessage}</p>
      ) : (
        <>
          {vm.email ? (
            <div style={{ fontSize: 12, marginBottom: 8 }}>
              <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.emailLabel}: </span>
              <span style={{ color: "var(--text-2)" }}>{vm.email}</span>
            </div>
          ) : null}
          <div style={{ fontSize: 12, marginBottom: 8 }}>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.roleLabel}: </span>
            <span className="badge badge-blue">{t(roleLabelKey(vm.roleName))}</span>
          </div>
          <div style={{ fontSize: 12, marginBottom: 10 }}>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.enforcementLabel}: </span>
            <span className={vm.enforcementBadgeClass}>{vm.enforcementText}</span>
          </div>
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {vm.permissions.map((permission) => (
              <span key={permission} className="badge badge-gray">
                {humanizePermission(permission, t)}
              </span>
            ))}
          </div>
        </>
      )}

      <p style={{ margin: "12px 0 0", fontSize: 11, color: "var(--text-3)", fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
