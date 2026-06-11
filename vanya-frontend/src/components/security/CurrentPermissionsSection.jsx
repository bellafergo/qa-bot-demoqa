import React, { useMemo } from "react";
import { useLang } from "../../i18n/LangContext";
import { usePermissions } from "../../context/PermissionsContext.jsx";
import { buildCurrentPermissionsViewModel } from "../../utils/permissionViewUtils.js";

export default function CurrentPermissionsSection() {
  const { t } = useLang();
  const { permissions, roleName, userId, loading, error } = usePermissions();

  const vm = useMemo(
    () => buildCurrentPermissionsViewModel({
      me: { permissions, role_name: roleName, user_id: userId },
      t,
    }),
    [permissions, roleName, userId, t],
  );

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{vm.title}</div>
      <p style={{ margin: "0 0 14px", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
        {vm.subtitle}
      </p>

      {loading ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>…</div>
      ) : error ? (
        <div className="alert alert-error" style={{ fontSize: 12 }}>{error}</div>
      ) : vm.empty ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{vm.emptyMessage}</p>
      ) : (
        <>
          <div style={{ fontSize: 12, marginBottom: 10 }}>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.roleLabel}: </span>
            <span className="badge badge-blue">{vm.roleName}</span>
          </div>
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {vm.permissions.map((permission) => (
              <span key={permission} className="badge badge-gray">{permission}</span>
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
