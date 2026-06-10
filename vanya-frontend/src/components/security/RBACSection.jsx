import React, { useCallback, useEffect, useMemo, useState } from "react";
import {
  getRbacReadiness,
  getSecurityPermissions,
  getSecurityRoles,
  apiErrorMessage,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import { buildRbacViewModel } from "../../utils/rbacViewUtils.js";
import PermissionCard from "./PermissionCard.jsx";
import RBACReadinessCard from "./RBACReadinessCard.jsx";
import RoleCard from "./RoleCard.jsx";

export default function RBACSection() {
  const { t } = useLang();
  const [rbac, setRbac] = useState(null);
  const [roles, setRoles] = useState(null);
  const [permissions, setPermissions] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const [rbacData, rolesData, permissionsData] = await Promise.all([
        getRbacReadiness(),
        getSecurityRoles(),
        getSecurityPermissions(),
      ]);
      setRbac(rbacData);
      setRoles(rolesData);
      setPermissions(permissionsData);
    } catch (err) {
      setError(apiErrorMessage(err));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    load();
  }, [load]);

  const vm = useMemo(
    () => buildRbacViewModel({ rbac, roles, permissions, t }),
    [rbac, roles, permissions, t],
  );

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{vm.title}</div>

      {loading ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>…</div>
      ) : error ? (
        <div className="alert alert-error" style={{ fontSize: 12 }}>{error}</div>
      ) : vm.empty ? (
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: 0 }}>{vm.emptyMessage}</p>
      ) : (
        <>
          <RBACReadinessCard vm={vm} />
          <div style={{ marginTop: 16 }}>
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
              {vm.rolesTitle}
            </div>
            <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
              {vm.roles.map((role) => (
                <RoleCard key={role.role_id} role={role} />
              ))}
            </ul>
          </div>
          <div style={{ marginTop: 16 }}>
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
              {vm.permissionsTitle}
            </div>
            <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
              {vm.permissions.map((permission) => (
                <PermissionCard key={permission.permission_id} permission={permission} />
              ))}
            </ul>
          </div>
        </>
      )}

      <p style={{ margin: "12px 0 0", fontSize: 11, color: "var(--text-3)", fontStyle: "italic" }}>
        {vm.readOnlyNote}
      </p>
    </div>
  );
}
