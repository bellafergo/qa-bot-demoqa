import React, { useCallback, useEffect, useMemo, useState } from "react";
import { getSsoAuthMe, getSecurityReadiness, apiErrorMessage } from "../../api";
import { useLang } from "../../i18n/LangContext";
import { buildSsoIdentityViewModel } from "../../utils/ssoLoginViewUtils.js";

export default function AuthenticationIdentitySection() {
  const { t } = useLang();
  const [identity, setIdentity] = useState(null);
  const [readiness, setReadiness] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const [identityData, readinessData] = await Promise.all([
        getSsoAuthMe().catch(() => null),
        getSecurityReadiness(),
      ]);
      setIdentity(identityData);
      setReadiness(readinessData);
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
    () => buildSsoIdentityViewModel({ identity, readiness, t }),
    [identity, readiness, t],
  );

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{vm.title}</div>

      {loading ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>…</div>
      ) : error ? (
        <div className="alert alert-error" style={{ fontSize: 12 }}>{error}</div>
      ) : (
        <div style={{ display: "grid", gap: 8, fontSize: 13 }}>
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.providerLabel}: </span>
            <span className="badge badge-blue">{vm.provider}</span>
          </div>
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.emailLabel}: </span>
            <span style={{ color: "var(--text-2)" }}>{vm.email}</span>
          </div>
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.displayNameLabel}: </span>
            <span style={{ color: "var(--text-2)" }}>{vm.displayName}</span>
          </div>
          <div>
            <span style={{ color: "var(--text-3)", fontWeight: 600 }}>{vm.methodLabel}: </span>
            <span style={{ color: "var(--text-2)" }}>{vm.authenticationMethod}</span>
          </div>
        </div>
      )}
    </div>
  );
}
