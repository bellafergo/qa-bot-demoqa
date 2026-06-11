// src/pages/LoginPage.jsx
import React, { useEffect, useMemo, useState } from "react";
import { Navigate } from "react-router-dom";
import { useAuth } from "../auth/AuthContext.jsx";
import { useLang } from "../i18n/LangContext.jsx";
import { ZuperioLogo } from "../ui";
import { getSsoLoginProviders, apiErrorMessage } from "../api";
import SSOLoginView from "../components/auth/SSOLoginView.jsx";
import { buildSsoLoginViewModel } from "../utils/ssoLoginViewUtils.js";

export default function LoginPage() {
  const { t } = useLang();
  const {
    session,
    loading,
    supabaseConfigured,
    signInWithGoogle,
    signInWithEnterpriseSso,
  } = useAuth();
  const [busy, setBusy] = useState(false);
  const [busyProvider, setBusyProvider] = useState("");
  const [err, setErr] = useState("");
  const [ssoProviders, setSsoProviders] = useState([]);

  useEffect(() => {
    setErr("");
    getSsoLoginProviders()
      .then((data) => setSsoProviders(data?.providers || []))
      .catch(() => setSsoProviders([]));
  }, []);

  const ssoVm = useMemo(
    () => buildSsoLoginViewModel({ providers: ssoProviders, t }),
    [ssoProviders, t],
  );

  const hasAnyLogin = supabaseConfigured || !ssoVm.empty;

  if (!loading && session) {
    return <Navigate to="/dashboard" replace />;
  }

  if (!hasAnyLogin && !loading) {
    return (
      <div style={{
        minHeight: "100vh", display: "flex", alignItems: "center", justifyContent: "center",
        background: "var(--bg)", padding: 24,
      }}>
        <div className="card" style={{ maxWidth: 420, width: "100%", textAlign: "center" }}>
          <div style={{ display: "flex", justifyContent: "center", marginBottom: 16 }}>
            <ZuperioLogo variant="mark" />
          </div>
          <p style={{ color: "var(--text-2)", fontSize: 13, marginBottom: 6 }}>{t("auth.brand_stack")}</p>
          <p style={{ color: "var(--text-3)", fontSize: 14 }}>{ssoVm.emptyMessage}</p>
        </div>
      </div>
    );
  }

  async function onGoogle() {
    setErr("");
    setBusy(true);
    try {
      const { error } = await signInWithGoogle();
      if (error) setErr(error.message || String(error));
    } catch (e) {
      setErr(e?.message || String(e));
    } finally {
      setBusy(false);
    }
  }

  async function onEnterpriseProvider(provider) {
    setErr("");
    setBusyProvider(provider);
    try {
      const { error } = await signInWithEnterpriseSso(provider);
      if (error) setErr(apiErrorMessage(error) || error.message || String(error));
    } catch (e) {
      setErr(e?.message || String(e));
    } finally {
      setBusyProvider("");
    }
  }

  return (
    <div style={{
      minHeight: "100vh", display: "flex", alignItems: "center", justifyContent: "center",
      background: "var(--bg)", padding: 24,
    }}>
      <div className="card" style={{ maxWidth: 420, width: "100%", padding: "36px 32px" }}>
        <div style={{ display: "flex", justifyContent: "center", marginBottom: 12 }}>
          <ZuperioLogo variant="mark" />
        </div>
        <p style={{
          fontSize: 11, fontWeight: 500, textAlign: "center", letterSpacing: "0.06em",
          textTransform: "uppercase", color: "var(--text-4)", margin: "0 0 8px",
        }}>{t("auth.brand_stack")}</p>
        <h1 style={{
          fontSize: 22, fontWeight: 600, textAlign: "center", margin: "0 0 8px",
          color: "var(--text-1)", letterSpacing: "-0.02em",
        }}>Vanya</h1>
        <p style={{
          fontSize: 14, color: "var(--text-3)", textAlign: "center", marginBottom: 28, lineHeight: 1.5,
        }}>{t("auth.subtitle")}</p>

        {err ? (
          <div className="alert alert-error" style={{ marginBottom: 16, fontSize: 13 }}>{err}</div>
        ) : null}

        {!ssoVm.empty ? (
          <div style={{ marginBottom: supabaseConfigured ? 18 : 0 }}>
            <SSOLoginView
              vm={ssoVm}
              busyProvider={busyProvider}
              onProviderLogin={onEnterpriseProvider}
            />
          </div>
        ) : null}

        {supabaseConfigured && !ssoVm.empty ? (
          <div style={{
            display: "flex", alignItems: "center", gap: 10, margin: "18px 0",
            color: "var(--text-4)", fontSize: 11, textTransform: "uppercase", letterSpacing: "0.06em",
          }}>
            <div style={{ flex: 1, height: 1, background: "var(--border)" }} />
            <span>{ssoVm.localDivider}</span>
            <div style={{ flex: 1, height: 1, background: "var(--border)" }} />
          </div>
        ) : null}

        {supabaseConfigured ? (
          <button
            type="button"
            className="btn btn-primary"
            style={{ width: "100%", justifyContent: "center", gap: 10 }}
            disabled={busy || loading || Boolean(busyProvider)}
            onClick={onGoogle}
          >
            <span style={{ fontSize: 18 }}>G</span>
            {busy ? t("auth.redirecting") : t("auth.google")}
          </button>
        ) : null}

        <p style={{ fontSize: 12, color: "var(--text-4)", textAlign: "center", marginTop: 20 }}>
          {t("auth.hint")}
        </p>
      </div>
    </div>
  );
}
