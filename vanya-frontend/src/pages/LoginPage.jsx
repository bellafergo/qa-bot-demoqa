// src/pages/LoginPage.jsx
import React, { useEffect, useState } from "react";
import { Navigate } from "react-router-dom";
import { useAuth } from "../auth/AuthContext.jsx";
import { useLang } from "../i18n/LangContext.jsx";
import { ZuperioLogo } from "../ui";

export default function LoginPage() {
  const { t } = useLang();
  const { session, loading, supabaseConfigured, signInWithGoogle } = useAuth();
  const [busy, setBusy] = useState(false);
  const [err, setErr] = useState("");

  useEffect(() => {
    setErr("");
  }, []);

  if (!supabaseConfigured) {
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
          <p style={{ color: "var(--text-3)", fontSize: 14 }}>{t("auth.config_missing")}</p>
        </div>
      </div>
    );
  }

  if (!loading && session) {
    return <Navigate to="/dashboard" replace />;
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

        <button
          type="button"
          className="btn btn-primary"
          style={{ width: "100%", justifyContent: "center", gap: 10 }}
          disabled={busy || loading}
          onClick={onGoogle}
        >
          <span style={{ fontSize: 18 }}>G</span>
          {busy ? t("auth.redirecting") : t("auth.google")}
        </button>

        <p style={{ fontSize: 12, color: "var(--text-4)", textAlign: "center", marginTop: 20 }}>
          {t("auth.hint")}
        </p>
      </div>
    </div>
  );
}
