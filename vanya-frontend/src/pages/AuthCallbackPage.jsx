// src/pages/AuthCallbackPage.jsx
import React, { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { supabase } from "../lib/supabaseClient.js";
import { useAuth } from "../auth/AuthContext.jsx";
import { SSO_PROVIDER_PENDING_KEY } from "../auth/AuthContext.jsx";
import { apiErrorMessage } from "../api";

export default function AuthCallbackPage() {
  const navigate = useNavigate();
  const { completeEnterpriseSsoCallback } = useAuth();
  const [msg, setMsg] = useState("Completing sign-in…");

  useEffect(() => {
    const params = new URLSearchParams(window.location.search);
    const code = params.get("code");
    const state = params.get("state");
    const provider = sessionStorage.getItem(SSO_PROVIDER_PENDING_KEY);

    if (code && state && provider) {
      completeEnterpriseSsoCallback(provider, code, state)
        .then(() => navigate("/dashboard", { replace: true }))
        .catch((err) => {
          setMsg(apiErrorMessage(err) || "Could not complete enterprise sign-in.");
          setTimeout(() => navigate("/login", { replace: true }), 1800);
        });
      return undefined;
    }

    if (!supabase) {
      navigate("/login", { replace: true });
      return undefined;
    }

    supabase.auth.getSession().then(({ data, error }) => {
      if (error || !data.session) {
        setMsg("Could not complete sign-in. Redirecting…");
        setTimeout(() => navigate("/login", { replace: true }), 1200);
        return;
      }
      navigate("/dashboard", { replace: true });
    });
    return undefined;
  }, [navigate, completeEnterpriseSsoCallback]);

  return (
    <div style={{
      minHeight: "100vh", display: "flex", alignItems: "center", justifyContent: "center",
      background: "var(--bg)", color: "var(--text-2)", fontSize: 14,
    }}>
      {msg}
    </div>
  );
}
