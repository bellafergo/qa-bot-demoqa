// src/pages/AuthCallbackPage.jsx
import React, { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { supabase } from "../lib/supabaseClient.js";

export default function AuthCallbackPage() {
  const navigate = useNavigate();
  const [msg, setMsg] = useState("Completing sign-in…");

  useEffect(() => {
    if (!supabase) {
      navigate("/login", { replace: true });
      return;
    }
    supabase.auth.getSession().then(({ data, error }) => {
      if (error || !data.session) {
        setMsg("Could not complete sign-in. Redirecting…");
        setTimeout(() => navigate("/login", { replace: true }), 1200);
        return;
      }
      navigate("/dashboard", { replace: true });
    });
  }, [navigate]);

  return (
    <div style={{
      minHeight: "100vh", display: "flex", alignItems: "center", justifyContent: "center",
      background: "var(--bg)", color: "var(--text-2)", fontSize: 14,
    }}>
      {msg}
    </div>
  );
}
