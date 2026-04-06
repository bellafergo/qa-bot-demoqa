// src/auth/PrivateRoute.jsx
import React from "react";
import { Navigate, useLocation } from "react-router-dom";
import { useAuth } from "./AuthContext.jsx";

export default function PrivateRoute({ children }) {
  const { session, loading, supabaseConfigured } = useAuth();
  const loc = useLocation();

  if (!supabaseConfigured) {
    return (
      <div className="page-wrap" style={{ padding: 40, maxWidth: 520, margin: "0 auto" }}>
        <div className="card">
          <h1 style={{ fontSize: 18, marginBottom: 8 }}>Configuration required</h1>
          <p style={{ fontSize: 14, color: "var(--text-2)", lineHeight: 1.6 }}>
            Set <code>VITE_SUPABASE_URL</code> and <code>VITE_SUPABASE_ANON_KEY</code> in the
            frontend environment, then rebuild.
          </p>
        </div>
      </div>
    );
  }

  if (loading) {
    return (
      <div style={{
        display: "flex", alignItems: "center", justifyContent: "center",
        minHeight: "100vh", color: "var(--text-3)", fontSize: 14,
      }}>
        Loading…
      </div>
    );
  }

  if (!session) {
    return <Navigate to="/login" replace state={{ from: loc.pathname }} />;
  }

  return children;
}
