// src/pages/SettingsPage.jsx
import React from "react";

export default function SettingsPage() {
  const apiBase = import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com";

  return (
    <div className="page-wrap" style={{ maxWidth: 640 }}>
      {/* ── Page header ────────────────────────────────── */}
      <div className="page-header">
        <h1 className="page-title">Settings</h1>
        <p className="page-subtitle">Application configuration and preferences</p>
      </div>

      {/* ── API config card ─────────────────────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">API Configuration</div>

        <div style={{ display: "grid", gap: 14 }}>
          <div>
            <label style={{ display: "block", fontSize: 13, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              API Base URL
            </label>
            <input
              className="input"
              value={apiBase}
              readOnly
            />
            <p style={{ margin: "6px 0 0", fontSize: 11, color: "var(--text-3)" }}>
              Configured via <code>VITE_API_BASE</code> environment variable
            </p>
          </div>
        </div>
      </div>

      {/* ── Coming soon card ─────────────────────────────── */}
      <div className="card">
        <div className="section-title">Upcoming Features</div>

        <div className="alert alert-info" style={{ marginBottom: 0 }}>
          <div>
            <div style={{ fontWeight: 600, marginBottom: 6 }}>More settings coming soon</div>
            <ul style={{ margin: 0, paddingLeft: 16, lineHeight: 2, fontSize: 13 }}>
              <li>Theme preferences (light / dark / system)</li>
              <li>Notification settings</li>
              <li>Default execution options</li>
              <li>API key management</li>
              <li>Team and workspace settings</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  );
}
