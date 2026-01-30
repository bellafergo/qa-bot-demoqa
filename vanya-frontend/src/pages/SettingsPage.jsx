// src/pages/SettingsPage.jsx
/**
 * SettingsPage - Placeholder for future settings
 */
import React from "react";

export default function SettingsPage() {
  return (
    <div style={{ padding: 20, maxWidth: 600, margin: "0 auto" }}>
      <h2 style={{ marginBottom: 16, fontWeight: 800, color: "white" }}>
        Settings
      </h2>

      <p style={{ marginBottom: 24, opacity: 0.75, color: "white", fontSize: 14 }}>
        Application settings and configuration.
      </p>

      <section
        style={{
          padding: 20,
          borderRadius: 12,
          border: "1px solid rgba(255,255,255,0.12)",
          background: "rgba(0,0,0,0.2)",
        }}
      >
        <h3 style={{ margin: "0 0 16px", fontWeight: 700, color: "white" }}>
          API Configuration
        </h3>

        <div style={{ display: "grid", gap: 12 }}>
          <div>
            <label
              style={{
                display: "block",
                marginBottom: 6,
                fontSize: 13,
                opacity: 0.8,
                color: "white",
              }}
            >
              API Base URL
            </label>
            <input
              value={
                import.meta?.env?.VITE_API_BASE ||
                "https://qa-bot-demoqa.onrender.com"
              }
              readOnly
              style={{
                width: "100%",
                padding: "10px 12px",
                borderRadius: 10,
                border: "1px solid rgba(255,255,255,0.14)",
                background: "rgba(0,0,0,0.35)",
                color: "white",
                opacity: 0.7,
                outline: "none",
              }}
            />
            <p style={{ marginTop: 6, fontSize: 11, opacity: 0.6, color: "white" }}>
              Set via VITE_API_BASE environment variable
            </p>
          </div>
        </div>

        <div
          style={{
            marginTop: 20,
            padding: "12px 16px",
            borderRadius: 10,
            background: "rgba(78,107,255,0.1)",
            border: "1px solid rgba(78,107,255,0.2)",
          }}
        >
          <p style={{ margin: 0, fontSize: 13, color: "white", opacity: 0.85 }}>
            More settings coming soon:
          </p>
          <ul
            style={{
              margin: "8px 0 0",
              paddingLeft: 20,
              fontSize: 12,
              color: "white",
              opacity: 0.7,
            }}
          >
            <li>Theme preferences</li>
            <li>Notification settings</li>
            <li>Default execution options</li>
            <li>API key management</li>
          </ul>
        </div>
      </section>
    </div>
  );
}
