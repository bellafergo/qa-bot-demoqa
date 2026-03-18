// src/pages/SettingsPage.jsx
import React from "react";
import { useLang } from "../i18n/LangContext";

export default function SettingsPage() {
  const { t } = useLang();
  const apiBase = import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com";

  return (
    <div className="page-wrap" style={{ maxWidth: 640 }}>
      {/* ── Page header ────────────────────────────────── */}
      <div className="page-header">
        <h1 className="page-title">{t("settings.title")}</h1>
        <p className="page-subtitle">{t("settings.subtitle")}</p>
      </div>

      {/* ── API config card ─────────────────────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("settings.api.title")}</div>

        <div style={{ display: "grid", gap: 14 }}>
          <div>
            <label style={{ display: "block", fontSize: 13, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("settings.api.url_label")}
            </label>
            <input
              className="input"
              value={apiBase}
              readOnly
            />
            <p style={{ margin: "6px 0 0", fontSize: 11, color: "var(--text-3)" }}>
              {t("settings.api.url_note")} (<code>VITE_API_BASE</code>)
            </p>
          </div>
        </div>
      </div>

      {/* ── Coming soon card ─────────────────────────────── */}
      <div className="card">
        <div className="section-title">{t("settings.upcoming.title")}</div>

        <div className="alert alert-info" style={{ marginBottom: 0 }}>
          <div>
            <div style={{ fontWeight: 600, marginBottom: 6 }}>{t("settings.upcoming.heading")}</div>
            <ul style={{ margin: 0, paddingLeft: 16, lineHeight: 2, fontSize: 13 }}>
              <li>{t("settings.upcoming.item1")}</li>
              <li>{t("settings.upcoming.item2")}</li>
              <li>{t("settings.upcoming.item3")}</li>
              <li>{t("settings.upcoming.item4")}</li>
              <li>{t("settings.upcoming.item5")}</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  );
}
