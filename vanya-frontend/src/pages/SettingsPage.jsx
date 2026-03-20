// src/pages/SettingsPage.jsx
import React from "react";
import { useLang } from "../i18n/LangContext";

function InfoRow({ label, value, mono = false, badge }) {
  return (
    <div style={{ display: "flex", alignItems: "center", gap: 12, padding: "10px 0", borderBottom: "1px solid var(--border)" }}>
      <span style={{ fontSize: 12, color: "var(--text-3)", minWidth: 160, flexShrink: 0 }}>{label}</span>
      {badge ? badge : (
        <span style={{ fontSize: 13, color: "var(--text-1)", fontFamily: mono ? "monospace" : undefined, wordBreak: "break-all" }}>
          {value}
        </span>
      )}
    </div>
  );
}

export default function SettingsPage() {
  const { t } = useLang();
  const apiBase = import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com";
  const mode = import.meta?.env?.MODE || "production";

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
        <div>
          <InfoRow
            label={t("settings.api.url_label")}
            value={apiBase}
            mono
          />
          <InfoRow
            label={t("settings.api.status_label")}
            badge={
              <span className="badge badge-green" style={{ fontSize: 11 }}>
                ● {t("settings.api.status_live")}
              </span>
            }
          />
        </div>
        <p style={{ margin: "10px 0 0", fontSize: 11, color: "var(--text-3)" }}>
          {t("settings.api.url_note")} (<code>VITE_API_BASE</code>)
        </p>
      </div>

      {/* ── Environment card ────────────────────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("settings.env.title")}</div>
        <div>
          <InfoRow label={t("settings.env.mode_label")}    value={mode === "production" ? t("settings.env.mode_value") : mode} />
          <InfoRow label={t("settings.env.version_label")} value={t("settings.env.version_value")} />
        </div>
      </div>

      {/* ── Roadmap card ─────────────────────────────────── */}
      <div className="card">
        <div className="section-title">{t("settings.upcoming.title")}</div>
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12 }}>
          {t("settings.upcoming.heading")}
        </div>
        <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
          {[
            t("settings.upcoming.item1"),
            t("settings.upcoming.item2"),
            t("settings.upcoming.item3"),
            t("settings.upcoming.item4"),
            t("settings.upcoming.item5"),
          ].map((item, i) => (
            <div key={i} style={{ display: "flex", alignItems: "flex-start", gap: 10, fontSize: 13, color: "var(--text-2)" }}>
              <span style={{ color: "var(--border)", fontSize: 11, marginTop: 2, flexShrink: 0 }}>◻</span>
              {item}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
