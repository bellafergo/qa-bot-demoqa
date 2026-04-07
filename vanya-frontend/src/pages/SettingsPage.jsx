// src/pages/SettingsPage.jsx
import React, { useEffect, useState } from "react";
import { useLang } from "../i18n/LangContext";
import { useTheme } from "../context/ThemeContext.jsx";
import { apiGet, apiErrorMessage, API_BASE } from "../api.js";

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

function ThemeAppearanceCard() {
  const { t } = useLang();
  const { preference, setPreference } = useTheme();
  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div className="section-title">{t("settings.theme.title")}</div>
      <p style={{ margin: "0 0 14px", fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
        {t("settings.theme.hint")}
      </p>
      <div className="zu-action-row" style={{ gap: 8 }}>
        {["dark", "light", "system"].map((k) => (
          <button
            key={k}
            type="button"
            className={`btn btn-sm ${preference === k ? "btn-primary" : "btn-secondary"}`}
            onClick={() => setPreference(k)}
          >
            {t(`settings.theme.${k}`)}
          </button>
        ))}
      </div>
    </div>
  );
}

export default function SettingsPage() {
  const { t } = useLang();
  const apiBase = API_BASE;
  const mode = import.meta?.env?.MODE || "production";

  const [meta, setMeta] = useState(null);
  const [metaLoading, setMetaLoading] = useState(false);
  const [metaError, setMetaError] = useState("");

  useEffect(() => {
    let cancelled = false;
    async function loadMeta() {
      setMetaLoading(true);
      setMetaError("");
      try {
        const data = await apiGet("/meta");
        if (!cancelled) setMeta(data || {});
      } catch (e) {
        if (!cancelled) setMetaError(apiErrorMessage(e) || "Failed to load platform diagnostics.");
      } finally {
        if (!cancelled) setMetaLoading(false);
      }
    }
    loadMeta();
    return () => {
      cancelled = true;
    };
  }, [apiBase]);

  function StatusBadge({ variant, text }) {
    return (
      <span className={`badge badge-${variant}`} style={{ fontSize: 11 }}>
        ● {text}
      </span>
    );
  }

  const m = meta || {};
  const hasOpenAI = m.has_openai_key === true;
  const hasDB = m.has_db === true;
  const hasCloudinary = m.has_cloudinary === true;

  const supabaseConfigured = m.supabase_configured === true;
  const supabaseOk = m.supabase_ok === true;
  const supabaseStrict = m.supabase_strict;

  const supabaseBadge = !supabaseConfigured
    ? { variant: "gray", text: "Disabled" }
    : supabaseOk
      ? { variant: "green", text: "OK" }
      : { variant: "orange", text: "Configured" };

  return (
    <div className="page-wrap" style={{ maxWidth: 640 }}>
      {/* ── Page header ────────────────────────────────── */}
      <div className="page-header">
        <h1 className="page-title">{t("settings.title")}</h1>
        <p className="page-subtitle">{t("settings.subtitle")}</p>
      </div>

      <ThemeAppearanceCard />

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

      {/* ── Platform health / diagnostics card ───────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">Platform Health</div>

        {metaLoading ? (
          <div style={{ fontSize: 12, color: "var(--text-3)", padding: "6px 0 10px" }}>Loading diagnostics…</div>
        ) : metaError ? (
          <div className="alert alert-error" style={{ marginTop: 12, marginBottom: 0, fontSize: 12 }}>
            {metaError}
          </div>
        ) : (
          <div>
            <InfoRow
              label="OpenAI"
              badge={
                <StatusBadge
                  variant={hasOpenAI ? "green" : "red"}
                  text={hasOpenAI ? "OK" : "Missing"}
                />
              }
            />

            <InfoRow
              label="Session TTL"
              value={m.session_ttl_s != null ? `${m.session_ttl_s}s` : "—"}
              mono
            />

            <InfoRow
              label="Database"
              badge={<StatusBadge variant={hasDB ? "green" : "gray"} text={hasDB ? "Enabled" : "Disabled"} />}
            />

            <InfoRow
              label="Cloudinary"
              badge={<StatusBadge variant={hasCloudinary ? "green" : "gray"} text={hasCloudinary ? "Enabled" : "Disabled"} />}
            />

            <InfoRow
              label="Supabase"
              badge={<StatusBadge variant={supabaseBadge.variant} text={supabaseBadge.text} />}
            />

            <InfoRow
              label="Supabase Strict"
              badge={
                supabaseStrict == null
                  ? <StatusBadge variant="gray" text="Unknown" />
                  : <StatusBadge variant={supabaseStrict ? "orange" : "gray"} text={supabaseStrict ? "Strict" : "Relaxed"} />
              }
            />

            <InfoRow
              label="Model"
              value={m.model || "—"}
            />

            <InfoRow
              label="Git Commit"
              value={m.render_git_commit || "—"}
              mono
            />

            {typeof m.sessions_in_memory === "number" && (
              <InfoRow
                label="Sessions (memory)"
                value={m.sessions_in_memory}
                mono
              />
            )}

            {typeof m.doc_cache_items === "number" && (
              <InfoRow
                label="Doc cache"
                value={m.doc_cache_items}
                mono
              />
            )}
          </div>
        )}
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
