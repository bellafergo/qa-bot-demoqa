// src/pages/SettingsPage.jsx
import React, { useEffect, useState } from "react";
import { useLang } from "../i18n/LangContext";
import { useTheme } from "../context/ThemeContext.jsx";
import { apiGet, apiErrorMessage } from "../api.js";
import { buildSettingsPlatformViewModel } from "../utils/settingsPlatformViewUtils.js";
import SecuritySection from "../components/security/SecuritySection.jsx";
import SSOConfigurationSection from "../components/security/SSOConfigurationSection.jsx";
import RBACSection from "../components/security/RBACSection.jsx";
import CurrentPermissionsSection from "../components/security/CurrentPermissionsSection.jsx";
import AuditTrailSection from "../components/security/AuditTrailSection.jsx";

function InfoRow({ label, value, badge }) {
  return (
    <div style={{ display: "flex", alignItems: "center", gap: 12, padding: "10px 0", borderBottom: "1px solid var(--border)" }}>
      <span style={{ fontSize: 12, color: "var(--text-3)", minWidth: 160, flexShrink: 0 }}>{label}</span>
      {badge ? badge : (
        <span style={{ fontSize: 13, color: "var(--text-1)", wordBreak: "break-word" }}>
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
  }, []);

  function StatusBadge({ variant, text }) {
    return (
      <span className={`badge badge-${variant}`} style={{ fontSize: 11 }}>
        ● {text}
      </span>
    );
  }

  const platformVm = buildSettingsPlatformViewModel(meta, t);
  const serviceConnected = platformVm.serviceOperational;

  return (
    <div className="page-wrap" style={{ maxWidth: 640 }}>
      <div className="page-header">
        <h1 className="page-title">{t("settings.title")}</h1>
        <p className="page-subtitle">{t("settings.subtitle")}</p>
      </div>

      <ThemeAppearanceCard />

      <SecuritySection />

      <SSOConfigurationSection />

      <RBACSection />

      <CurrentPermissionsSection />

      <AuditTrailSection />

      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{platformVm.serviceTitle}</div>
        <div>
          <InfoRow
            label={platformVm.serviceConnectedLabel}
            badge={
              <StatusBadge
                variant={serviceConnected ? "green" : "orange"}
                text={serviceConnected ? t("settings.api.status_live") : platformVm.aiEngineStatus}
              />
            }
          />
          <InfoRow
            label={platformVm.serviceStatusLabel}
            badge={
              <StatusBadge
                variant={serviceConnected ? "green" : "orange"}
                text={serviceConnected ? platformVm.aiEngineStatus : platformVm.databaseStatus}
              />
            }
          />
        </div>
      </div>

      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("settings.env.title")}</div>
        <div>
          <InfoRow label={t("settings.env.mode_label")} value={mode === "production" ? t("settings.env.mode_value") : mode} />
          <InfoRow label={t("settings.env.version_label")} value={t("settings.env.version_value")} />
        </div>
      </div>

      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{platformVm.platformTitle}</div>

        {metaLoading ? (
          <div style={{ fontSize: 12, color: "var(--text-3)", padding: "6px 0 10px" }}>{t("settings.platform.loading")}</div>
        ) : metaError ? (
          <div className="alert alert-error" style={{ marginTop: 12, marginBottom: 0, fontSize: 12 }}>
            {metaError}
          </div>
        ) : (
          <div>
            <InfoRow label={platformVm.aiEngineLabel} value={platformVm.aiEngineStatus} />
            <InfoRow label={platformVm.databaseLabel} value={platformVm.databaseStatus} />
            <InfoRow label={platformVm.evidenceStorageLabel} value={platformVm.evidenceStorageStatus} />
            <InfoRow label={platformVm.authServiceLabel} value={platformVm.authServiceStatus} />
            <InfoRow label={platformVm.lastUpdatedLabel} value={platformVm.lastUpdatedValue} />
          </div>
        )}
      </div>

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
