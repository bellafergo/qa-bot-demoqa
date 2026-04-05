// src/pages/IntegrationsPage.jsx
import React, { useCallback, useEffect, useState } from "react";
import {
  listIntegrations,
  getIntegration,
  runHealthCheck,
  enableIntegration,
  disableIntegration,
  updateIntegrationConfig,
  getIntegrationActions,
} from "../api";
import { useLang } from "../i18n/LangContext";

// ── Helpers ───────────────────────────────────────────────────────────────────

const HEALTH_COLOR = {
  ok:            "var(--green, #22c55e)",
  degraded:      "var(--yellow, #eab308)",
  unreachable:   "var(--red, #ef4444)",
  unconfigured:  "var(--text-2, #94a3b8)",
  unknown:       "var(--text-2, #94a3b8)",
};

// Translation key map for health labels
const HEALTH_KEY = {
  ok:           "integrations.health.ok",
  degraded:     "integrations.health.degraded",
  unreachable:  "integrations.health.unreachable",
  unconfigured: "integrations.health.unconfigured",
  unknown:      "integrations.health.unknown",
};

const CONNECTOR_ICONS = {
  jira:    "◈",
  github:  "◎",
  slack:   "✦",
  qmetry:  "⊞",
};

function HealthBadge({ health }) {
  const { t } = useLang();
  const color = HEALTH_COLOR[health] || HEALTH_COLOR.unknown;
  const label = HEALTH_KEY[health] ? t(HEALTH_KEY[health]) : (health || t("integrations.health.unknown"));
  return (
    <span style={{
      display: "inline-flex", alignItems: "center", gap: 5,
      fontSize: 11, fontWeight: 500,
      padding: "2px 8px", borderRadius: 20,
      background: `${color}18`,
      color: color,
      border: `1px solid ${color}40`,
    }}>
      <span style={{ width: 6, height: 6, borderRadius: "50%", background: color, flexShrink: 0 }} />
      {label}
    </span>
  );
}

function StatusPill({ enabled }) {
  const { t } = useLang();
  return (
    <span style={{
      fontSize: 10, fontWeight: 400,
      padding: "2px 7px", borderRadius: 20, textTransform: "uppercase",
      background: enabled ? "rgba(34,197,94,0.12)" : "rgba(148,163,184,0.12)",
      color:      enabled ? "var(--green, #22c55e)" : "var(--text-2, #94a3b8)",
      border: `1px solid ${enabled ? "rgba(34,197,94,0.3)" : "rgba(148,163,184,0.2)"}`,
    }}>
      {enabled ? t("integrations.status.enabled") : t("integrations.status.disabled")}
    </span>
  );
}

function fmtTs(ts) {
  if (!ts) return "—";
  try { return new Date(ts).toLocaleString(); } catch { return ts; }
}

/** Relative time for last_check_at (minimal, lang-aware). */
function fmtRelative(ts, lang) {
  if (!ts) return "—";
  const d = new Date(ts);
  const ms = d.getTime();
  if (Number.isNaN(ms)) return fmtTs(ts);
  const sec = Math.floor((Date.now() - ms) / 1000);
  if (sec < 0) return fmtTs(ts);
  const es = lang === "es";
  if (sec < 60) return es ? `hace ${sec}s` : `${sec}s ago`;
  const min = Math.floor(sec / 60);
  if (min < 60) return es ? `hace ${min} min` : `${min} min ago`;
  const hr = Math.floor(min / 60);
  if (hr < 48) return es ? `hace ${hr} h` : `${hr}h ago`;
  return fmtTs(ts);
}

// ── Config form ───────────────────────────────────────────────────────────────

const CONFIG_FIELDS = {
  jira: [
    { key: "base_url",   label: "Base URL",   placeholder: "https://yourorg.atlassian.net", type: "text" },
    { key: "workspace",  label: "User Email",  placeholder: "user@example.com",             type: "text" },
    { key: "project_key",label: "Project Key", placeholder: "QA",                           type: "text" },
    { key: "token",      label: "API Token",   placeholder: "Jira API token",               type: "password", secret: true },
  ],
  github: [
    { key: "workspace",  label: "Org / User",  placeholder: "myorg",                        type: "text" },
    { key: "token",      label: "Access Token", placeholder: "ghp_…",                       type: "password", secret: true },
  ],
  slack: [
    { key: "token",      label: "Bot Token",           placeholder: "xoxb-…",   type: "password", secret: true },
    { key: "channel",    label: "Default channel",      placeholder: "#qa-alerts", type: "text" },
    { key: "workspace",  label: "Workspace (optional)", placeholder: "myworkspace", type: "text" },
  ],
  qmetry: [
    { key: "base_url",   label: "Base URL",     placeholder: "https://jira.example.com",    type: "text" },
    { key: "project_key",label: "Project Key",  placeholder: "QA",                          type: "text" },
    { key: "api_key",    label: "API Key",       placeholder: "QMetry API key",              type: "password", secret: true },
  ],
};

function ConfigForm({ connectorId, currentConfig, onSaved }) {
  const { t } = useLang();
  const fields = CONFIG_FIELDS[connectorId] || [];
  const [values, setValues] = useState(() => {
    const init = {};
    fields.forEach(f => { init[f.key] = ""; });
    return init;
  });
  const [saving, setSaving] = useState(false);
  const [error, setError]   = useState(null);
  const [ok, setOk]         = useState(false);

  const handleChange = (key, val) => setValues(v => ({ ...v, [key]: val }));

  const buildPayload = () => {
    const payload = {};
    fields.forEach(f => { if (values[f.key]) payload[f.key] = values[f.key]; });
    if (connectorId === "slack") payload.auth_type = "token";
    return payload;
  };

  const handleSave = async () => {
    setSaving(true); setError(null); setOk(false);
    try {
      const cfg = await updateIntegrationConfig(connectorId, buildPayload());
      setOk(true);
      const cleared = { ...values };
      fields.filter(f => f.secret).forEach(f => { cleared[f.key] = ""; });
      setValues(cleared);
      if (onSaved) await Promise.resolve(onSaved(cfg));
    } catch (e) {
      setError(e.message);
    } finally {
      setSaving(false);
    }
  };

  const handleSaveAndEnable = async () => {
    setSaving(true); setError(null); setOk(false);
    try {
      const cfg = await updateIntegrationConfig(connectorId, buildPayload());
      await enableIntegration(connectorId);
      setOk(true);
      const cleared = { ...values };
      fields.filter(f => f.secret).forEach(f => { cleared[f.key] = ""; });
      setValues(cleared);
      if (onSaved) await Promise.resolve(onSaved(cfg));
    } catch (e) {
      setError(e.message);
    } finally {
      setSaving(false);
    }
  };

  if (!fields.length) return null;

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 10 }}>
        {t("integrations.config.title")}
      </div>
      {connectorId === "slack" && (
        <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 10, lineHeight: 1.45 }}>
          {t("integrations.slack.hint_health")}
        </div>
      )}
      <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
        {fields.map(f => (
          <div key={f.key}>
            <div style={{ fontSize: 11, color: "var(--text-2)", marginBottom: 3 }}>
              {f.label}
              {f.secret && (
                <span style={{ marginLeft: 6, fontSize: 10, color: "var(--text-3, #64748b)", background: "rgba(148,163,184,0.1)", padding: "1px 5px", borderRadius: 4 }}>
                  {t("integrations.config.write_only")}
                </span>
              )}
              {f.secret && currentConfig && currentConfig[`${f.key}_present`] && (
                <span style={{ marginLeft: 6, fontSize: 10, color: "var(--green, #22c55e)" }}>{t("integrations.config.set")}</span>
              )}
            </div>
            <input
              className="input"
              type={f.type}
              placeholder={f.secret ? t("integrations.config.ph_secret") : f.placeholder}
              value={values[f.key]}
              onChange={e => handleChange(f.key, e.target.value)}
              style={{ width: "100%", boxSizing: "border-box" }}
            />
          </div>
        ))}
      </div>

      {error && (
        <div className="alert alert-error" style={{ marginTop: 8, fontSize: 12 }}>{error}</div>
      )}
      {ok && (
        <div style={{ marginTop: 8, fontSize: 12, color: "var(--green, #22c55e)" }}>{t("integrations.config.saved")}</div>
      )}

      <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginTop: 10 }}>
        <button
          className="btn btn-primary btn-sm"
          onClick={handleSave}
          disabled={saving}
        >
          {saving ? t("integrations.config.saving") : t("integrations.config.save")}
        </button>
        {connectorId === "slack" && (
          <button
            className="btn btn-secondary btn-sm"
            onClick={handleSaveAndEnable}
            disabled={saving}
          >
            {saving ? t("integrations.config.saving") : t("integrations.config.save_enable")}
          </button>
        )}
      </div>
    </div>
  );
}

// ── Connector card ────────────────────────────────────────────────────────────

function ConnectorCard({ summary, onAction }) {
  const { t, lang } = useLang();
  const [expanded,  setExpanded]  = useState(false);
  const [detail,    setDetail]    = useState(null);
  const [actions,   setActions]   = useState(null);
  const [checking,  setChecking]  = useState(false);
  const [toggling,  setToggling]  = useState(false);
  const [config,    setConfig]    = useState(null);

  const loadDetail = useCallback(async () => {
    try {
      const [status, actData] = await Promise.all([
        getIntegration(summary.connector_id),
        getIntegrationActions(summary.connector_id),
      ]);
      setDetail(status);
      setConfig(status.config_summary || null);
      setActions(actData.actions || []);
    } catch { /* best-effort */ }
  }, [summary.connector_id]);

  const handleExpand = () => {
    if (!expanded) loadDetail();
    setExpanded(e => !e);
  };

  const handleHealthCheck = async (e) => {
    e.stopPropagation();
    setChecking(true);
    try {
      const status = await runHealthCheck(summary.connector_id);
      setDetail(status);
      if (onAction) await onAction(summary.connector_id, "health", status);
    } catch { /* handled by caller */ }
    finally { setChecking(false); }
  };

  const handleToggle = async (e) => {
    e.stopPropagation();
    setToggling(true);
    try {
      if (summary.enabled) {
        await disableIntegration(summary.connector_id);
      } else {
        await enableIntegration(summary.connector_id);
      }
      if (onAction) await onAction(summary.connector_id, "toggle");
    } catch { /* best-effort */ }
    finally { setToggling(false); }
  };

  const icon = CONNECTOR_ICONS[summary.connector_id] || "◇";
  const health = (detail && detail.health) || summary.health;

  return (
    <div className="card" style={{ marginBottom: 12, transition: "box-shadow 0.15s" }}>
      {/* Header row */}
      <div
        style={{ display: "flex", alignItems: "center", gap: 12, cursor: "pointer", userSelect: "none" }}
        onClick={handleExpand}
        role="button"
        aria-expanded={expanded}
      >
        {/* Icon */}
        <div style={{
          width: 38, height: 38, borderRadius: 10, flexShrink: 0,
          background: "var(--accent-muted, rgba(79,107,255,0.1))",
          display: "flex", alignItems: "center", justifyContent: "center",
          fontSize: 18, color: "var(--accent)",
        }}>
          {icon}
        </div>

        {/* Name + description */}
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
            <span style={{ fontWeight: 600, fontSize: 14 }}>{summary.connector_name}</span>
            <StatusPill enabled={summary.enabled} />
            <HealthBadge health={health} />
          </div>
          <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 2, lineHeight: 1.4 }}>
            {summary.description}
          </div>
        </div>

        {/* Actions */}
        <div style={{ display: "flex", gap: 6, flexShrink: 0 }} onClick={e => e.stopPropagation()}>
          <button
            className="btn btn-secondary btn-sm"
            onClick={handleHealthCheck}
            disabled={checking}
            title={t("integrations.card.check")}
          >
            {checking ? "…" : t("integrations.card.check")}
          </button>
          <button
            className="btn btn-sm"
            style={{
              background: summary.enabled ? "rgba(239,68,68,0.1)" : "rgba(34,197,94,0.1)",
              color:      summary.enabled ? "var(--red, #ef4444)" : "var(--green, #22c55e)",
              border:     `1px solid ${summary.enabled ? "rgba(239,68,68,0.3)" : "rgba(34,197,94,0.3)"}`,
            }}
            onClick={handleToggle}
            disabled={toggling}
          >
            {toggling ? "…" : summary.enabled ? t("integrations.card.disable") : t("integrations.card.enable")}
          </button>
          <span style={{ fontSize: 12, color: "var(--text-2)", alignSelf: "center", paddingLeft: 4 }}>
            {expanded ? "▲" : "▼"}
          </span>
        </div>
      </div>

      {/* Expanded detail */}
      {expanded && (
        <div style={{ marginTop: 16, borderTop: "1px solid var(--border)", paddingTop: 16 }}>
          {/* Health detail */}
          {detail && (
            <div style={{ marginBottom: 14 }}>
              <div style={{ fontSize: 12, marginBottom: 8, color: "var(--text-2)" }}>
                <span style={{ color: detail.health === "ok" ? "var(--green, #22c55e)" : "var(--red, #ef4444)", fontWeight: 600 }}>
                  {detail.health === "ok" ? "✓ " : "✗ "}
                  {detail.health === "ok" ? t("integrations.health.connected_ok") : t("integrations.health.connected_fail")}
                </span>
                {detail.last_check_message && (
                  <span style={{ marginLeft: 8, fontFamily: "monospace", fontSize: 11, color: "var(--text-3)" }}>
                    {detail.last_check_message}
                  </span>
                )}
              </div>
              <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 6 }}>
                {t("integrations.card.last_check")}
              </div>
              <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                <span style={{ fontWeight: 500 }}>{t("integrations.card.last_check_relative")}:</span>{" "}
                {fmtRelative(detail.last_check_at, lang)}
              </div>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>
                {fmtTs(detail.last_check_at)}
              </div>
            </div>
          )}

          {/* Supported actions */}
          {actions && actions.length > 0 && (
            <div style={{ marginBottom: 14 }}>
              <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 6 }}>
                {t("integrations.card.supported")}
              </div>
              <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
                {actions.map(a => (
                  <span key={a} style={{
                    fontSize: 11, padding: "3px 9px", borderRadius: 20,
                    background: "var(--accent-muted, rgba(79,107,255,0.08))",
                    color: "var(--accent)", border: "1px solid var(--accent-border, rgba(79,107,255,0.2))",
                  }}>
                    {a}
                  </span>
                ))}
              </div>
            </div>
          )}

          {/* Config form */}
          <ConfigForm
            connectorId={summary.connector_id}
            currentConfig={config}
            onSaved={async () => {
              try {
                const st = await getIntegration(summary.connector_id);
                setDetail(st);
                setConfig(st.config_summary || null);
              } catch { /* best-effort */ }
              if (onAction) await onAction(summary.connector_id, "config");
            }}
          />
        </div>
      )}
    </div>
  );
}

// ── Page ──────────────────────────────────────────────────────────────────────

export default function IntegrationsPage() {
  const { t } = useLang();
  const [connectors, setConnectors] = useState([]);
  const [loading,    setLoading]    = useState(true);
  const [error,      setError]      = useState(null);

  const load = useCallback(async () => {
    setLoading(true); setError(null);
    try {
      const data = await listIntegrations();
      setConnectors(Array.isArray(data) ? data : []);
    } catch (e) {
      setError(e.message || t("integrations.error.load"));
    } finally {
      setLoading(false);
    }
  }, [t]);

  useEffect(() => { load(); }, [load]);

  const handleAction = useCallback(async (connectorId, type) => {
    if (type === "toggle" || type === "health" || type === "config") await load();
  }, [load]);

  const enabledCount = connectors.filter(c => c.enabled).length;
  const healthyCount = connectors.filter(c => c.health === "ok").length;

  const kpiItems = [
    { labelKey: "integrations.kpi.connectors", value: connectors.length },
    { labelKey: "integrations.kpi.enabled",    value: enabledCount,  color: "var(--green, #22c55e)" },
    { labelKey: "integrations.kpi.healthy",    value: healthyCount,  color: "var(--green, #22c55e)" },
  ];

  return (
    <div className="page-wrap">
      {/* Hero */}
      <div style={{ marginBottom: 24 }}>
        <h1 style={{ fontSize: 22, fontWeight: 600, margin: 0 }}>{t("integrations.hero.title")}</h1>
        <p style={{ fontSize: 13, color: "var(--text-2)", margin: "4px 0 0" }}>
          {t("integrations.hero.subtitle")}
        </p>
      </div>

      {/* KPI strip */}
      <div style={{ display: "flex", gap: 12, marginBottom: 24, flexWrap: "wrap" }}>
        {kpiItems.map(k => (
          <div key={k.labelKey} className="kpi-card" style={{ minWidth: 110 }}>
            <div className="kpi-value" style={k.color ? { color: k.color } : {}}>{loading ? "…" : k.value}</div>
            <div className="kpi-label">{t(k.labelKey)}</div>
          </div>
        ))}
      </div>

      {/* Error */}
      {error && (
        <div className="alert alert-error" style={{ marginBottom: 16 }}>{error}</div>
      )}

      {/* Loading */}
      {loading && (
        <div style={{ fontSize: 13, color: "var(--text-2)", padding: 24, textAlign: "center" }}>
          {t("integrations.loading")}
        </div>
      )}

      {/* Connector cards */}
      {!loading && !error && connectors.length === 0 && (
        <div className="card" style={{ padding: "48px 32px", textAlign: "center" }}>
          <div style={{ fontSize: 36, marginBottom: 12 }}>◇</div>
          <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)", marginBottom: 8 }}>
            {t("integrations.empty.title")}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.7, maxWidth: 400, margin: "0 auto" }}>
            {t("integrations.empty.desc")}
          </div>
        </div>
      )}

      {!loading && connectors.map(c => (
        <ConnectorCard
          key={c.connector_id}
          summary={c}
          onAction={handleAction}
        />
      ))}
    </div>
  );
}
