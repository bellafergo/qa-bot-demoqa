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

// ── Helpers ───────────────────────────────────────────────────────────────────

const HEALTH_COLOR = {
  ok:            "var(--green, #22c55e)",
  degraded:      "var(--yellow, #eab308)",
  unreachable:   "var(--red, #ef4444)",
  unconfigured:  "var(--text-2, #94a3b8)",
  unknown:       "var(--text-2, #94a3b8)",
};

const HEALTH_LABEL = {
  ok:            "Healthy",
  degraded:      "Degraded",
  unreachable:   "Unreachable",
  unconfigured:  "Not configured",
  unknown:       "Unknown",
};

const CONNECTOR_ICONS = {
  jira:    "◈",
  github:  "◎",
  slack:   "✦",
  qmetry:  "⊞",
};

function HealthBadge({ health }) {
  const color = HEALTH_COLOR[health] || HEALTH_COLOR.unknown;
  const label = HEALTH_LABEL[health] || health;
  return (
    <span style={{
      display: "inline-flex", alignItems: "center", gap: 5,
      fontSize: 11, fontWeight: 600,
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
  return (
    <span style={{
      fontSize: 10, fontWeight: 700,
      padding: "2px 7px", borderRadius: 20, textTransform: "uppercase",
      background: enabled ? "rgba(34,197,94,0.12)" : "rgba(148,163,184,0.12)",
      color:      enabled ? "var(--green, #22c55e)" : "var(--text-2, #94a3b8)",
      border: `1px solid ${enabled ? "rgba(34,197,94,0.3)" : "rgba(148,163,184,0.2)"}`,
    }}>
      {enabled ? "Enabled" : "Disabled"}
    </span>
  );
}

function fmtTs(ts) {
  if (!ts) return "—";
  try { return new Date(ts).toLocaleString(); } catch { return ts; }
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
    { key: "workspace",  label: "Workspace",   placeholder: "myworkspace",                  type: "text" },
    { key: "channel",    label: "Channel",      placeholder: "#qa-alerts",                  type: "text" },
    { key: "token",      label: "Bot Token",    placeholder: "xoxb-…",                      type: "password", secret: true },
  ],
  qmetry: [
    { key: "base_url",   label: "Base URL",     placeholder: "https://jira.example.com",    type: "text" },
    { key: "project_key",label: "Project Key",  placeholder: "QA",                          type: "text" },
    { key: "api_key",    label: "API Key",       placeholder: "QMetry API key",              type: "password", secret: true },
  ],
};

function ConfigForm({ connectorId, currentConfig, onSaved }) {
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

  const handleSave = async () => {
    setSaving(true); setError(null); setOk(false);
    try {
      const payload = {};
      fields.forEach(f => { if (values[f.key]) payload[f.key] = values[f.key]; });
      const cfg = await updateIntegrationConfig(connectorId, payload);
      setOk(true);
      // Clear secret fields after save
      const cleared = { ...values };
      fields.filter(f => f.secret).forEach(f => { cleared[f.key] = ""; });
      setValues(cleared);
      if (onSaved) onSaved(cfg);
    } catch (e) {
      setError(e.message);
    } finally {
      setSaving(false);
    }
  };

  if (!fields.length) return null;

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ fontSize: 11, fontWeight: 700, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 10 }}>
        Configuration
      </div>
      <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
        {fields.map(f => (
          <div key={f.key}>
            <div style={{ fontSize: 11, color: "var(--text-2)", marginBottom: 3 }}>
              {f.label}
              {f.secret && (
                <span style={{ marginLeft: 6, fontSize: 10, color: "var(--text-3, #64748b)", background: "rgba(148,163,184,0.1)", padding: "1px 5px", borderRadius: 4 }}>
                  write-only
                </span>
              )}
              {f.secret && currentConfig && currentConfig[`${f.key}_present`] && (
                <span style={{ marginLeft: 6, fontSize: 10, color: "var(--green, #22c55e)" }}>● set</span>
              )}
            </div>
            <input
              className="input"
              type={f.type}
              placeholder={f.secret ? "Leave blank to keep existing" : f.placeholder}
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
        <div style={{ marginTop: 8, fontSize: 12, color: "var(--green, #22c55e)" }}>Configuration saved. Secrets are not stored — only a presence flag is kept.</div>
      )}

      <button
        className="btn btn-primary btn-sm"
        style={{ marginTop: 10 }}
        onClick={handleSave}
        disabled={saving}
      >
        {saving ? "Saving…" : "Save Config"}
      </button>
    </div>
  );
}

// ── Connector card ────────────────────────────────────────────────────────────

function ConnectorCard({ summary, onAction }) {
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
      if (onAction) onAction(summary.connector_id, "health", status);
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
      if (onAction) onAction(summary.connector_id, "toggle");
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
            <span style={{ fontWeight: 700, fontSize: 14 }}>{summary.connector_name}</span>
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
            title="Run health check"
          >
            {checking ? "…" : "Check"}
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
            {toggling ? "…" : summary.enabled ? "Disable" : "Enable"}
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
              <div style={{ fontSize: 11, fontWeight: 700, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 6 }}>
                Last Health Check
              </div>
              <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                {fmtTs(detail.last_check_at)}
              </div>
              {detail.last_check_message && (
                <div style={{ fontSize: 12, marginTop: 4, padding: "6px 10px", background: "var(--surface-2, #f8fafc)", borderRadius: 6, fontFamily: "monospace", color: "var(--text)" }}>
                  {detail.last_check_message}
                </div>
              )}
            </div>
          )}

          {/* Supported actions */}
          {actions && actions.length > 0 && (
            <div style={{ marginBottom: 14 }}>
              <div style={{ fontSize: 11, fontWeight: 700, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 6 }}>
                Supported Actions
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
            onSaved={(newCfg) => {
              setConfig(newCfg);
              if (onAction) onAction(summary.connector_id, "config", newCfg);
            }}
          />
        </div>
      )}
    </div>
  );
}

// ── Page ──────────────────────────────────────────────────────────────────────

export default function IntegrationsPage() {
  const [connectors, setConnectors] = useState([]);
  const [loading,    setLoading]    = useState(true);
  const [error,      setError]      = useState(null);

  const load = useCallback(async () => {
    setLoading(true); setError(null);
    try {
      const data = await listIntegrations();
      setConnectors(Array.isArray(data) ? data : []);
    } catch (e) {
      setError(e.message || "Failed to load integrations");
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { load(); }, [load]);

  // Reload the full list after any toggle/config change so status pills update
  const handleAction = useCallback(async (connectorId, type) => {
    if (type === "toggle") await load();
  }, [load]);

  const enabledCount = connectors.filter(c => c.enabled).length;
  const healthyCount = connectors.filter(c => c.health === "ok").length;

  return (
    <div className="page-wrap">
      {/* Hero */}
      <div style={{ marginBottom: 24 }}>
        <h1 style={{ fontSize: 22, fontWeight: 800, margin: 0 }}>Integrations</h1>
        <p style={{ fontSize: 13, color: "var(--text-2)", margin: "4px 0 0" }}>
          Connect Vanya to your enterprise tools. Secrets are never stored or returned — only presence flags are kept.
        </p>
      </div>

      {/* KPI strip */}
      <div style={{ display: "flex", gap: 12, marginBottom: 24, flexWrap: "wrap" }}>
        {[
          { label: "Connectors",    value: connectors.length },
          { label: "Enabled",       value: enabledCount,  color: "var(--green, #22c55e)" },
          { label: "Healthy",       value: healthyCount,  color: "var(--green, #22c55e)" },
        ].map(k => (
          <div key={k.label} className="kpi-card" style={{ minWidth: 110 }}>
            <div className="kpi-value" style={k.color ? { color: k.color } : {}}>{loading ? "…" : k.value}</div>
            <div className="kpi-label">{k.label}</div>
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
          Loading connectors…
        </div>
      )}

      {/* Connector cards */}
      {!loading && !error && connectors.length === 0 && (
        <div style={{ fontSize: 13, color: "var(--text-2)", padding: 24, textAlign: "center" }}>
          No connectors available.
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
