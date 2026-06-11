// src/pages/IntegrationsPage.jsx
import React, { useCallback, useEffect, useRef, useState } from "react";
import {
  listIntegrations,
  getIntegration,
  runHealthCheck,
  enableIntegration,
  disableIntegration,
  updateIntegrationConfig,
  getIntegrationActions,
  getProjectGitHubInstallUrl,
  connectProjectGitHubApp,
  listProjectGitHubRepositories,
  selectProjectGitHubRepository,
  getProjectGitHubStatus,
  disconnectProjectGitHub,
  getProjectAzureDevOpsAuthorizeUrl,
  getProjectAzureDevOpsStatus,
  disconnectProjectAzureDevOps,
  listProjectAzureDevOpsOrganizations,
  listProjectAzureDevOpsProjects,
  listProjectAzureDevOpsRepositories,
  selectProjectAzureDevOpsTarget,
  getJiraStatus,
  getQMetryStatus,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import JiraIntegrationPanel from "../components/integrations/JiraIntegrationPanel.jsx";
import QMetryIntegrationPanel from "../components/integrations/QMetryIntegrationPanel.jsx";
import { deriveJiraHeaderState } from "../utils/jiraViewUtils.js";
import { deriveQMetryHeaderState } from "../utils/qmetryViewUtils.js";

const CONNECTOR_DISPLAY_ORDER = ["github", "azure_devops", "jira", "qmetry"];

function sortConnectors(connectors) {
  return [...connectors].sort((a, b) => {
    const ia = CONNECTOR_DISPLAY_ORDER.indexOf(a.connector_id);
    const ib = CONNECTOR_DISPLAY_ORDER.indexOf(b.connector_id);
    const ra = ia === -1 ? 100 : ia;
    const rb = ib === -1 ? 100 : ib;
    if (ra !== rb) return ra - rb;
    return a.connector_id.localeCompare(b.connector_id);
  });
}

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
  jira:         "◈",
  github:       "◎",
  azure_devops: "⬡",
  slack:        "✦",
  qmetry:       "⊞",
};

function HealthBadge({ health, labelOverride }) {
  const { t } = useLang();
  const color = HEALTH_COLOR[health] || HEALTH_COLOR.unknown;
  const label = labelOverride || (HEALTH_KEY[health] ? t(HEALTH_KEY[health]) : (health || t("integrations.health.unknown")));
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

/** Single source of truth for GitHub App header badges (card + panel). */
function deriveGitHubHeaderState(status) {
  if (!status?.installation_id) {
    return { enabled: false, health: "unconfigured", labelKey: "integrations.health.unconfigured" };
  }
  if (status.connected && status.validation_ok) {
    return { enabled: true, health: "ok", labelKey: "integrations.github.header_healthy" };
  }
  return { enabled: true, health: "degraded", labelKey: "integrations.github.header_connected" };
}

function deriveAzureHeaderState(status) {
  if (!status?.enabled || status?.provider === "none") {
    return { enabled: false, health: "unconfigured", labelKey: "integrations.health.unconfigured" };
  }
  if (status.connected && status.validation_ok) {
    return { enabled: true, health: "ok", labelKey: "integrations.azure.header_healthy" };
  }
  return { enabled: true, health: "degraded", labelKey: "integrations.azure.header_connected" };
}

/** Align KPI counts with SCM card badges (project OAuth), not legacy registry flags. */
function effectiveConnectorStatus(summary, ghStatus, azStatus, jiraStatus, qmetryStatus) {
  if (summary.connector_id === "github") {
    const st = deriveGitHubHeaderState(ghStatus);
    return { enabled: st.enabled, health: st.health };
  }
  if (summary.connector_id === "azure_devops") {
    const st = deriveAzureHeaderState(azStatus);
    return { enabled: st.enabled, health: st.health };
  }
  if (summary.connector_id === "jira") {
    const st = deriveJiraHeaderState(jiraStatus);
    return { enabled: Boolean(summary.enabled) || st.enabled, health: st.health };
  }
  if (summary.connector_id === "qmetry") {
    const st = deriveQMetryHeaderState(qmetryStatus);
    return { enabled: Boolean(summary.enabled) || st.enabled, health: st.health };
  }
  return { enabled: Boolean(summary.enabled), health: summary.health || "unknown" };
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

// ── GitHub App (project-scoped) ───────────────────────────────────────────────

function ActionsMenu({ items, disabled }) {
  const [open, setOpen] = useState(false);
  const wrapRef = useRef(null);

  useEffect(() => {
    if (!open) return undefined;
    function onDoc(e) {
      if (wrapRef.current && !wrapRef.current.contains(e.target)) setOpen(false);
    }
    document.addEventListener("mousedown", onDoc);
    return () => document.removeEventListener("mousedown", onDoc);
  }, [open]);

  if (!items.length) return null;

  return (
    <div ref={wrapRef} style={{ position: "relative" }}>
      <button
        type="button"
        className="btn btn-secondary btn-sm"
        onClick={() => setOpen((v) => !v)}
        disabled={disabled}
        aria-haspopup="menu"
        aria-expanded={open}
        title="Actions"
      >
        ⋮
      </button>
      {open ? (
        <div
          role="menu"
          style={{
            position: "absolute",
            right: 0,
            top: "calc(100% + 4px)",
            minWidth: 200,
            background: "var(--surface, #fff)",
            border: "1px solid var(--border)",
            borderRadius: 8,
            boxShadow: "0 8px 24px rgba(0,0,0,0.12)",
            zIndex: 20,
            padding: "4px 0",
          }}
        >
          {items.map((item, idx) => (
            item.separator ? (
              <div key={`sep-${idx}`} style={{ height: 1, background: "var(--border)", margin: "4px 0" }} />
            ) : (
              <button
                key={item.key}
                type="button"
                role="menuitem"
                className="btn"
                style={{
                  display: "block",
                  width: "100%",
                  textAlign: "left",
                  background: "transparent",
                  border: "none",
                  borderRadius: 0,
                  padding: "8px 14px",
                  fontSize: 13,
                  color: item.danger ? "var(--red, #ef4444)" : "var(--text-1)",
                  fontWeight: item.danger ? 500 : 400,
                }}
                disabled={item.disabled}
                onClick={() => {
                  setOpen(false);
                  if (item.onClick) item.onClick();
                }}
              >
                {item.label}
              </button>
            )
          ))}
        </div>
      ) : null}
    </div>
  );
}

function GitHubDisconnectDialog({ open, busy, onConfirm, onCancel }) {
  const { t } = useLang();
  if (!open) return null;

  return (
    <div
      style={{
        position: "fixed",
        inset: 0,
        background: "rgba(0,0,0,0.45)",
        zIndex: 10040,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        padding: 16,
      }}
      role="dialog"
      aria-modal="true"
      aria-labelledby="gh-disconnect-title"
    >
      <div className="card" style={{ width: "min(520px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
          <div id="gh-disconnect-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {t("integrations.github.disconnect_title")}
          </div>
          <p style={{ fontSize: 13, color: "var(--text-2)", margin: "10px 0 0", lineHeight: 1.55 }}>
            {t("integrations.github.disconnect_intro")}
          </p>
          <ul style={{ fontSize: 13, color: "var(--text-2)", margin: "12px 0 0", paddingLeft: 0, listStyle: "none", lineHeight: 1.7 }}>
            <li>✓ {t("integrations.github.disconnect_item_installation")}</li>
            <li>✓ {t("integrations.github.disconnect_item_repo")}</li>
            <li>✓ {t("integrations.github.disconnect_item_state")}</li>
          </ul>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "12px 0 0", lineHeight: 1.55 }}>
            {t("integrations.github.disconnect_footer")}
          </p>
        </div>
        <div style={{ padding: "14px 20px", display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={onCancel} disabled={busy}>
            {t("common.cancel")}
          </button>
          <button
            type="button"
            className="btn btn-sm btn-primary"
            style={{ background: "var(--red)", borderColor: "var(--red)", color: "#fff" }}
            onClick={onConfirm}
            disabled={busy}
          >
            {busy ? t("common.working") : t("integrations.github.disconnect_confirm")}
          </button>
        </div>
      </div>
    </div>
  );
}

function AzureDevOpsPanel({ onStatusChange }) {
  const { t } = useLang();
  const { currentProject } = useProject();
  const projectId = currentProject?.id || "";

  const [status, setStatus] = useState(null);
  const [loading, setLoading] = useState(false);
  const [connecting, setConnecting] = useState(false);
  const [verifying, setVerifying] = useState(false);
  const [disconnecting, setDisconnecting] = useState(false);
  const [disconnectOpen, setDisconnectOpen] = useState(false);
  const [error, setError] = useState("");

  const [orgs, setOrgs] = useState([]);
  const [projects, setProjects] = useState([]);
  const [repos, setRepos] = useState([]);
  const [selectedOrg, setSelectedOrg] = useState("");
  const [selectedProject, setSelectedProject] = useState("");
  const [selectedRepoId, setSelectedRepoId] = useState("");
  const [selecting, setSelecting] = useState(false);
  const oauthHandled = useRef(false);

  const refreshStatus = useCallback(async (validate = false) => {
    if (!projectId) {
      setStatus(null);
      if (onStatusChange) onStatusChange(null);
      return;
    }
    setLoading(true);
    try {
      const st = await getProjectAzureDevOpsStatus(projectId, validate);
      setStatus(st);
      if (onStatusChange) onStatusChange(st);
    } catch (e) {
      setError(e?.message || t("az.error.status"));
      setStatus(null);
      if (onStatusChange) onStatusChange(null);
    } finally {
      setLoading(false);
    }
  }, [projectId, onStatusChange, t]);

  useEffect(() => {
    refreshStatus(false);
  }, [refreshStatus]);

  useEffect(() => {
    if (!projectId || oauthHandled.current) return;
    const params = new URLSearchParams(window.location.search);
    const oauth = params.get("azure_oauth");
    const pid = (params.get("project_id") || "").trim().toLowerCase();
    if (!oauth || pid !== projectId.toLowerCase()) return;
    oauthHandled.current = true;
    if (oauth === "1") {
      refreshStatus(true);
    } else if (oauth === "0") {
      setError(params.get("msg") || t("az.error.connect"));
    }
    window.history.replaceState({}, "", window.location.pathname);
  }, [projectId, refreshStatus, t]);

  useEffect(() => {
    if (!projectId || !status?.enabled || status?.connected) return;
    (async () => {
      try {
        const data = await listProjectAzureDevOpsOrganizations(projectId);
        setOrgs(data.organizations || []);
      } catch { /* best-effort */ }
    })();
  }, [projectId, status?.enabled, status?.connected]);

  useEffect(() => {
    if (!projectId || !selectedOrg) {
      setProjects([]);
      return;
    }
    (async () => {
      try {
        const data = await listProjectAzureDevOpsProjects(projectId, selectedOrg);
        setProjects(data.projects || []);
      } catch (e) {
        setError(e?.message || t("az.error.projects"));
      }
    })();
  }, [projectId, selectedOrg, t]);

  useEffect(() => {
    if (!projectId || !selectedOrg || !selectedProject) {
      setRepos([]);
      return;
    }
    (async () => {
      try {
        const data = await listProjectAzureDevOpsRepositories(projectId, selectedOrg, selectedProject);
        setRepos(data.repositories || []);
      } catch (e) {
        setError(e?.message || t("az.error.repos"));
      }
    })();
  }, [projectId, selectedOrg, selectedProject, t]);

  async function handleConnect() {
    if (!projectId) return;
    setError("");
    setConnecting(true);
    try {
      const { authorize_url: url } = await getProjectAzureDevOpsAuthorizeUrl(projectId);
      if (url) window.location.href = url;
    } catch (e) {
      setError(e?.message || t("az.error.authorize"));
    } finally {
      setConnecting(false);
    }
  }

  async function handleVerify() {
    setVerifying(true);
    setError("");
    try {
      await refreshStatus(true);
    } finally {
      setVerifying(false);
    }
  }

  async function handleSelectTarget() {
    if (!projectId || !selectedOrg || !selectedProject || !selectedRepoId) return;
    const meta = repos.find((r) => r.id === selectedRepoId);
    setSelecting(true);
    setError("");
    try {
      await selectProjectAzureDevOpsTarget(projectId, {
        organization: selectedOrg,
        azure_project: selectedProject,
        repository_id: selectedRepoId,
        repository_name: meta?.name || "",
        default_branch: meta?.default_branch || "main",
      });
      await refreshStatus(true);
    } catch (e) {
      setError(e?.message || t("az.error.select"));
    } finally {
      setSelecting(false);
    }
  }

  async function handleDisconnect() {
    if (!projectId) return;
    setDisconnecting(true);
    setError("");
    try {
      await disconnectProjectAzureDevOps(projectId);
      setOrgs([]);
      setProjects([]);
      setRepos([]);
      setSelectedOrg("");
      setSelectedProject("");
      setSelectedRepoId("");
      await refreshStatus(false);
      setDisconnectOpen(false);
    } catch (e) {
      setError(e?.message || t("integrations.azure.disconnect_error"));
    } finally {
      setDisconnecting(false);
    }
  }

  const showTargetPicker = Boolean(status?.enabled && !status?.connected);

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 10 }}>
        {t("integrations.azure.panel_title")}
      </div>
      <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, lineHeight: 1.5 }}>
        {t("az.subtitle")}
      </p>

      {!projectId ? (
        <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("integrations.azure.need_project")}</p>
      ) : (
        <>
          {loading || connecting ? (
            <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12 }}>{t("az.loading")}</p>
          ) : null}

          {status && !status.oauth_configured ? (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
              {t("az.oauth_not_configured")}
            </div>
          ) : null}

          {status?.enabled ? (
            <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
              {status.organization ? (
                <div><span style={{ color: "var(--text-3)" }}>{t("az.organization")}:</span> {status.organization}</div>
              ) : null}
              {status.azure_project ? (
                <div><span style={{ color: "var(--text-3)" }}>{t("az.project")}:</span> {status.azure_project}</div>
              ) : null}
              {status.full_name && status.connected ? (
                <div><span style={{ color: "var(--text-3)" }}>{t("integrations.azure.repo")}:</span> {status.full_name}</div>
              ) : null}
              {status.validation_message ? (
                <div style={{ marginTop: 6 }}>
                  {status.validation_ok ? (
                    <span style={{ color: "var(--green, #22c55e)" }}>✓ {status.validation_message}</span>
                  ) : (
                    <span style={{ color: "var(--red, #ef4444)" }}>✗ {status.validation_message}</span>
                  )}
                </div>
              ) : null}
            </div>
          ) : null}

          {error ? (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>{error}</div>
          ) : null}

          {!status?.enabled ? (
            <div>
              <button
                type="button"
                className="btn btn-primary btn-sm"
                onClick={handleConnect}
                disabled={!status?.oauth_configured || connecting}
              >
                {connecting ? t("az.connecting") : t("az.connect")}
              </button>
              <p style={{ fontSize: 11, color: "var(--text-3)", marginTop: 8 }}>{t("integrations.azure.oauth_hint")}</p>
            </div>
          ) : null}

          {showTargetPicker ? (
            <div style={{ marginTop: 12 }}>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("az.select_target")}</div>
              <div style={{ display: "flex", flexDirection: "column", gap: 8, maxWidth: 420 }}>
                <select className="input" value={selectedOrg} onChange={(e) => { setSelectedOrg(e.target.value); setSelectedProject(""); setSelectedRepoId(""); }}>
                  <option value="">{t("az.select_org")}</option>
                  {orgs.map((o) => (
                    <option key={o.account_id || o.account_name} value={o.account_name}>{o.account_name}</option>
                  ))}
                </select>
                <select className="input" value={selectedProject} onChange={(e) => { setSelectedProject(e.target.value); setSelectedRepoId(""); }} disabled={!selectedOrg}>
                  <option value="">{t("az.select_project")}</option>
                  {projects.map((p) => (
                    <option key={p.id || p.name} value={p.name}>{p.name}</option>
                  ))}
                </select>
                <select className="input" value={selectedRepoId} onChange={(e) => setSelectedRepoId(e.target.value)} disabled={!selectedProject}>
                  <option value="">{t("az.select_repo")}</option>
                  {repos.map((r) => (
                    <option key={r.id} value={r.id}>{r.name}</option>
                  ))}
                </select>
                <button type="button" className="btn btn-primary btn-sm" onClick={handleSelectTarget} disabled={selecting || !selectedRepoId}>
                  {selecting ? t("az.selecting") : t("az.select_target_btn")}
                </button>
              </div>
            </div>
          ) : null}

          {status?.connected ? (
            <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginTop: 12 }}>
              <button type="button" className="btn btn-secondary btn-sm" onClick={handleVerify} disabled={verifying}>
                {verifying ? t("common.working") : t("integrations.azure.verify")}
              </button>
              <button type="button" className="btn btn-secondary btn-sm" onClick={handleConnect} disabled={connecting}>
                {t("integrations.azure.reconnect")}
              </button>
              <button type="button" className="btn btn-sm" style={{ color: "var(--red)" }} onClick={() => setDisconnectOpen(true)}>
                {t("integrations.azure.disconnect")}
              </button>
            </div>
          ) : status?.enabled ? (
            <div style={{ display: "flex", gap: 8, marginTop: 12 }}>
              <button type="button" className="btn btn-sm" style={{ color: "var(--red)" }} onClick={() => setDisconnectOpen(true)}>
                {t("integrations.azure.disconnect")}
              </button>
            </div>
          ) : null}
        </>
      )}

      {disconnectOpen ? (
        <GitHubDisconnectDialog
          open={disconnectOpen}
          busy={disconnecting}
          onConfirm={handleDisconnect}
          onCancel={() => setDisconnectOpen(false)}
        />
      ) : null}
    </div>
  );
}

function GitHubAppPanel({ onStatusChange }) {
  const { t } = useLang();
  const { currentProject } = useProject();
  const projectId = currentProject?.id || "";

  const [status, setStatus] = useState(null);
  const [loading, setLoading] = useState(false);
  const [connecting, setConnecting] = useState(false);
  const [verifying, setVerifying] = useState(false);
  const [error, setError] = useState("");
  const [repos, setRepos] = useState([]);
  const [reposLoading, setReposLoading] = useState(false);
  const [selectedRepo, setSelectedRepo] = useState("");
  const [selectingRepo, setSelectingRepo] = useState(false);
  const [changingRepo, setChangingRepo] = useState(false);
  const [disconnectOpen, setDisconnectOpen] = useState(false);
  const [disconnecting, setDisconnecting] = useState(false);
  const callbackHandled = useRef(false);

  const loadRepos = useCallback(async () => {
    if (!projectId) return;
    setReposLoading(true);
    try {
      const data = await listProjectGitHubRepositories(projectId);
      setRepos(data.repositories || []);
    } catch (e) {
      setError(e?.message || t("gh.error.status"));
      setRepos([]);
    } finally {
      setReposLoading(false);
    }
  }, [projectId, t]);

  const refreshStatus = useCallback(async (validate = false) => {
    if (!projectId) {
      setStatus(null);
      return null;
    }
    setLoading(true);
    setError("");
    try {
      const st = await getProjectGitHubStatus(projectId, validate);
      setStatus(st);
      if (st?.full_name) setSelectedRepo(st.full_name);
      if (onStatusChange) onStatusChange(st);
      return st;
    } catch (e) {
      setError(e?.message || t("gh.error.status"));
      setStatus(null);
      return null;
    } finally {
      setLoading(false);
    }
  }, [projectId, t, onStatusChange]);

  useEffect(() => {
    refreshStatus(false);
  }, [refreshStatus]);

  useEffect(() => {
    if (!projectId || callbackHandled.current) return;

    const params = new URLSearchParams(window.location.search);
    const iid = (params.get("installation_id") || params.get("github_installation_id") || "").trim();
    const stateProject = (params.get("state") || "").trim().toLowerCase();
    if (!iid) return;
    if (stateProject && stateProject !== projectId.toLowerCase()) return;

    callbackHandled.current = true;
    setConnecting(true);
    setError("");

    (async () => {
      try {
        await connectProjectGitHubApp(projectId, { installation_id: iid });
        await refreshStatus(true);
        if (stateProject || iid) {
          window.history.replaceState({}, "", window.location.pathname);
        }
      } catch (e) {
        setError(e?.message || t("gh.error.connect"));
      } finally {
        setConnecting(false);
      }
    })();
  }, [projectId, refreshStatus, t]);

  useEffect(() => {
    if (!projectId || !status?.installation_id) return;
    const needsRepo = !status.full_name && !changingRepo;
    if (needsRepo || changingRepo) loadRepos();
  }, [projectId, status?.installation_id, status?.full_name, changingRepo, loadRepos]);

  async function handleConnect() {
    if (!projectId) return;
    setError("");
    try {
      const { install_url: url } = await getProjectGitHubInstallUrl(projectId);
      if (url) window.location.href = url;
    } catch (e) {
      setError(e?.message || t("gh.error.install"));
    }
  }

  async function handleVerify() {
    if (!projectId) return;
    setVerifying(true);
    setError("");
    try {
      await refreshStatus(true);
    } finally {
      setVerifying(false);
    }
  }

  async function handleSelectRepo() {
    if (!projectId || !selectedRepo) return;
    const [owner, repo] = selectedRepo.split("/");
    if (!owner || !repo) return;
    setSelectingRepo(true);
    setError("");
    try {
      const meta = repos.find((r) => r.full_name === selectedRepo);
      await selectProjectGitHubRepository(projectId, {
        owner,
        repo,
        default_branch: meta?.default_branch || "main",
      });
      setChangingRepo(false);
      await refreshStatus(true);
    } catch (e) {
      setError(e?.message || t("gh.error.select_repo"));
    } finally {
      setSelectingRepo(false);
    }
  }

  async function handleDisconnect() {
    if (!projectId) return;
    setDisconnecting(true);
    setError("");
    try {
      await disconnectProjectGitHub(projectId);
      setChangingRepo(false);
      setSelectedRepo("");
      setRepos([]);
      await refreshStatus(false);
      setDisconnectOpen(false);
    } catch (e) {
      setError(e?.message || t("integrations.github.disconnect_error"));
    } finally {
      setDisconnecting(false);
    }
  }

  const showRepoPicker = Boolean(
    status?.installation_id && (!status.full_name || changingRepo),
  );
  const accountLabel = status?.owner || status?.connected_by || "";
  const showActionsMenu = Boolean(status?.installation_id);

  const menuItems = [];
  if (status?.installation_id && status.full_name && !changingRepo) {
    menuItems.push(
      { key: "verify", label: t("integrations.github.verify"), onClick: handleVerify, disabled: verifying },
      { key: "change", label: t("integrations.github.change_repo"), onClick: () => { setChangingRepo(true); setSelectedRepo(""); } },
      { key: "reconnect", label: t("integrations.github.reconnect"), onClick: handleConnect, disabled: connecting },
      { separator: true },
      { key: "disconnect", label: t("integrations.github.disconnect"), onClick: () => setDisconnectOpen(true), danger: true },
    );
  } else if (status?.installation_id) {
    menuItems.push(
      { key: "reconnect", label: t("integrations.github.reconnect"), onClick: handleConnect, disabled: connecting },
      { separator: true },
      { key: "disconnect", label: t("integrations.github.disconnect"), onClick: () => setDisconnectOpen(true), danger: true },
    );
  }

  return (
    <div style={{ marginTop: 16 }}>
      <div style={{ fontSize: 11, fontWeight: 500, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-2)", marginBottom: 10 }}>
        {t("integrations.github.panel_title")}
      </div>
      <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, lineHeight: 1.5 }}>
        {t("gh.subtitle")}
      </p>

      {!projectId ? (
        <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("integrations.github.need_project")}</p>
      ) : (
        <>
          {loading || connecting ? (
            <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12 }}>{t("gh.loading")}</p>
          ) : null}

          {status?.needs_migration ? (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
              {t("gh.needs_migration")}
            </div>
          ) : null}

          {status && !status.app_configured ? (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
              {t("gh.app_not_configured")}
            </div>
          ) : null}

          {status?.installation_id ? (
            <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
              {status.installation_id ? (
                <div><span style={{ color: "var(--text-3)" }}>{t("gh.installation_id")}:</span>{" "}
                  <span style={{ fontFamily: "ui-monospace, monospace" }}>{status.installation_id}</span>
                </div>
              ) : null}
              {accountLabel ? (
                <div><span style={{ color: "var(--text-3)" }}>{t("integrations.github.account")}:</span> {accountLabel}</div>
              ) : null}
              {status.full_name && !changingRepo ? (
                <div><span style={{ color: "var(--text-3)" }}>{t("integrations.github.repo")}:</span> {status.full_name}</div>
              ) : null}
              {status.default_branch && status.full_name && !changingRepo ? (
                <div><span style={{ color: "var(--text-3)" }}>{t("gh.branch")}:</span> {status.default_branch}</div>
              ) : null}
              {status.validation_message ? (
                <div style={{ marginTop: 6 }}>
                  {status.validation_ok ? (
                    <span style={{ color: "var(--green, #22c55e)" }}>✓ {status.validation_message}</span>
                  ) : (
                    <span style={{ color: "var(--red, #ef4444)" }}>✗ {status.validation_message}</span>
                  )}
                </div>
              ) : null}
            </div>
          ) : null}

          {error ? (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>{error}</div>
          ) : null}

          {!status?.installation_id ? (
            <div>
              <button
                type="button"
                className="btn btn-primary btn-sm"
                onClick={handleConnect}
                disabled={!status?.app_configured || connecting}
              >
                {connecting ? t("gh.connecting") : t("gh.connect_github")}
              </button>
              <p style={{ fontSize: 11, color: "var(--text-3)", marginTop: 8 }}>{t("integrations.github.install_hint")}</p>
            </div>
          ) : null}

          {showRepoPicker ? (
            <div style={{ marginTop: 12 }}>
              <p style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 8 }}>
                {t("integrations.github.select_repo_prompt")}
              </p>
              {reposLoading ? (
                <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("gh.loading_repos")}</p>
              ) : (
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
                  <select
                    className="input"
                    style={{ flex: 1, minWidth: 220 }}
                    value={selectedRepo}
                    onChange={(e) => setSelectedRepo(e.target.value)}
                  >
                    <option value="">{t("gh.select_repo_placeholder")}</option>
                    {repos.map((r) => (
                      <option key={r.full_name} value={r.full_name}>{r.full_name}</option>
                    ))}
                  </select>
                  <button
                    type="button"
                    className="btn btn-primary btn-sm"
                    onClick={handleSelectRepo}
                    disabled={!selectedRepo || selectingRepo}
                  >
                    {selectingRepo ? t("gh.selecting_repo") : t("integrations.github.select_repo_btn")}
                  </button>
                  {changingRepo ? (
                    <button
                      type="button"
                      className="btn btn-secondary btn-sm"
                      onClick={() => { setChangingRepo(false); setSelectedRepo(status?.full_name || ""); }}
                    >
                      {t("common.cancel")}
                    </button>
                  ) : null}
                </div>
              )}
            </div>
          ) : null}

          {showActionsMenu ? (
            <div style={{ display: "flex", justifyContent: "flex-end", marginTop: 12 }}>
              <ActionsMenu items={menuItems} disabled={disconnecting || connecting} />
            </div>
          ) : null}

          <GitHubDisconnectDialog
            open={disconnectOpen}
            busy={disconnecting}
            onConfirm={handleDisconnect}
            onCancel={() => setDisconnectOpen(false)}
          />
        </>
      )}
    </div>
  );
}

// ── Connector card ────────────────────────────────────────────────────────────

function ConnectorCard({ summary, onAction, jiraStatus, onJiraStatusChange, qmetryStatus, onQMetryStatusChange }) {
  const { t, lang } = useLang();
  const { currentProject } = useProject();
  const isGitHub = summary.connector_id === "github";
  const isAzureDevOps = summary.connector_id === "azure_devops";
  const isJira = summary.connector_id === "jira";
  const isQmetry = summary.connector_id === "qmetry";
  const isScmPanel = isGitHub || isAzureDevOps;
  const [expanded,  setExpanded]  = useState(false);
  const [detail,    setDetail]    = useState(null);
  const [actions,   setActions]   = useState(null);
  const [checking,  setChecking]  = useState(false);
  const [toggling,  setToggling]  = useState(false);
  const [config,    setConfig]    = useState(null);
  const [ghStatus,  setGhStatus]  = useState(null);
  const [azStatus,  setAzStatus]  = useState(null);
  const [jiraRefreshToken, setJiraRefreshToken] = useState(0);
  const [qmetryRefreshToken, setQmetryRefreshToken] = useState(0);

  const loadGitHubHeaderStatus = useCallback(async () => {
    if (!isGitHub || !currentProject?.id) {
      setGhStatus(null);
      return;
    }
    try {
      const st = await getProjectGitHubStatus(currentProject.id, false);
      setGhStatus(st);
    } catch {
      setGhStatus(null);
    }
  }, [isGitHub, currentProject?.id]);

  const loadAzureHeaderStatus = useCallback(async () => {
    if (!isAzureDevOps || !currentProject?.id) {
      setAzStatus(null);
      return;
    }
    try {
      const st = await getProjectAzureDevOpsStatus(currentProject.id, false);
      setAzStatus(st);
    } catch {
      setAzStatus(null);
    }
  }, [isAzureDevOps, currentProject?.id]);

  useEffect(() => {
    loadGitHubHeaderStatus();
  }, [loadGitHubHeaderStatus]);

  useEffect(() => {
    loadAzureHeaderStatus();
  }, [loadAzureHeaderStatus]);

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
  const ghHeader = isGitHub ? deriveGitHubHeaderState(ghStatus) : null;
  const azHeader = isAzureDevOps ? deriveAzureHeaderState(azStatus) : null;
  const jiraHeader = isJira ? deriveJiraHeaderState(jiraStatus) : null;
  const qmetryHeader = isQmetry ? deriveQMetryHeaderState(qmetryStatus) : null;
  const scmHeader = ghHeader || azHeader;
  const health = isScmPanel
    ? scmHeader.health
    : (isJira && jiraHeader
      ? jiraHeader.health
      : (isQmetry && qmetryHeader
        ? qmetryHeader.health
        : ((detail && detail.health) || summary.health)));
  const healthLabel = isScmPanel && scmHeader
    ? t(scmHeader.labelKey)
    : (isJira && jiraHeader
      ? t(jiraHeader.labelKey)
      : (isQmetry && qmetryHeader ? t(qmetryHeader.labelKey) : undefined));
  const enabled = isScmPanel ? Boolean(scmHeader?.enabled) : summary.enabled;

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
            <StatusPill enabled={enabled} />
            <HealthBadge health={health} labelOverride={healthLabel} />
          </div>
          <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 2, lineHeight: 1.4 }}>
            {summary.description}
          </div>
        </div>

        {/* Actions */}
        <div style={{ display: "flex", gap: 6, flexShrink: 0 }} onClick={e => e.stopPropagation()}>
          {summary.connector_id !== "github" && summary.connector_id !== "azure_devops" && (
            <>
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
            </>
          )}
          <span style={{ fontSize: 12, color: "var(--text-2)", alignSelf: "center", paddingLeft: 4 }}>
            {expanded ? "▲" : "▼"}
          </span>
        </div>
      </div>

      {/* Expanded detail */}
      {expanded && (
        <div style={{ marginTop: 16, borderTop: "1px solid var(--border)", paddingTop: 16 }}>
          {/* Health detail — legacy connectors only */}
          {!isScmPanel && detail && (
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

          {/* Supported actions — legacy connectors only */}
          {!isScmPanel && actions && actions.length > 0 && (
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

          {/* Config form — GitHub uses GitHub App panel instead of PAT */}
          {summary.connector_id === "github" ? (
            <GitHubAppPanel onStatusChange={setGhStatus} />
          ) : summary.connector_id === "azure_devops" ? (
            <AzureDevOpsPanel onStatusChange={setAzStatus} />
          ) : summary.connector_id === "jira" ? (
            <>
              <ConfigForm
                connectorId={summary.connector_id}
                currentConfig={config}
                onSaved={async () => {
                  try {
                    const st = await getIntegration(summary.connector_id);
                    setDetail(st);
                    setConfig(st.config_summary || null);
                  } catch { /* best-effort */ }
                  try {
                    const jst = await getJiraStatus();
                    if (onJiraStatusChange) onJiraStatusChange(jst);
                  } catch { /* best-effort */ }
                  setJiraRefreshToken((n) => n + 1);
                  if (onAction) await onAction(summary.connector_id, "config");
                }}
              />
              <JiraIntegrationPanel refreshToken={jiraRefreshToken} />
            </>
          ) : summary.connector_id === "qmetry" ? (
            <>
              <ConfigForm
                connectorId={summary.connector_id}
                currentConfig={config}
                onSaved={async () => {
                  try {
                    const st = await getIntegration(summary.connector_id);
                    setDetail(st);
                    setConfig(st.config_summary || null);
                  } catch { /* best-effort */ }
                  try {
                    const qst = await getQMetryStatus();
                    if (onQMetryStatusChange) onQMetryStatusChange(qst);
                  } catch { /* best-effort */ }
                  setQmetryRefreshToken((n) => n + 1);
                  if (onAction) await onAction(summary.connector_id, "config");
                }}
              />
              <QMetryIntegrationPanel refreshToken={qmetryRefreshToken} />
            </>
          ) : (
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
          )}
        </div>
      )}
    </div>
  );
}

// ── Page ──────────────────────────────────────────────────────────────────────

export default function IntegrationsPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const [connectors, setConnectors] = useState([]);
  const [loading,    setLoading]    = useState(true);
  const [error,      setError]      = useState(null);
  const [ghStatus,   setGhStatus]   = useState(null);
  const [azStatus,   setAzStatus]   = useState(null);
  const [jiraStatus, setJiraStatus] = useState(null);
  const [qmetryStatus, setQmetryStatus] = useState(null);

  const loadJiraHeaderStatus = useCallback(async () => {
    try {
      const st = await getJiraStatus();
      setJiraStatus(st);
    } catch {
      setJiraStatus(null);
    }
  }, []);

  const loadQMetryHeaderStatus = useCallback(async () => {
    try {
      const st = await getQMetryStatus();
      setQmetryStatus(st);
    } catch {
      setQmetryStatus(null);
    }
  }, []);

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

  useEffect(() => {
    loadJiraHeaderStatus();
  }, [loadJiraHeaderStatus]);

  useEffect(() => {
    loadQMetryHeaderStatus();
  }, [loadQMetryHeaderStatus]);

  useEffect(() => {
    const pid = currentProject?.id;
    if (!pid) {
      setGhStatus(null);
      setAzStatus(null);
      return;
    }
    let cancelled = false;
    (async () => {
      try {
        const [gh, az] = await Promise.all([
          getProjectGitHubStatus(pid, false).catch(() => null),
          getProjectAzureDevOpsStatus(pid, false).catch(() => null),
        ]);
        if (!cancelled) {
          setGhStatus(gh);
          setAzStatus(az);
        }
      } catch {
        if (!cancelled) {
          setGhStatus(null);
          setAzStatus(null);
        }
      }
    })();
    return () => { cancelled = true; };
  }, [currentProject?.id]);

  const handleAction = useCallback(async (connectorId, type) => {
    if (type === "toggle" || type === "health" || type === "config") await load();
    if (connectorId === "jira" && (type === "toggle" || type === "health" || type === "config")) {
      await loadJiraHeaderStatus();
    }
    if (connectorId === "qmetry" && (type === "toggle" || type === "health" || type === "config")) {
      await loadQMetryHeaderStatus();
    }
  }, [load, loadJiraHeaderStatus, loadQMetryHeaderStatus]);

  const enabledCount = connectors.filter((c) => effectiveConnectorStatus(c, ghStatus, azStatus, jiraStatus, qmetryStatus).enabled).length;
  const healthyCount = connectors.filter((c) => effectiveConnectorStatus(c, ghStatus, azStatus, jiraStatus, qmetryStatus).health === "ok").length;

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

      {!loading && sortConnectors(connectors).map(c => (
        <ConnectorCard
          key={c.connector_id}
          summary={c}
          onAction={handleAction}
          jiraStatus={jiraStatus}
          onJiraStatusChange={setJiraStatus}
          qmetryStatus={qmetryStatus}
          onQMetryStatusChange={setQmetryStatus}
        />
      ))}
    </div>
  );
}
