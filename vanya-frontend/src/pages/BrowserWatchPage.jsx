// src/pages/BrowserWatchPage.jsx
/**
 * Browser Watch — monitoring, create/edit, evidence links (Phase 3G–3I).
 * List: GET /browser-inspections/watch
 * Create: POST /browser-inspections/watch
 * Update: PATCH /browser-inspections/watch/{id}
 * Run evidence: /evidence/run/:runId (inspection run id)
 */
import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useSearchParams } from "react-router-dom";
import {
  listBrowserInspectionWatches,
  createBrowserInspectionWatch,
  patchBrowserInspectionWatch,
  postBrowserWatchRunNow,
  postBrowserWatchBaselineUseLatest,
  listLocalAgents,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import { useToast } from "../context/ToastContext.jsx";
import { shouldClearWatchFieldErrorWhenSwitchingToCloud } from "../lib/browserWatchUi.js";
import BrowserWatchSummaryHeader from "../components/browser-watch/BrowserWatchSummaryHeader.jsx";
import BrowserWatchList from "../components/browser-watch/BrowserWatchList.jsx";
import BrowserWatchDetailPanel from "../components/browser-watch/BrowserWatchDetailPanel.jsx";
import {
  BROWSER_WATCH_I18N_KEYS,
  buildWatchSummaryViewModel,
} from "../utils/browserWatchViewUtils.js";

function agentHeartbeatHealth(agent) {
  if (!agent?.enabled || String(agent.status || "").toLowerCase() === "disabled") return "disabled";
  const iso = agent.last_seen_at;
  if (!iso) return "offline";
  const ms = Date.now() - new Date(iso).getTime();
  if (!Number.isFinite(ms)) return "offline";
  if (ms < 2 * 60 * 1000) return "online";
  if (ms < 10 * 60 * 1000) return "stale";
  return "offline";
}

function execModeBadgeLabel(em, t) {
  const v = String(em || "cloud").toLowerCase();
  if (v === "local_agent") return t("watch.mode.badge_local_agent");
  return t("watch.mode.badge_cloud");
}

export default function BrowserWatchPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const { showToast } = useToast();
  const [searchParams] = useSearchParams();
  const drilldownWatchId = String(searchParams.get("watch") || "").trim() || null;
  const [watches, setWatches] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [selectedId, setSelectedId] = useState(null);
  const [detailVersion, setDetailVersion] = useState(0);
  const [busy, setBusy] = useState(() => new Set());
  const [formOpen, setFormOpen] = useState(false);
  const [formMode, setFormMode] = useState("create");
  const [formEditWatch, setFormEditWatch] = useState(null);
  const selectedIdRef = useRef(null);
  const drilldownToastShownRef = useRef(null);

  useEffect(() => {
    selectedIdRef.current = selectedId;
  }, [selectedId]);

  const selectedWatch = useMemo(() => watches.find((w) => w.watch_id === selectedId) || null, [watches, selectedId]);

  const loadWatches = useCallback(async (opts = {}) => {
    const silent = Boolean(opts.silent);
    if (!silent) {
      setLoading(true);
      setError(null);
    }
    try {
      const pid = currentProject?.id ? String(currentProject.id).trim() : "";
      const data = await listBrowserInspectionWatches({
        project_id: pid || undefined,
        limit: 200,
      });
      setWatches(Array.isArray(data) ? data : []);
    } catch (e) {
      const msg = apiErrorMessage(e);
      if (!silent) {
        setError(msg);
        setWatches([]);
      } else {
        showToast(msg, "error");
      }
    } finally {
      if (!silent) setLoading(false);
    }
  }, [currentProject?.id, showToast]);

  useEffect(() => {
    loadWatches({ silent: false });
  }, [loadWatches]);

  useEffect(() => {
    drilldownToastShownRef.current = null;
  }, [drilldownWatchId]);

  useEffect(() => {
    if (!drilldownWatchId || loading) return;
    const found = watches.some((w) => String(w.watch_id || "") === drilldownWatchId);
    if (found) {
      setSelectedId(drilldownWatchId);
      return;
    }
    if (drilldownToastShownRef.current === drilldownWatchId) return;
    drilldownToastShownRef.current = drilldownWatchId;
    showToast(t("incident.qa.drilldown.entity_unavailable"), "warning");
  }, [drilldownWatchId, loading, watches, showToast, t]);

  const bumpDetailIfSelected = useCallback((watchId) => {
    if (selectedIdRef.current === watchId) {
      setDetailVersion((v) => v + 1);
    }
  }, []);

  const setBusyId = (id, on) => {
    setBusy((prev) => {
      const n = new Set(prev);
      if (on) n.add(id);
      else n.delete(id);
      return n;
    });
  };

  const onRunNow = async (w) => {
    const watchId = w.watch_id;
    const force = !w.enabled;
    setBusyId(watchId, true);
    try {
      const out = await postBrowserWatchRunNow(watchId, { force });
      await loadWatches({ silent: true });
      bumpDetailIfSelected(watchId);

      let msg = t("watch.toast.run_ok");
      if (out?.alert_triggered) msg += " · " + t("watch.toast.run_alert");
      if (out?.alert_dedupe_suppressed) msg += " · " + t("watch.toast.run_deduped");
      const variant = out?.alert_triggered || out?.alert_dedupe_suppressed ? "warning" : "success";
      showToast(msg, variant);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setBusyId(watchId, false);
    }
  };

  const onBaseline = async (watchId) => {
    setBusyId(watchId, true);
    try {
      await postBrowserWatchBaselineUseLatest(watchId);
      showToast(t("watch.toast.baseline_ok"), "success");
      await loadWatches({ silent: true });
      bumpDetailIfSelected(watchId);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setBusyId(watchId, false);
    }
  };

  const onToggleEnabled = async (w) => {
    const watchId = w.watch_id;
    setBusyId(watchId, true);
    try {
      await patchBrowserInspectionWatch(watchId, { enabled: !w.enabled });
      showToast(w.enabled ? t("watch.toast.disabled") : t("watch.toast.enabled"), "success");
      await loadWatches({ silent: true });
      bumpDetailIfSelected(watchId);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setBusyId(watchId, false);
    }
  };

  const onWatchCreated = useCallback(async (created) => {
    const wid = created?.watch_id;
    if (wid) {
      setSelectedId(wid);
      selectedIdRef.current = wid;
    }
    await loadWatches({ silent: true });
    if (wid) setDetailVersion((v) => v + 1);
  }, [loadWatches]);

  const onWatchUpdated = useCallback(
    async (updated) => {
      const wid = updated?.watch_id;
      await loadWatches({ silent: true });
      if (wid && selectedIdRef.current === wid) setDetailVersion((v) => v + 1);
    },
    [loadWatches],
  );

  const summaryVm = useMemo(() => buildWatchSummaryViewModel(watches, t), [watches, t]);

  const openCreateModal = () => {
    setFormMode("create");
    setFormEditWatch(null);
    setFormOpen(true);
  };

  const openEditModal = (w) => {
    setFormMode("edit");
    setFormEditWatch(w);
    setFormOpen(true);
  };

  return (
    <div style={{ padding: "24px 24px 40px", maxWidth: 1400, margin: "0 auto" }}>
      <WatchFormModal
        open={formOpen}
        mode={formMode}
        editWatch={formEditWatch}
        defaultProjectId={currentProject?.id ? String(currentProject.id).trim() : ""}
        onClose={() => {
          setFormOpen(false);
          setFormEditWatch(null);
        }}
        t={t}
        showToast={showToast}
        onCreated={async (created) => {
          showToast(t(BROWSER_WATCH_I18N_KEYS.toastCreated), "success");
          await onWatchCreated(created);
          setFormOpen(false);
          setFormEditWatch(null);
        }}
        onUpdated={async (updated) => {
          showToast(t("watch.toast.update_ok"), "success");
          await onWatchUpdated(updated);
          setFormOpen(false);
          setFormEditWatch(null);
        }}
      />

      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "flex-start",
          gap: 16,
          flexWrap: "wrap",
        }}
      >
        <div>
          <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>{t(BROWSER_WATCH_I18N_KEYS.title)}</h1>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "6px 0 0" }}>{t(BROWSER_WATCH_I18N_KEYS.subtitle)}</p>
          <p style={{ fontSize: 12, color: "var(--text-2)", margin: "8px 0 0" }}>
            {currentProject ? t("watch.scope_project", { name: currentProject.name }) : t("watch.scope_all")}
          </p>
        </div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
          <button type="button" className="btn btn-primary btn-sm" onClick={openCreateModal}>
            {t("watch.action.create")}
          </button>
          <button type="button" className="btn btn-secondary btn-sm" onClick={() => loadWatches({ silent: false })} disabled={loading}>
            {t("watch.refresh")}
          </button>
        </div>
      </div>

      {!loading && !error && watches.length > 0 ? <BrowserWatchSummaryHeader vm={summaryVm} /> : null}

      <div className="browser-watch-layout" style={{ marginTop: 20 }}>
        <div className="browser-watch-layout__list">
          {loading && (
            <div style={{ padding: 20, color: "var(--text-3)", fontSize: 13 }}>{t("watch.loading")}</div>
          )}
          {!loading && error && (
            <div className="alert alert-error" style={{ marginTop: 8 }}>
              {t("watch.error")} {error}
            </div>
          )}
          {!loading && !error && watches.length === 0 && (
            <div className="card" style={{ padding: 28, textAlign: "center" }}>
              <div style={{ fontSize: 15, fontWeight: 600 }}>{t(BROWSER_WATCH_I18N_KEYS.emptyTitle)}</div>
              <div style={{ fontSize: 13, color: "var(--text-2)", marginTop: 8, lineHeight: 1.6, maxWidth: 420, margin: "8px auto 0" }}>
                {t(BROWSER_WATCH_I18N_KEYS.emptyDesc)}
              </div>
              <button type="button" className="btn btn-primary btn-sm" style={{ marginTop: 16 }} onClick={openCreateModal}>
                {t("watch.action.create")}
              </button>
            </div>
          )}
          {!loading && !error && watches.length > 0 && (
            <BrowserWatchList watches={watches} selectedId={selectedId} t={t} onSelect={setSelectedId} />
          )}
        </div>

        <BrowserWatchDetailPanel
          watchId={selectedId}
          detailVersion={detailVersion}
          selectedWatch={selectedWatch}
          busy={busy}
          t={t}
          showToast={showToast}
          onRunNow={onRunNow}
          onEdit={openEditModal}
          onBaseline={onBaseline}
          onToggleEnabled={onToggleEnabled}
        />
      </div>
    </div>
  );
}

function WatchFormModal({ open, mode, editWatch, defaultProjectId, onClose, t, showToast, onCreated, onUpdated }) {
  const [url, setUrl] = useState("");
  const [projectId, setProjectId] = useState("");
  const [intervalMinutes, setIntervalMinutes] = useState(60);
  const [changeThreshold, setChangeThreshold] = useState("medium");
  const [compareMode, setCompareMode] = useState("last");
  const [enabled, setEnabled] = useState(true);
  const [executionMode, setExecutionMode] = useState("cloud");
  const [localAgentId, setLocalAgentId] = useState("");
  const [localAgents, setLocalAgents] = useState([]);
  const [agentsLoading, setAgentsLoading] = useState(false);
  const [busy, setBusy] = useState(false);
  const [fieldError, setFieldError] = useState("");
  /** Which validation failed — used to clear stale errors when switching execution_mode to cloud. */
  const [fieldErrorKind, setFieldErrorKind] = useState("none");

  const watchProjectId = useMemo(() => {
    if (mode === "edit" && editWatch) return String(editWatch.project_id || "").trim();
    return (projectId.trim() || String(defaultProjectId || "").trim());
  }, [mode, editWatch, projectId, defaultProjectId]);

  useEffect(() => {
    if (!open) return;
    if (mode === "edit" && editWatch) {
      setUrl(editWatch.url || "");
      setProjectId("");
      setIntervalMinutes(Number(editWatch.interval_minutes) || 60);
      setChangeThreshold(editWatch.change_threshold || "medium");
      setCompareMode(String(editWatch.compare_mode || "last").toLowerCase() === "baseline" ? "baseline" : "last");
      setEnabled(Boolean(editWatch.enabled));
      const em = String(editWatch.execution_mode || "cloud").toLowerCase() === "local_agent" ? "local_agent" : "cloud";
      setExecutionMode(em);
      setLocalAgentId(String(editWatch.local_agent_id || "").trim());
      setFieldError("");
      setFieldErrorKind("none");
      return;
    }
    setUrl("");
    setProjectId(defaultProjectId || "");
    setIntervalMinutes(60);
    setChangeThreshold("medium");
    setCompareMode("last");
    setEnabled(true);
    setExecutionMode("cloud");
    setLocalAgentId("");
    setFieldError("");
    setFieldErrorKind("none");
  }, [open, mode, editWatch, defaultProjectId]);

  useEffect(() => {
    if (executionMode !== "cloud") return;
    if (shouldClearWatchFieldErrorWhenSwitchingToCloud(fieldErrorKind)) {
      setFieldError("");
      setFieldErrorKind("none");
    }
  }, [executionMode, fieldErrorKind]);

  useEffect(() => {
    if (!open) return;
    const onKey = (e) => {
      if (e.key === "Escape" && !busy) onClose?.();
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [open, busy, onClose]);

  useEffect(() => {
    if (!open || !watchProjectId) {
      setLocalAgents([]);
      return;
    }
    let cancelled = false;
    (async () => {
      setAgentsLoading(true);
      try {
        const data = await listLocalAgents({ project_id: watchProjectId, limit: 100 });
        if (!cancelled) setLocalAgents(Array.isArray(data) ? data : []);
      } catch {
        if (!cancelled) setLocalAgents([]);
      } finally {
        if (!cancelled) setAgentsLoading(false);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [open, watchProjectId]);

  const eligibleAgents = useMemo(() => localAgents.filter((a) => a.enabled), [localAgents]);
  const selectedAgent = useMemo(
    () => localAgents.find((a) => a.agent_id === localAgentId) || null,
    [localAgents, localAgentId],
  );

  if (!open) return null;

  const baselineIdForWarn =
    mode === "edit" && editWatch ? String(editWatch.baseline_inspection_id || "").trim() : "";
  const showBaselineWarn = compareMode === "baseline" && !baselineIdForWarn;

  const showNoAgentsWarn = executionMode === "local_agent" && !agentsLoading && eligibleAgents.length === 0;
  const showDisabledAgentWarn = executionMode === "local_agent" && localAgentId && selectedAgent && !selectedAgent.enabled;
  const hk = selectedAgent ? agentHeartbeatHealth(selectedAgent) : null;
  const showOfflineWarn =
    executionMode === "local_agent" && localAgentId && selectedAgent && hk && hk !== "online" && hk !== "disabled";

  const submit = async () => {
    const u = url.trim();
    if (!u) {
      setFieldError(t("watch.create.err_url"));
      setFieldErrorKind("generic");
      return;
    }
    const n = parseInt(String(intervalMinutes), 10);
    if (!Number.isFinite(n) || n < 5 || n > 1440) {
      setFieldError(t("watch.create.err_interval"));
      setFieldErrorKind("generic");
      return;
    }
    if (!["low", "medium", "high"].includes(changeThreshold)) {
      setFieldError(t("watch.create.err_threshold"));
      setFieldErrorKind("generic");
      return;
    }
    if (!["last", "baseline"].includes(compareMode)) {
      setFieldError(t("watch.create.err_compare"));
      setFieldErrorKind("generic");
      return;
    }
    const em = executionMode === "local_agent" ? "local_agent" : "cloud";
    if (em === "local_agent") {
      const pidCreate = (projectId.trim() || String(defaultProjectId || "").trim());
      if (mode !== "edit" && !pidCreate) {
        setFieldError(t("watch.create.err_project_local"));
        setFieldErrorKind("project_local");
        return;
      }
      if (!localAgentId.trim()) {
        setFieldError(t("watch.create.err_local_agent"));
        setFieldErrorKind("agent");
        return;
      }
    }
    setFieldError("");
    setFieldErrorKind("none");
    setBusy(true);
    try {
      if (mode === "edit" && editWatch?.watch_id) {
        const patch = {
          url: u,
          interval_minutes: n,
          change_threshold: changeThreshold,
          enabled,
          compare_mode: compareMode,
          execution_mode: em,
        };
        if (em === "local_agent") patch.local_agent_id = localAgentId.trim();
        const updated = await patchBrowserInspectionWatch(editWatch.watch_id, patch);
        await onUpdated?.(updated);
      } else {
        const body = {
          url: u,
          interval_minutes: n,
          change_threshold: changeThreshold,
          enabled,
          execution_mode: em,
          compare_mode: compareMode,
        };
        const pid = projectId.trim();
        if (pid) body.project_id = pid;
        if (em === "local_agent") body.local_agent_id = localAgentId.trim();
        const created = await createBrowserInspectionWatch(body);
        await onCreated?.(created);
      }
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setBusy(false);
    }
  };

  const title = mode === "edit" ? t("watch.edit.title") : t("watch.create.title");
  const submitLabel = mode === "edit" ? t("watch.edit.submit") : t("watch.create.submit");

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
      aria-labelledby="watch-form-modal-title"
      onMouseDown={(e) => {
        if (e.target === e.currentTarget && !busy) onClose?.();
      }}
    >
      <div className="card" style={{ width: "min(520px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)", maxHeight: "90vh", overflowY: "auto" }}>
          <div id="watch-form-modal-title" style={{ fontSize: 16, fontWeight: 600, color: "var(--text-1)" }}>
            {title}
          </div>
          {fieldError ? (
            <div className="alert alert-error" style={{ marginTop: 10, fontSize: 12 }}>
              {fieldError}
            </div>
          ) : null}
          {showBaselineWarn ? (
            <div className="alert alert-warn" style={{ marginTop: 10, fontSize: 12 }}>
              {t("watch.warn_baseline_missing")}
            </div>
          ) : null}
          {showNoAgentsWarn ? (
            <div className="alert alert-warn" style={{ marginTop: 10, fontSize: 12 }}>
              {t("watch.create.warn_no_agents")}
            </div>
          ) : null}
          {showDisabledAgentWarn ? (
            <div className="alert alert-warn" style={{ marginTop: 10, fontSize: 12 }}>
              {t("watch.create.warn_agent_disabled")}
            </div>
          ) : null}
          {showOfflineWarn ? (
            <div className="alert alert-warn" style={{ marginTop: 10, fontSize: 12 }}>
              {t("watch.create.warn_agent_offline")}
            </div>
          ) : null}
          {!watchProjectId && executionMode === "local_agent" ? (
            <div className="alert alert-warn" style={{ marginTop: 10, fontSize: 12 }}>
              {mode === "edit" ? t("watch.edit.warn_no_project") : t("watch.create.err_project_local")}
            </div>
          ) : null}

          <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 14, marginBottom: 6 }}>
            {t("watch.create.url")} *
          </label>
          <input
            className="input"
            autoFocus
            value={url}
            onChange={(e) => setUrl(e.target.value)}
            disabled={busy}
            placeholder="https://"
            style={{ width: "100%" }}
          />

          {mode === "create" ? (
            <>
              <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 12, marginBottom: 4 }}>
                {t("watch.create.project_id")}
              </label>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>{t("watch.create.project_hint")}</div>
              <input
                className="input"
                value={projectId}
                onChange={(e) => setProjectId(e.target.value)}
                disabled={busy}
                style={{ width: "100%" }}
              />
            </>
          ) : editWatch?.project_id ? (
            <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 10 }}>
              project_id: <span style={{ fontFamily: "monospace" }}>{editWatch.project_id}</span>
            </div>
          ) : null}

          <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 12, marginBottom: 6 }}>
            {t("watch.create.interval")} (5–1440)
          </label>
          <input
            className="input"
            type="number"
            min={5}
            max={1440}
            value={intervalMinutes}
            onChange={(e) => setIntervalMinutes(e.target.value)}
            disabled={busy}
            style={{ width: "100%" }}
          />

          <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 12, marginBottom: 6 }}>
            {t("watch.create.threshold")} *
          </label>
          <select
            className="input"
            value={changeThreshold}
            onChange={(e) => setChangeThreshold(e.target.value)}
            disabled={busy}
            style={{ width: "100%" }}
          >
            <option value="low">{t("watch.create.th.low")}</option>
            <option value="medium">{t("watch.create.th.medium")}</option>
            <option value="high">{t("watch.create.th.high")}</option>
          </select>

          <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 12, marginBottom: 6 }}>
            {t("watch.create.compare_mode")} *
          </label>
          <select className="input" value={compareMode} onChange={(e) => setCompareMode(e.target.value)} disabled={busy} style={{ width: "100%" }}>
            <option value="last">{t("watch.compare.last")}</option>
            <option value="baseline">{t("watch.compare.baseline")}</option>
          </select>

          <label style={{ display: "flex", alignItems: "center", gap: 8, marginTop: 14, fontSize: 13, color: "var(--text-2)" }}>
            <input type="checkbox" checked={enabled} onChange={(e) => setEnabled(e.target.checked)} disabled={busy} />
            {t("watch.create.enabled")}
          </label>

          <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 12, marginBottom: 6 }}>
            {t("watch.create.execution_mode")}
          </label>
          <select
            className="input"
            value={executionMode}
            onChange={(e) => {
              const v = e.target.value === "local_agent" ? "local_agent" : "cloud";
              setExecutionMode(v);
              if (v === "cloud") setLocalAgentId("");
            }}
            disabled={busy}
            style={{ width: "100%" }}
          >
            <option value="cloud">{t("watch.create.execution_cloud")}</option>
            <option value="local_agent">{t("watch.create.execution_local_agent")}</option>
          </select>

          {executionMode === "local_agent" ? (
            <>
              <label style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 12, marginBottom: 6 }}>
                {t("watch.create.local_agent")} *
              </label>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>{t("watch.create.local_agent_hint")}</div>
              {agentsLoading ? (
                <div style={{ fontSize: 12, color: "var(--text-3)" }}>{t("watch.loading")}</div>
              ) : (
                <select
                  className="input"
                  value={localAgentId}
                  onChange={(e) => setLocalAgentId(e.target.value)}
                  disabled={busy || !watchProjectId}
                  style={{ width: "100%" }}
                >
                  <option value="">—</option>
                  {eligibleAgents.map((a) => (
                    <option key={a.agent_id} value={a.agent_id}>
                      {a.name} (
                      {a.agent_id.length <= 12 ? a.agent_id : `${a.agent_id.slice(0, 8)}…`})
                    </option>
                  ))}
                  {localAgentId && !eligibleAgents.some((a) => a.agent_id === localAgentId) ? (
                    <option value={localAgentId}>
                      {(localAgents.find((a) => a.agent_id === localAgentId)?.name || localAgentId).slice(0, 64)}
                    </option>
                  ) : null}
                </select>
              )}
            </>
          ) : (
            <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 6 }}>{t("watch.create.execution_cloud")}</div>
          )}
        </div>
        <div style={{ padding: "14px 20px", display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
          <button type="button" className="btn btn-secondary btn-sm" onClick={() => onClose?.()} disabled={busy}>
            {t("common.cancel")}
          </button>
          <button type="button" className="btn btn-primary btn-sm" onClick={submit} disabled={busy}>
            {busy ? t("common.working") : submitLabel}
          </button>
        </div>
      </div>
    </div>
  );
}
