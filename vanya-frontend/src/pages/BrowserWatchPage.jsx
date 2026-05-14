// src/pages/BrowserWatchPage.jsx
/**
 * Browser Watch — monitoring, create/edit, evidence links (Phase 3G–3I).
 * List: GET /browser-inspections/watch
 * Create: POST /browser-inspections/watch
 * Update: PATCH /browser-inspections/watch/{id}
 * Run evidence: /evidence/run/:runId (inspection run id)
 */
import React, { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Link } from "react-router-dom";
import {
  listBrowserInspectionWatches,
  createBrowserInspectionWatch,
  patchBrowserInspectionWatch,
  postBrowserWatchRunNow,
  postBrowserWatchBaselineUseLatest,
  getBrowserWatchMetrics,
  getBrowserWatchEventsPage,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import { useToast } from "../context/ToastContext.jsx";

function fmtTs(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

function statusBadgeClass(status) {
  const v = String(status || "").toLowerCase();
  if (v === "healthy") return "badge-green";
  if (v === "changed") return "badge-orange";
  if (v === "failed") return "badge-red";
  if (v === "disabled") return "badge-gray";
  return "badge-gray";
}

function shortRunId(id) {
  if (id == null || id === "" || id === "_") return "";
  const s = String(id);
  return s.length <= 10 ? s : `${s.slice(0, 8)}…`;
}

function InspectionRunRef({ id, t, showToast }) {
  if (!id || id === "_") {
    return <span style={{ color: "var(--text-3)" }}>—</span>;
  }
  const to = `/evidence/run/${encodeURIComponent(id)}`;
  const short = shortRunId(id);
  const copy = async (e) => {
    e.preventDefault();
    e.stopPropagation();
    try {
      await navigator.clipboard.writeText(String(id));
      showToast(t("watch.copy_ok"), "success");
    } catch {
      showToast(t("watch.copy_fail"), "error");
    }
  };
  return (
    <span style={{ display: "inline-flex", alignItems: "center", gap: 4, flexWrap: "wrap" }} onClick={(e) => e.stopPropagation()}>
      <Link to={to} style={{ fontFamily: "monospace", fontSize: 11 }} title={String(id)}>
        {short}
      </Link>
      <button
        type="button"
        className="btn btn-secondary btn-sm"
        style={{ padding: "1px 6px", fontSize: 10, minHeight: 22 }}
        onClick={copy}
        title={t("watch.copy_id")}
      >
        ⧉
      </button>
    </span>
  );
}

export default function BrowserWatchPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const { showToast } = useToast();
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
          showToast(t("watch.toast.create_ok"), "success");
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
          <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>{t("watch.title")}</h1>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "6px 0 0" }}>{t("watch.subtitle")}</p>
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

      <div style={{ display: "flex", gap: 20, marginTop: 20, flexWrap: "wrap", alignItems: "stretch" }}>
        <div style={{ flex: "1 1 520px", minWidth: 280 }}>
          {loading && (
            <div style={{ padding: 20, color: "var(--text-3)", fontSize: 13 }}>{t("watch.loading")}</div>
          )}
          {!loading && error && (
            <div className="alert alert-error" style={{ marginTop: 8 }}>
              {t("watch.error")} {error}
            </div>
          )}
          {!loading && !error && watches.length === 0 && (
            <div className="card" style={{ padding: 24, textAlign: "center" }}>
              <div style={{ fontSize: 15, fontWeight: 600 }}>{t("watch.empty_title")}</div>
              <div style={{ fontSize: 13, color: "var(--text-2)", marginTop: 8, lineHeight: 1.6 }}>
                {t("watch.empty_desc")}
              </div>
              <button type="button" className="btn btn-primary btn-sm" style={{ marginTop: 16 }} onClick={openCreateModal}>
                {t("watch.action.create")}
              </button>
            </div>
          )}
          {!loading && !error && watches.length > 0 && (
            <div style={{ overflowX: "auto", border: "1px solid var(--border)", borderRadius: 8 }}>
              <table className="data-table" style={{ margin: 0, minWidth: 880 }}>
                <thead>
                  <tr>
                    <th>{t("watch.col.url")}</th>
                    <th>{t("watch.col.status")}</th>
                    <th>{t("watch.col.threshold")}</th>
                    <th>{t("watch.col.compare")}</th>
                    <th>{t("watch.col.last_insp")}</th>
                    <th>{t("watch.col.change")}</th>
                    <th>{t("watch.col.visual")}</th>
                    <th>{t("watch.col.last_run")}</th>
                    <th>{t("watch.col.alert")}</th>
                    <th>{t("watch.col.baseline")}</th>
                    <th>{t("watch.col.actions")}</th>
                  </tr>
                </thead>
                <tbody>
                  {watches.map((w) => (
                    <tr
                      key={w.watch_id}
                      onClick={() => setSelectedId(w.watch_id)}
                      style={{
                        cursor: "pointer",
                        background: selectedId === w.watch_id ? "var(--accent-light)" : undefined,
                      }}
                    >
                      <td style={{ maxWidth: 200, wordBreak: "break-all", fontSize: 12 }}>{w.url}</td>
                      <td>
                        <span className={`badge ${statusBadgeClass(w.current_status || w.last_status)}`} style={{ fontSize: 10 }}>
                          {w.current_status || w.last_status || "—"}
                        </span>
                      </td>
                      <td style={{ fontSize: 12 }}>{w.change_threshold}</td>
                      <td style={{ fontSize: 12 }}>{w.compare_mode}</td>
                      <td style={{ fontSize: 12 }}>
                        <InspectionRunRef id={w.last_inspection_id} t={t} showToast={showToast} />
                      </td>
                      <td style={{ fontSize: 12 }}>{w.last_change_level ?? w.last_effective_change_level ?? "—"}</td>
                      <td style={{ fontSize: 12 }}>{w.last_visual_change_level ?? "—"}</td>
                      <td style={{ fontSize: 12, whiteSpace: "nowrap" }}>{fmtTs(w.last_run_at)}</td>
                      <td style={{ fontSize: 12, whiteSpace: "nowrap" }}>{fmtTs(w.last_alert_at)}</td>
                      <td style={{ fontSize: 12 }}>
                        <InspectionRunRef id={w.baseline_inspection_id} t={t} showToast={showToast} />
                      </td>
                      <td onClick={(e) => e.stopPropagation()}>
                        <div style={{ display: "flex", flexDirection: "column", gap: 4, alignItems: "stretch" }}>
                          <button
                            type="button"
                            className="btn btn-secondary btn-sm"
                            disabled={busy.has(w.watch_id)}
                            onClick={() => openEditModal(w)}
                          >
                            {t("watch.action.edit")}
                          </button>
                          <button
                            type="button"
                            className="btn btn-primary btn-sm"
                            disabled={busy.has(w.watch_id)}
                            onClick={() => onRunNow(w)}
                          >
                            {t("watch.action.run")}
                          </button>
                          <button
                            type="button"
                            className="btn btn-secondary btn-sm"
                            disabled={busy.has(w.watch_id)}
                            onClick={() => onBaseline(w.watch_id)}
                          >
                            {t("watch.action.baseline")}
                          </button>
                          <button
                            type="button"
                            className="btn btn-secondary btn-sm"
                            disabled={busy.has(w.watch_id)}
                            onClick={() => onToggleEnabled(w)}
                          >
                            {w.enabled ? t("watch.action.disable") : t("watch.action.enable")}
                          </button>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>

        <DetailPanel watchId={selectedId} detailVersion={detailVersion} selectedWatch={selectedWatch} t={t} showToast={showToast} />
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
  const [busy, setBusy] = useState(false);
  const [fieldError, setFieldError] = useState("");

  useEffect(() => {
    if (!open) return;
    if (mode === "edit" && editWatch) {
      setUrl(editWatch.url || "");
      setProjectId("");
      setIntervalMinutes(Number(editWatch.interval_minutes) || 60);
      setChangeThreshold(editWatch.change_threshold || "medium");
      setCompareMode(String(editWatch.compare_mode || "last").toLowerCase() === "baseline" ? "baseline" : "last");
      setEnabled(Boolean(editWatch.enabled));
      setFieldError("");
      return;
    }
    setUrl("");
    setProjectId(defaultProjectId || "");
    setIntervalMinutes(60);
    setChangeThreshold("medium");
    setCompareMode("last");
    setEnabled(true);
    setFieldError("");
  }, [open, mode, editWatch, defaultProjectId]);

  useEffect(() => {
    if (!open) return;
    const onKey = (e) => {
      if (e.key === "Escape" && !busy) onClose?.();
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [open, busy, onClose]);

  if (!open) return null;

  const baselineIdForWarn =
    mode === "edit" && editWatch ? String(editWatch.baseline_inspection_id || "").trim() : "";
  const showBaselineWarn = compareMode === "baseline" && !baselineIdForWarn;

  const submit = async () => {
    const u = url.trim();
    if (!u) {
      setFieldError(t("watch.create.err_url"));
      return;
    }
    const n = parseInt(String(intervalMinutes), 10);
    if (!Number.isFinite(n) || n < 5 || n > 1440) {
      setFieldError(t("watch.create.err_interval"));
      return;
    }
    if (!["low", "medium", "high"].includes(changeThreshold)) {
      setFieldError(t("watch.create.err_threshold"));
      return;
    }
    if (!["last", "baseline"].includes(compareMode)) {
      setFieldError(t("watch.create.err_compare"));
      return;
    }
    setFieldError("");
    setBusy(true);
    try {
      if (mode === "edit" && editWatch?.watch_id) {
        const updated = await patchBrowserInspectionWatch(editWatch.watch_id, {
          url: u,
          interval_minutes: n,
          change_threshold: changeThreshold,
          enabled,
          compare_mode: compareMode,
        });
        await onUpdated?.(updated);
      } else {
        const body = {
          url: u,
          interval_minutes: n,
          change_threshold: changeThreshold,
          enabled,
          execution_mode: "cloud",
          compare_mode: compareMode,
        };
        const pid = projectId.trim();
        if (pid) body.project_id = pid;
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
      <div className="card" style={{ width: "min(480px, 100%)", padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
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
          <input className="input" readOnly value="cloud" disabled style={{ width: "100%", opacity: 0.85 }} />
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>{t("watch.create.execution_cloud")}</div>
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

function DetailPanel({ watchId, detailVersion, selectedWatch, t, showToast }) {
  const [metrics, setMetrics] = useState(null);
  const [mLoading, setMLoading] = useState(false);
  const [events, setEvents] = useState([]);
  const [nextCursor, setNextCursor] = useState(null);
  const [eLoading, setELoading] = useState(false);
  const [eMoreLoading, setEMoreLoading] = useState(false);

  useEffect(() => {
    if (!watchId) {
      setMetrics(null);
      setEvents([]);
      setNextCursor(null);
      return;
    }
    let cancelled = false;
    (async () => {
      setMLoading(true);
      setELoading(true);
      setEvents([]);
      setNextCursor(null);
      try {
        const [m, evp] = await Promise.all([
          getBrowserWatchMetrics(watchId),
          getBrowserWatchEventsPage(watchId, { limit: 25 }),
        ]);
        if (cancelled) return;
        setMetrics(m);
        setEvents(Array.isArray(evp?.items) ? evp.items : []);
        setNextCursor(evp?.next_cursor ?? null);
      } catch (e) {
        if (!cancelled) {
          setMetrics(null);
          setEvents([]);
          showToast(apiErrorMessage(e), "error");
        }
      } finally {
        if (!cancelled) {
          setMLoading(false);
          setELoading(false);
        }
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [watchId, detailVersion, showToast]);

  const loadMore = async () => {
    if (!watchId || !nextCursor) return;
    setEMoreLoading(true);
    try {
      const evp = await getBrowserWatchEventsPage(watchId, { limit: 25, cursor: nextCursor });
      const more = Array.isArray(evp?.items) ? evp.items : [];
      setEvents((prev) => [...prev, ...more]);
      setNextCursor(evp?.next_cursor ?? null);
    } catch (e) {
      showToast(apiErrorMessage(e), "error");
    } finally {
      setEMoreLoading(false);
    }
  };

  if (!watchId) {
    return (
      <div
        style={{
          flex: "1 1 320px",
          minWidth: 280,
          border: "1px dashed var(--border)",
          borderRadius: 8,
          padding: 20,
          color: "var(--text-3)",
          fontSize: 13,
        }}
      >
        {t("watch.detail.select")}
      </div>
    );
  }

  return (
    <div style={{ flex: "1 1 360px", minWidth: 280 }}>
      <div className="card" style={{ padding: 16 }}>
        <div
          style={{
            fontSize: 12,
            fontWeight: 600,
            textTransform: "uppercase",
            letterSpacing: "0.06em",
            color: "var(--text-3)",
            marginBottom: 10,
          }}
        >
          {t("watch.detail.metrics")}
        </div>
        {mLoading && <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("watch.detail.loading_m")}</div>}
        {!mLoading && metrics && (
          <dl style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: "6px 12px", margin: 0, fontSize: 13 }}>
            <dt style={{ color: "var(--text-3)" }}>current_status</dt>
            <dd style={{ margin: 0 }}>{metrics.current_status ?? "—"}</dd>
            <dt style={{ color: "var(--text-3)" }}>total_runs</dt>
            <dd style={{ margin: 0 }}>{metrics.total_runs}</dd>
            <dt style={{ color: "var(--text-3)" }}>total_diffs</dt>
            <dd style={{ margin: 0 }}>{metrics.total_diffs}</dd>
            <dt style={{ color: "var(--text-3)" }}>alerts_triggered</dt>
            <dd style={{ margin: 0 }}>{metrics.alerts_triggered}</dd>
            <dt style={{ color: "var(--text-3)" }}>last_change_level</dt>
            <dd style={{ margin: 0 }}>{metrics.last_change_level ?? "—"}</dd>
            <dt style={{ color: "var(--text-3)" }}>last_visual_change_level</dt>
            <dd style={{ margin: 0 }}>{metrics.last_visual_change_level ?? "—"}</dd>
            <dt style={{ color: "var(--text-3)" }}>last_run_at</dt>
            <dd style={{ margin: 0 }}>{fmtTs(metrics.last_run_at)}</dd>
            <dt style={{ color: "var(--text-3)" }}>last_alert_at</dt>
            <dd style={{ margin: 0 }}>{fmtTs(metrics.last_alert_at)}</dd>
            {selectedWatch?.last_inspection_id ? (
              <>
                <dt style={{ color: "var(--text-3)" }}>last_inspection</dt>
                <dd style={{ margin: 0 }}>
                  <InspectionRunRef id={selectedWatch.last_inspection_id} t={t} showToast={showToast} />
                </dd>
              </>
            ) : null}
            {selectedWatch?.baseline_inspection_id ? (
              <>
                <dt style={{ color: "var(--text-3)" }}>baseline</dt>
                <dd style={{ margin: 0 }}>
                  <InspectionRunRef id={selectedWatch.baseline_inspection_id} t={t} showToast={showToast} />
                </dd>
              </>
            ) : null}
          </dl>
        )}
      </div>

      <div className="card" style={{ padding: 16, marginTop: 12 }}>
        <div
          style={{
            fontSize: 12,
            fontWeight: 600,
            textTransform: "uppercase",
            letterSpacing: "0.06em",
            color: "var(--text-3)",
            marginBottom: 10,
          }}
        >
          {t("watch.detail.events")}
        </div>
        {eLoading && <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("watch.detail.loading_e")}</div>}
        {!eLoading && events.length === 0 && (
          <div style={{ fontSize: 13, color: "var(--text-2)" }}>{t("watch.detail.no_events")}</div>
        )}
        {!eLoading && events.length > 0 && (
          <ul style={{ listStyle: "none", margin: 0, padding: 0, maxHeight: 360, overflowY: "auto" }}>
            {events.map((e) => (
              <li
                key={e.event_id}
                style={{
                  borderBottom: "1px solid var(--border)",
                  padding: "8px 0",
                  fontSize: 12,
                }}
              >
                <div style={{ display: "flex", justifyContent: "space-between", gap: 8 }}>
                  <span className="badge badge-gray" style={{ fontSize: 10 }}>
                    {e.event_type}
                  </span>
                  <span style={{ color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtTs(e.created_at)}</span>
                </div>
                <div style={{ color: "var(--text-2)", marginTop: 4, wordBreak: "break-word" }}>
                  {(e.summary || "").slice(0, 200)}
                </div>
                {e.base_inspection_id || e.target_inspection_id ? (
                  <div
                    style={{
                      marginTop: 6,
                      display: "flex",
                      flexWrap: "wrap",
                      gap: "8px 12px",
                      alignItems: "center",
                      fontSize: 11,
                      color: "var(--text-3)",
                    }}
                  >
                    {e.base_inspection_id ? (
                      <span style={{ display: "inline-flex", alignItems: "center", gap: 4 }}>
                        {t("watch.ev.base")}: <InspectionRunRef id={e.base_inspection_id} t={t} showToast={showToast} />
                      </span>
                    ) : null}
                    {e.target_inspection_id ? (
                      <span style={{ display: "inline-flex", alignItems: "center", gap: 4 }}>
                        {t("watch.ev.target")}: <InspectionRunRef id={e.target_inspection_id} t={t} showToast={showToast} />
                      </span>
                    ) : null}
                  </div>
                ) : null}
              </li>
            ))}
          </ul>
        )}
        {!eLoading && nextCursor ? (
          <button
            type="button"
            className="btn btn-secondary btn-sm"
            style={{ marginTop: 10 }}
            disabled={eMoreLoading}
            onClick={loadMore}
          >
            {eMoreLoading ? t("common.working") : t("watch.detail.load_more")}
          </button>
        ) : null}
      </div>
    </div>
  );
}
