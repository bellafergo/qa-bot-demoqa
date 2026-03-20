// src/pages/ExecutionPage.jsx
/**
 * Execution Center — worker pool, job queue, and batch execution.
 * GET /execution/status, GET /orchestrator/jobs, POST /execution/run-batch
 */
import React, { useState, useEffect, useCallback, useRef } from "react";
import { useNavigate } from "react-router-dom";
import { getExecStatus, listJobs, runBatch, getJob, retryFailed, alertingReady, sendAlert } from "../api";
import { useLang } from "../i18n/LangContext";

const POLL_INTERVAL_MS = 7000;

function statusClass(s) {
  const v = String(s || "").toLowerCase();
  if (v === "completed")               return "badge-green";
  if (v === "failed" || v === "error") return "badge-red";
  if (v === "running")                 return "badge-blue";
  if (v === "queued")                  return "badge-orange";
  if (v === "partial")                 return "badge-orange";
  return "badge-gray";
}

function fmtDate(iso) {
  if (!iso) return "—";
  try { return new Date(iso).toLocaleString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" }); }
  catch { return "—"; }
}

function fmtMs(ms) {
  if (ms == null) return "—";
  return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`;
}

// Compute duration from timestamps when the model property isn't serialized
function jobDurationMs(j) {
  if (j.duration_ms != null) return j.duration_ms;
  if (j.started_at && j.finished_at) {
    return Math.round(new Date(j.finished_at) - new Date(j.started_at));
  }
  return null;
}

export default function ExecutionPage() {
  const { t } = useLang();
  const navigate = useNavigate();

  const [status, setStatus]   = useState(null);
  const [jobs, setJobs]       = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError]     = useState("");

  // Batch run form
  const [batchInput, setBatchInput]     = useState("");
  const [batchLoading, setBatchLoading] = useState(false);
  const [batchResult, setBatchResult]   = useState(null);

  // Job detail
  const [selectedJob, setSelectedJob]     = useState(null);
  const [jobDetail, setJobDetail]         = useState(null);
  const [detailLoading, setDetailLoading] = useState(false);

  // Retry failed flow (human confirmation)
  const [retryFailedOpen, setRetryFailedOpen] = useState(false);
  const [retryFailedBusy, setRetryFailedBusy] = useState(false);
  const [retryFailedError, setRetryFailedError] = useState("");
  const [retryEnqueuedJobId, setRetryEnqueuedJobId] = useState(null);

  // Alerting (human confirmation)
  const [alertingReadyState, setAlertingReadyState] = useState(null);
  const [sendAlertOpen, setSendAlertOpen] = useState(false);
  const [sendAlertBusy, setSendAlertBusy] = useState(false);
  const [sendAlertError, setSendAlertError] = useState("");
  const [sendAlertSuccess, setSendAlertSuccess] = useState(false);
  const [sendAlertConnector, setSendAlertConnector] = useState("slack");
  const [sendAlertRecipients, setSendAlertRecipients] = useState("");

  // Auto-poll interval ref — keeps one interval at a time
  const pollRef = useRef(null);

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const [st, jl] = await Promise.all([
        getExecStatus().catch(() => null),
        listJobs(50).catch(() => []),
      ]);
      setStatus(st);
      setJobs(Array.isArray(jl) ? jl : []);
    } catch (e) {
      setError(e?.message || t("exec.batch.load_error"));
    } finally {
      setLoading(false);
    }
  }, [t]);

  // Initial load
  useEffect(() => { load(); }, [load]);

  // Check if alerting is available (for Send alert button)
  useEffect(() => {
    alertingReady().then((r) => setAlertingReadyState(r)).catch(() => setAlertingReadyState({ ready: false }));
  }, []);

  // Auto-dismiss send alert success toast
  useEffect(() => {
    if (!sendAlertSuccess) return;
    const t = setTimeout(() => setSendAlertSuccess(false), 4000);
    return () => clearTimeout(t);
  }, [sendAlertSuccess]);

  // Auto-poll while any job is queued or running; stop when all are done
  useEffect(() => {
    const hasActive = jobs.some(j => j.status === "queued" || j.status === "running");
    if (hasActive && !pollRef.current) {
      pollRef.current = setInterval(load, POLL_INTERVAL_MS);
    } else if (!hasActive && pollRef.current) {
      clearInterval(pollRef.current);
      pollRef.current = null;
    }
  }, [jobs, load]);

  // Clean up interval on unmount
  useEffect(() => {
    return () => {
      if (pollRef.current) {
        clearInterval(pollRef.current);
        pollRef.current = null;
      }
    };
  }, []);

  async function handleBatch() {
    // Normalize: trim, remove empty, deduplicate
    const ids = [...new Set(
      batchInput.split(/[\n,]+/).map(s => s.trim()).filter(Boolean)
    )];
    if (!ids.length) return;
    setBatchLoading(true);
    setBatchResult(null);
    try {
      const r = await runBatch({ test_case_ids: ids });
      setBatchResult({ ok: true, ...r });
      setBatchInput(""); // clear on success so user sees the job in the list
      load();
    } catch (e) {
      setBatchResult({ ok: false, error: e?.message || t("exec.batch.submit_error") });
    } finally {
      setBatchLoading(false);
    }
  }

  async function openJobDetail(job_id) {
    setSelectedJob(job_id);
    setDetailLoading(true);
    setJobDetail(null);
    try {
      const d = await getJob(job_id);
      setJobDetail(d);
    } catch (e) {
      setJobDetail({ error: e?.message });
    } finally {
      setDetailLoading(false);
    }
  }

  // ── Local confirm modal (operational actions require explicit approval) ──
  function ConfirmModal({
    open,
    title,
    description,
    busy = false,
    error = "",
    onConfirm,
    onCancel,
    children,
  }) {
    if (!open) return null;
    return (
      <div
        style={{
          position: "fixed",
          inset: 0,
          background: "rgba(0,0,0,0.45)",
          zIndex: 9999,
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          padding: 16,
        }}
        role="dialog"
        aria-modal="true"
      >
        <div className="card" style={{ width: "min(680px, 100%)", padding: 0, overflow: "hidden" }}>
          <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
            <div style={{ fontSize: 15, fontWeight: 900, color: "var(--text-1)", marginBottom: 6 }}>{title}</div>
            <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>{description}</div>
          </div>
          <div style={{ padding: "14px 20px" }}>
            {children}
            {error && (
              <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
                {error}
              </div>
            )}
            <div style={{ display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
              <button className="btn btn-secondary btn-sm" onClick={onCancel} disabled={busy}>
                {t("common.cancel")}
              </button>
              <button className="btn btn-primary btn-sm" onClick={onConfirm} disabled={busy}>
                {busy ? t("common.working") : t("common.confirm")}
              </button>
            </div>
          </div>
        </div>
      </div>
    );
  }

  const st = status || {};
  const hasActive = jobs.some(j => j.status === "queued" || j.status === "running");
  const jobFailedCount = (jobDetail?.failed_count ?? 0);
  const jobErrorCount = (jobDetail?.error_count ?? 0);
  const jobCanRetryFailed =
    !!jobDetail &&
    (jobDetail.status === "failed" || jobDetail.status === "completed" || jobDetail.status === "partial") &&
    (jobFailedCount > 0 || jobErrorCount > 0);

  const formatTpl = (tpl, vars) => {
    let s = String(tpl ?? "");
    for (const [k, v] of Object.entries(vars || {})) {
      s = s.replaceAll(`{${k}}`, String(v));
    }
    return s;
  };

  const kpiItems = [
    { labelKey: "exec.kpi.active_workers", value: loading ? "…" : st.active_workers ?? "—",  accent: st.active_workers > 0 ? "var(--green)" : undefined },
    { labelKey: "exec.kpi.queue_depth",    value: loading ? "…" : st.queue_depth ?? "—",      accent: st.queue_depth > 0 ? "var(--orange)" : undefined },
    { labelKey: "exec.kpi.max_workers",    value: loading ? "…" : st.max_workers ?? "—" },
    { labelKey: "exec.kpi.ui_slots",       value: loading ? "…" : `${st.running_ui_workers ?? 0}/${st.max_ui_workers ?? "—"}` },
    { labelKey: "exec.kpi.api_slots",      value: loading ? "…" : `${st.running_api_workers ?? 0}/${st.max_api_workers ?? "—"}` },
    { labelKey: "exec.kpi.completed",      value: loading ? "…" : st.completed_tasks ?? "—",  accent: "var(--green)" },
    { labelKey: "exec.kpi.retried",        value: loading ? "…" : st.retried_tasks ?? "—",    accent: st.retried_tasks > 0 ? "var(--orange)" : undefined },
  ];

  return (
    <div className="page-wrap">

      {/* Page header */}
      <div style={{ marginBottom: 24 }}>
        <h1 style={{ fontSize: 20, fontWeight: 800, margin: 0, color: "var(--text-1)" }}>{t("exec.page.title")}</h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 0" }}>{t("exec.page.subtitle")}</p>
      </div>

      {/* Status cards */}
      <div className="kpi-grid" style={{ gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))", marginBottom: 24 }}>
        {kpiItems.map(({ labelKey, value, accent }) => (
          <div key={labelKey} className="kpi-card">
            <div className="kpi-label">{t(labelKey)}</div>
            <div className="kpi-value" style={accent ? { color: accent } : {}}>{value}</div>
          </div>
        ))}
      </div>

      <div style={{ display: "grid", gridTemplateColumns: "minmax(0,2fr) minmax(0,1fr)", gap: 24, alignItems: "start" }}>

        {/* Jobs list */}
        <div>
          {error && <div className="alert alert-error" style={{ marginBottom: 12 }}>{error}</div>}
          <div className="card" style={{ padding: 0, overflow: "hidden" }}>
            <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between" }}>
              <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
                <div className="section-title" style={{ margin: 0 }}>
                  {loading ? t("exec.jobs.loading") : `${jobs.length} ${jobs.length !== 1 ? t("exec.jobs.recent_plural") : t("exec.jobs.recent")}`}
                </div>
                {hasActive && (
                  <span className="badge badge-blue" style={{ fontSize: 10 }}>● {t("exec.jobs.polling")}</span>
                )}
              </div>
              <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>{t("exec.jobs.refresh")}</button>
            </div>

            {jobs.length === 0 && !loading ? (
              <div style={{ padding: "32px 24px", textAlign: "center" }}>
                <div style={{ fontSize: 28, marginBottom: 10 }}>◈</div>
                <div style={{ fontSize: 13, fontWeight: 700, color: "var(--text-2)", marginBottom: 6 }}>{t("exec.jobs.none")}</div>
                <div style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.6 }}>
                  {t("exec.batch.desc")}
                </div>
              </div>
            ) : (
              <table className="data-table">
                <thead><tr>
                  <th>{t("exec.jobs.col.job_id")}</th>
                  <th>{t("exec.jobs.col.type")}</th>
                  <th>{t("exec.jobs.col.status")}</th>
                  <th>{t("exec.jobs.col.tests")}</th>
                  <th>{t("exec.jobs.col.pass_fail")}</th>
                  <th>{t("exec.jobs.col.duration")}</th>
                  <th>{t("exec.jobs.col.created")}</th>
                  <th style={{ width: 70 }}>{t("exec.jobs.col.actions")}</th>
                </tr></thead>
                <tbody>
                  {jobs.map((j, i) => (
                    <tr
                      key={j.job_id || i}
                      style={{ cursor: "pointer", background: selectedJob === j.job_id ? "var(--accent-light)" : undefined }}
                      onClick={() => openJobDetail(j.job_id)}
                    >
                      <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>
                        {(j.job_id || "").slice(0, 14)}…
                      </td>
                      <td><span className="badge badge-gray">{j.job_type || "suite"}</span></td>
                      <td><span className={`badge ${statusClass(j.status)}`}>{j.status}</span></td>
                      <td style={{ fontSize: 12 }}>{j.total_count ?? "—"}</td>
                      <td style={{ fontSize: 12, color: "var(--text-2)" }}>
                        <span style={{ color: "var(--green)", fontWeight: 600 }}>{j.passed_count ?? 0}</span>
                        {" / "}
                        <span style={{ color: "var(--red)", fontWeight: 600 }}>{j.failed_count ?? 0}</span>
                      </td>
                      <td style={{ fontSize: 12, color: "var(--text-3)" }}>{fmtMs(jobDurationMs(j))}</td>
                      <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(j.created_at)}</td>
                      <td onClick={e => e.stopPropagation()} style={{ padding: "4px 10px" }}>
                        {j.run_ids?.length > 0 ? (
                          <button
                            className="btn btn-secondary btn-sm"
                            style={{ fontSize: 11, padding: "2px 8px", whiteSpace: "nowrap" }}
                            onClick={() => navigate("/runs", { state: { tab: 0, run_id: j.run_ids[0] } })}
                            title={t("exec.jobs.open_runs_tip")}
                          >
                            {t("exec.jobs.open_runs")}
                          </button>
                        ) : (
                          <span style={{ fontSize: 10, color: "var(--text-3)" }}>—</span>
                        )}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            )}
          </div>
        </div>

        {/* Right: batch form + job detail */}
        <div style={{ display: "flex", flexDirection: "column", gap: 20 }}>

          {/* Batch run form */}
          <div className="card">
            <div className="section-title">{t("exec.batch.title")}</div>
            <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 10 }}>
              {t("exec.batch.desc")}
            </div>
            <textarea
              className="input"
              rows={5}
              placeholder={"TC-LOGIN-001\nTC-CHECKOUT-002\nTC-DEMO-003"}
              value={batchInput}
              onChange={e => setBatchInput(e.target.value)}
              style={{ width: "100%", fontFamily: "monospace", fontSize: 12, resize: "vertical" }}
            />
            <button
              className="btn btn-primary"
              onClick={handleBatch}
              disabled={batchLoading || !batchInput.trim()}
              style={{ marginTop: 10, width: "100%" }}
            >
              {batchLoading ? t("exec.batch.enqueueing") : t("exec.batch.run")}
            </button>
            {batchResult && (
              <div className={`alert ${batchResult.ok ? "alert-success" : "alert-error"}`} style={{ marginTop: 10 }}>
                {batchResult.ok
                  ? `✓ Job ${batchResult.job_id?.slice(0, 14)}… ${t("exec.batch.enqueued")} — ${batchResult.total_count} ${t("exec.batch.tests_status")} ${batchResult.status}`
                  : `✗ ${batchResult.error}`
                }
              </div>
            )}
          </div>

          {/* Job detail panel */}
          {selectedJob && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 10 }}>{t("exec.detail.title")}</div>
              {detailLoading ? (
                <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("exec.detail.loading")}</div>
              ) : jobDetail?.error && !jobDetail?.job_id ? (
                <div className="alert alert-error">{jobDetail.error}</div>
              ) : jobDetail ? (
                <div>
                  {/* Job ID */}
                  <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-3)", marginBottom: 8, wordBreak: "break-all" }}>
                    {jobDetail.job_id}
                  </div>
                  {/* Error message — prominent when present */}
                  {jobDetail.error_message && (
                    <div className="alert alert-error" style={{ marginBottom: 10, fontSize: 12 }}>
                      <span style={{ fontWeight: 700, marginRight: 6 }}>{t("exec.detail.error_message")}:</span>
                      {jobDetail.error_message}
                    </div>
                  )}
                  {/* Status badges */}
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
                    <span className={`badge ${statusClass(jobDetail.status)}`}>{jobDetail.status}</span>
                    <span className="badge badge-gray">{jobDetail.total_count} {t("exec.detail.tests")}</span>
                    <span className="badge badge-green">{jobDetail.passed_count ?? 0} {t("exec.detail.pass")}</span>
                    <span className="badge badge-red">{jobDetail.failed_count ?? 0} {t("exec.detail.fail")}</span>
                    {jobDetail.error_count > 0 && <span className="badge badge-orange">{jobDetail.error_count} {t("exec.detail.error")}</span>}
                    {jobDurationMs(jobDetail) != null && (
                      <span className="badge badge-gray">{fmtMs(jobDurationMs(jobDetail))}</span>
                    )}
                  </div>
                  {/* Timestamps */}
                  <div style={{ fontSize: 11, color: "var(--text-3)", display: "flex", flexWrap: "wrap", gap: "4px 16px", marginBottom: 10 }}>
                    {jobDetail.created_at && (
                      <span><span style={{ fontWeight: 700, color: "var(--text-2)" }}>{t("exec.detail.ts.created")}</span> {fmtDate(jobDetail.created_at)}</span>
                    )}
                    {jobDetail.started_at && (
                      <span><span style={{ fontWeight: 700, color: "var(--text-2)" }}>{t("exec.detail.ts.started")}</span> {fmtDate(jobDetail.started_at)}</span>
                    )}
                    {jobDetail.finished_at && (
                      <span><span style={{ fontWeight: 700, color: "var(--text-2)" }}>{t("exec.detail.ts.finished")}</span> {fmtDate(jobDetail.finished_at)}</span>
                    )}
                  </div>
                  {/* Correlation: parent job / retry jobs */}
                  {(jobDetail.parent_job_id || (jobDetail.retry_job_ids?.length > 0)) && (
                    <div style={{ marginBottom: 10, padding: "8px 12px", background: "var(--accent-light)", borderRadius: "var(--r-sm)", border: "1px solid var(--accent-border)", fontSize: 12 }}>
                      {jobDetail.parent_job_id && (
                        <div style={{ marginBottom: jobDetail.retry_job_ids?.length ? 6 : 0 }}>
                          <span style={{ fontWeight: 700, color: "var(--text-2)", marginRight: 6 }}>{t("exec.detail.parent_job")}:</span>
                          <span style={{ fontFamily: "monospace", marginRight: 8 }}>{jobDetail.parent_job_id}</span>
                          <button
                            className="btn btn-secondary btn-sm"
                            style={{ fontSize: 11, padding: "2px 8px" }}
                            onClick={() => openJobDetail(jobDetail.parent_job_id)}
                          >
                            {t("exec.detail.open_parent_job")}
                          </button>
                        </div>
                      )}
                      {jobDetail.retry_job_ids?.length > 0 && (
                        <div>
                          <span style={{ fontWeight: 700, color: "var(--text-2)", marginRight: 6 }}>{t("exec.detail.retry_jobs")}:</span>
                          {jobDetail.retry_job_ids.map((rid) => (
                            <span key={rid} style={{ display: "inline-flex", alignItems: "center", gap: 4, marginRight: 8, marginBottom: 4 }}>
                              <span style={{ fontFamily: "monospace", fontSize: 11 }}>{rid}</span>
                              <button
                                className="btn btn-secondary btn-sm"
                                style={{ fontSize: 10, padding: "2px 6px" }}
                                onClick={() => openJobDetail(rid)}
                              >
                                {t("exec.detail.open_retry_job")}
                              </button>
                            </span>
                          ))}
                        </div>
                      )}
                    </div>
                  )}

                  {/* Navigation to Runs */}
                  {jobDetail.run_ids?.length > 0 && (
                    <div style={{ marginBottom: 10 }}>
                      <button
                        className="btn btn-secondary btn-sm"
                        onClick={() => navigate("/runs", { state: { tab: 0, run_id: jobDetail.run_ids[0] } })}
                      >
                        {t("exec.detail.open_runs")} ({jobDetail.run_ids.length})
                      </button>
                    </div>
                  )}

                  {/* Operational actions (CI / SRE) — always visible when job selected */}
                  <div style={{ marginTop: 12, paddingTop: 10, borderTop: "1px solid var(--border)" }}>
                    <div style={{ fontSize: 12, color: "var(--text-2)", fontWeight: 700, marginBottom: 10 }}>
                      {t("exec.action.operational_actions")}
                    </div>
                    <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
                      {jobCanRetryFailed && (
                        <button
                          className="btn btn-primary btn-sm"
                          onClick={() => {
                            setRetryFailedError("");
                            setRetryEnqueuedJobId(null);
                            setRetryFailedOpen(true);
                          }}
                        >
                          {t("exec.action.retry_failed")}
                        </button>
                      )}
                      <button
                        className="btn btn-secondary btn-sm"
                        disabled={!alertingReadyState?.ready}
                        title={alertingReadyState?.ready ? undefined : (alertingReadyState?.reason || t("exec.action.alerting_not_configured"))}
                        style={!alertingReadyState?.ready ? { opacity: 0.65, cursor: "not-allowed" } : undefined}
                        onClick={() => {
                          setSendAlertError("");
                          setSendAlertSuccess(false);
                          setSendAlertConnector("slack");
                          setSendAlertRecipients("");
                          setSendAlertOpen(true);
                        }}
                      >
                        {t("exec.action.send_alert")}
                      </button>
                      <button
                        className="btn btn-secondary btn-sm"
                        disabled
                        title={t("exec.action.assign_owner_not_configured")}
                        style={{ opacity: 0.65, cursor: "not-allowed" }}
                      >
                        {t("runs.action_panel.assign_owner")}
                      </button>
                    </div>
                    {jobCanRetryFailed && (
                      <div style={{ marginTop: 10, fontSize: 12, color: "var(--text-3)", lineHeight: 1.6 }}>
                        {formatTpl(
                          t("exec.action.retry_failed_hint"),
                          { failedCount: jobFailedCount, errorCount: jobErrorCount },
                        )}
                      </div>
                    )}
                  </div>

                  {retryEnqueuedJobId && (
                    <div style={{ marginTop: 12, fontSize: 12, color: "var(--text-2)" }}>
                      {t("exec.action.retry_enqueued_prefix")}{" "}
                      <span style={{ fontFamily: "monospace" }}>{retryEnqueuedJobId}</span>
                      {" "}
                      <button
                        className="btn btn-secondary btn-sm"
                        style={{ marginLeft: 10 }}
                        onClick={() => openJobDetail(retryEnqueuedJobId)}
                      >
                        {t("exec.action.retry_open_new_job")}
                      </button>
                    </div>
                  )}

                  {/* Trigger context — shown when job originated from Risk Selection or PR Analysis */}
                  {(() => {
                    if (!jobDetail.context_json) return null;
                    try {
                      const ctx = JSON.parse(jobDetail.context_json);
                      if (!ctx?.source) return null;
                      const sourceLabel = ctx.source === "pr_analysis"
                        ? "◎ PR Analysis"
                        : ctx.source === "risk_selection"
                          ? "◈ Risk Selection"
                          : ctx.source;
                      return (
                        <div style={{ marginBottom: 10, padding: "10px 14px", background: "var(--accent-light)", borderRadius: "var(--r-sm)", border: "1px solid var(--accent-border)" }}>
                          <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
                            <span className="badge badge-blue" style={{ fontSize: 11 }}>{sourceLabel}</span>
                            {ctx.selection_type && (
                              <span className="badge badge-gray" style={{ fontSize: 10 }}>{ctx.selection_type}</span>
                            )}
                            {ctx.pr_title && (
                              <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text)" }}>{ctx.pr_title}</span>
                            )}
                            {ctx.pr_branch && (
                              <span className="badge badge-gray" style={{ fontSize: 10 }}>⎇ {ctx.pr_branch}</span>
                            )}
                            {ctx.selected_modules?.map(m => (
                              <span key={m} className="badge badge-gray" style={{ fontSize: 10 }}>{m}</span>
                            ))}
                            {ctx.selected_test_ids?.length > 0 && (
                              <span style={{ fontSize: 11, color: "var(--text-3)" }}>
                                {ctx.selected_test_ids.length} tests
                              </span>
                            )}
                          </div>
                        </div>
                      );
                    } catch { return null; }
                  })()}
                  {jobDetail.scheduling_notes && (
                    <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 10, lineHeight: 1.5 }}>
                      {jobDetail.scheduling_notes}
                    </div>
                  )}
                  {jobDetail.results?.length > 0 && (
                    <table className="data-table" style={{ fontSize: 11 }}>
                      <thead><tr>
                        <th>{t("exec.detail.col.test_case")}</th>
                        <th>{t("exec.detail.col.status")}</th>
                        <th style={{ width: 55 }}>{t("exec.detail.col.attempt")}</th>
                        <th style={{ width: 80 }}>{t("exec.detail.col.duration")}</th>
                      </tr></thead>
                      <tbody>
                        {jobDetail.results.map((r, i) => (
                          <tr
                            key={i}
                            style={{ cursor: r.run_id ? "pointer" : "default" }}
                            title={r.run_id ? t("exec.detail.open_run_tip") : undefined}
                            onClick={() => r.run_id && navigate("/runs", { state: { tab: 0, run_id: r.run_id } })}
                          >
                            <td style={{ fontFamily: "monospace" }}>
                              {r.test_case_id || "—"}
                              {r.run_id && (
                                <span style={{ marginLeft: 6, fontSize: 10, color: "var(--accent)", opacity: 0.7 }}>↗</span>
                              )}
                            </td>
                            <td><span className={`badge ${statusClass(r.status)}`}>{r.status}</span></td>
                            <td style={{ color: "var(--text-3)" }}>{r.attempt ?? 1}</td>
                            <td style={{ color: "var(--text-3)" }}>{fmtMs(r.duration_ms)}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  )}
                </div>
              ) : null}
            </div>
          )}
        </div>
      </div>

      <ConfirmModal
        open={retryFailedOpen}
        busy={retryFailedBusy}
        error={retryFailedError}
        title={t("exec.action.retry_failed_confirmation_title")}
        description={
          jobDetail
            ? formatTpl(
                t("exec.action.retry_failed_confirmation_desc"),
                {
                  jobId: jobDetail.job_id ? jobDetail.job_id.slice(0, 14) : "—",
                  failedCount: jobFailedCount,
                  errorCount: jobErrorCount,
                },
              )
            : t("exec.action.retry_failed_confirmation_desc_fallback")
        }
        onCancel={() => setRetryFailedOpen(false)}
        onConfirm={async () => {
          if (!jobDetail?.job_id) {
            setRetryFailedError(t("exec.action.retry_failed_error_missing_job_id"));
            return;
          }
          setRetryFailedBusy(true);
          setRetryFailedError("");
          try {
            const res = await retryFailed({ job_id: jobDetail.job_id });
            setRetryFailedOpen(false);
            setRetryEnqueuedJobId(res?.job_id || null);
            // Refresh job list so the new job appears in the UI.
            load();
          } catch (e) {
            setRetryFailedError(e?.message || t("exec.action.retry_failed_error_enqueue_failed"));
          } finally {
            setRetryFailedBusy(false);
          }
        }}
      />

      <ConfirmModal
        open={sendAlertOpen}
        busy={sendAlertBusy}
        error={sendAlertError}
        title={t("exec.action.send_alert_confirmation_title")}
        description={
          jobDetail
            ? formatTpl(t("exec.action.send_alert_confirmation_desc"), {
                jobId: (jobDetail.job_id || "").slice(0, 14) + (jobDetail.job_id?.length > 14 ? "…" : ""),
              })
            : "Send an alert for this job."
        }
        onCancel={() => {
          setSendAlertOpen(false);
          setSendAlertError("");
          setSendAlertSuccess(false);
        }}
        onConfirm={async () => {
          if (!jobDetail?.job_id) {
            setSendAlertError(t("exec.action.send_alert_error"));
            return;
          }
          if (sendAlertConnector === "email" && !sendAlertRecipients.trim()) {
            setSendAlertError(t("exec.action.send_alert_recipients_required"));
            return;
          }
          setSendAlertBusy(true);
          setSendAlertError("");
          try {
            const msg = [
              `Job ${jobDetail.job_id} — ${jobDetail.status}`,
              `Passed: ${jobDetail.passed_count ?? 0}, Failed: ${jobDetail.failed_count ?? 0}, Error: ${jobDetail.error_count ?? 0}`,
            ].join(". ");
            const payload = {
              connector_id: sendAlertConnector,
              job_id: jobDetail.job_id,
              run_id: jobDetail.run_ids?.[0] || undefined,
              message: msg,
            };
            if (sendAlertConnector === "email") {
              payload.recipients = sendAlertRecipients
                .split(/[\n,]+/)
                .map((s) => s.trim())
                .filter(Boolean);
            }
            await sendAlert(payload);
            setSendAlertOpen(false);
            setSendAlertSuccess(true);
          } catch (e) {
            setSendAlertError(e?.message || t("exec.action.send_alert_error"));
          } finally {
            setSendAlertBusy(false);
          }
        }}
      >
        <div style={{ marginBottom: 12, display: "flex", flexDirection: "column", gap: 10 }}>
          <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
            <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", minWidth: 72 }}>{t("exec.action.send_alert_connector")}:</span>
            <select
              className="input"
              value={sendAlertConnector}
              onChange={(e) => setSendAlertConnector(e.target.value)}
              style={{ minWidth: 140, fontSize: 12 }}
              aria-label={t("exec.action.send_alert_connector")}
            >
              <option value="slack">{t("exec.action.send_alert_connector_slack")}</option>
              <option value="email">{t("exec.action.send_alert_connector_email")}</option>
              <option value="teams">{t("exec.action.send_alert_connector_teams")}</option>
            </select>
          </div>
          {sendAlertConnector === "email" && (
            <div style={{ display: "flex", alignItems: "flex-start", gap: 8 }}>
              <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", minWidth: 72, paddingTop: 6 }}>{t("exec.action.send_alert_recipients")}:</span>
              <textarea
                className="input"
                value={sendAlertRecipients}
                onChange={(e) => setSendAlertRecipients(e.target.value)}
                placeholder={t("exec.action.send_alert_recipients_placeholder")}
                rows={2}
                style={{ flex: 1, fontSize: 12, resize: "vertical", minWidth: 180 }}
                aria-label={t("exec.action.send_alert_recipients")}
              />
            </div>
          )}
        </div>
      </ConfirmModal>

      {sendAlertSuccess && (
        <div
          className="alert alert-success"
          style={{ position: "fixed", bottom: 20, right: 20, zIndex: 9998, maxWidth: 320 }}
          role="status"
        >
          {t("exec.action.send_alert_success")}
        </div>
      )}
    </div>
  );
}
