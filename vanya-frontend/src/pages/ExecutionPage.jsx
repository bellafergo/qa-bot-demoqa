// src/pages/ExecutionPage.jsx
/**
 * Execution Center — worker pool, job queue, and batch execution.
 * GET /execution/status, GET /orchestrator/jobs, POST /execution/run-batch
 */
import React, { useState, useEffect, useCallback } from "react";
import { getExecStatus, listJobs, runBatch, getJob } from "../api";
import { useLang } from "../i18n/LangContext";

function statusClass(s) {
  const v = String(s || "").toLowerCase();
  if (v === "completed")              return "badge-green";
  if (v === "failed" || v === "error") return "badge-red";
  if (v === "running")                return "badge-blue";
  if (v === "queued")                 return "badge-orange";
  if (v === "partial")                return "badge-orange";
  return "badge-gray";
}

function fmtDate(iso) {
  if (!iso) return "—";
  try { return new Date(iso).toLocaleString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" }); }
  catch { return "—"; }
}

export default function ExecutionPage() {
  const { t } = useLang();

  const [status, setStatus]         = useState(null);
  const [jobs, setJobs]             = useState([]);
  const [loading, setLoading]       = useState(true);
  const [error, setError]           = useState("");

  // Batch run form
  const [batchInput, setBatchInput] = useState("");
  const [batchLoading, setBatchLoading] = useState(false);
  const [batchResult, setBatchResult]   = useState(null);

  // Job detail
  const [selectedJob, setSelectedJob] = useState(null);
  const [jobDetail, setJobDetail]     = useState(null);
  const [detailLoading, setDetailLoading] = useState(false);

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

  useEffect(() => { load(); }, [load]);

  async function handleBatch() {
    const ids = batchInput.split(/[\n,]+/).map(s => s.trim()).filter(Boolean);
    if (!ids.length) return;
    setBatchLoading(true);
    setBatchResult(null);
    try {
      const r = await runBatch({ test_case_ids: ids });
      setBatchResult({ ok: true, ...r });
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

  const st = status || {};

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
              <div className="section-title" style={{ margin: 0 }}>
                {loading ? t("exec.jobs.loading") : `${jobs.length} ${jobs.length !== 1 ? t("exec.jobs.recent_plural") : t("exec.jobs.recent")}`}
              </div>
              <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>{t("exec.jobs.refresh")}</button>
            </div>
            {jobs.length === 0 && !loading ? (
              <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("exec.jobs.none")}</div>
            ) : (
              <table className="data-table">
                <thead><tr>
                  <th>{t("exec.jobs.col.job_id")}</th>
                  <th>{t("exec.jobs.col.type")}</th>
                  <th>{t("exec.jobs.col.status")}</th>
                  <th>{t("exec.jobs.col.tests")}</th>
                  <th>{t("exec.jobs.col.pass_fail")}</th>
                  <th>{t("exec.jobs.col.created")}</th>
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
                      <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(j.created_at)}</td>
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
              ) : jobDetail?.error ? (
                <div className="alert alert-error">{jobDetail.error}</div>
              ) : jobDetail ? (
                <div>
                  <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-3)", marginBottom: 8, wordBreak: "break-all" }}>
                    {jobDetail.job_id}
                  </div>
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
                    <span className={`badge ${statusClass(jobDetail.status)}`}>{jobDetail.status}</span>
                    <span className="badge badge-gray">{jobDetail.total_count} {t("exec.detail.tests")}</span>
                    <span className="badge badge-green">{jobDetail.passed_count ?? 0} {t("exec.detail.pass")}</span>
                    <span className="badge badge-red">{jobDetail.failed_count ?? 0} {t("exec.detail.fail")}</span>
                    {jobDetail.error_count > 0 && <span className="badge badge-orange">{jobDetail.error_count} {t("exec.detail.error")}</span>}
                  </div>
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
                        <th>{t("exec.detail.col.attempt")}</th>
                      </tr></thead>
                      <tbody>
                        {jobDetail.results.map((r, i) => (
                          <tr key={i}>
                            <td style={{ fontFamily: "monospace" }}>{r.test_case_id || "—"}</td>
                            <td><span className={`badge ${statusClass(r.status)}`}>{r.status}</span></td>
                            <td style={{ color: "var(--text-3)" }}>{r.attempt ?? 1}</td>
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
    </div>
  );
}
