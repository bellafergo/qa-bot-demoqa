// src/pages/BatchRunnerPage.jsx
/**
 * Batch Runner — multi-test execution from catalog + recent orchestrator jobs.
 * GET /tests, POST /execution/run-batch | POST /orchestrator/jobs/single, GET /orchestrator/jobs
 */
import React, { useState, useEffect, useCallback, useRef } from "react";
import { useLang } from "../i18n/LangContext";
import { listTests, listJobs, runBatch, enqueueSingle, getJob } from "../api";

const JOB_POLL_MS = 10_000;
const AVG_SEC_PER_TEST = 9.8;

function priorityClass(p) {
  const v = String(p || "").toLowerCase();
  if (v === "critical") return "badge-red";
  if (v === "high")     return "badge-orange";
  if (v === "medium")   return "badge-blue";
  return "badge-gray";
}

function jobStatusClass(s) {
  const v = String(s || "").toLowerCase();
  if (v === "completed") return "badge-green";
  if (v === "failed")    return "badge-red";
  if (v === "running")   return "badge-blue";
  if (v === "queued")    return "badge-orange";
  if (v === "partial")   return "badge-orange";
  return "badge-gray";
}

function fmtDate(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, { month: "short", day: "numeric", hour: "2-digit", minute: "2-digit" });
  } catch {
    return "—";
  }
}

function fmtMs(ms) {
  if (ms == null) return "—";
  return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`;
}

function jobDurationMs(j) {
  if (j.duration_ms != null) return j.duration_ms;
  if (j.started_at && j.finished_at) {
    return Math.round(new Date(j.finished_at) - new Date(j.started_at));
  }
  return null;
}

function estimateBatchSeconds(n) {
  if (n <= 0) return 0;
  const raw = n * AVG_SEC_PER_TEST;
  return Math.ceil(raw / 5) * 5;
}

export default function BatchRunnerPage() {
  const { t } = useLang();

  const [tests, setTests]           = useState([]);
  const [selectedIds, setSelectedIds] = useState(() => new Set());
  const [loadingTests, setLoadingTests] = useState(true);
  const [testsError, setTestsError] = useState("");

  const [jobs, setJobs]         = useState([]);
  const [jobsError, setJobsError] = useState("");
  const [loadingJobs, setLoadingJobs] = useState(true);

  const [batchLabel, setBatchLabel]   = useState("");
  const [environment, setEnvironment] = useState("default");
  const [runMode, setRunMode]         = useState("parallel");

  const [launching, setLaunching] = useState(false);
  const [banner, setBanner]       = useState(null);

  const [expandedJobId, setExpandedJobId] = useState(null);
  const [jobDetailById, setJobDetailById] = useState({});
  const [detailLoadingId, setDetailLoadingId] = useState(null);

  const jobsSectionRef = useRef(null);
  const pollRef        = useRef(null);

  const loadTests = useCallback(async () => {
    setLoadingTests(true);
    setTestsError("");
    try {
      const data = await listTests({ limit: 500 });
      setTests(Array.isArray(data) ? data : []);
    } catch (e) {
      setTestsError(e?.message || t("batch.tests_load_error"));
      setTests([]);
    } finally {
      setLoadingTests(false);
    }
  }, [t]);

  const loadJobs = useCallback(async (silent = false) => {
    if (!silent) setLoadingJobs(true);
    setJobsError("");
    try {
      const jl = await listJobs(20);
      setJobs(Array.isArray(jl) ? jl : []);
    } catch (e) {
      setJobsError(e?.message || t("batch.jobs_load_error"));
    } finally {
      if (!silent) setLoadingJobs(false);
    }
  }, [t]);

  useEffect(() => {
    loadTests();
    loadJobs(false);
  }, [loadTests, loadJobs]);

  useEffect(() => {
    const hasActive = jobs.some(j => j.status === "queued" || j.status === "running");
    if (hasActive && !pollRef.current) {
      pollRef.current = setInterval(() => {
        loadJobs(true);
      }, JOB_POLL_MS);
    } else if (!hasActive && pollRef.current) {
      clearInterval(pollRef.current);
      pollRef.current = null;
    }
    return () => {
      if (pollRef.current) {
        clearInterval(pollRef.current);
        pollRef.current = null;
      }
    };
  }, [jobs, loadJobs]);

  const nSel = selectedIds.size;
  const estSec = estimateBatchSeconds(nSel);

  function selectAll() {
    setSelectedIds(new Set(tests.map(tc => tc.test_case_id)));
  }
  function selectCritical() {
    setSelectedIds(new Set(tests.filter(tc => tc.priority === "critical").map(tc => tc.test_case_id)));
  }
  function selectSmoke() {
    setSelectedIds(new Set(tests.filter(tc => tc.type === "smoke").map(tc => tc.test_case_id)));
  }
  function clearSelection() {
    setSelectedIds(new Set());
  }

  function toggleId(id) {
    setSelectedIds(prev => {
      const s = new Set(prev);
      if (s.has(id)) s.delete(id);
      else s.add(id);
      return s;
    });
  }

  async function ensureJobDetail(jobId) {
    if (jobDetailById[jobId]) return;
    setDetailLoadingId(jobId);
    try {
      const d = await getJob(jobId);
      setJobDetailById(prev => ({ ...prev, [jobId]: d }));
    } catch (e) {
      setJobDetailById(prev => ({ ...prev, [jobId]: { _error: e?.message || "error" } }));
    } finally {
      setDetailLoadingId(null);
    }
  }

  function toggleJobRow(jobId) {
    if (expandedJobId === jobId) {
      setExpandedJobId(null);
      return;
    }
    setExpandedJobId(jobId);
    ensureJobDetail(jobId);
  }

  async function handleExecute() {
    const ids = [...selectedIds];
    if (!ids.length || launching) return;

    setLaunching(true);
    setBanner(null);

    try {
      if (runMode === "parallel") {
        const rb = await runBatch({ test_case_ids: ids, environment });
        const n = rb?.total_count ?? ids.length;
        setBanner({ type: "ok", text: t("batch.banner_parallel_ok").replace("{{n}}", String(n)) });
      } else {
        let ok = 0;
        const fails = [];
        for (const test_case_id of ids) {
          try {
            await enqueueSingle({ test_case_id, environment });
            ok++;
          } catch (e) {
            fails.push(`${test_case_id}: ${e?.message || "error"}`);
          }
        }
        if (ok === ids.length) {
          setBanner({ type: "ok", text: t("batch.banner_seq_ok").replace("{{n}}", String(ok)) });
        } else if (ok > 0) {
          setBanner({
            type: "warn",
            text: t("batch.banner_seq_partial")
              .replace("{{ok}}", String(ok))
              .replace("{{total}}", String(ids.length)),
            detail: fails.join("\n"),
          });
        } else {
          setBanner({ type: "err", text: t("batch.banner_seq_fail"), detail: fails.join("\n") });
        }
      }

      setSelectedIds(new Set());
      await loadJobs(true);
      setTimeout(() => {
        jobsSectionRef.current?.scrollIntoView({ behavior: "smooth", block: "start" });
      }, 100);
    } catch (e) {
      setBanner({ type: "err", text: e?.message || t("batch.banner_parallel_fail") });
    } finally {
      setLaunching(false);
    }
  }

  return (
    <div className="page-wrap">

      {banner && (
        <div
          className={`alert ${banner.type === "ok" ? "alert-success" : banner.type === "warn" ? "" : "alert-error"}`}
          style={{
            marginBottom: 16,
            fontSize: 13,
            ...(banner.type === "warn"
              ? {
                  background: "rgba(234, 179, 8, 0.12)",
                  border: "1px solid rgba(234, 179, 8, 0.35)",
                  color: "var(--text-1)",
                }
              : {}),
          }}
        >
          {banner.text}
          {banner.detail && (
            <pre style={{ margin: "8px 0 0", fontSize: 11, whiteSpace: "pre-wrap", color: "var(--text-2)" }}>
              {banner.detail}
            </pre>
          )}
        </div>
      )}

      <div style={{ display: "flex", gap: 20, flexWrap: "wrap", alignItems: "flex-start" }}>
        {/* ── Test selector ───────────────────────────────────────────── */}
        <div className="card" style={{ flex: "2 1 420px", minWidth: 280 }}>
          <div className="section-title" style={{ marginBottom: 12 }}>{t("batch.section_tests")}</div>

          {loadingTests && (
            <div style={{ padding: 16, color: "var(--text-3)" }}>…</div>
          )}
          {testsError && !loadingTests && (
            <div className="alert alert-error" style={{ marginBottom: 12 }}>
              {testsError}
              <button type="button" className="btn btn-secondary btn-sm" style={{ marginLeft: 10 }} onClick={loadTests}>
                {t("batch.retry")}
              </button>
            </div>
          )}

          {!loadingTests && !testsError && (
            <>
              <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginBottom: 12 }}>
                <button type="button" className="btn btn-secondary btn-sm" onClick={selectAll}>{t("batch.sel_all")}</button>
                <button type="button" className="btn btn-secondary btn-sm" onClick={selectCritical}>{t("batch.sel_critical")}</button>
                <button type="button" className="btn btn-secondary btn-sm" onClick={selectSmoke}>{t("batch.sel_smoke")}</button>
                <button type="button" className="btn btn-secondary btn-sm" onClick={clearSelection}>{t("batch.sel_clear")}</button>
              </div>

              <div style={{ overflowX: "auto" }}>
                <table className="data-table" style={{ fontSize: 12 }}>
                  <thead>
                    <tr>
                      <th style={{ width: 36 }} />
                      <th>{t("batch.col_name")}</th>
                      <th>{t("batch.col_module")}</th>
                      <th>{t("batch.col_type")}</th>
                      <th>{t("batch.col_priority")}</th>
                      <th>{t("batch.col_steps")}</th>
                    </tr>
                  </thead>
                  <tbody>
                    {tests.map(tc => (
                      <tr key={tc.test_case_id}>
                        <td>
                          <input
                            type="checkbox"
                            checked={selectedIds.has(tc.test_case_id)}
                            onChange={() => toggleId(tc.test_case_id)}
                          />
                        </td>
                        <td style={{ fontWeight: 500 }}>{tc.name}</td>
                        <td><span className="badge badge-gray" style={{ fontSize: 10 }}>{tc.module}</span></td>
                        <td><span className="badge badge-blue" style={{ fontSize: 10 }}>{tc.type}</span></td>
                        <td><span className={`badge ${priorityClass(tc.priority)}`} style={{ fontSize: 10 }}>{tc.priority}</span></td>
                        <td style={{ color: "var(--text-2)" }}>{tc.steps_count ?? "—"}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </>
          )}
        </div>

        {/* ── Config ─────────────────────────────────────────────────── */}
        <div className="card" style={{ flex: "1 1 260px", minWidth: 240 }}>
          <div className="section-title" style={{ marginBottom: 12 }}>{t("batch.section_config")}</div>

          <div style={{ marginBottom: 12 }}>
            <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 4 }}>{t("batch.label_batch")}</div>
            <input
              className="input"
              style={{ width: "100%" }}
              placeholder={t("batch.label_placeholder")}
              value={batchLabel}
              onChange={e => setBatchLabel(e.target.value)}
              disabled={launching}
            />
          </div>

          <div style={{ marginBottom: 12 }}>
            <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 4 }}>{t("batch.env")}</div>
            <select
              className="input"
              style={{ width: "100%" }}
              value={environment}
              onChange={e => setEnvironment(e.target.value)}
              disabled={launching}
            >
              <option value="default">default</option>
              <option value="staging">staging</option>
              <option value="production">production</option>
            </select>
          </div>

          <div style={{ marginBottom: 12 }}>
            <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>{t("batch.mode")}</div>
            <label style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 6, fontSize: 13 }}>
              <input
                type="radio"
                name="runMode"
                checked={runMode === "parallel"}
                onChange={() => setRunMode("parallel")}
                disabled={launching}
              />
              {t("batch.mode_parallel")}
            </label>
            <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: 13 }}>
              <input
                type="radio"
                name="runMode"
                checked={runMode === "sequential"}
                onChange={() => setRunMode("sequential")}
                disabled={launching}
              />
              {t("batch.mode_sequential")}
            </label>
          </div>

          <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 12 }}>
            {t("batch.count_est")
              .replace("{{n}}", String(nSel))
              .replace("{{s}}", String(estSec))}
          </div>

          <button
            type="button"
            className="btn btn-primary"
            style={{ width: "100%" }}
            disabled={nSel === 0 || launching}
            onClick={handleExecute}
          >
            {launching ? t("batch.launching") : t("batch.run_btn").replace("{{n}}", String(nSel))}
          </button>
        </div>
      </div>

      {/* ── Recent jobs ─────────────────────────────────────────────── */}
      <div ref={jobsSectionRef} className="card" style={{ marginTop: 20 }}>
        <div className="section-title" style={{ marginBottom: 12 }}>{t("batch.section_jobs")}</div>

        {jobsError && (
          <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>{jobsError}</div>
        )}

        {loadingJobs && !jobs.length && !jobsError && (
          <div style={{ padding: 16, color: "var(--text-3)" }}>…</div>
        )}

        {!loadingJobs && !jobs.length && !jobsError && (
          <div style={{ padding: 24, textAlign: "center", color: "var(--text-3)", fontSize: 13 }}>
            {t("batch.jobs_empty")}
          </div>
        )}

        {jobs.length > 0 && (
          <div style={{ overflowX: "auto" }}>
            <table className="data-table" style={{ fontSize: 12 }}>
              <thead>
                <tr>
                  <th>{t("batch.job_col_id")}</th>
                  <th>{t("batch.job_col_tests")}</th>
                  <th>{t("batch.job_col_status")}</th>
                  <th>{t("batch.job_col_started")}</th>
                  <th>{t("batch.job_col_duration")}</th>
                  <th>{t("batch.job_col_action")}</th>
                </tr>
              </thead>
              <tbody>
                {jobs.map(j => (
                  <React.Fragment key={j.job_id}>
                    <tr>
                      <td style={{ fontFamily: "monospace", fontSize: 11 }}>{(j.job_id || "").slice(0, 12)}…</td>
                      <td>{j.total_count ?? (j.test_case_ids?.length ?? "—")}</td>
                      <td><span className={`badge ${jobStatusClass(j.status)}`}>{j.status}</span></td>
                      <td style={{ color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(j.started_at || j.created_at)}</td>
                      <td style={{ color: "var(--text-2)" }}>{fmtMs(jobDurationMs(j))}</td>
                      <td>
                        <button type="button" className="btn btn-secondary btn-sm" onClick={() => toggleJobRow(j.job_id)}>
                          {expandedJobId === j.job_id ? t("batch.job_hide") : t("batch.job_detail")}
                        </button>
                      </td>
                    </tr>
                    {expandedJobId === j.job_id && (
                      <tr>
                        <td colSpan={6} style={{ background: "var(--surface-2)", padding: 12, verticalAlign: "top" }}>
                          {detailLoadingId === j.job_id && <div style={{ fontSize: 12 }}>…</div>}
                          {!detailLoadingId && jobDetailById[j.job_id]?._error && (
                            <div className="alert alert-error" style={{ margin: 0, fontSize: 12 }}>
                              {jobDetailById[j.job_id]._error}
                            </div>
                          )}
                          {!detailLoadingId && jobDetailById[j.job_id] && !jobDetailById[j.job_id]._error && (
                            <JobDetailPanel job={jobDetailById[j.job_id]} t={t} />
                          )}
                        </td>
                      </tr>
                    )}
                  </React.Fragment>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  );
}

function JobDetailPanel({ job, t }) {
  const r = job.results;
  const hasResults = Array.isArray(r) && r.length > 0;

  return (
    <div style={{ fontSize: 12 }}>
      <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fill, minmax(160px, 1fr))", gap: 8, marginBottom: 10 }}>
        <div><span style={{ color: "var(--text-3)" }}>job_id</span>{" "}<span style={{ fontFamily: "monospace" }}>{job.job_id}</span></div>
        <div><span style={{ color: "var(--text-3)" }}>job_type</span>{" "}{job.job_type}</div>
        <div><span style={{ color: "var(--text-3)" }}>status</span>{" "}{job.status}</div>
        <div><span style={{ color: "var(--text-3)" }}>environment</span>{" "}{job.environment}</div>
        <div><span style={{ color: "var(--text-3)" }}>total_count</span>{" "}{job.total_count}</div>
        <div><span style={{ color: "var(--text-3)" }}>completed</span>{" "}{job.completed_count}</div>
        <div><span style={{ color: "var(--text-3)" }}>passed / failed / error</span>{" "}
          {job.passed_count} / {job.failed_count} / {job.error_count}
        </div>
        {job.error_message && (
          <div style={{ gridColumn: "1 / -1", color: "var(--red)" }}>error_message: {job.error_message}</div>
        )}
        {Array.isArray(job.retry_job_ids) && job.retry_job_ids.length > 0 && (
          <div style={{ gridColumn: "1 / -1" }}>
            <span style={{ color: "var(--text-3)" }}>retry_job_ids</span>{" "}
            <span style={{ fontFamily: "monospace", fontSize: 11 }}>{job.retry_job_ids.join(", ")}</span>
          </div>
        )}
      </div>

      {hasResults && (
        <>
          <div style={{ fontWeight: 600, marginBottom: 6 }}>{t("batch.detail_results")}</div>
          <table className="data-table" style={{ fontSize: 11 }}>
            <thead>
              <tr>
                <th>test_case_id</th>
                <th>status</th>
                <th>run_id</th>
                <th>duration_ms</th>
              </tr>
            </thead>
            <tbody>
              {r.map((row, i) => (
                <tr key={i}>
                  <td style={{ fontFamily: "monospace" }}>{row.test_case_id ?? "—"}</td>
                  <td>{row.status ?? "—"}</td>
                  <td style={{ fontFamily: "monospace", fontSize: 10 }}>{(row.run_id || "").slice(0, 14) || "—"}</td>
                  <td>{row.duration_ms ?? "—"}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </>
      )}

      {!hasResults && (
        <pre style={{
          margin: 0,
          padding: 10,
          background: "var(--bg)",
          borderRadius: 6,
          fontSize: 10,
          overflow: "auto",
          maxHeight: 220,
        }}
        >
          {JSON.stringify(job, null, 2)}
        </pre>
      )}
    </div>
  );
}
