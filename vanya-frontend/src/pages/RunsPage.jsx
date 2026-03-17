// src/pages/RunsPage.jsx
/**
 * Runs & RCA — Evidence Lookup + Run History with root cause and business risk analysis.
 * GET /runs/{id} | GET /test-runs | POST /rca/analyze | POST /business-risk/analyze
 */
import React, { useState, useEffect, useCallback } from "react";
import { listTestRuns, getTestRun, analyzeRCA, analyzeRisk, getRunClusters } from "../api";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

const TABS = ["Evidence Lookup", "Run History"];

// ── helpers ──────────────────────────────────────────────────────────────────

function statusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s === "pass" || s === "completed") return "badge badge-green";
  if (s === "fail" || s === "failed" || s === "error") return "badge badge-red";
  if (s === "running") return "badge badge-blue";
  return "badge badge-gray";
}

function stepStatusColor(status) {
  const s = String(status || "").toLowerCase();
  if (s.includes("pass")) return "var(--green)";
  if (s.includes("fail") || s.includes("error")) return "var(--red)";
  return "var(--text-3)";
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

function RiskBadge({ level }) {
  if (!level) return null;
  const v = String(level).toLowerCase();
  if (v === "high")   return <span className="badge badge-red">HIGH RISK</span>;
  if (v === "medium") return <span className="badge badge-orange">MEDIUM RISK</span>;
  return <span className="badge badge-green">LOW RISK</span>;
}

// ── Evidence helpers ──────────────────────────────────────────────────────────

function toDataUri(b64) {
  if (!b64) return null;
  return b64.startsWith("data:") ? b64 : `data:image/png;base64,${b64}`;
}

function getScreenshotSrc(detail) {
  // 1. top-level screenshot_b64 (present in chat/execute runs)
  if (detail.screenshot_b64) return toDataUri(detail.screenshot_b64);
  // 2. meta.screenshot_b64 / meta.screenshot_url
  const meta = detail.meta || {};
  if (meta.screenshot_b64) return toDataUri(meta.screenshot_b64);
  if (meta.screenshot_url) return meta.screenshot_url;
  // 3. last step_result that has a screenshot
  const steps = detail.steps_result || [];
  for (let i = steps.length - 1; i >= 0; i--) {
    const s = steps[i];
    if (s?.screenshot_b64) return toDataUri(s.screenshot_b64);
    if (s?.screenshot_url) return s.screenshot_url;
  }
  return null;
}

function EvidenceCard({ detail }) {
  const screenshotSrc = getScreenshotSrc(detail);
  const hasLinks = detail.evidence_url || detail.report_url;
  const hasAnything = screenshotSrc || hasLinks;

  return (
    <div className="card">
      <div className="section-title" style={{ marginBottom: 10 }}>Evidence</div>
      {!hasAnything ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>No evidence available for this run.</div>
      ) : (
        <>
          {screenshotSrc && (
            <img
              src={screenshotSrc}
              alt="Run evidence screenshot"
              style={{
                width: "100%",
                borderRadius: 6,
                border: "1px solid var(--border)",
                objectFit: "contain",
                marginBottom: hasLinks ? 10 : 0,
                display: "block",
              }}
            />
          )}
          {hasLinks && (
            <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
              {detail.evidence_url && (
                <a href={detail.evidence_url} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">
                  Open Evidence ↗
                </a>
              )}
              {detail.report_url && (
                <a href={detail.report_url} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">
                  Open Report ↗
                </a>
              )}
            </div>
          )}
        </>
      )}
    </div>
  );
}

function FailureAnalysisPanel({ fa, style }) {
  if (!fa) return null;
  const typeCls = (() => {
    const t = String(fa.failure_type || "").toLowerCase();
    if (t === "navigation_failed") return "badge badge-red";
    if (t === "unknown")           return "badge badge-gray";
    return "badge badge-orange";
  })();
  const confCls = (() => {
    const c = String(fa.confidence || "").toLowerCase();
    if (c === "high")   return "badge badge-green";
    if (c === "medium") return "badge badge-orange";
    return "badge badge-gray";
  })();
  return (
    <div className="card" style={style}>
      <div className="section-title" style={{ marginBottom: 12 }}>Failure Analysis</div>
      <div style={{ display: "grid", gridTemplateColumns: "90px 1fr", gap: "8px 12px", alignItems: "center" }}>
        <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>Type</div>
        <div><span className={typeCls}>{fa.failure_type || "—"}</span></div>
        <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>Layer</div>
        <div style={{ fontSize: 12, color: "var(--text-2)" }}>{fa.layer || "—"}</div>
        {fa.target && (
          <>
            <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>Target</div>
            <div><code style={{ fontSize: 11, color: "var(--text)" }}>{fa.target}</code></div>
          </>
        )}
        <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>Confidence</div>
        <div><span className={confCls}>{fa.confidence || "—"}</span></div>
      </div>
    </div>
  );
}

function FailureClustersPanel({ clusters, loading }) {
  if (loading) {
    return (
      <div className="card" style={{ marginBottom: 24 }}>
        <div className="section-title" style={{ marginBottom: 10 }}>Failure Clusters</div>
        <div style={{ fontSize: 13, color: "var(--text-3)" }}>Loading clusters…</div>
      </div>
    );
  }
  return (
    <div className="card" style={{ marginBottom: 24, padding: 0, overflow: "hidden" }}>
      <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", gap: 10 }}>
        <div className="section-title" style={{ margin: 0 }}>Failure Clusters</div>
        {clusters.length > 0 && <span className="badge badge-red">{clusters.length} cluster{clusters.length !== 1 ? "s" : ""}</span>}
      </div>
      {clusters.length === 0 ? (
        <div style={{ padding: "16px 20px", fontSize: 13, color: "var(--text-3)" }}>No clustered failures yet.</div>
      ) : (
        <table className="data-table">
          <thead>
            <tr>
              <th>Failure Type</th>
              <th>Target</th>
              <th style={{ width: 80, textAlign: "right" }}>Count</th>
            </tr>
          </thead>
          <tbody>
            {clusters.map((c) => {
              const typeCls = (() => {
                const t = String(c.failure_type || "").toLowerCase();
                if (t === "navigation_failed") return "badge badge-red";
                if (t === "unknown")           return "badge badge-gray";
                return "badge badge-orange";
              })();
              return (
                <tr key={c.cluster_id}>
                  <td><span className={typeCls}>{c.failure_type || "—"}</span></td>
                  <td style={{ fontSize: 12, color: "var(--text-2)", fontFamily: "monospace" }}>
                    {c.target || <span style={{ color: "var(--text-3)" }}>—</span>}
                  </td>
                  <td style={{ textAlign: "right", fontWeight: 700, fontSize: 14, color: "var(--text)" }}>{c.count}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      )}
    </div>
  );
}

// ── Evidence Lookup tab ───────────────────────────────────────────────────────

function EvidenceLookupTab() {
  const [evidenceId, setEvidenceId] = useState("");
  const [loading, setLoading]       = useState(false);
  const [run, setRun]               = useState(null);
  const [error, setError]           = useState("");

  const handleFetch = useCallback(async () => {
    const id = evidenceId.trim();
    if (!id) return;
    setLoading(true);
    setError("");
    setRun(null);
    try {
      const res = await fetch(`${API_BASE}/runs/${id}?format=json`, {
        method: "GET",
        headers: { Accept: "application/json" },
      });
      if (!res.ok) {
        if (res.status === 404) {
          setError(`Run not found: ${id}`);
        } else {
          const data = await res.json().catch(() => ({}));
          setError(data?.detail || `HTTP ${res.status}`);
        }
      } else {
        const data = await res.json();
        setRun(data);
      }
    } catch (e) {
      setError(e?.message || "Network error");
    } finally {
      setLoading(false);
    }
  }, [evidenceId]);

  const healedEntries = (run?.resolution_log || []).filter(e => e?.fallback_used === true);
  const hasHealing = healedEntries.length > 0;

  const passedSteps = run?.steps?.filter(s => String(s.status || "").toLowerCase().includes("pass")).length ?? 0;
  const failedSteps = run?.steps?.filter(s => {
    const s2 = String(s.status || "").toLowerCase();
    return s2.includes("fail") || s2.includes("error");
  }).length ?? 0;
  const totalSteps = run?.steps?.length ?? 0;

  return (
    <>
      <div className="card" style={{ marginBottom: 24 }}>
        <div className="page-header" style={{ marginBottom: 16 }}>
          <h1 className="page-title">Evidence Lookup</h1>
          <p className="page-subtitle">Retrieve test execution results by evidence ID</p>
        </div>
        <div style={{ display: "flex", gap: 10 }}>
          <input
            className="input"
            value={evidenceId}
            onChange={e => setEvidenceId(e.target.value)}
            onKeyDown={e => e.key === "Enter" && handleFetch()}
            placeholder="Evidence ID (e.g., EV-abc123…)"
            style={{ flex: 1 }}
          />
          <button className="btn btn-primary" onClick={handleFetch} disabled={loading || !evidenceId.trim()}>
            {loading ? "Loading…" : "Fetch Run"}
          </button>
        </div>
        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {run && (
        <>
          <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 20, flexWrap: "wrap" }}>
            <h2 style={{ margin: 0, fontSize: 18, fontWeight: 800, color: "var(--text)" }}>Run Details</h2>
            <span className={statusBadgeClass(run.status)}>{run.status || "unknown"}</span>
            {run.evidence_id && <code style={{ fontSize: 12, color: "var(--text-2)" }}>{run.evidence_id}</code>}
            {hasHealing && <span className="badge badge-orange" title={`${healedEntries.length} selector(s) auto-healed`}>⚡ Auto-healed ({healedEntries.length})</span>}
          </div>

          <div className="kpi-grid">
            {run.duration_ms != null && (
              <div className="kpi-card">
                <div className="kpi-label">Duration</div>
                <div className="kpi-value">{run.duration_ms}<span style={{ fontSize: 14, fontWeight: 600, color: "var(--text-2)" }}>ms</span></div>
              </div>
            )}
            {totalSteps > 0 && <div className="kpi-card"><div className="kpi-label">Total Steps</div><div className="kpi-value">{totalSteps}</div></div>}
            {totalSteps > 0 && <div className="kpi-card"><div className="kpi-label">Passed</div><div className="kpi-value" style={{ color: "var(--green)" }}>{passedSteps}</div></div>}
            {totalSteps > 0 && failedSteps > 0 && <div className="kpi-card"><div className="kpi-label">Failed</div><div className="kpi-value" style={{ color: "var(--red)" }}>{failedSteps}</div></div>}
            {run.expected && <div className="kpi-card"><div className="kpi-label">Expected</div><div style={{ fontSize: 14, fontWeight: 700, color: "var(--text)", marginTop: 4 }}>{run.expected}</div></div>}
            {run.outcome && <div className="kpi-card"><div className="kpi-label">Outcome</div><div style={{ fontSize: 14, fontWeight: 700, color: "var(--text)", marginTop: 4 }}>{run.outcome}</div></div>}
          </div>

          {(run.meta?.base_url || run.evidence_url || run.report_url) && (
            <div className="card" style={{ marginBottom: 20 }}>
              <div style={{ display: "flex", gap: 20, flexWrap: "wrap", alignItems: "center" }}>
                {run.meta?.base_url && (
                  <div>
                    <div className="section-title" style={{ marginBottom: 2 }}>Base URL</div>
                    <div style={{ fontSize: 13, color: "var(--text-2)", wordBreak: "break-all" }}>{run.meta.base_url}</div>
                  </div>
                )}
                <div style={{ marginLeft: "auto", display: "flex", gap: 10, flexShrink: 0 }}>
                  {run.evidence_url && <a href={run.evidence_url} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">View Evidence ↗</a>}
                  {run.report_url && <a href={run.report_url} target="_blank" rel="noreferrer" className="btn btn-primary btn-sm">Download Report ↗</a>}
                </div>
              </div>
            </div>
          )}

          {run.reason && (
            <div className="card" style={{ marginBottom: 20 }}>
              <div className="section-title">Failure Reason</div>
              <div style={{ fontSize: 13, color: "var(--text)", lineHeight: 1.6 }}>{run.reason}</div>
            </div>
          )}

          <FailureAnalysisPanel fa={run.failure_analysis} style={{ marginBottom: 20 }} />

          {run.steps?.length > 0 && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
                <div className="section-title" style={{ margin: 0 }}>Execution Steps — {totalSteps} total</div>
              </div>
              <div style={{ overflowX: "auto" }}>
                <table className="data-table">
                  <thead><tr><th style={{ width: 44 }}>#</th><th>Action</th><th>Target / URL</th><th>Status</th><th>Duration</th></tr></thead>
                  <tbody>
                    {run.steps.map((step, i) => (
                      <tr key={i}>
                        <td style={{ color: "var(--text-3)", fontWeight: 600 }}>{step.index ?? i + 1}</td>
                        <td><code style={{ fontSize: 12 }}>{step.action || "—"}</code></td>
                        <td style={{ maxWidth: 320, wordBreak: "break-all", fontSize: 12, color: "var(--text-2)" }}>{step.url || step.selector || step.value || "—"}</td>
                        <td><span style={{ fontSize: 11, fontWeight: 700, color: stepStatusColor(step.status), textTransform: "uppercase", letterSpacing: "0.03em" }}>{step.status || "—"}</span></td>
                        <td style={{ color: "var(--text-3)", fontSize: 12, whiteSpace: "nowrap" }}>{step.duration_ms ? `${step.duration_ms}ms` : "—"}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {hasHealing && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", gap: 10 }}>
                <div className="section-title" style={{ margin: 0 }}>Selector Healing</div>
                <span className="badge badge-orange">⚡ {healedEntries.length} auto-healed</span>
              </div>
              <div style={{ overflowX: "auto" }}>
                <table className="data-table">
                  <thead><tr><th style={{ width: 44 }}>Step</th><th>Action</th><th>Original Selector</th><th>Healed Selector</th><th style={{ width: 110 }}>Strategy</th></tr></thead>
                  <tbody>
                    {healedEntries.map((e, i) => (
                      <tr key={i}>
                        <td style={{ color: "var(--text-3)", fontWeight: 600 }}>{e.step_index != null ? e.step_index + 1 : "—"}</td>
                        <td><code style={{ fontSize: 12 }}>{e.action || "—"}</code></td>
                        <td style={{ fontSize: 12, color: "var(--red)", wordBreak: "break-all", maxWidth: 200 }}>{e.original_selector || e.primary || "—"}</td>
                        <td style={{ fontSize: 12, color: "var(--green)", wordBreak: "break-all", maxWidth: 200 }}>{e.resolved || "—"}</td>
                        <td><span className="badge badge-gray" style={{ fontSize: 11 }}>{e.fallback_type || e.used || "—"}</span></td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {run.logs?.length > 0 && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
                <div className="section-title" style={{ margin: 0 }}>Execution Logs</div>
              </div>
              <pre className="code-block" style={{ margin: 0, borderRadius: 0, border: "none", maxHeight: 320, overflow: "auto" }}>
                {run.logs.join("\n")}
              </pre>
            </div>
          )}

          {run.screenshot_b64 && (
            <div className="card">
              <div className="section-title">Screenshot</div>
              <img
                src={run.screenshot_b64.startsWith("data:image/") ? run.screenshot_b64 : `data:image/png;base64,${run.screenshot_b64}`}
                alt="Test run screenshot"
                style={{ maxWidth: "100%", borderRadius: "var(--r-sm)", border: "1px solid var(--border)", marginTop: 4 }}
              />
            </div>
          )}
        </>
      )}
    </>
  );
}

// ── Run History tab ───────────────────────────────────────────────────────────

function RunHistoryTab() {
  const [runs, setRuns]             = useState([]);
  const [loading, setLoading]       = useState(true);
  const [error, setError]           = useState("");
  const [selected, setSelected]     = useState(null); // run_id
  const [detail, setDetail]         = useState(null);
  const [detailLoading, setDetailLoading] = useState(false);

  // RCA / risk panels
  const [rcaResult, setRcaResult]   = useState(null);
  const [rcaLoading, setRcaLoading] = useState(false);
  const [riskResult, setRiskResult] = useState(null);
  const [riskLoading, setRiskLoading] = useState(false);

  // Failure clusters
  const [clusters, setClusters]           = useState([]);
  const [clustersLoading, setClustersLoading] = useState(true);

  const loadClusters = useCallback(async () => {
    setClustersLoading(true);
    try {
      const data = await getRunClusters(100);
      setClusters(Array.isArray(data) ? data : []);
    } catch {
      setClusters([]);
    } finally {
      setClustersLoading(false);
    }
  }, []);

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const data = await listTestRuns({ limit: 100 });
      setRuns(Array.isArray(data) ? data : (data?.runs ?? []));
    } catch (e) {
      setError(e?.message || "Failed to load runs");
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { load(); loadClusters(); }, [load, loadClusters]);

  async function openDetail(run_id) {
    setSelected(run_id);
    setDetail(null);
    setRcaResult(null);
    setRiskResult(null);
    setDetailLoading(true);
    try {
      const d = await getTestRun(run_id);
      setDetail(d);
    } catch (e) {
      setDetail({ error: e?.message });
    } finally {
      setDetailLoading(false);
    }
  }

  async function handleRCA() {
    if (!detail || detail.error) return;
    setRcaLoading(true);
    setRcaResult(null);
    try {
      const res = await analyzeRCA({ run_id: selected, run: detail });
      setRcaResult(res);
    } catch (e) {
      setRcaResult({ error: e?.message });
    } finally {
      setRcaLoading(false);
    }
  }

  async function handleRisk() {
    if (!detail || detail.error) return;
    setRiskLoading(true);
    setRiskResult(null);
    try {
      const res = await analyzeRisk({ run_id: selected, run: detail, rca: rcaResult });
      setRiskResult(res);
    } catch (e) {
      setRiskResult({ error: e?.message });
    } finally {
      setRiskLoading(false);
    }
  }

  return (
    <div>
      <FailureClustersPanel clusters={clusters} loading={clustersLoading} />

      <div style={{ display: "grid", gridTemplateColumns: "minmax(0,2fr) minmax(0,1fr)", gap: 24, alignItems: "start" }}>

      {/* Runs list */}
      <div>
        {error && <div className="alert alert-error" style={{ marginBottom: 12 }}>{error}</div>}
        <div className="card" style={{ padding: 0, overflow: "hidden" }}>
          <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between" }}>
            <div className="section-title" style={{ margin: 0 }}>
              {loading ? "Loading…" : `${runs.length} run${runs.length !== 1 ? "s" : ""}`}
            </div>
            <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>↻ Refresh</button>
          </div>

          {runs.length === 0 && !loading ? (
            <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>No runs recorded yet.</div>
          ) : (
            <table className="data-table">
              <thead><tr>
                <th>Run ID</th>
                <th>Test Case</th>
                <th>Status</th>
                <th>Duration</th>
                <th>Created</th>
              </tr></thead>
              <tbody>
                {runs.map((r, i) => (
                  <tr
                    key={r.run_id || i}
                    style={{ cursor: "pointer", background: selected === r.run_id ? "var(--accent-light)" : undefined }}
                    onClick={() => openDetail(r.run_id)}
                  >
                    <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>
                      {(r.run_id || "").slice(0, 14)}…
                    </td>
                    <td style={{ fontWeight: 600, fontSize: 13 }}>{r.test_case_id || "—"}</td>
                    <td><span className={statusBadgeClass(r.status)}>{r.status || "—"}</span></td>
                    <td style={{ fontSize: 12, color: "var(--text-3)" }}>{fmtMs(r.duration_ms)}</td>
                    <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(r.created_at)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
        </div>
      </div>

      {/* Detail panel */}
      {selected && (
        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>


          {/* Run summary */}
          <div className="card">
            <div className="section-title" style={{ marginBottom: 10 }}>Run Detail</div>
            {detailLoading ? (
              <div style={{ fontSize: 13, color: "var(--text-3)" }}>Loading…</div>
            ) : detail?.error ? (
              <div className="alert alert-error">{detail.error}</div>
            ) : detail ? (
              <>
                <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-3)", marginBottom: 8, wordBreak: "break-all" }}>
                  {detail.run_id}
                </div>
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
                  <span className={statusBadgeClass(detail.status)}>{detail.status}</span>
                  {detail.duration_ms != null && <span className="badge badge-gray">{fmtMs(detail.duration_ms)}</span>}
                  {detail.test_case_id && <span className="badge badge-gray">{detail.test_case_id}</span>}
                </div>
                {detail.reason && (
                  <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.5, marginBottom: 8 }}>
                    <strong>Reason:</strong> {detail.reason}
                  </div>
                )}
                {detail.steps?.length > 0 && (
                  <div style={{ fontSize: 12, color: "var(--text-3)" }}>
                    {detail.steps.length} step(s) — {detail.steps.filter(s => String(s.status || "").toLowerCase().includes("pass")).length} passed
                  </div>
                )}

                {/* RCA / Risk buttons */}
                <div style={{ display: "flex", gap: 8, marginTop: 14, flexWrap: "wrap" }}>
                  <button className="btn btn-secondary btn-sm" onClick={handleRCA} disabled={rcaLoading}>
                    {rcaLoading ? "Analyzing…" : "◎ RCA"}
                  </button>
                  <button className="btn btn-secondary btn-sm" onClick={handleRisk} disabled={riskLoading}>
                    {riskLoading ? "Analyzing…" : "⚠ Risk"}
                  </button>
                </div>
              </>
            ) : null}
          </div>

          {/* Evidence */}
          {detail && !detail.error && <EvidenceCard detail={detail} />}

          {/* Failure Analysis */}
          <FailureAnalysisPanel fa={detail?.failure_analysis} />

          {/* RCA result */}
          {rcaResult && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 10 }}>Root Cause Analysis</div>
              {rcaResult.error ? (
                <div className="alert alert-error">{rcaResult.error}</div>
              ) : (
                <>
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
                    {rcaResult.root_cause_category && <span className="badge badge-blue">{rcaResult.root_cause_category}</span>}
                    {rcaResult.impacted_layer && <span className="badge badge-gray">{rcaResult.impacted_layer}</span>}
                    {rcaResult.confidence && <span className="badge badge-gray">confidence: {rcaResult.confidence}</span>}
                  </div>
                  {rcaResult.summary && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginBottom: 8 }}>{rcaResult.summary}</div>
                  )}
                  {rcaResult.probable_cause && (
                    <div style={{ fontSize: 12, color: "var(--text)", fontWeight: 600, marginBottom: 6 }}>
                      Probable cause: {rcaResult.probable_cause}
                    </div>
                  )}
                  {rcaResult.recommended_action && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                      <strong>Recommended:</strong> {rcaResult.recommended_action}
                    </div>
                  )}
                </>
              )}
            </div>
          )}

          {/* Business risk result */}
          {riskResult && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 10 }}>Business Risk</div>
              {riskResult.error ? (
                <div className="alert alert-error">{riskResult.error}</div>
              ) : (
                <>
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
                    <RiskBadge level={riskResult.risk_level} />
                    {riskResult.confidence && <span className="badge badge-gray">confidence: {riskResult.confidence}</span>}
                  </div>
                  {riskResult.summary && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginBottom: 8 }}>{riskResult.summary}</div>
                  )}
                  {riskResult.impacted_business_areas?.length > 0 && (
                    <div style={{ marginBottom: 8 }}>
                      <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 4 }}>Impact Areas</div>
                      <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
                        {riskResult.impacted_business_areas.map(a => <span key={a} className="badge badge-orange">{a}</span>)}
                      </div>
                    </div>
                  )}
                  {riskResult.recommended_action && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                      <strong>Recommended:</strong> {riskResult.recommended_action}
                    </div>
                  )}
                </>
              )}
            </div>
          )}
        </div>
      )}
      </div>
    </div>
  );
}

// ── Page root ─────────────────────────────────────────────────────────────────

export default function RunsPage() {
  const [activeTab, setActiveTab] = useState(0);

  return (
    <div className="page-wrap">

      {/* Tab bar */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "2px solid var(--border)", paddingBottom: 0 }}>
        {TABS.map((t, i) => (
          <button
            key={i}
            onClick={() => setActiveTab(i)}
            style={{
              padding: "8px 16px",
              background: "none",
              border: "none",
              borderBottom: activeTab === i ? "2px solid var(--accent)" : "2px solid transparent",
              marginBottom: -2,
              fontWeight: activeTab === i ? 700 : 500,
              fontSize: 13,
              color: activeTab === i ? "var(--accent)" : "var(--text-2)",
              cursor: "pointer",
            }}
          >
            {t}
          </button>
        ))}
      </div>

      {activeTab === 0 && <EvidenceLookupTab />}
      {activeTab === 1 && <RunHistoryTab />}
    </div>
  );
}
