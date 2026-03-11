// src/pages/RunsPage.jsx
/**
 * RunsPage — Evidence lookup with KPI cards, status badges, clean step table.
 * GET /runs/{evidence_id}
 */
import React, { useState, useCallback } from "react";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

function statusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s.includes("pass")) return "badge badge-green";
  if (s.includes("fail") || s.includes("error")) return "badge badge-red";
  return "badge badge-gray";
}

function stepStatusColor(status) {
  const s = String(status || "").toLowerCase();
  if (s.includes("pass")) return "var(--green)";
  if (s.includes("fail") || s.includes("error")) return "var(--red)";
  return "var(--text-3)";
}

export default function RunsPage() {
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

  // Selector healing — sourced from resolution_log (always present) or healing_log (new)
  const healedEntries = (run?.resolution_log || []).filter(e => e?.fallback_used === true);
  const hasHealing = healedEntries.length > 0;

  const passedSteps = run?.steps?.filter(s => String(s.status || "").toLowerCase().includes("pass")).length ?? 0;
  const failedSteps = run?.steps?.filter(s => {
    const s2 = String(s.status || "").toLowerCase();
    return s2.includes("fail") || s2.includes("error");
  }).length ?? 0;
  const totalSteps = run?.steps?.length ?? 0;

  return (
    <div className="page-wrap">
      {/* ── Search card ───────────────────────────────────── */}
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
          <button
            className="btn btn-primary"
            onClick={handleFetch}
            disabled={loading || !evidenceId.trim()}
          >
            {loading ? "Loading…" : "Fetch Run"}
          </button>
        </div>

        {error && (
          <div className="alert alert-error" style={{ marginTop: 12 }}>
            {error}
          </div>
        )}
      </div>

      {/* ── Run details ───────────────────────────────────── */}
      {run && (
        <>
          {/* Status header */}
          <div style={{
            display: "flex",
            alignItems: "center",
            gap: 12,
            marginBottom: 20,
            flexWrap: "wrap",
          }}>
            <h2 style={{ margin: 0, fontSize: 18, fontWeight: 800, color: "var(--text)" }}>
              Run Details
            </h2>
            <span className={statusBadgeClass(run.status)}>
              {run.status || "unknown"}
            </span>
            {run.evidence_id && (
              <code style={{ fontSize: 12, color: "var(--text-2)" }}>
                {run.evidence_id}
              </code>
            )}
            {hasHealing && (
              <span className="badge badge-orange" title={`${healedEntries.length} selector(s) auto-healed`}>
                ⚡ Auto-healed ({healedEntries.length})
              </span>
            )}
          </div>

          {/* KPI cards */}
          <div className="kpi-grid">
            {run.duration_ms != null && (
              <div className="kpi-card">
                <div className="kpi-label">Duration</div>
                <div className="kpi-value">{run.duration_ms}<span style={{ fontSize: 14, fontWeight: 600, color: "var(--text-2)" }}>ms</span></div>
              </div>
            )}
            {totalSteps > 0 && (
              <div className="kpi-card">
                <div className="kpi-label">Total Steps</div>
                <div className="kpi-value">{totalSteps}</div>
              </div>
            )}
            {totalSteps > 0 && (
              <div className="kpi-card">
                <div className="kpi-label">Passed</div>
                <div className="kpi-value" style={{ color: "var(--green)" }}>{passedSteps}</div>
              </div>
            )}
            {totalSteps > 0 && failedSteps > 0 && (
              <div className="kpi-card">
                <div className="kpi-label">Failed</div>
                <div className="kpi-value" style={{ color: "var(--red)" }}>{failedSteps}</div>
              </div>
            )}
            {run.expected && (
              <div className="kpi-card">
                <div className="kpi-label">Expected</div>
                <div style={{ fontSize: 14, fontWeight: 700, color: "var(--text)", marginTop: 4 }}>{run.expected}</div>
              </div>
            )}
            {run.outcome && (
              <div className="kpi-card">
                <div className="kpi-label">Outcome</div>
                <div style={{ fontSize: 14, fontWeight: 700, color: "var(--text)", marginTop: 4 }}>{run.outcome}</div>
              </div>
            )}
          </div>

          {/* Meta / links row */}
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
                  {run.evidence_url && (
                    <a
                      href={run.evidence_url}
                      target="_blank"
                      rel="noreferrer"
                      className="btn btn-secondary btn-sm"
                    >
                      View Evidence ↗
                    </a>
                  )}
                  {run.report_url && (
                    <a
                      href={run.report_url}
                      target="_blank"
                      rel="noreferrer"
                      className="btn btn-primary btn-sm"
                    >
                      Download Report ↗
                    </a>
                  )}
                </div>
              </div>
            </div>
          )}

          {/* Reason */}
          {run.reason && (
            <div className="card" style={{ marginBottom: 20 }}>
              <div className="section-title">Failure Reason</div>
              <div style={{ fontSize: 13, color: "var(--text)", lineHeight: 1.6 }}>
                {run.reason}
              </div>
            </div>
          )}

          {/* Steps table */}
          {run.steps?.length > 0 && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
                <div className="section-title" style={{ margin: 0 }}>
                  Execution Steps — {totalSteps} total
                </div>
              </div>
              <div style={{ overflowX: "auto" }}>
                <table className="data-table">
                  <thead>
                    <tr>
                      <th style={{ width: 44 }}>#</th>
                      <th>Action</th>
                      <th>Target / URL</th>
                      <th>Status</th>
                      <th>Duration</th>
                    </tr>
                  </thead>
                  <tbody>
                    {run.steps.map((step, i) => (
                      <tr key={i}>
                        <td style={{ color: "var(--text-3)", fontWeight: 600 }}>
                          {step.index ?? i + 1}
                        </td>
                        <td>
                          <code style={{ fontSize: 12 }}>{step.action || "—"}</code>
                        </td>
                        <td style={{ maxWidth: 320, wordBreak: "break-all", fontSize: 12, color: "var(--text-2)" }}>
                          {step.url || step.selector || step.value || "—"}
                        </td>
                        <td>
                          <span style={{
                            fontSize: 11,
                            fontWeight: 700,
                            color: stepStatusColor(step.status),
                            textTransform: "uppercase",
                            letterSpacing: "0.03em",
                          }}>
                            {step.status || "—"}
                          </span>
                        </td>
                        <td style={{ color: "var(--text-3)", fontSize: 12, whiteSpace: "nowrap" }}>
                          {step.duration_ms ? `${step.duration_ms}ms` : "—"}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {/* Selector Healing Panel */}
          {hasHealing && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{
                padding: "14px 20px",
                borderBottom: "1px solid var(--border)",
                display: "flex",
                alignItems: "center",
                gap: 10,
              }}>
                <div className="section-title" style={{ margin: 0 }}>Selector Healing</div>
                <span className="badge badge-orange">⚡ {healedEntries.length} auto-healed</span>
              </div>
              <div style={{ overflowX: "auto" }}>
                <table className="data-table">
                  <thead>
                    <tr>
                      <th style={{ width: 44 }}>Step</th>
                      <th>Action</th>
                      <th>Original Selector</th>
                      <th>Healed Selector</th>
                      <th style={{ width: 110 }}>Strategy</th>
                    </tr>
                  </thead>
                  <tbody>
                    {healedEntries.map((e, i) => (
                      <tr key={i}>
                        <td style={{ color: "var(--text-3)", fontWeight: 600 }}>
                          {e.step_index != null ? e.step_index + 1 : "—"}
                        </td>
                        <td>
                          <code style={{ fontSize: 12 }}>{e.action || "—"}</code>
                        </td>
                        <td style={{ fontSize: 12, color: "var(--red)", wordBreak: "break-all", maxWidth: 200 }}>
                          {e.original_selector || e.primary || "—"}
                        </td>
                        <td style={{ fontSize: 12, color: "var(--green)", wordBreak: "break-all", maxWidth: 200 }}>
                          {e.resolved || "—"}
                        </td>
                        <td>
                          <span className="badge badge-gray" style={{ fontSize: 11 }}>
                            {e.fallback_type || e.used || "—"}
                          </span>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {/* Logs */}
          {run.logs?.length > 0 && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
                <div className="section-title" style={{ margin: 0 }}>Execution Logs</div>
              </div>
              <pre className="code-block" style={{
                margin: 0,
                borderRadius: 0,
                border: "none",
                maxHeight: 320,
                overflow: "auto",
              }}>
                {run.logs.join("\n")}
              </pre>
            </div>
          )}

          {/* Screenshot */}
          {run.screenshot_b64 && (
            <div className="card">
              <div className="section-title">Screenshot</div>
              <img
                src={
                  run.screenshot_b64.startsWith("data:image/")
                    ? run.screenshot_b64
                    : `data:image/png;base64,${run.screenshot_b64}`
                }
                alt="Test run screenshot"
                style={{
                  maxWidth: "100%",
                  borderRadius: "var(--r-sm)",
                  border: "1px solid var(--border)",
                  marginTop: 4,
                }}
              />
            </div>
          )}
        </>
      )}
    </div>
  );
}
