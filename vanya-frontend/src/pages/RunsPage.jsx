// src/pages/RunsPage.jsx
/**
 * Runs & RCA — Evidence Lookup + Run History with root cause and business risk analysis.
 * GET /runs/{id} | GET /test-runs | POST /rca/analyze | POST /business-risk/analyze
 */
import React, { useState, useEffect, useCallback } from "react";
import { listTestRuns, getTestRun, analyzeRCA, analyzeRisk, getRunClusters } from "../api";
import { useLang } from "../i18n/LangContext";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

// Tab identifiers — rendered via t() in the component
const TAB_KEYS = ["runs.tab.evidence_lookup", "runs.tab.run_history"];

// ── helpers ──────────────────────────────────────────────────────────────────

function statusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s === "pass" || s === "passed" || s === "completed") return "badge badge-green";
  if (s === "fail" || s === "failed" || s === "error")     return "badge badge-red";
  if (s === "running")                                     return "badge badge-blue";
  if (s === "queued")                                      return "badge badge-orange";
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

// ── Run type / backend inference helpers ──────────────────────────────────────

function inferRunType(run) {
  if (!run) return null;
  const runner = (run.meta?.runner || "").toLowerCase();
  if (runner === "desktop")                          return "desktop";
  if (runner === "api")                              return "api";
  const tt = (run.test_type || "").toLowerCase();
  if (tt === "desktop")                              return "desktop";
  if (tt === "api")                                  return "api";
  const tcId = (run.test_id || run.test_case_id || "").toUpperCase();
  if (tcId.startsWith("TC-POS-") || tcId.startsWith("POS-")) return "desktop";
  return "ui";
}

function inferBackend(run) {
  if (!run || run.meta?.is_mock == null) return null;
  return run.meta.is_mock ? "mock" : "real";
}

function bestStepTarget(step) {
  return step.target || step.window || step.control ||
         step.selector || step.url || step.value || "—";
}

// ── Type / backend badges ─────────────────────────────────────────────────────

function RunTypeBadge({ runType }) {
  if (!runType || runType === "ui") return null;
  if (runType === "desktop")
    return <span className="badge badge-blue" style={{ fontSize: 10, letterSpacing: "0.04em" }}>⊞ DESKTOP</span>;
  if (runType === "api")
    return <span className="badge badge-gray" style={{ fontSize: 10, letterSpacing: "0.04em" }}>⌥ API</span>;
  return null;
}

function BackendBadge({ backend }) {
  if (!backend) return null;
  if (backend === "mock")
    return <span className="badge badge-orange" style={{ fontSize: 10 }}>◎ mock</span>;
  return <span className="badge badge-green" style={{ fontSize: 10 }}>● real</span>;
}

function RiskBadge({ level }) {
  const { t } = useLang();
  if (!level) return null;
  const v = String(level).toLowerCase();
  if (v === "high")   return <span className="badge badge-red">{t("runs.risk.high")}</span>;
  if (v === "medium") return <span className="badge badge-orange">{t("runs.risk.medium")}</span>;
  return <span className="badge badge-green">{t("runs.risk.low")}</span>;
}

// ── Evidence helpers ──────────────────────────────────────────────────────────

function toDataUri(b64) {
  if (!b64) return null;
  return b64.startsWith("data:") ? b64 : `data:image/png;base64,${b64}`;
}

function getScreenshotSrc(detail) {
  // 1. canonical artifacts.screenshot_b64 (new contract)
  const artifacts = detail.artifacts || {};
  if (artifacts.screenshot_b64) return toDataUri(artifacts.screenshot_b64);
  // 2. top-level screenshot_b64 (legacy chat/execute runs)
  if (detail.screenshot_b64) return toDataUri(detail.screenshot_b64);
  // 3. meta.screenshot_b64 / meta.screenshot_url (legacy)
  const meta = detail.meta || {};
  if (meta.screenshot_b64) return toDataUri(meta.screenshot_b64);
  if (meta.screenshot_url) return meta.screenshot_url;
  // 4. last step entry with a screenshot
  const steps = detail.steps_result || detail.steps || [];
  for (let i = steps.length - 1; i >= 0; i--) {
    const s = steps[i];
    if (s?.screenshot_b64) return toDataUri(s.screenshot_b64);
    if (s?.screenshot_url) return s.screenshot_url;
  }
  return null;
}

function EvidenceCard({ detail, runType }) {
  const { t } = useLang();
  // Support both canonical (artifacts.*) and legacy (flat fields)
  const artifacts    = detail.artifacts || {};
  const evidenceUrl  = artifacts.evidence_url  || detail.evidence_url;
  const reportUrl    = artifacts.report_url    || detail.report_url;
  const screenshotSrc = getScreenshotSrc(detail);
  const hasLinks     = evidenceUrl || reportUrl;
  const hasAnything  = screenshotSrc || hasLinks;
  const isDesktop    = (runType || inferRunType(detail)) === "desktop";

  return (
    <div className="card">
      <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.evidence.title")}</div>
      {!hasAnything ? (
        <div style={{ fontSize: 12, color: "var(--text-3)" }}>{t("runs.evidence.none")}</div>
      ) : (
        <>
          {screenshotSrc && (
            <>
              {isDesktop && (
                <div style={{ fontSize: 10, fontWeight: 700, color: "var(--accent)", textTransform: "uppercase", letterSpacing: "0.07em", marginBottom: 6 }}>
                  ⊞ {t("runs.desktop.screenshot")}
                </div>
              )}
              <img
                src={screenshotSrc}
                alt={t("runs.detail.screenshot")}
                style={{
                  width: "100%",
                  borderRadius: 6,
                  border: isDesktop ? "2px solid var(--accent)" : "1px solid var(--border)",
                  objectFit: "contain",
                  marginBottom: hasLinks ? 10 : 0,
                  display: "block",
                }}
              />
            </>
          )}
          {hasLinks && (
            <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
              {evidenceUrl && (
                <a href={evidenceUrl} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">
                  {t("runs.evidence.open")}
                </a>
              )}
              {reportUrl && (
                <a href={reportUrl} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">
                  {t("runs.evidence.report")}
                </a>
              )}
            </div>
          )}
        </>
      )}
    </div>
  );
}

function DesktopContextCard({ run }) {
  const { t } = useLang();
  if (!run) return null;
  const runType = inferRunType(run);
  if (runType !== "desktop") return null;

  const meta       = run.meta || {};
  const appPath    = meta.app_path    || run.app_path;
  const winLogin   = meta.win_login   || run.win_login;
  const winMain    = meta.win_main    || run.win_main;
  const window_    = winLogin || winMain;
  const backend    = inferBackend(run);
  const evidenceId = run.evidence_id;

  if (!appPath && !window_ && !backend && !evidenceId) return null;

  const rows = [
    appPath    && { label: t("runs.desktop.app_path"), value: appPath },
    window_    && { label: t("runs.desktop.window"),   value: window_ },
    backend    && { label: t("runs.desktop.backend"),  value: <BackendBadge backend={backend} /> },
    evidenceId && { label: "Evidence ID", value: <code style={{ fontSize: 11, color: "var(--text-2)" }}>{evidenceId}</code> },
  ].filter(Boolean);

  return (
    <div className="card" style={{ borderLeft: "3px solid var(--accent)" }}>
      <div className="section-title" style={{ marginBottom: 10 }}>⊞ {t("runs.desktop.context")}</div>
      <div style={{ display: "grid", gridTemplateColumns: "90px 1fr", gap: "6px 12px", alignItems: "center" }}>
        {rows.map(({ label, value }) => (
          <React.Fragment key={label}>
            <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{label}</div>
            <div style={{ fontSize: 12, color: "var(--text-2)", wordBreak: "break-all" }}>{value}</div>
          </React.Fragment>
        ))}
      </div>
    </div>
  );
}

function FailureAnalysisPanel({ fa, style }) {
  const { t } = useLang();
  if (!fa) return null;
  const typeCls = (() => {
    const v = String(fa.failure_type || "").toLowerCase();
    if (v === "navigation_failed") return "badge badge-red";
    if (v === "unknown")           return "badge badge-gray";
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
      <div className="section-title" style={{ marginBottom: 12 }}>{t("runs.failure_analysis.title")}</div>
      <div style={{ display: "grid", gridTemplateColumns: "90px 1fr", gap: "8px 12px", alignItems: "center" }}>
        <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.type")}</div>
        <div><span className={typeCls}>{fa.failure_type || "—"}</span></div>
        <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.layer")}</div>
        <div style={{ fontSize: 12, color: "var(--text-2)" }}>{fa.layer || "—"}</div>
        {fa.target && (
          <>
            <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.target")}</div>
            <div><code style={{ fontSize: 11, color: "var(--text)" }}>{fa.target}</code></div>
          </>
        )}
        <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.confidence")}</div>
        <div><span className={confCls}>{fa.confidence || "—"}</span></div>
      </div>
    </div>
  );
}

function FailureClustersPanel({ clusters, loading }) {
  const { t } = useLang();
  if (loading) {
    return (
      <div className="card" style={{ marginBottom: 24 }}>
        <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.clusters.title")}</div>
        <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("runs.clusters.loading")}</div>
      </div>
    );
  }
  return (
    <div className="card" style={{ marginBottom: 24, padding: 0, overflow: "hidden" }}>
      <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", gap: 10 }}>
        <div className="section-title" style={{ margin: 0 }}>{t("runs.clusters.title")}</div>
        {clusters.length > 0 && <span className="badge badge-red">{clusters.length} cluster{clusters.length !== 1 ? "s" : ""}</span>}
      </div>
      {clusters.length === 0 ? (
        <div style={{ padding: "16px 20px", fontSize: 13, color: "var(--text-3)" }}>{t("runs.clusters.none")}</div>
      ) : (
        <table className="data-table">
          <thead>
            <tr>
              <th>{t("runs.clusters.col.type")}</th>
              <th>{t("runs.clusters.col.target")}</th>
              <th style={{ width: 80, textAlign: "right" }}>{t("runs.clusters.col.count")}</th>
            </tr>
          </thead>
          <tbody>
            {clusters.map((c) => {
              const typeCls = (() => {
                const v = String(c.failure_type || "").toLowerCase();
                if (v === "navigation_failed") return "badge badge-red";
                if (v === "unknown")           return "badge badge-gray";
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
  const { t } = useLang();
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
          <h1 className="page-title">{t("runs.lookup.title")}</h1>
          <p className="page-subtitle">{t("runs.lookup.subtitle")}</p>
        </div>
        <div style={{ display: "flex", gap: 10 }}>
          <input
            className="input"
            value={evidenceId}
            onChange={e => setEvidenceId(e.target.value)}
            onKeyDown={e => e.key === "Enter" && handleFetch()}
            placeholder={t("runs.lookup.placeholder")}
            style={{ flex: 1 }}
          />
          <button className="btn btn-primary" onClick={handleFetch} disabled={loading || !evidenceId.trim()}>
            {loading ? t("runs.lookup.loading") : t("runs.lookup.fetch")}
          </button>
        </div>
        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {run && (
        <>
          <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 20, flexWrap: "wrap" }}>
            <h2 style={{ margin: 0, fontSize: 18, fontWeight: 800, color: "var(--text)" }}>{t("runs.detail.title")}</h2>
            <span className={statusBadgeClass(run.status)}>{run.status || "unknown"}</span>
            <RunTypeBadge runType={inferRunType(run)} />
            <BackendBadge backend={inferBackend(run)} />
            {run.evidence_id && <code style={{ fontSize: 12, color: "var(--text-2)" }}>{run.evidence_id}</code>}
            {hasHealing && (
              <span className="badge badge-orange" title={`${healedEntries.length} selector(s) auto-healed`}>
                ⚡ {healedEntries.length} {t("runs.detail.auto_healed")}
              </span>
            )}
          </div>

          <div className="kpi-grid">
            {run.duration_ms != null && (
              <div className="kpi-card">
                <div className="kpi-label">{t("runs.detail.duration")}</div>
                <div className="kpi-value">{run.duration_ms}<span style={{ fontSize: 14, fontWeight: 600, color: "var(--text-2)" }}>ms</span></div>
              </div>
            )}
            {totalSteps > 0 && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.total_steps")}</div><div className="kpi-value">{totalSteps}</div></div>}
            {totalSteps > 0 && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.passed")}</div><div className="kpi-value" style={{ color: "var(--green)" }}>{passedSteps}</div></div>}
            {totalSteps > 0 && failedSteps > 0 && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.failed")}</div><div className="kpi-value" style={{ color: "var(--red)" }}>{failedSteps}</div></div>}
            {run.expected && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.expected")}</div><div style={{ fontSize: 14, fontWeight: 700, color: "var(--text)", marginTop: 4 }}>{run.expected}</div></div>}
            {run.outcome && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.outcome")}</div><div style={{ fontSize: 14, fontWeight: 700, color: "var(--text)", marginTop: 4 }}>{run.outcome}</div></div>}
          </div>

          {/* Evidence links — support both canonical (artifacts.*) and legacy (flat) fields */}
          {(() => {
            const arts = run.artifacts || {};
            const evUrl = arts.evidence_url || run.evidence_url;
            const rpUrl = arts.report_url   || run.report_url;
            const baseUrl = run.meta?.base_url;
            if (!baseUrl && !evUrl && !rpUrl) return null;
            return (
              <div className="card" style={{ marginBottom: 20 }}>
                <div style={{ display: "flex", gap: 20, flexWrap: "wrap", alignItems: "center" }}>
                  {baseUrl && (
                    <div>
                      <div className="section-title" style={{ marginBottom: 2 }}>{t("runs.detail.base_url")}</div>
                      <div style={{ fontSize: 13, color: "var(--text-2)", wordBreak: "break-all" }}>{baseUrl}</div>
                    </div>
                  )}
                  <div style={{ marginLeft: "auto", display: "flex", gap: 10, flexShrink: 0 }}>
                    {evUrl && <a href={evUrl} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">{t("runs.detail.view_evidence")}</a>}
                    {rpUrl && <a href={rpUrl} target="_blank" rel="noreferrer" className="btn btn-primary btn-sm">{t("runs.detail.download_report")}</a>}
                  </div>
                </div>
              </div>
            );
          })()}

          {run.reason && (
            <div className="card" style={{ marginBottom: 20 }}>
              <div className="section-title">{t("runs.detail.failure_reason")}</div>
              <div style={{ fontSize: 13, color: "var(--text)", lineHeight: 1.6 }}>{run.reason}</div>
            </div>
          )}

          <FailureAnalysisPanel fa={run.failure_analysis} style={{ marginBottom: 20 }} />

          <DesktopContextCard run={run} />

          {run.steps?.length > 0 && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "16px 20px", borderBottom: "1px solid var(--border)" }}>
                <div className="section-title" style={{ margin: 0 }}>
                  {t("runs.detail.exec_steps")} — {totalSteps} {t("runs.detail.total_label")}
                </div>
              </div>
              <div style={{ overflowX: "auto" }}>
                <table className="data-table">
                  <thead><tr>
                    <th style={{ width: 44 }}>{t("runs.detail.col.num")}</th>
                    <th>{t("runs.detail.col.action")}</th>
                    <th>{t("runs.detail.col.target_url")}</th>
                    <th>{t("runs.detail.col.status")}</th>
                    <th>{t("runs.detail.col.duration")}</th>
                  </tr></thead>
                  <tbody>
                    {run.steps.map((step, i) => (
                      <tr key={i}>
                        <td style={{ color: "var(--text-3)", fontWeight: 600 }}>{step.index ?? i + 1}</td>
                        <td><code style={{ fontSize: 12 }}>{step.action || "—"}</code></td>
                        <td style={{ maxWidth: 320, wordBreak: "break-all", fontSize: 12, color: "var(--text-2)" }}>{bestStepTarget(step)}</td>
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
                <div className="section-title" style={{ margin: 0 }}>{t("runs.detail.selector_healing")}</div>
                <span className="badge badge-orange">⚡ {healedEntries.length} {t("runs.detail.auto_healed")}</span>
              </div>
              <div style={{ overflowX: "auto" }}>
                <table className="data-table">
                  <thead><tr>
                    <th style={{ width: 44 }}>{t("runs.detail.col.step")}</th>
                    <th>{t("runs.detail.col.action")}</th>
                    <th>{t("runs.detail.col.orig_sel")}</th>
                    <th>{t("runs.detail.col.healed_sel")}</th>
                    <th style={{ width: 110 }}>{t("runs.detail.col.strategy")}</th>
                  </tr></thead>
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
                <div className="section-title" style={{ margin: 0 }}>{t("runs.detail.logs")}</div>
              </div>
              <pre className="code-block" style={{ margin: 0, borderRadius: 0, border: "none", maxHeight: 320, overflow: "auto" }}>
                {run.logs.join("\n")}
              </pre>
            </div>
          )}

          {run.screenshot_b64 && (
            <div className="card">
              <div className="section-title">{t("runs.detail.screenshot")}</div>
              <img
                src={run.screenshot_b64.startsWith("data:image/") ? run.screenshot_b64 : `data:image/png;base64,${run.screenshot_b64}`}
                alt={t("runs.detail.screenshot")}
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
  const { t } = useLang();
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
      setError(e?.message || t("runs.history.error"));
    } finally {
      setLoading(false);
    }
  }, [t]);

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
              {loading ? t("runs.history.loading") : `${runs.length} run${runs.length !== 1 ? "s" : ""}`}
            </div>
            <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>{t("runs.history.refresh")}</button>
          </div>

          {runs.length === 0 && !loading ? (
            <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("runs.history.none")}</div>
          ) : (
            <table className="data-table">
              <thead><tr>
                <th>{t("runs.history.col.run_id")}</th>
                <th>{t("runs.history.col.test_case")}</th>
                <th>{t("runs.history.col.status")}</th>
                <th>{t("runs.history.col.duration")}</th>
                <th>{t("runs.history.col.created")}</th>
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
                    <td style={{ fontWeight: 600, fontSize: 13 }}>
                      <span style={{ display: "flex", alignItems: "center", gap: 6, flexWrap: "wrap" }}>
                        {r.test_id || r.test_case_id || "—"}
                        <RunTypeBadge runType={inferRunType(r)} />
                      </span>
                    </td>
                    <td><span className={statusBadgeClass(r.status)}>{r.status || "—"}</span></td>
                    <td style={{ fontSize: 12, color: "var(--text-3)" }}>{fmtMs(r.duration_ms)}</td>
                    <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(r.started_at || r.created_at)}</td>
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
            <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.history.run_detail")}</div>
            {detailLoading ? (
              <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("runs.history.loading")}</div>
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
                  {(detail.test_id || detail.test_case_id) && <span className="badge badge-gray">{detail.test_id || detail.test_case_id}</span>}
                  <RunTypeBadge runType={inferRunType(detail)} />
                  <BackendBadge backend={inferBackend(detail)} />
                </div>
                {detail.reason && (
                  <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.5, marginBottom: 8 }}>
                    <strong>{t("runs.history.reason")}</strong> {detail.reason}
                  </div>
                )}
                {detail.steps?.length > 0 && (
                  <div style={{ fontSize: 12, color: "var(--text-3)" }}>
                    {detail.steps.length} {t("runs.history.steps_label")} — {detail.steps.filter(s => String(s.status || "").toLowerCase().includes("pass")).length} {t("runs.history.passed_label")}
                  </div>
                )}

                {/* RCA / Risk buttons */}
                <div style={{ display: "flex", gap: 8, marginTop: 14, flexWrap: "wrap" }}>
                  <button className="btn btn-secondary btn-sm" onClick={handleRCA} disabled={rcaLoading}>
                    {rcaLoading ? t("runs.history.analyzing") : t("runs.history.rca")}
                  </button>
                  <button className="btn btn-secondary btn-sm" onClick={handleRisk} disabled={riskLoading}>
                    {riskLoading ? t("runs.history.analyzing") : t("runs.history.risk")}
                  </button>
                </div>
              </>
            ) : null}
          </div>

          {/* Evidence */}
          {detail && !detail.error && <EvidenceCard detail={detail} runType={inferRunType(detail)} />}

          {/* Desktop context */}
          {detail && !detail.error && <DesktopContextCard run={detail} />}

          {/* Failure Analysis */}
          <FailureAnalysisPanel fa={detail?.failure_analysis} />

          {/* RCA result */}
          {rcaResult && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.rca.title")}</div>
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
                      {t("runs.rca.probable_cause")} {rcaResult.probable_cause}
                    </div>
                  )}
                  {rcaResult.recommended_action && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                      <strong>{t("runs.rca.recommended")}</strong> {rcaResult.recommended_action}
                    </div>
                  )}
                </>
              )}
            </div>
          )}

          {/* Business risk result */}
          {riskResult && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.risk.title")}</div>
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
                      <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 4 }}>{t("runs.risk.impact_areas")}</div>
                      <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
                        {riskResult.impacted_business_areas.map(a => <span key={a} className="badge badge-orange">{a}</span>)}
                      </div>
                    </div>
                  )}
                  {riskResult.recommended_action && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                      <strong>{t("runs.risk.recommended")}</strong> {riskResult.recommended_action}
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
  const { t } = useLang();
  const [activeTab, setActiveTab] = useState(0);

  return (
    <div className="page-wrap">

      {/* Tab bar */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "2px solid var(--border)", paddingBottom: 0 }}>
        {TAB_KEYS.map((key, i) => (
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
            {t(key)}
          </button>
        ))}
      </div>

      {activeTab === 0 && <EvidenceLookupTab />}
      {activeTab === 1 && <RunHistoryTab />}
    </div>
  );
}
