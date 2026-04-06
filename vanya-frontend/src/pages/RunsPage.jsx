// src/pages/RunsPage.jsx
/**
 * Runs & RCA — Evidence Lookup + Run History with root cause and business risk analysis.
 * GET /runs/{id} | GET /test-runs | POST /rca/analyze | POST /business-risk/analyze
 */
import React, { useState, useEffect, useCallback, useRef } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import {
  listTestRuns,
  getTestRun,
  analyzeRCA,
  analyzeRisk,
  getRunClusters,
  enqueueSingle,
  apiFetch,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";

// Tab identifiers — rendered via t() in the component
const TAB_KEYS = ["runs.tab.history", "runs.tab.lookup"];

// ── helpers ──────────────────────────────────────────────────────────────────

function statusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s === "pass" || s === "passed" || s === "completed") return "badge badge-green";
  if (s === "fail" || s === "failed")                      return "badge badge-red";
  if (s === "error")                                      return "badge badge-orange";
  if (s === "running")                                     return "badge badge-blue";
  if (s === "queued")                                      return "badge badge-orange";
  if (s === "planning" || s === "compiled")                 return "badge badge-orange";
  if (s === "canceled" || s === "cancelled")              return "badge badge-gray";
  return "badge badge-gray";
}

function statusIcon(status) {
  const s = String(status || "").toLowerCase();
  if (s === "pass" || s === "passed" || s === "completed") return "✓";
  if (s === "fail" || s === "failed") return "✕";
  if (s === "error") return "⚠";
  if (s === "running") return "⏱";
  if (s === "queued") return "⏳";
  if (s === "planning" || s === "compiled") return "⌁";
  if (s === "canceled" || s === "cancelled") return "⦸";
  return "•";
}

function statusBadgeText(status) {
  const label = status || "—";
  return `${statusIcon(status)} ${label}`;
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

function getStepsCount(run) {
  if (!run) return 0;
  const n =
    run.steps_count ??
    (Array.isArray(run.steps) ? run.steps.length : null) ??
    (Array.isArray(run.steps_result) ? run.steps_result.length : null) ??
    0;
  const nn = Number(n);
  return Number.isFinite(nn) ? nn : 0;
}

function hasEvidenceForList(run) {
  if (!run) return false;
  const arts = run?.artifacts || {};
  return Boolean(
    run.evidence_url ||
      run.report_url ||
      arts.evidence_url ||
      arts.report_url ||
      arts.screenshot_b64 ||
      run.meta?.evidence_url ||
      run.meta?.report_url
  );
}

function CorrelationIdChip({ value }) {
  const [copied, setCopied] = useState(false);
  const copy = async () => {
    if (!value) return;
    try {
      await navigator.clipboard.writeText(String(value));
      setCopied(true);
      window.setTimeout(() => setCopied(false), 900);
    } catch {
      // ignore (clipboard may not be available)
    }
  };
  if (!value) return null;
  return (
    <span style={{ display: "inline-flex", alignItems: "center", gap: 8 }}>
      <span className="badge badge-gray" style={{ fontSize: 10, opacity: 0.9 }}>correlation</span>
      <code style={{ fontSize: 11, color: "var(--text-2)" }}>{value}</code>
      <button
        className="btn btn-secondary btn-sm"
        onClick={copy}
        style={{ fontSize: 11, padding: "4px 10px" }}
        title="Copy correlation_id"
      >
        {copied ? "Copied" : "Copy"}
      </button>
    </span>
  );
}

function DebugAccordion({ detail }) {
  const errType = detail?.error_type || detail?.meta?.error_type;
  const stepIndex = detail?.step_index ?? detail?.meta?.step_index;
  const hint = detail?.hint ?? detail?.meta?.hint;
  const rawReason = detail?.reason || detail?.message || detail?.error_message || "";
  const rawMessage = rawReason || detail?.runner?.reason || "";
  const hasAny = Boolean(errType || stepIndex != null || hint || rawMessage);

  if (!hasAny) return null;

  return (
    <div className="card" style={{ marginBottom: 20 }}>
      <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
        <div className="section-title" style={{ margin: 0 }}>Debug</div>
      </div>
      <div style={{ padding: 16 }}>
        <details>
          <summary style={{ cursor: "pointer", fontSize: 13, color: "var(--text-2)" }}>
            Show error details
          </summary>
          <div style={{ marginTop: 12, display: "grid", gap: 10 }}>
            {errType && (
              <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  error_type
                </span>
                <span className="badge badge-orange">{errType}</span>
              </div>
            )}
            {stepIndex != null && (
              <div>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  step_index
                </span>{" "}
                <code style={{ fontSize: 12, color: "var(--text-2)" }}>{String(stepIndex)}</code>
              </div>
            )}
            {hint && (
              <div>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  hint
                </span>{" "}
                <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginTop: 4 }}>{hint}</div>
              </div>
            )}
            {rawMessage && (
              <div>
                <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>
                  raw reason/message
                </span>
                <pre className="code-block" style={{ marginTop: 6, maxHeight: 180, overflow: "auto", whiteSpace: "pre-wrap" }}>
                  {rawMessage}
                </pre>
              </div>
            )}
          </div>
        </details>
      </div>
    </div>
  );
}

function ConfirmModal({
  open,
  title,
  description,
  busy = false,
  error = "",
  confirmLabel,
  cancelLabel,
  onConfirm,
  onCancel,
}) {
  const { t } = useLang();
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
          <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>{title}</div>
          <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>{description}</div>
        </div>
        <div style={{ padding: "14px 20px" }}>
          {error && (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
              {error}
            </div>
          )}
          <div style={{ display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
            <button
              className="btn btn-secondary btn-sm"
              onClick={onCancel}
              disabled={busy}
            >
              {cancelLabel || t("common.cancel")}
            </button>
            <button className="btn btn-primary btn-sm" onClick={onConfirm} disabled={busy}>
              {busy ? t("common.working") : confirmLabel || t("common.confirm")}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
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
  if (!detail) return null;
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
  if (!detail) return null;
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
                <div style={{ fontSize: 10, fontWeight: 400, color: "var(--accent)", textTransform: "uppercase", letterSpacing: "0.07em", marginBottom: 6 }}>
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
            <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{label}</div>
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
        <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.type")}</div>
        <div><span className={typeCls}>{fa.failure_type || "—"}</span></div>
        <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.layer")}</div>
        <div style={{ fontSize: 12, color: "var(--text-2)" }}>{fa.layer || "—"}</div>
        {fa.target && (
          <>
            <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.target")}</div>
            <div><code style={{ fontSize: 11, color: "var(--text-1)" }}>{fa.target}</code></div>
          </>
        )}
        <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em" }}>{t("runs.failure_analysis.confidence")}</div>
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
                  <td style={{ textAlign: "right", fontWeight: 600, fontSize: 14, color: "var(--text-1)" }}>{c.count}</td>
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
      const res = await apiFetch(`/runs/${encodeURIComponent(id)}?format=json`, {
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
  const stepsCount = getStepsCount(run);
  const screenshotSrc = getScreenshotSrc(run);

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
            <h2 style={{ margin: 0, fontSize: 18, fontWeight: 600, color: "var(--text-1)" }}>{t("runs.detail.title")}</h2>
            <span className={statusBadgeClass(run.status)}>{statusBadgeText(run.status || "unknown")}</span>
            <RunTypeBadge runType={inferRunType(run)} />
            <BackendBadge backend={inferBackend(run)} />
            {run.evidence_id && <code style={{ fontSize: 12, color: "var(--text-2)" }}>{run.evidence_id}</code>}
            {stepsCount > 0 && <span className="badge badge-gray">{stepsCount} steps</span>}
            {(run.started_at || run.created_at) && (
              <span className="badge badge-gray" title="Start time">
                {fmtDate(run.started_at || run.created_at)}
              </span>
            )}
            <CorrelationIdChip value={run.correlation_id || run.meta?.correlation_id} />
            {hasHealing && (
              <span className="badge badge-orange" title={`${healedEntries.length} selector(s) auto-healed`}>
                ⚡ {healedEntries.length} {t("runs.detail.auto_healed")}
              </span>
            )}
          </div>

          {/* Trigger context — present when run originated from Risk Selection or PR Analysis */}
          {run.meta?.trigger_context && (() => {
            const ctx = run.meta.trigger_context;
            const sourceLabel = ctx.source === "pr_analysis"
              ? "◎ PR Analysis"
              : ctx.source === "risk_selection"
                ? "◈ Risk Selection"
                : ctx.source;
            return (
              <div className="card" style={{ marginBottom: 16, padding: "10px 16px" }}>
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
                  <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginRight: 4 }}>
                    {t("runs.trigger.label")}
                  </span>
                  <span className="badge badge-blue" style={{ fontSize: 11 }}>{sourceLabel}</span>
                  {ctx.selection_type && (
                    <span className="badge badge-gray" style={{ fontSize: 10 }}>{ctx.selection_type}</span>
                  )}
                  {ctx.pr_title && (
                    <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-1)" }}>{ctx.pr_title}</span>
                  )}
                  {ctx.pr_branch && (
                    <span className="badge badge-gray" style={{ fontSize: 10 }}>⎇ {ctx.pr_branch}</span>
                  )}
                  {ctx.selected_modules?.map(m => (
                    <span key={m} className="badge badge-gray" style={{ fontSize: 10 }}>{m}</span>
                  ))}
                  {ctx.selected_test_ids?.length > 0 && (
                    <span style={{ fontSize: 11, color: "var(--text-3)" }}>
                      {ctx.selected_test_ids.length} {t("runs.trigger.tests_label")}
                    </span>
                  )}
                </div>
              </div>
            );
          })()}

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
            {run.expected && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.expected")}</div><div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginTop: 4 }}>{run.expected}</div></div>}
            {run.outcome && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.outcome")}</div><div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginTop: 4 }}>{run.outcome}</div></div>}
          </div>

          {/* Evidence links — support both canonical (artifacts.*) and legacy (flat) fields */}
          {(() => {
            const arts = run?.artifacts || {};
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

          {(run.error_summary || run.reason || run.message || run.error_message) && (
            <div className="card" style={{ marginBottom: 20 }}>
              <div className="section-title">{t("runs.detail.failure_reason")}</div>
              <div style={{ fontSize: 13, color: "var(--text-1)", lineHeight: 1.6 }}>
                {run.error_summary || run.reason || run.message || run.error_message}
              </div>
              {(run.hint || run.meta?.hint) && (
                <div style={{ marginTop: 8, fontSize: 12, color: "var(--text-3)", lineHeight: 1.6 }}>
                  <strong>Hint:</strong> {run.hint || run.meta?.hint}
                </div>
              )}
            </div>
          )}

          <DebugAccordion detail={run} />

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
                        <td><span style={{ fontSize: 11, fontWeight: 500, color: stepStatusColor(step.status), textTransform: "uppercase", letterSpacing: "0.03em" }}>{step.status || "—"}</span></td>
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

          {screenshotSrc && (
            <div className="card">
              <div className="section-title">{t("runs.detail.screenshot")}</div>
              <img
                src={screenshotSrc}
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

function RunHistoryTab({ initialRunId }) {
  const { t } = useLang();
  const { currentProject } = useProject();
  const navigate = useNavigate();
  const [runs, setRuns]             = useState([]);
  const [loading, setLoading]       = useState(true);
  const [error, setError]           = useState("");
  const [selected, setSelected]     = useState(null); // run_id
  const [detail, setDetail]         = useState(null);
  const [detailLoading, setDetailLoading] = useState(false);
  const autoOpenedRef    = useRef(false);
  const selectedRowRef   = useRef(null);

  // RCA / risk panels
  const [rcaResult, setRcaResult]   = useState(null);
  const [rcaLoading, setRcaLoading] = useState(false);
  const [riskResult, setRiskResult] = useState(null);
  const [riskLoading, setRiskLoading] = useState(false);

  // Failure clusters
  const [clusters, setClusters]           = useState([]);
  const [clustersLoading, setClustersLoading] = useState(true);

  // Operational filters (frontend-only; no backend changes)
  const [statusFilter, setStatusFilter] = useState("all");
  const [flakyFilter, setFlakyFilter] = useState("all");
  const [quarantineFilter, setQuarantineFilter] = useState("all");
  const [retryFilter, setRetryFilter] = useState("all");

  // Human confirmation for operational actions
  const [confirmRetryOpen, setConfirmRetryOpen] = useState(false);
  const [retryBusy, setRetryBusy] = useState(false);
  const [retryError, setRetryError] = useState("");
  const [lastEnqueuedJobId, setLastEnqueuedJobId] = useState(null);

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
      const opts = { limit: 100 };
      if (currentProject?.id) opts.project_id = currentProject.id;
      const data = await listTestRuns(opts);
      setRuns(Array.isArray(data) ? data : (data?.runs ?? []));
    } catch (e) {
      setError(e?.message || t("runs.history.error"));
    } finally {
      setLoading(false);
    }
  }, [t, currentProject?.id]);

  useEffect(() => {
    load();
    loadClusters();
  }, [load, loadClusters]);

  // Auto-open a specific run when navigated from Execution Center
  useEffect(() => {
    if (initialRunId && !autoOpenedRef.current) {
      autoOpenedRef.current = true;
      openDetail(initialRunId);
    }
  }, [initialRunId]);

  // Scroll selected row into view once runs are loaded and a row is selected
  useEffect(() => {
    if (selected && selectedRowRef.current) {
      selectedRowRef.current.scrollIntoView({ block: "nearest", behavior: "smooth" });
    }
  }, [selected, runs]);

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

  // Apply operational filters in the UI (frontend-only, no backend changes)
  const filteredRuns = runs.filter(r => {
    if (statusFilter !== "all" && r.status !== statusFilter) return false;

    const meta = r.meta || {};
    const flakyPresent = Boolean(meta.flaky_signal);
    const quarantineRecommended = meta.quarantine_recommended === true;
    const retryPolicyApplied = meta.retry_policy_applied === true;

    if (flakyFilter !== "all" && !flakyPresent) return false;
    if (quarantineFilter !== "all" && !quarantineRecommended) return false;
    if (retryFilter !== "all" && !retryPolicyApplied) return false;

    return true;
  });

  const filtersActive =
    statusFilter !== "all" || flakyFilter !== "all" || quarantineFilter !== "all" || retryFilter !== "all";

  return (
    <div>
      <FailureClustersPanel clusters={clusters} loading={clustersLoading} />

      <div style={{ display: "flex", flexDirection: "column", gap: 20, width: "100%" }}>

      {/* Runs list */}
      <div>
        {error && <div className="alert alert-error" style={{ marginBottom: 12 }}>{error}</div>}
        <div className="card" style={{ padding: 0, overflow: "hidden" }}>
          <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between", flexWrap: "wrap", gap: 10 }}>
            <div style={{ display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
              <div className="section-title" style={{ margin: 0 }}>
                {loading
                  ? t("runs.history.loading")
                  : `${filteredRuns.length} run${filteredRuns.length !== 1 ? "s" : ""}`}
              </div>
              {currentProject && (
                <span className="badge badge-gray" style={{ fontSize: 11 }}>
                  {t("runs.active_project", { name: currentProject.name })}
                </span>
              )}
            </div>
            <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>{t("runs.history.refresh")}</button>
          </div>

          {/* Filters (compact, frontend-only) */}
          <div style={{ padding: "12px 20px", display: "flex", gap: 16, flexWrap: "wrap", alignItems: "center" }}>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.status")}:</span>
              <select
                className="input"
                value={statusFilter}
                onChange={e => setStatusFilter(e.target.value)}
                style={{ minWidth: 130 }}
                aria-label={t("runs.filter.status")}
              >
                <option value="all">{t("runs.filter.status_all")}</option>
                <option value="passed">{t("runs.filter.status_passed")}</option>
                <option value="failed">{t("runs.filter.status_failed")}</option>
                <option value="error">{t("runs.filter.status_error")}</option>
                <option value="running">{t("runs.filter.status_running")}</option>
              </select>
            </div>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.flaky")}:</span>
              <select
                className="input"
                value={flakyFilter}
                onChange={e => setFlakyFilter(e.target.value)}
                style={{ minWidth: 110 }}
                aria-label={t("runs.filter.flaky")}
              >
                <option value="all">{t("runs.filter.flaky_all")}</option>
                <option value="only">{t("runs.filter.flaky_only")}</option>
              </select>
            </div>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.quarantine")}:</span>
              <select
                className="input"
                value={quarantineFilter}
                onChange={e => setQuarantineFilter(e.target.value)}
                style={{ minWidth: 130 }}
                aria-label={t("runs.filter.quarantine")}
              >
                <option value="all">{t("runs.filter.quarantine_all")}</option>
                <option value="only">{t("runs.filter.quarantine_only")}</option>
              </select>
            </div>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.retry")}:</span>
              <select
                className="input"
                value={retryFilter}
                onChange={e => setRetryFilter(e.target.value)}
                style={{ minWidth: 120 }}
                aria-label={t("runs.filter.retry")}
              >
                <option value="all">{t("runs.filter.retry_all")}</option>
                <option value="only">{t("runs.filter.retry_only")}</option>
              </select>
            </div>
          </div>

          {!loading && runs.length === 0 ? (
            <div style={{ padding: "32px 24px", textAlign: "center" }}>
              <div style={{ fontSize: 28, marginBottom: 10 }}>◈</div>
              <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>{t("runs.empty.title")}</div>
              <div style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.7, marginBottom: 16, maxWidth: 320, margin: "0 auto 16px" }}>
                {currentProject ? t("runs.empty.project_desc") : t("runs.empty.desc")}
              </div>
              <div style={{ display: "flex", gap: 8, justifyContent: "center", flexWrap: "wrap" }}>
                <button className="btn btn-primary btn-sm" onClick={() => navigate("/catalog")}>{t("runs.empty.cta_catalog")}</button>
                <button className="btn btn-secondary btn-sm" onClick={() => navigate("/generate")}>{t("runs.empty.cta_generate")}</button>
              </div>
            </div>
          ) : !loading && filteredRuns.length === 0 ? (
            <div style={{ padding: "32px 24px", textAlign: "center" }}>
              <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>
                {filtersActive ? t("runs.filter.no_results") : t("runs.history.none")}
              </div>
              {filtersActive && (
                <div style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.7, margin: "0 auto 16px", maxWidth: 320 }}>
                  {t("runs.filter.adjust_desc")}
                </div>
              )}
            </div>
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
                {filteredRuns.map((r, i) => (
                  <tr
                    key={r.run_id || i}
                    ref={r.run_id === selected ? selectedRowRef : null}
                    style={{
                      cursor: "pointer",
                      background: selected === r.run_id ? "var(--accent-light)" : undefined,
                      borderLeft: selected === r.run_id ? "3px solid var(--accent)" : "3px solid transparent",
                    }}
                    onClick={() => openDetail(r.run_id)}
                  >
                    <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>
                      {(r.run_id || "").slice(0, 14)}…
                    </td>
                    <td style={{ fontWeight: 600, fontSize: 13 }}>
                      <span style={{ display: "flex", alignItems: "center", gap: 6, flexWrap: "wrap" }}>
                        {r.test_id || r.test_case_id || "—"}
                        <RunTypeBadge runType={inferRunType(r)} />
                        {getStepsCount(r) > 0 && (
                          <span className="badge badge-gray" style={{ fontSize: 10 }} title="Executed steps">
                            {getStepsCount(r)} steps
                          </span>
                        )}
                      </span>
                    </td>
                    <td>
                      <span style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
                        <span className={statusBadgeClass(r.status)}>{statusBadgeText(r.status)}</span>
                        {r.meta?.retry_policy_applied && (
                          <span
                            className="badge badge-blue"
                            style={{ fontSize: 10 }}
                            title="Auto retry applied"
                          >
                            Auto retry: {r.meta?.retry_count ?? 0}
                          </span>
                        )}
                        {r.meta?.flaky_signal && (
                          <span
                            className="badge badge-gray"
                            style={{ fontSize: 10 }}
                            title={
                              r.meta?.flaky_score != null
                                ? `Historically unstable (score: ${Math.round(Number(r.meta?.flaky_score) * 100)}%)`
                                : "Historically unstable"
                            }
                          >
                            Flaky signal
                          </span>
                        )}
                        {r.meta?.quarantine_recommended && (
                          <span
                            className="badge badge-orange"
                            style={{ fontSize: 10 }}
                            title="Quarantine recommended (inconsistent after retries)"
                          >
                            Quarantine
                          </span>
                        )}
                        {hasEvidenceForList(r) && (
                          <span className="badge badge-green" style={{ fontSize: 10 }} title="Evidence available">
                            E
                          </span>
                        )}
                      </span>
                    </td>
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
            <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 10, flexWrap: "wrap" }}>
              <div className="section-title" style={{ margin: 0 }}>{t("runs.history.run_detail")}</div>
              {initialRunId && selected === initialRunId && (
                <span
                  className="badge badge-gray"
                  style={{ fontSize: 10, letterSpacing: "0.04em", opacity: 0.75 }}
                  title={t("runs.from_execution_tip")}
                >
                  {t("runs.from_execution")}
                </span>
              )}
            </div>
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
                  <span className={statusBadgeClass(detail.status)}>{statusBadgeText(detail.status)}</span>
                  {detail.duration_ms != null && <span className="badge badge-gray">{fmtMs(detail.duration_ms)}</span>}
                  {getStepsCount(detail) > 0 && (
                    <span className="badge badge-gray" title="Executed steps">{getStepsCount(detail)} steps</span>
                  )}
                  {(detail.test_id || detail.test_case_id) && <span className="badge badge-gray">{detail.test_id || detail.test_case_id}</span>}
                  {detail.started_at || detail.created_at ? (
                    <span className="badge badge-gray" title="Start time">{fmtDate(detail.started_at || detail.created_at)}</span>
                  ) : null}
                  <RunTypeBadge runType={inferRunType(detail)} />
                  <BackendBadge backend={inferBackend(detail)} />
                  <CorrelationIdChip value={detail.correlation_id || detail.meta?.correlation_id} />
              {detail.meta?.retry_policy_applied && (
                <span className="badge badge-blue" style={{ fontSize: 10 }} title="Auto retry applied">
                  Auto retry: {detail.meta?.retry_count ?? 0}
                </span>
              )}
              {detail.meta?.flaky_signal && (
                <span
                  className="badge badge-gray"
                  style={{ fontSize: 10 }}
                  title={
                    detail.meta?.flaky_score != null
                      ? `Historically unstable (score: ${Math.round(Number(detail.meta?.flaky_score) * 100)}%)`
                      : "Historically unstable"
                  }
                >
                  Flaky signal
                </span>
              )}
              {detail.meta?.quarantine_recommended && (
                <span className="badge badge-orange" style={{ fontSize: 10 }} title="Quarantine recommended (inconsistent after retries)">
                  Quarantine recommended
                </span>
              )}
                </div>
                {(detail.error_summary || detail.reason || detail.message || detail.error_message) && (
                  <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 8 }}>
                    <strong>{t("runs.history.reason")}</strong>{" "}
                    {detail.error_summary || detail.reason || detail.message || detail.error_message}
                    {(detail.hint || detail.meta?.hint) && (
                      <div style={{ marginTop: 6, color: "var(--text-3)" }}>
                        <strong>Hint:</strong> {detail.hint || detail.meta?.hint}
                      </div>
                    )}
                  </div>
                )}

                <DebugAccordion detail={detail} />

                {/* Action Panel: recommended next steps with explicit confirmation for operations */}
                {detail && (() => {
                  const meta = detail.meta || {};
                  const statusNorm = String(detail.status || "").toLowerCase();
                  const quarantineRecommended = meta.quarantine_recommended === true;
                  const retryPolicyApplied = meta.retry_policy_applied === true;
                  const flakySignalPresent = !!meta.flaky_signal;
                  const failedOrError = statusNorm === "failed" || statusNorm === "error";

                  const shouldShowPanel = quarantineRecommended || retryPolicyApplied || flakySignalPresent || failedOrError;
                  if (!shouldShowPanel) return null;

                  const testCaseId = detail.test_id || detail.test_case_id;
                  const correlationId = detail.correlation_id || meta.correlation_id;
                  const evidenceUrl = detail?.evidence_url || detail?.artifacts?.evidence_url;
                  const reportUrl = detail?.report_url || detail?.artifacts?.report_url;

                  const canRetry =
                    !!testCaseId &&
                    failedOrError &&
                    (retryPolicyApplied || quarantineRecommended || flakySignalPresent);

                  return (
                    <div style={{ marginTop: 14 }}>
                      <div className="section-title" style={{ marginBottom: 10 }}>
                        {t("runs.action_panel.recommended_actions")}{" "}
                        <span style={{ color: "var(--text-3)", fontWeight: 500, fontSize: 12 }}>
                          {t("runs.action_panel.requires_confirmation")}
                        </span>
                      </div>
                      <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
                        {evidenceUrl && (
                          <a className="btn btn-secondary btn-sm" href={evidenceUrl} target="_blank" rel="noreferrer">
                            {t("runs.action_panel.open_evidence")}
                          </a>
                        )}
                        {reportUrl && (
                          <a className="btn btn-secondary btn-sm" href={reportUrl} target="_blank" rel="noreferrer">
                            {t("runs.action_panel.open_report")}
                          </a>
                        )}
                        {correlationId && (
                          <button
                            className="btn btn-secondary btn-sm"
                            onClick={async () => {
                              try {
                                await navigator.clipboard.writeText(String(correlationId));
                              } catch {
                                // No-op: clipboard may be unavailable; avoids side effects.
                              }
                            }}
                            title={t("runs.action_panel.copy_correlation_id")}
                          >
                            {t("runs.action_panel.copy_correlation_id")}
                          </button>
                        )}

                        {canRetry && (
                          <button
                            className="btn btn-primary btn-sm"
                            onClick={() => {
                              setRetryError("");
                              setConfirmRetryOpen(true);
                            }}
                            disabled={retryBusy}
                          >
                            {t("runs.action_panel.retry_test_case")}
                          </button>
                        )}

                        {quarantineRecommended && (
                          <button
                            className="btn btn-secondary btn-sm"
                            disabled
                            title={t("runs.action_panel.quarantine_recommend_disabled_title")}
                          >
                            {t("runs.action_panel.quarantine_recommend")}
                          </button>
                        )}

                        <button
                          className="btn btn-secondary btn-sm"
                          disabled
                          title={t("runs.action_panel.create_ticket_disabled_title")}
                        >
                          {t("runs.action_panel.create_ticket")}
                        </button>
                        <button
                          className="btn btn-secondary btn-sm"
                          disabled
                          title={t("runs.action_panel.assign_owner_disabled_title")}
                        >
                          {t("runs.action_panel.assign_owner")}
                        </button>
                      </div>

                      {lastEnqueuedJobId && (
                        <div style={{ marginTop: 10, fontSize: 12, color: "var(--text-2)" }}>
                          {t("runs.action_panel.retry_job_enqueued_prefix")} <span style={{ fontFamily: "monospace" }}>{lastEnqueuedJobId}</span>
                        </div>
                      )}
                    </div>
                  );
                })()}

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
          {detail && !detail.error && (
            <>
              <EvidenceCard detail={detail} runType={inferRunType(detail)} />
              <div style={{ textAlign: "right", marginTop: -8 }}>
                <button
                  className="btn btn-secondary btn-sm"
                  style={{ fontSize: 11 }}
                  onClick={() => navigate("/evidence")}
                >
                  {t("runs.view_evidence")}
                </button>
              </div>
            </>
          )}

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
                    <div style={{ fontSize: 12, color: "var(--text-1)", fontWeight: 600, marginBottom: 6 }}>
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
                  {riskResult.affected_business_flow && riskResult.affected_business_flow !== "unknown" && (
                    <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>
                      <span className="badge badge-orange">{riskResult.affected_business_flow}</span>
                    </div>
                  )}
                  {riskResult.impact_summary && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginBottom: 8 }}>{riskResult.impact_summary}</div>
                  )}
                  {riskResult.priority_recommendation && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                      <strong>{t("runs.risk.recommended")}</strong> {riskResult.priority_recommendation}
                    </div>
                  )}
                </>
              )}
            </div>
          )}
        </div>
      )}
      <ConfirmModal
        open={confirmRetryOpen}
        busy={retryBusy}
        error={retryError}
        title={t("runs.action_panel.retry_confirmation_title")}
        cancelLabel={t("common.cancel")}
        confirmLabel={t("runs.action_panel.retry_confirmation_confirm")}
        description={`${t("runs.action_panel.retry_confirmation_desc_prefix")} ${
          detail && (detail.test_id || detail.test_case_id)
            ? `"${detail.test_id || detail.test_case_id}"`
            : t("common.unknown")
        }${t("runs.action_panel.retry_confirmation_desc_suffix")}`}
        onCancel={() => {
          setConfirmRetryOpen(false);
          setRetryError("");
        }}
        onConfirm={async () => {
          if (!detail) return;
          const meta = detail.meta || {};
          const testCaseId = detail.test_id || detail.test_case_id;
          if (!testCaseId) {
            setRetryError(t("runs.action_panel.retry_error_missing_id"));
            return;
          }
          setRetryBusy(true);
          setRetryError("");
          try {
            const env = meta.environment || detail.environment || "default";
            const job = await enqueueSingle({ test_case_id: testCaseId, environment: env });
            setLastEnqueuedJobId(job?.job_id || job?.id || null);
            setConfirmRetryOpen(false);
          } catch (e) {
            setRetryError(e?.message || t("runs.action_panel.retry_error_enqueue_failed"));
          } finally {
            setRetryBusy(false);
          }
        }}
      />
      </div>
    </div>
  );
}

// ── Page root ─────────────────────────────────────────────────────────────────

export default function RunsPage() {
  const { t } = useLang();
  const location = useLocation();
  const navigate  = useNavigate();
  const navState  = location.state || {};
  const [activeTab, setActiveTab] = useState(navState.tab ?? 0);
  const [initialRunId] = useState(navState.run_id || null);

  // Clear navigation state from history so back/forward doesn't re-trigger auto-open
  useEffect(() => {
    if (navState.run_id || navState.tab != null) {
      navigate(location.pathname, { replace: true, state: null });
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <div className="page-wrap">

      {/* Page header */}
      <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", marginBottom: 20, flexWrap: "wrap", gap: 12 }}>
        <div>
          <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>{t("runs.page.title")}</h1>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 0" }}>{t("runs.page.subtitle")}</p>
        </div>
        <button
          className="btn btn-secondary btn-sm"
          onClick={() => navigate("/batch")}
          title={t("runs.page.batch_tip")}
          style={{ alignSelf: "flex-start" }}
        >
          {t("runs.page.batch_link")}
        </button>
      </div>

      {/* Tab bar */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "2px solid var(--border)", paddingBottom: 0 }}>
        {TAB_KEYS.map((key, i) => (
          <button
            key={i}
            onClick={() => setActiveTab(i)}
            style={{
              padding: "8px 16px",
              background: activeTab === i ? "var(--accent-light)" : "transparent",
              border: "none",
              borderBottom: activeTab === i ? "2px solid var(--accent-border)" : "2px solid transparent",
              marginBottom: -2,
              fontWeight: activeTab === i ? 500 : 400,
              fontSize: 13,
              color: activeTab === i ? "var(--accent)" : "var(--text-3)",
              cursor: "pointer",
              borderRadius: "8px 8px 0 0",
              transition: "background 0.15s, color 0.15s",
            }}
          >
            {t(key)}
          </button>
        ))}
      </div>

      {activeTab === 0 && <RunHistoryTab initialRunId={initialRunId} />}
      {activeTab === 1 && <EvidenceLookupTab />}
    </div>
  );
}
