// src/pages/EvidencePage.jsx
/**
 * Evidence Library — lean centralized view of all run evidence.
 * GET /evidences  (lean projection — no screenshot_b64, no blobs)
 *
 * Full evidence detail remains at GET /runs/{evidence_id} (HTML report).
 * This page only shows links; no inline images.
 */
import React, { useState, useEffect, useCallback } from "react";
import { useNavigate } from "react-router-dom";
import { listEvidences, enqueueSingle } from "../api";
import { useLang } from "../i18n/LangContext";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

// ── helpers ──────────────────────────────────────────────────────────────────

function statusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s === "passed" || s === "pass" || s === "completed") return "badge badge-green";
  if (s === "failed" || s === "fail")                        return "badge badge-red";
  if (s === "error")                                         return "badge badge-orange";
  if (s === "running")                                      return "badge badge-blue";
  if (s === "queued")                                       return "badge badge-orange";
  if (s === "planning" || s === "compiled")                 return "badge badge-orange";
  if (s === "canceled" || s === "cancelled")                return "badge badge-gray";
  return "badge badge-gray";
}

function statusIcon(status) {
  const s = String(status || "").toLowerCase();
  if (s === "passed" || s === "pass" || s === "completed") return "✓";
  if (s === "failed" || s === "fail") return "✕";
  if (s === "error") return "⚠";
  if (s === "running") return "⏱";
  if (s === "queued" || s === "planning" || s === "compiled") return "⏳";
  if (s === "canceled" || s === "cancelled") return "⦸";
  return "•";
}

function statusBadgeText(status) {
  const label = status || "—";
  return `${statusIcon(status)} ${label}`;
}

function fmtDate(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short", day: "numeric",
      hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

function fmtMs(ms) {
  if (ms == null || ms === 0) return "—";
  return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`;
}

function truncate(str, n = 80) {
  if (!str) return null;
  return str.length > n ? str.slice(0, n) + "…" : str;
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
          <div style={{ fontSize: 15, fontWeight: 900, color: "var(--text-1)", marginBottom: 6 }}>{title}</div>
          <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>{description}</div>
        </div>
        <div style={{ padding: "14px 20px" }}>
          {error && (
            <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
              {error}
            </div>
          )}
          <div style={{ display: "flex", gap: 10, justifyContent: "flex-end", flexWrap: "wrap" }}>
            <button className="btn btn-secondary btn-sm" onClick={onCancel} disabled={busy}>
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

function getStepsCount(row) {
  if (!row) return 0;
  const n =
    row.steps_count ??
    (Array.isArray(row.steps) ? row.steps.length : null) ??
    (Array.isArray(row.steps_result) ? row.steps_result.length : null) ??
    0;
  const nn = Number(n);
  return Number.isFinite(nn) ? nn : 0;
}

function getCorrelationId(row) {
  if (!row) return null;
  return row.correlation_id || row.meta?.correlation_id || null;
}

function getCanonicalEvidenceUrls(row) {
  if (!row) return { evidenceUrl: null, reportUrl: null };
  const artifacts = row.artifacts || {};
  const evidenceUrl =
    row.evidence_url ||
    artifacts.evidence_url ||
    row.meta?.evidence_url ||
    null;
  const reportUrl =
    row.report_url ||
    artifacts.report_url ||
    row.meta?.report_url ||
    null;
  return { evidenceUrl, reportUrl };
}

// ── component ─────────────────────────────────────────────────────────────────

export default function EvidencePage() {
  const { t } = useLang();
  const navigate = useNavigate();

  const [rows, setRows]         = useState([]);
  const [loading, setLoading]   = useState(true);
  const [error, setError]       = useState(null);
  const [search, setSearch]     = useState("");
  const [statusFilter, setStatusFilter] = useState("");
  const [flakyFilter, setFlakyFilter] = useState("");
  const [quarantineFilter, setQuarantineFilter] = useState("");

  const load = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const data = await listEvidences({ limit: 50 });
      setRows(Array.isArray(data) ? data : []);
    } catch (e) {
      setError(e.message || "Failed to load evidence");
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { load(); }, [load]);

  // ── client-side filtering ─────────────────────────────────────────────────

  const statuses = [...new Set(rows.map(r => r.status).filter(Boolean))].sort();

  const filtered = rows.filter(r => {
    if (statusFilter && r.status !== statusFilter) return false;

    const meta = r.meta || {};
    if (flakyFilter === "only" && !meta.flaky_signal) return false;
    if (quarantineFilter === "only" && meta.quarantine_recommended !== true) return false;

    if (search) {
      const q = search.toLowerCase();
      const matchId   = (r.test_id   || "").toLowerCase().includes(q);
      const matchName = (r.test_name || "").toLowerCase().includes(q);
      const matchErr  = (r.error_summary || "").toLowerCase().includes(q);
      if (!matchId && !matchName && !matchErr) return false;
    }
    return true;
  });

  const hasActiveFilter = search || statusFilter || flakyFilter || quarantineFilter;

  const clearFilters = () => {
    setSearch("");
    setStatusFilter("");
    setFlakyFilter("");
    setQuarantineFilter("");
  };

  // ── render ────────────────────────────────────────────────────────────────

  return (
    <div className="page-wrap">
      {/* Header */}
      <div style={{ marginBottom: 24 }}>
        <h1 style={{ fontSize: 20, fontWeight: 800, margin: 0, color: "var(--text-1)" }}>
          {t("ev.title")}
        </h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 0" }}>
          {t("ev.subtitle")}
        </p>
      </div>

      {/* Filters */}
      <div className="card" style={{ marginBottom: 20, padding: "14px 20px" }}>
        <div style={{ display: "flex", gap: 16, flexWrap: "wrap", alignItems: "center" }}>
          {/* Search */}
          <input
            className="input"
            type="text"
            placeholder={t("ev.filter.search_ph")}
            value={search}
            onChange={e => setSearch(e.target.value)}
            style={{ flex: "1 1 220px", minWidth: 180, maxWidth: 340 }}
          />

          {/* Status dropdown */}
          <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
            <span style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("ev.filter.status")}:</span>
            <select
              className="input"
              value={statusFilter}
              onChange={e => setStatusFilter(e.target.value)}
              style={{ minWidth: 130 }}
              aria-label={t("ev.filter.status")}
            >
              <option value="">{t("ev.filter.all")}</option>
              {statuses.map(s => (
                <option key={s} value={s}>{s}</option>
              ))}
            </select>
          </div>

          {/* Flaky dropdown */}
          <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
            <span style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("ev.filter.flaky")}:</span>
            <select
              className="input"
              value={flakyFilter}
              onChange={e => setFlakyFilter(e.target.value)}
              style={{ minWidth: 110 }}
              aria-label={t("ev.filter.flaky")}
            >
              <option value="">{t("ev.filter.flaky_all")}</option>
              <option value="only">{t("ev.filter.flaky_only")}</option>
            </select>
          </div>

          {/* Quarantine dropdown */}
          <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
            <span style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("ev.filter.quarantine")}:</span>
            <select
              className="input"
              value={quarantineFilter}
              onChange={e => setQuarantineFilter(e.target.value)}
              style={{ minWidth: 130 }}
              aria-label={t("ev.filter.quarantine")}
            >
              <option value="">{t("ev.filter.quarantine_all")}</option>
              <option value="only">{t("ev.filter.quarantine_only")}</option>
            </select>
          </div>

          {/* Clear */}
          {hasActiveFilter && (
            <button
              className="btn btn-secondary btn-sm"
              onClick={clearFilters}
            >
              {t("ev.filter.clear")}
            </button>
          )}

          {/* Refresh */}
          <button
            className="btn btn-secondary btn-sm"
            onClick={load}
            disabled={loading}
            style={{ marginLeft: "auto" }}
          >
            {loading ? "…" : "↻"}
          </button>
        </div>
      </div>

      {/* Content */}
      {loading && (
        <div className="card" style={{ color: "var(--text-3)", fontSize: 13, padding: "24px 20px" }}>
          {t("ev.loading")}
        </div>
      )}

      {!loading && error && (
        <div className="card" style={{ borderColor: "var(--red-border)", padding: "16px 20px" }}>
          <div style={{ color: "var(--red-text)", fontSize: 13 }}>{error}</div>
          <button className="btn btn-secondary btn-sm" style={{ marginTop: 10 }} onClick={load}>
            Retry
          </button>
        </div>
      )}

      {!loading && !error && filtered.length === 0 && (
        <div className="card" style={{ padding: "48px 32px", textAlign: "center" }}>
          <div style={{ fontSize: 32, marginBottom: 12 }}>⊟</div>
          <div style={{ fontSize: 14, fontWeight: 700, color: "var(--text-1)", marginBottom: 6 }}>
            {rows.length === 0 ? t("ev.empty_state.title") : t("ev.filter.no_results")}
          </div>
          <div style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.7, marginBottom: 16, maxWidth: 340, margin: "0 auto 16px" }}>
            {rows.length === 0 ? t("ev.empty_state.desc") : t("ev.filter.no_results")}
          </div>
          {rows.length === 0 && (
            <button className="btn btn-primary btn-sm" onClick={() => navigate("/runs")}>
              {t("ev.empty_state.cta")}
            </button>
          )}
        </div>
      )}

      {!loading && !error && filtered.length > 0 && (
        <div className="card" style={{ padding: 0, overflow: "hidden" }}>
          <div style={{ overflowX: "auto" }}>
            <table className="data-table" style={{ width: "100%", minWidth: 720 }}>
              <thead>
                <tr>
                  <th style={{ width: 130 }}>{t("ev.col.date")}</th>
                  <th style={{ width: 130 }}>{t("ev.col.test_id")}</th>
                  <th>{t("ev.col.test_name")}</th>
                  <th style={{ width: 90 }}>{t("ev.col.status")}</th>
                  <th style={{ width: 80 }}>{t("ev.col.duration")}</th>
                  <th>{t("ev.col.error")}</th>
                  <th style={{ width: 180 }}>{t("ev.col.actions")}</th>
                </tr>
              </thead>
              <tbody>
                {filtered.map(r => (
                  <EvidenceRow key={r.run_id} row={r} t={t} navigate={navigate} />
                ))}
              </tbody>
            </table>
          </div>

          {/* Footer count */}
          <div style={{
            padding: "10px 20px",
            borderTop: "1px solid var(--border)",
            fontSize: 11,
            color: "var(--text-3)",
          }}>
            {filtered.length} record{filtered.length !== 1 ? "s" : ""}
            {hasActiveFilter ? ` (filtered from ${rows.length})` : ""}
          </div>
        </div>
      )}
    </div>
  );
}

// ── row component ─────────────────────────────────────────────────────────────

function EvidenceRow({ row, t, navigate }) {
  const evidenceHref = row.run_id
    ? `${API_BASE}/runs/${row.run_id}`
    : null;

  const { evidenceUrl, reportUrl } = getCanonicalEvidenceUrls(row);
  const correlationId = getCorrelationId(row);
  const stepsCount = getStepsCount(row);
  const errorSummary = row.error_summary || row.reason || row.message || row.error_message || null;
  const hint = row.hint || row.meta?.hint || null;
  const errorType = row.error_type || row.meta?.error_type || null;
  const stepIndex = row.step_index ?? row.meta?.step_index ?? null;
  const meta = row.meta || {};
  const retryCount = meta.retry_count ?? null;
  const retryPolicyApplied = meta.retry_policy_applied ?? null;
  const quarantineRecommended = meta.quarantine_recommended ?? null;
  const finalOutcomeReason = meta.final_outcome_reason ?? null;
  const flakySignal = meta.flaky_signal ?? null;
  const flakyScore = meta.flaky_score ?? null;
  const flipRate = meta.flip_rate ?? null;

  // Human confirmation for operational actions (retry). No side effects without explicit approval.
  const [confirmRetryOpen, setConfirmRetryOpen] = useState(false);
  const [retryBusy, setRetryBusy] = useState(false);
  const [retryError, setRetryError] = useState("");
  const [lastEnqueuedJobId, setLastEnqueuedJobId] = useState(null);

  const statusNorm = String(row.status || "").toLowerCase();
  const failedOrError = statusNorm === "failed" || statusNorm === "error";

  const shouldShowActionPanel =
    failedOrError &&
    (quarantineRecommended === true || retryPolicyApplied === true || !!flakySignal) &&
    (row.test_id || row.test_case_id);

  const testCaseId = row.test_id || row.test_case_id;
  const environment = meta.environment || row.environment || "default";

  return (
    <>
      <tr>
      {/* Date */}
      <td style={{ fontSize: 12, color: "var(--text-3)", whiteSpace: "nowrap" }}>
        {fmtDate(row.started_at)}
      </td>

      {/* Test ID */}
      <td>
        <span style={{ fontFamily: "monospace", fontSize: 12 }}>
          {row.test_id || "—"}
        </span>
      </td>

      {/* Test name */}
      <td style={{ fontSize: 13, color: "var(--text-2)", maxWidth: 200 }}>
        <span title={row.test_name || ""}>
          {truncate(row.test_name, 50) || (row.suite_name ? `Suite: ${row.suite_name}` : "—")}
        </span>
      </td>

      {/* Status */}
      <td>
        <span className={statusBadgeClass(row.status)}>
          {statusBadgeText(row.status || "unknown")}
        </span>
      </td>

      {/* Duration */}
      <td style={{ fontSize: 12, color: "var(--text-3)", whiteSpace: "nowrap" }}>
        {fmtMs(row.duration_ms)}
      </td>

      {/* Error summary */}
      <td style={{ fontSize: 12, color: "var(--red-text)", maxWidth: 260 }}>
        {errorSummary ? (
          <span title={errorSummary}>
            {truncate(errorSummary, 80)}
          </span>
        ) : (
          <span style={{ color: "var(--text-3)" }}>—</span>
        )}
      </td>

      {/* Actions */}
      <td>
        <div style={{ display: "flex", gap: 6, flexWrap: "wrap" }}>
          {evidenceHref ? (
            <a
              href={evidenceHref}
              target="_blank"
              rel="noreferrer"
              className="btn btn-secondary btn-sm"
            >
              {t("ev.action.view")}
            </a>
          ) : (
            <span style={{ fontSize: 12, color: "var(--text-3)" }}>{t("ev.no_link")}</span>
          )}
          {reportUrl && (
            <a
              href={reportUrl}
              target="_blank"
              rel="noreferrer"
              className="btn btn-primary btn-sm"
            >
              {t("ev.action.report")}
            </a>
          )}
          {evidenceUrl && (
            <a
              href={evidenceUrl}
              target="_blank"
              rel="noreferrer"
              className="btn btn-secondary btn-sm"
              title="Evidence"
            >
              Evidence ↗
            </a>
          )}
          {row.run_id && navigate && (
            <button
              className="btn btn-secondary btn-sm"
              style={{ fontSize: 11 }}
              onClick={() => navigate("/runs", { state: { tab: 0, run_id: row.run_id } })}
            >
              {t("ev.view_run")}
            </button>
          )}
        </div>

        {/* Lightweight debug panel (per row) */}
        {(errorType || stepIndex != null || hint || errorSummary || correlationId || retryCount != null || retryPolicyApplied || quarantineRecommended != null || finalOutcomeReason || flakySignal || flakyScore != null || flipRate != null) && (
          <div style={{ marginTop: 10 }}>
            <details>
              <summary style={{ cursor: "pointer", fontSize: 12, color: "var(--text-2)" }}>
                Debug
              </summary>
              <div style={{ marginTop: 10, display: "grid", gap: 8 }}>
                {(row.duration_ms != null || stepsCount > 0 || row.started_at || row.created_at) && (
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
                    {row.duration_ms != null && (
                      <span className="badge badge-gray">{fmtMs(row.duration_ms)}</span>
                    )}
                    {stepsCount > 0 && (
                      <span className="badge badge-gray">{stepsCount} steps</span>
                    )}
                    {(row.started_at || row.created_at) && (
                      <span className="badge badge-gray">{fmtDate(row.started_at || row.created_at)}</span>
                    )}
                  </div>
                )}
                {correlationId && (
                  <div style={{ display: "flex", gap: 8, alignItems: "center", flexWrap: "wrap" }}>
                    <span className="badge badge-gray" style={{ fontSize: 10 }}>correlation</span>
                    <code style={{ fontSize: 11, color: "var(--text-2)" }}>{correlationId}</code>
                  </div>
                )}
                {errorType && <span className="badge badge-orange">error_type: {errorType}</span>}
                {stepIndex != null && (
                  <div>
                    <span style={{ fontSize: 11, fontWeight: 800, color: "var(--text-3)", textTransform: "uppercase" }}>step_index</span>{" "}
                    <code style={{ fontSize: 12, color: "var(--text-2)" }}>{String(stepIndex)}</code>
                  </div>
                )}
                {retryPolicyApplied && (
                  <span className="badge badge-blue" style={{ fontSize: 10 }} title="Auto retry applied">
                    Auto retry: {retryCount ?? 0}
                  </span>
                )}
                {quarantineRecommended === true && (
                  <span className="badge badge-orange" style={{ fontSize: 10 }} title="Quarantine recommended">
                    Quarantine recommended
                  </span>
                )}
                {flakySignal && (
                  <span className="badge badge-gray" style={{ fontSize: 10 }} title="Flaky signal">
                    Flaky signal
                  </span>
                )}
                {(flakyScore != null || flipRate != null) && (
                  <div style={{ fontSize: 12, color: "var(--text-3)" }}>
                    {flakyScore != null && (
                      <div style={{ marginBottom: 4 }}>
                        Flaky score: <span style={{ fontFamily: "monospace", color: "var(--text-2)" }}>{Math.round(Number(flakyScore) * 100)}%</span>
                      </div>
                    )}
                    {flipRate != null && (
                      <div style={{ marginBottom: 4 }}>
                        Flip rate: <span style={{ fontFamily: "monospace", color: "var(--text-2)" }}>{Number(flipRate).toFixed(2)}</span>
                      </div>
                    )}
                  </div>
                )}
                {finalOutcomeReason && (
                  <div style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>
                    <strong>Final outcome reason:</strong> {finalOutcomeReason}
                  </div>
                )}
                {hint && (
                  <div>
                    <span style={{ fontSize: 11, fontWeight: 800, color: "var(--text-3)", textTransform: "uppercase" }}>hint</span>
                    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginTop: 4 }}>{hint}</div>
                  </div>
                )}
                {errorSummary && (
                  <div>
                    <span style={{ fontSize: 11, fontWeight: 800, color: "var(--text-3)", textTransform: "uppercase" }}>raw</span>
                    <pre className="code-block" style={{ marginTop: 6, maxHeight: 140, overflow: "auto" }}>
                      {errorSummary}
                    </pre>
                  </div>
                )}

                {shouldShowActionPanel && (
                  <div style={{ marginTop: 10, paddingTop: 10, borderTop: "1px solid var(--border)" }}>
                    <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 8, fontWeight: 800 }}>
                      {t("runs.action_panel.suggested_next_steps")}{" "}
                      <span style={{ fontWeight: 600, color: "var(--text-3)" }}>
                        {t("runs.action_panel.requires_confirmation_for_operations")}
                      </span>
                    </div>
                    <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
                      {evidenceUrl && (
                        <a className="btn btn-secondary btn-sm" href={evidenceUrl} target="_blank" rel="noreferrer">
                          {t("runs.action_panel.open_evidence")}
                        </a>
                      )}
                      {reportUrl && (
                        <a className="btn btn-primary btn-sm" href={reportUrl} target="_blank" rel="noreferrer">
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
                              // no-op
                            }
                          }}
                          title={t("runs.action_panel.copy_correlation_id")}
                        >
                          {t("runs.action_panel.copy_correlation_id")}
                        </button>
                      )}

                      <button
                        className="btn btn-primary btn-sm"
                        disabled={retryBusy}
                        onClick={() => {
                          setRetryError("");
                          setConfirmRetryOpen(true);
                        }}
                      >
                        {t("runs.action_panel.retry_test_case")}
                      </button>

                      <button
                        className="btn btn-secondary btn-sm"
                        disabled
                        title={t("runs.action_panel.quarantine_recommend_disabled_title")}
                      >
                        {t("runs.action_panel.quarantine_recommend")}
                      </button>

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
                )}
              </div>
            </details>
          </div>
        )}
      </td>
    </tr>
      <ConfirmModal
        open={confirmRetryOpen}
        busy={retryBusy}
        error={retryError}
        title={t("runs.action_panel.retry_confirmation_title")}
        cancelLabel={t("common.cancel")}
        confirmLabel={t("runs.action_panel.retry_confirmation_confirm")}
        description={`${t("runs.action_panel.retry_confirmation_desc_prefix")} ${
          testCaseId ? `"${testCaseId}"` : t("common.unknown")
        }${t("runs.action_panel.retry_confirmation_desc_suffix")}`}
        onCancel={() => {
          setConfirmRetryOpen(false);
          setRetryError("");
        }}
        onConfirm={async () => {
          if (!testCaseId) {
            setRetryError(t("runs.action_panel.retry_error_missing_id"));
            return;
          }
          setRetryBusy(true);
          setRetryError("");
          try {
            const job = await enqueueSingle({ test_case_id: testCaseId, environment });
            setLastEnqueuedJobId(job?.job_id || job?.id || null);
            setConfirmRetryOpen(false);
          } catch (e) {
            setRetryError(e?.message || t("runs.action_panel.retry_error_enqueue_failed"));
          } finally {
            setRetryBusy(false);
          }
        }}
      />
    </>
  );
}
