// src/pages/EvidencePage.jsx
/**
 * Evidence Library — lean centralized view of all run evidence.
 * GET /evidences  (lean projection — no screenshot_b64, no blobs)
 *
 * Full evidence detail remains at GET /runs/{evidence_id} (HTML report).
 * This page only shows links; no inline images.
 */
import React, { useState, useEffect, useCallback } from "react";
import { listEvidences } from "../api";
import { useLang } from "../i18n/LangContext";

const API_BASE = (
  import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com"
).replace(/\/$/, "");

// ── helpers ──────────────────────────────────────────────────────────────────

function statusBadgeClass(status) {
  const s = String(status || "").toLowerCase();
  if (s === "passed" || s === "pass")      return "badge badge-green";
  if (s === "failed" || s === "fail")      return "badge badge-red";
  if (s === "error")                       return "badge badge-red";
  if (s === "running")                     return "badge badge-blue";
  if (s === "queued")                      return "badge badge-orange";
  return "badge badge-gray";
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

// ── component ─────────────────────────────────────────────────────────────────

export default function EvidencePage() {
  const { t } = useLang();

  const [rows, setRows]         = useState([]);
  const [loading, setLoading]   = useState(true);
  const [error, setError]       = useState(null);
  const [search, setSearch]     = useState("");
  const [statusFilter, setStatusFilter] = useState("");

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
    if (search) {
      const q = search.toLowerCase();
      const matchId   = (r.test_id   || "").toLowerCase().includes(q);
      const matchName = (r.test_name || "").toLowerCase().includes(q);
      const matchErr  = (r.error_summary || "").toLowerCase().includes(q);
      if (!matchId && !matchName && !matchErr) return false;
    }
    return true;
  });

  const hasActiveFilter = search || statusFilter;

  const clearFilters = () => {
    setSearch("");
    setStatusFilter("");
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
        <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "center" }}>
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
          <select
            className="input"
            value={statusFilter}
            onChange={e => setStatusFilter(e.target.value)}
            style={{ flex: "0 0 auto", minWidth: 160 }}
          >
            <option value="">{t("ev.filter.all")}</option>
            {statuses.map(s => (
              <option key={s} value={s}>{s}</option>
            ))}
          </select>

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
        <div className="card" style={{ color: "var(--text-3)", fontSize: 13, padding: "24px 20px" }}>
          {t("ev.empty")}
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
                  <EvidenceRow key={r.run_id} row={r} t={t} />
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

function EvidenceRow({ row, t }) {
  const evidenceHref = row.run_id
    ? `${API_BASE}/runs/${row.run_id}`
    : null;

  return (
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
          {row.status || "unknown"}
        </span>
      </td>

      {/* Duration */}
      <td style={{ fontSize: 12, color: "var(--text-3)", whiteSpace: "nowrap" }}>
        {fmtMs(row.duration_ms)}
      </td>

      {/* Error summary */}
      <td style={{ fontSize: 12, color: "var(--red-text)", maxWidth: 260 }}>
        {row.error_summary ? (
          <span title={row.error_summary}>
            {truncate(row.error_summary, 80)}
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
          {row.report_url && (
            <a
              href={row.report_url}
              target="_blank"
              rel="noreferrer"
              className="btn btn-primary btn-sm"
            >
              {t("ev.action.report")}
            </a>
          )}
        </div>
      </td>
    </tr>
  );
}
