// src/pages/CatalogPage.jsx
/**
 * Test Catalog — browse, filter, and run catalog test cases.
 * GET /tests, POST /tests/{id}/run, POST /execution/run-batch
 */
import React, { useState, useEffect, useCallback } from "react";
import { useLocation } from "react-router-dom";
import { listTests, runTest, runBatch, listVersions, rollbackTest } from "../api";
import { useLang } from "../i18n/LangContext";

const API_BASE = (import.meta?.env?.VITE_API_BASE || "https://qa-bot-demoqa.onrender.com").replace(/\/$/, "");

function statusClass(s) {
  const v = String(s || "").toLowerCase();
  if (v === "pass")     return "badge-green";
  if (v === "fail" || v === "error") return "badge-red";
  if (v === "active")   return "badge-green";
  if (v === "inactive") return "badge-gray";
  return "badge-gray";
}

function priorityClass(p) {
  const v = String(p || "").toLowerCase();
  if (v === "critical") return "badge-red";
  if (v === "high")     return "badge-orange";
  if (v === "medium")   return "badge-blue";
  return "badge-gray";
}

function fmtMs(ms) {
  if (ms == null) return "—";
  return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`;
}

export default function CatalogPage() {
  const { t } = useLang();
  const location = useLocation();
  const highlightModule = location.state?.highlightModule;

  const [filters, setFilters]   = useState({ module: "", type: "", priority: "", status: "active", test_type: "" });
  const [tests, setTests]       = useState([]);
  const [loading, setLoading]   = useState(false);
  const [error, setError]       = useState("");

  const [selected, setSelected]         = useState(new Set());
  const [runningId, setRunningId]       = useState(null);
  const [runResult, setRunResult]       = useState(null);   // { tc_id, run }
  const [batchResult, setBatchResult]   = useState(null);
  const [batchLoading, setBatchLoading] = useState(false);

  const [expanded, setExpanded] = useState(null);   // tc_id of expanded row

  // ── Version history state ─────────────────────────────────────────────────
  const [versionsOpen,    setVersionsOpen]    = useState(null);   // tc_id whose history is open
  const [versions,        setVersions]        = useState([]);
  const [versionsLoading, setVersionsLoading] = useState(false);
  const [versionsError,   setVersionsError]   = useState("");
  const [rollingBack,     setRollingBack]      = useState(null);   // version_number being rolled back
  const [rollbackMsg,     setRollbackMsg]      = useState("");     // success / error message

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    setSelected(new Set());
    setBatchResult(null);
    try {
      const params = {};
      if (filters.module)    params.module    = filters.module;
      if (filters.type)      params.type      = filters.type;
      if (filters.priority)  params.priority  = filters.priority;
      if (filters.status !== undefined) params.status = filters.status;
      if (filters.test_type) params.test_type = filters.test_type;
      params.limit = 200;
      const data = await listTests(params);
      setTests(Array.isArray(data) ? data : []);
    } catch (e) {
      setError(e?.message || t("catalog.error.load_failed"));
    } finally {
      setLoading(false);
    }
  }, [filters, t]);

  useEffect(() => { load(); }, []);   // load on mount

  function toggleSelect(id) {
    setSelected(prev => {
      const s = new Set(prev);
      s.has(id) ? s.delete(id) : s.add(id);
      return s;
    });
  }

  function toggleAll() {
    if (selected.size === tests.length) {
      setSelected(new Set());
    } else {
      setSelected(new Set(tests.map(tc => tc.test_case_id)));
    }
  }

  async function handleRunSingle(tc_id) {
    setRunningId(tc_id);
    setRunResult(null);
    try {
      const run = await runTest(tc_id, { headless: true });
      setRunResult({ tc_id, run });
    } catch (e) {
      setRunResult({ tc_id, error: e?.message || t("catalog.error.run_failed") });
    } finally {
      setRunningId(null);
    }
  }

  async function handleRunBatch() {
    if (selected.size === 0) return;
    setBatchLoading(true);
    setBatchResult(null);
    try {
      const result = await runBatch({ test_case_ids: Array.from(selected) });
      setBatchResult(result);
    } catch (e) {
      setBatchResult({ error: e?.message || t("catalog.error.batch_failed") });
    } finally {
      setBatchLoading(false);
    }
  }

  async function handleOpenVersions(tc_id) {
    if (versionsOpen === tc_id) {
      setVersionsOpen(null);
      setVersions([]);
      setRollbackMsg("");
      return;
    }
    setVersionsOpen(tc_id);
    setVersions([]);
    setVersionsError("");
    setRollbackMsg("");
    setVersionsLoading(true);
    try {
      const data = await listVersions(tc_id);
      setVersions(Array.isArray(data) ? data : []);
    } catch (e) {
      setVersionsError(e?.message || t("catalog.versions.loading"));
    } finally {
      setVersionsLoading(false);
    }
  }

  async function handleRollback(tc_id, version_number, currentVersion) {
    const confirmMsg = t("catalog.versions.rollback_confirm").replace("{v}", version_number);
    if (!window.confirm(confirmMsg)) return;
    setRollingBack(version_number);
    setRollbackMsg("");
    try {
      const res = await rollbackTest(tc_id, { version: version_number, reason: `Rollback to v${version_number}` });
      const msg = t("catalog.versions.rollback_ok").replace("{v}", res.new_version);
      setRollbackMsg(msg);
      // Refresh test list to show updated version number
      await load();
      // Refresh version history
      const data = await listVersions(tc_id);
      setVersions(Array.isArray(data) ? data : []);
    } catch (e) {
      setRollbackMsg(`✗ ${t("catalog.versions.rollback_err")}: ${e?.message || ""}`);
    } finally {
      setRollingBack(null);
    }
  }

  return (
    <div className="page-wrap">

      {/* Filters */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "flex-end" }}>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("catalog.filter.module_label")}</label>
            <input
              className="input"
              placeholder={t("catalog.filter.module_placeholder")}
              value={filters.module}
              onChange={e => setFilters(f => ({ ...f, module: e.target.value }))}
              style={{ width: 140 }}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("catalog.filter.type_label")}</label>
            <select className="input" value={filters.type} onChange={e => setFilters(f => ({ ...f, type: e.target.value }))} style={{ width: 130 }}>
              <option value="">{t("catalog.filter.type_all")}</option>
              {["smoke","regression","functional","negative","e2e"].map(v => <option key={v} value={v}>{v}</option>)}
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("catalog.filter.priority_label")}</label>
            <select className="input" value={filters.priority} onChange={e => setFilters(f => ({ ...f, priority: e.target.value }))} style={{ width: 120 }}>
              <option value="">{t("catalog.filter.priority_all")}</option>
              {["critical","high","medium","low"].map(v => <option key={v} value={v}>{v}</option>)}
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("catalog.filter.status_label")}</label>
            <select className="input" value={filters.status} onChange={e => setFilters(f => ({ ...f, status: e.target.value }))} style={{ width: 110 }}>
              <option value="active">{t("catalog.filter.status_active")}</option>
              <option value="inactive">{t("catalog.filter.status_inactive")}</option>
              <option value="">{t("catalog.filter.status_all")}</option>
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("catalog.filter.test_type_label")}</label>
            <select className="input" value={filters.test_type} onChange={e => setFilters(f => ({ ...f, test_type: e.target.value }))} style={{ width: 100 }}>
              <option value="">{t("catalog.filter.test_type_all")}</option>
              <option value="ui">UI</option>
              <option value="api">API</option>
            </select>
          </div>
          <button className="btn btn-primary" onClick={load} disabled={loading}>
            {loading ? t("catalog.filter.loading") : t("catalog.filter.search")}
          </button>
        </div>
      </div>

      {error && <div className="alert alert-error" style={{ marginBottom: 16 }}>{error}</div>}

      {/* Batch action bar */}
      {selected.size > 0 && (
        <div className="card" style={{ marginBottom: 12, display: "flex", alignItems: "center", gap: 12, padding: "12px 18px", background: "var(--accent-light)", borderColor: "var(--accent-border)" }}>
          <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>{selected.size} {t("catalog.batch.selected")}</span>
          <button className="btn btn-primary btn-sm" onClick={handleRunBatch} disabled={batchLoading}>
            {batchLoading ? t("catalog.batch.enqueueing") : `${t("catalog.batch.run_batch")} (${selected.size})`}
          </button>
          <button className="btn btn-secondary btn-sm" onClick={() => setSelected(new Set())}>{t("catalog.batch.clear")}</button>
          {batchResult && !batchResult.error && (
            <span style={{ fontSize: 12, color: "var(--green)", fontWeight: 600 }}>
              ✓ Job {batchResult.job_id?.slice(0, 12)}… {t("catalog.batch.enqueued")} (status: {batchResult.status})
            </span>
          )}
          {batchResult?.error && (
            <span style={{ fontSize: 12, color: "var(--red)" }}>✗ {batchResult.error}</span>
          )}
        </div>
      )}

      {/* Run result */}
      {runResult && (
        <div className={`alert ${runResult.error ? "alert-error" : "alert-success"}`} style={{ marginBottom: 16 }}>
          {runResult.error
            ? `✗ ${runResult.tc_id}: ${runResult.error}`
            : `✓ ${runResult.tc_id} → ${runResult.run?.status} (${fmtMs(runResult.run?.duration_ms)})`
          }
        </div>
      )}

      {/* Table */}
      <div className="card" style={{ padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between" }}>
          <div className="section-title" style={{ margin: 0 }}>
            {loading ? t("catalog.table.loading") : `${tests.length} test${tests.length !== 1 ? "s" : ""}`}
          </div>
          {tests.length > 0 && (
            <button className="btn btn-secondary btn-sm" onClick={toggleAll}>
              {selected.size === tests.length ? t("catalog.table.deselect_all") : t("catalog.table.select_all")}
            </button>
          )}
        </div>

        {tests.length === 0 && !loading ? (
          <div style={{ padding: "24px 20px", color: "var(--text-3)", fontSize: 13 }}>
            {t("catalog.table.no_results")}
          </div>
        ) : (
          <table className="data-table">
            <thead><tr>
              <th style={{ width: 36 }}></th>
              <th>{t("catalog.table.col.id")}</th>
              <th>{t("catalog.table.col.name")}</th>
              <th>{t("catalog.table.col.module")}</th>
              <th>{t("catalog.table.col.type")}</th>
              <th>{t("catalog.table.col.priority")}</th>
              <th>{t("catalog.table.col.runner")}</th>
              <th>{t("catalog.table.col.steps")}</th>
              <th style={{ width: 110 }}>{t("catalog.table.col.actions")}</th>
            </tr></thead>
            <tbody>
              {tests.map(tc => (
                <React.Fragment key={tc.test_case_id}>
                  <tr
                    style={{
                      cursor: "pointer",
                      ...(tc.module === highlightModule && { border: "2px solid var(--accent)" }),
                    }}
                    onClick={() => setExpanded(expanded === tc.test_case_id ? null : tc.test_case_id)}
                  >
                    <td onClick={e => e.stopPropagation()}>
                      <input
                        type="checkbox"
                        checked={selected.has(tc.test_case_id)}
                        onChange={() => toggleSelect(tc.test_case_id)}
                      />
                    </td>
                    <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>{tc.test_case_id}</td>
                    <td style={{ fontWeight: 600, fontSize: 13 }}>{tc.name}</td>
                    <td><span className="badge badge-gray">{tc.module}</span></td>
                    <td><span className="badge badge-blue">{tc.type}</span></td>
                    <td><span className={`badge ${priorityClass(tc.priority)}`}>{tc.priority}</span></td>
                    <td><span className="badge badge-gray">{tc.test_type || "ui"}</span></td>
                    <td style={{ fontSize: 12, color: "var(--text-2)" }}>{tc.steps_count}</td>
                    <td onClick={e => e.stopPropagation()} style={{ whiteSpace: "nowrap" }}>
                      <button
                        className="btn btn-primary btn-sm"
                        disabled={runningId === tc.test_case_id}
                        onClick={() => handleRunSingle(tc.test_case_id)}
                        style={{ marginRight: 4 }}
                      >
                        {runningId === tc.test_case_id ? "…" : t("catalog.table.run")}
                      </button>
                      <button
                        className={`btn btn-sm ${versionsOpen === tc.test_case_id ? "btn-primary" : "btn-secondary"}`}
                        onClick={() => handleOpenVersions(tc.test_case_id)}
                        title={t("catalog.versions.title")}
                      >
                        ⏱ {t("catalog.versions.btn")}
                      </button>
                    </td>
                  </tr>
                  {expanded === tc.test_case_id && (
                    <tr>
                      <td colSpan={9} style={{ background: "var(--surface-2)", padding: "12px 20px" }}>
                        <div style={{ fontSize: 12, color: "var(--text-2)", display: "flex", gap: 24, flexWrap: "wrap" }}>
                          <span><b>ID:</b> {tc.id}</span>
                          <span><b>{t("catalog.row.status")}</b> <span className={`badge ${statusClass(tc.status)}`}>{tc.status}</span></span>
                          <span><b>{t("catalog.row.version")}</b> {tc.version}</span>
                          {tc.tags?.length > 0 && <span><b>{t("catalog.row.tags")}</b> {tc.tags.join(", ")}</span>}
                          <span><b>{t("catalog.row.steps")}</b> {tc.steps_count}</span>
                          <span><b>{t("catalog.row.updated")}</b> {tc.updated_at ? new Date(tc.updated_at).toLocaleDateString() : "—"}</span>
                        </div>
                      </td>
                    </tr>
                  )}
                  {versionsOpen === tc.test_case_id && (
                    <tr>
                      <td colSpan={9} style={{ background: "var(--accent-light)", borderTop: "1px solid var(--border)", padding: "14px 20px" }}>
                        <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 10 }}>
                          <span style={{ fontWeight: 700, fontSize: 13, color: "var(--accent)" }}>
                            ⏱ {t("catalog.versions.title")} — {tc.test_case_id}
                          </span>
                          <span style={{ fontSize: 11, color: "var(--text-3)" }}>
                            Rollback creates a new version — history is preserved.
                          </span>
                          <button className="btn btn-secondary btn-sm" style={{ marginLeft: "auto", fontSize: 11 }} onClick={() => setVersionsOpen(null)}>
                            {t("catalog.versions.close")}
                          </button>
                        </div>
                        {rollbackMsg && (
                          <div className={`alert ${rollbackMsg.startsWith("✗") ? "alert-error" : "alert-success"}`} style={{ marginBottom: 10, fontSize: 12 }}>
                            {rollbackMsg}
                          </div>
                        )}
                        {versionsLoading && (
                          <div style={{ fontSize: 12, color: "var(--text-3)" }}>{t("catalog.versions.loading")}</div>
                        )}
                        {versionsError && (
                          <div style={{ fontSize: 12, color: "var(--red)" }}>{versionsError}</div>
                        )}
                        {!versionsLoading && versions.length === 0 && !versionsError && (
                          <div style={{ fontSize: 12, color: "var(--text-3)" }}>{t("catalog.versions.empty")}</div>
                        )}
                        {versions.length > 0 && (
                          <table className="data-table" style={{ fontSize: 12 }}>
                            <thead><tr>
                              <th style={{ width: 60 }}>{t("catalog.versions.col.v")}</th>
                              <th>{t("catalog.versions.col.date")}</th>
                              <th>{t("catalog.versions.col.source")}</th>
                              <th>{t("catalog.versions.col.note")}</th>
                              <th style={{ width: 100 }}></th>
                            </tr></thead>
                            <tbody>
                              {versions.map(v => {
                                const isCurrent = v.version_number === tc.version;
                                return (
                                  <tr key={v.version_number} style={isCurrent ? { background: "var(--accent-light)", fontWeight: 600 } : {}}>
                                    <td>
                                      v{v.version_number}
                                      {isCurrent && (
                                        <span className="badge badge-green" style={{ marginLeft: 4, fontSize: 10 }}>
                                          {t("catalog.versions.current")}
                                        </span>
                                      )}
                                    </td>
                                    <td style={{ color: "var(--text-2)" }}>
                                      {v.created_at ? new Date(v.created_at).toLocaleString() : "—"}
                                    </td>
                                    <td>
                                      <span className="badge badge-gray" style={{ fontSize: 10 }}>{v.source || "manual"}</span>
                                    </td>
                                    <td style={{ color: "var(--text-2)", maxWidth: 240, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                                      {v.change_note || "—"}
                                    </td>
                                    <td>
                                      {!isCurrent && (
                                        <button
                                          className="btn btn-secondary btn-sm"
                                          style={{ fontSize: 11 }}
                                          disabled={!!rollingBack}
                                          onClick={() => handleRollback(tc.test_case_id, v.version_number, tc.version)}
                                        >
                                          {rollingBack === v.version_number ? "…" : t("catalog.versions.rollback_btn")}
                                        </button>
                                      )}
                                    </td>
                                  </tr>
                                );
                              })}
                            </tbody>
                          </table>
                        )}
                      </td>
                    </tr>
                  )}
                </React.Fragment>
              ))}
            </tbody>
          </table>
        )}
      </div>
    </div>
  );
}
