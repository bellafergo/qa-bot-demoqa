// src/pages/CatalogPage.jsx
/**
 * Test Catalog — browse, filter, and run catalog test cases.
 * GET /tests, POST /tests/{id}/run, POST /execution/run-batch
 */
import React, { useState, useEffect, useCallback } from "react";
import { listTests, runTest, runBatch } from "../api";

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
      setError(e?.message || "Failed to load tests");
    } finally {
      setLoading(false);
    }
  }, [filters]);

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
      setSelected(new Set(tests.map(t => t.test_case_id)));
    }
  }

  async function handleRunSingle(tc_id) {
    setRunningId(tc_id);
    setRunResult(null);
    try {
      const run = await runTest(tc_id, { headless: true });
      setRunResult({ tc_id, run });
    } catch (e) {
      setRunResult({ tc_id, error: e?.message || "Run failed" });
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
      setBatchResult({ error: e?.message || "Batch failed" });
    } finally {
      setBatchLoading(false);
    }
  }

  return (
    <div className="page-wrap">

      {/* Filters */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "flex-end" }}>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Module</label>
            <input
              className="input"
              placeholder="e.g. checkout"
              value={filters.module}
              onChange={e => setFilters(f => ({ ...f, module: e.target.value }))}
              style={{ width: 140 }}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Type</label>
            <select className="input" value={filters.type} onChange={e => setFilters(f => ({ ...f, type: e.target.value }))} style={{ width: 130 }}>
              <option value="">All types</option>
              {["smoke","regression","functional","negative","e2e"].map(v => <option key={v} value={v}>{v}</option>)}
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Priority</label>
            <select className="input" value={filters.priority} onChange={e => setFilters(f => ({ ...f, priority: e.target.value }))} style={{ width: 120 }}>
              <option value="">All priorities</option>
              {["critical","high","medium","low"].map(v => <option key={v} value={v}>{v}</option>)}
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Status</label>
            <select className="input" value={filters.status} onChange={e => setFilters(f => ({ ...f, status: e.target.value }))} style={{ width: 110 }}>
              <option value="active">Active</option>
              <option value="inactive">Inactive</option>
              <option value="">All</option>
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Test type</label>
            <select className="input" value={filters.test_type} onChange={e => setFilters(f => ({ ...f, test_type: e.target.value }))} style={{ width: 100 }}>
              <option value="">All</option>
              <option value="ui">UI</option>
              <option value="api">API</option>
            </select>
          </div>
          <button className="btn btn-primary" onClick={load} disabled={loading}>
            {loading ? "Loading…" : "Search"}
          </button>
        </div>
      </div>

      {error && <div className="alert alert-error" style={{ marginBottom: 16 }}>{error}</div>}

      {/* Batch action bar */}
      {selected.size > 0 && (
        <div className="card" style={{ marginBottom: 12, display: "flex", alignItems: "center", gap: 12, padding: "12px 18px", background: "var(--accent-light)", borderColor: "var(--accent-border)" }}>
          <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>{selected.size} selected</span>
          <button className="btn btn-primary btn-sm" onClick={handleRunBatch} disabled={batchLoading}>
            {batchLoading ? "Enqueueing…" : `⚡ Run batch (${selected.size})`}
          </button>
          <button className="btn btn-secondary btn-sm" onClick={() => setSelected(new Set())}>Clear</button>
          {batchResult && !batchResult.error && (
            <span style={{ fontSize: 12, color: "var(--green)", fontWeight: 600 }}>
              ✓ Job {batchResult.job_id?.slice(0, 12)}… enqueued (status: {batchResult.status})
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
            {loading ? "Loading…" : `${tests.length} test${tests.length !== 1 ? "s" : ""}`}
          </div>
          {tests.length > 0 && (
            <button className="btn btn-secondary btn-sm" onClick={toggleAll}>
              {selected.size === tests.length ? "Deselect all" : "Select all"}
            </button>
          )}
        </div>

        {tests.length === 0 && !loading ? (
          <div style={{ padding: "24px 20px", color: "var(--text-3)", fontSize: 13 }}>
            No tests match the current filters.
          </div>
        ) : (
          <table className="data-table">
            <thead><tr>
              <th style={{ width: 36 }}></th>
              <th>ID</th>
              <th>Name</th>
              <th>Module</th>
              <th>Type</th>
              <th>Priority</th>
              <th>Runner</th>
              <th>Steps</th>
              <th style={{ width: 110 }}>Actions</th>
            </tr></thead>
            <tbody>
              {tests.map(tc => (
                <React.Fragment key={tc.test_case_id}>
                  <tr
                    style={{ cursor: "pointer" }}
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
                    <td onClick={e => e.stopPropagation()}>
                      <button
                        className="btn btn-primary btn-sm"
                        disabled={runningId === tc.test_case_id}
                        onClick={() => handleRunSingle(tc.test_case_id)}
                      >
                        {runningId === tc.test_case_id ? "…" : "▶ Run"}
                      </button>
                    </td>
                  </tr>
                  {expanded === tc.test_case_id && (
                    <tr>
                      <td colSpan={9} style={{ background: "var(--surface-2)", padding: "12px 20px" }}>
                        <div style={{ fontSize: 12, color: "var(--text-2)", display: "flex", gap: 24, flexWrap: "wrap" }}>
                          <span><b>ID:</b> {tc.id}</span>
                          <span><b>Status:</b> <span className={`badge ${statusClass(tc.status)}`}>{tc.status}</span></span>
                          <span><b>Version:</b> {tc.version}</span>
                          {tc.tags?.length > 0 && <span><b>Tags:</b> {tc.tags.join(", ")}</span>}
                          <span><b>Steps:</b> {tc.steps_count}</span>
                          <span><b>Updated:</b> {tc.updated_at ? new Date(tc.updated_at).toLocaleDateString() : "—"}</span>
                        </div>
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
