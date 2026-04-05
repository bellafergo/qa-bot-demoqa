// src/pages/CatalogPage.jsx
/**
 * Test Catalog — browse, filter, and run catalog test cases.
 * GET /tests, POST /tests/{id}/run, POST /execution/run-batch
 */
import React, { useState, useEffect, useCallback } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { listTests, getTest, runTest, runBatch, updateTest, listVersions, rollbackTest, diffVersions, previewAutoFix } from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";

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

/** Alineado con services/test_auto_fixer.is_unstable_selector (subset mínimo). */
function isFragileSelectorString(sel) {
  if (sel == null || typeof sel !== "string" || !sel.trim()) return false;
  const s = sel.trim();
  if (/^#:[A-Za-z0-9]{4,}:-/.test(s)) return true;
  if (/:nth-child\(\d+\)$/.test(s)) return true;
  if (/#[a-z]+-[a-f0-9]{6,}/i.test(s)) return true;
  return false;
}

function selectorFromStepOrAssertion(obj) {
  if (!obj || typeof obj !== "object") return null;
  if (typeof obj.selector === "string" && obj.selector.trim()) return obj.selector.trim();
  if (typeof obj.target === "string" && obj.target.trim()) return obj.target.trim();
  return null;
}

/** Pasos/assertions en JSON (editor) contienen algún selector frágil. */
function jsonStepsOrAssertionsHaveFragileSelectors(stepsJson, assertionsJson) {
  const scan = (raw) => {
    let arr;
    try {
      arr = JSON.parse(raw || "[]");
    } catch {
      return false;
    }
    if (!Array.isArray(arr)) return false;
    return arr.some((row) => isFragileSelectorString(selectorFromStepOrAssertion(row)));
  };
  return scan(stepsJson) || scan(assertionsJson);
}

// ── Diff display component ────────────────────────────────────────────────────

function DiffPanel({ diff, t }) {
  const { steps = [], assertions = [], fields = {} } = diff.diff || {};
  const hasSteps      = steps.length > 0;
  const hasAssertions = assertions.length > 0;
  const hasFields     = Object.keys(fields).length > 0;

  const lineStyle = added => ({
    fontFamily:  "monospace",
    fontSize:    12,
    padding:     "2px 8px",
    borderRadius: 3,
    marginBottom: 2,
    background:   added ? "rgba(34,197,94,0.10)" : "rgba(239,68,68,0.10)",
    color:        added ? "var(--green)"          : "var(--red)",
  });

  if (diff.identical) {
    return (
      <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 10, fontStyle: "italic" }}>
        {t("catalog.versions.identical")}
      </div>
    );
  }

  return (
    <div style={{ marginBottom: 12, border: "1px solid var(--border)", borderRadius: 6, overflow: "hidden" }}>
      <div style={{ background: "var(--surface-2)", padding: "6px 12px", fontSize: 11, fontWeight: 500, color: "var(--text-2)", borderBottom: "1px solid var(--border)" }}>
        v{diff.from_version} → v{diff.to_version}
      </div>
      <div style={{ padding: "10px 12px" }}>
        {hasFields && (
          <div style={{ marginBottom: 8 }}>
            <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", marginBottom: 4 }}>{t("catalog.versions.diff_fields")}</div>
            {Object.entries(fields).map(([field, change]) => (
              <div key={field} style={{ fontFamily: "monospace", fontSize: 12, marginBottom: 2 }}>
                <span style={{ color: "var(--text-2)", fontWeight: 600 }}>{field}:</span>{" "}
                <span style={{ color: "var(--red)", textDecoration: "line-through" }}>{change.from}</span>
                {" → "}
                <span style={{ color: "var(--green)" }}>{change.to}</span>
              </div>
            ))}
          </div>
        )}
        {hasSteps && (
          <div style={{ marginBottom: 8 }}>
            <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", marginBottom: 4 }}>{t("catalog.versions.diff_steps")}</div>
            {steps.map((entry, i) => (
              <div key={i} style={lineStyle(entry.type === "added")}>
                {entry.type === "added" ? "+ " : "- "}{entry.value}
              </div>
            ))}
          </div>
        )}
        {hasAssertions && (
          <div>
            <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", marginBottom: 4 }}>{t("catalog.versions.diff_assertions")}</div>
            {assertions.map((entry, i) => (
              <div key={i} style={lineStyle(entry.type === "added")}>
                {entry.type === "added" ? "+ " : "- "}{entry.value}
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}

export default function CatalogPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const location = useLocation();
  const navigate = useNavigate();
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

  // ── Edit state ────────────────────────────────────────────────────────────
  const [editingId,  setEditingId]  = useState(null);
  const [editForm,   setEditForm]   = useState(null);
  const [editError,  setEditError]  = useState("");
  const [saving,     setSaving]     = useState(false);

  // ── Auto-fix preview state ────────────────────────────────────────────────
  const [fixPreviewResult,  setFixPreviewResult]  = useState(null);
  const [fixPreviewLoading, setFixPreviewLoading] = useState(false);

  // ── Version history state ─────────────────────────────────────────────────
  const [versionsOpen,    setVersionsOpen]    = useState(null);   // tc_id whose history is open
  const [versions,        setVersions]        = useState([]);
  const [versionsLoading, setVersionsLoading] = useState(false);
  const [versionsError,   setVersionsError]   = useState("");
  const [rollingBack,     setRollingBack]      = useState(null);   // version_number being rolled back
  const [rollbackMsg,     setRollbackMsg]      = useState("");     // success / error message
  const [compareFrom,     setCompareFrom]      = useState("");
  const [compareTo,       setCompareTo]        = useState("");
  const [diffResult,      setDiffResult]       = useState(null);
  const [diffLoading,     setDiffLoading]      = useState(false);
  const [diffError,       setDiffError]        = useState("");

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    setSelected(new Set());
    setBatchResult(null);
    try {
      const params = {};
      if (filters.module?.trim()) params.search = filters.module.trim();
      if (filters.type)      params.type      = filters.type;
      if (filters.priority)  params.priority  = filters.priority;
      if (filters.status !== undefined) params.status = filters.status;
      if (filters.test_type) params.test_type = filters.test_type;
      params.limit = 200;
      if (currentProject?.id) params.project_id = currentProject.id;
      const data = await listTests(params);
      setTests(Array.isArray(data) ? data : []);
    } catch (e) {
      setError(e?.message || t("catalog.error.load_failed"));
    } finally {
      setLoading(false);
    }
  }, [filters, t, currentProject?.id]);

  useEffect(() => {
    load();
  }, [load]);

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
      setDiffResult(null);
      setDiffError("");
      return;
    }
    setVersionsOpen(tc_id);
    setVersions([]);
    setVersionsError("");
    setRollbackMsg("");
    setDiffResult(null);
    setDiffError("");
    setCompareFrom("");
    setCompareTo("");
    setVersionsLoading(true);
    try {
      const data = await listVersions(tc_id);
      const arr = Array.isArray(data) ? data : [];
      setVersions(arr);
      // Pre-fill compare selectors: oldest → newest
      if (arr.length >= 2) {
        const nums = arr.map(v => v.version_number).sort((a, b) => a - b);
        setCompareFrom(String(nums[nums.length - 2]));
        setCompareTo(String(nums[nums.length - 1]));
      }
    } catch (e) {
      setVersionsError(e?.message || t("catalog.versions.loading"));
    } finally {
      setVersionsLoading(false);
    }
  }

  async function handleEdit(tc_id) {
    setEditError("");
    try {
      const full = await getTest(tc_id);
      setEditingId(tc_id);
      setExpanded(tc_id);
      setEditForm({
        name:       full.name,
        module:     full.module,
        priority:   full.priority,
        changeNote: "",
        stepsJson:      JSON.stringify(
          full.steps?.map(s => s.model_dump ? s.model_dump() : s) ?? [], null, 2
        ),
        assertionsJson: JSON.stringify(
          full.assertions?.map(a => a.model_dump ? a.model_dump() : a) ?? [], null, 2
        ),
      });
    } catch (e) {
      setEditError(e?.message || "Failed to load test");
    }
  }

  function handleCancelEdit() {
    setEditingId(null);
    setEditForm(null);
    setEditError("");
    setFixPreviewResult(null);
  }

  async function handleAutoFix() {
    let steps, assertions;
    try {
      steps      = JSON.parse(editForm.stepsJson);
      assertions = JSON.parse(editForm.assertionsJson);
    } catch {
      setEditError(t("catalog.edit.autofix_invalid_json"));
      return;
    }
    setEditError("");
    setFixPreviewResult(null);
    setFixPreviewLoading(true);
    try {
      const result = await previewAutoFix({ steps, assertions });
      setFixPreviewResult(result);
    } catch (e) {
      setEditError(e?.message || "Auto-fix preview failed");
    } finally {
      setFixPreviewLoading(false);
    }
  }

  function handleApplyFix() {
    if (!fixPreviewResult) return;
    setEditForm(f => ({
      ...f,
      stepsJson:      JSON.stringify(fixPreviewResult.steps,      null, 2),
      assertionsJson: JSON.stringify(fixPreviewResult.assertions, null, 2),
    }));
    setFixPreviewResult(null);
  }

  async function handleSave(tc_id) {
    let steps, assertions;
    try {
      steps      = JSON.parse(editForm.stepsJson);
      assertions = JSON.parse(editForm.assertionsJson);
    } catch {
      setEditError("Invalid JSON in steps or assertions — please fix and retry.");
      return;
    }
    setSaving(true);
    setEditError("");
    try {
      await updateTest(tc_id, {
        name:         editForm.name,
        module:       editForm.module,
        priority:     editForm.priority,
        steps,
        assertions,
        _change_note: editForm.changeNote || "Edited from UI",
      });
      handleCancelEdit();
      await load();
    } catch (e) {
      setEditError(e?.message || "Save failed");
    } finally {
      setSaving(false);
    }
  }

  async function handleCompare(tc_id) {
    if (!compareFrom || !compareTo) return;
    setDiffLoading(true);
    setDiffResult(null);
    setDiffError("");
    try {
      const res = await diffVersions(tc_id, compareFrom, compareTo);
      setDiffResult(res);
    } catch (e) {
      setDiffError(e?.message || "Diff failed");
    } finally {
      setDiffLoading(false);
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

      {/* Page header */}
      <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", flexWrap: "wrap", gap: 12, marginBottom: 20 }}>
        <div>
          <h1 style={{ fontSize: 20, fontWeight: 600, margin: 0, color: "var(--text-1)" }}>{t("catalog.page.title")}</h1>
          <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 0" }}>{t("catalog.page.subtitle")}</p>
          {currentProject && (
            <div style={{ marginTop: 8, display: "inline-flex", alignItems: "center", gap: 8 }}>
              <span
                aria-hidden
                style={{
                  width: 8,
                  height: 8,
                  borderRadius: "50%",
                  background: currentProject.color || "var(--accent)",
                }}
              />
              <span className="badge badge-gray" style={{ fontSize: 11, fontWeight: 500 }}>
                {t("catalog.active_project", { name: currentProject.name })}
              </span>
            </div>
          )}
        </div>
        <button className="btn btn-primary" onClick={() => navigate("/generate")}>
          ⊕ {t("catalog.page.generate_btn")}
        </button>
      </div>

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
              onKeyDown={e => { if (e.key === "Enter") load(); }}
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
          {runResult.error ? (
            <div>
              {`✗ ${runResult.tc_id}: ${runResult.error}`}
            </div>
          ) : (
            <div style={{ display: "flex", flexDirection: "column", gap: 6 }}>
              <div>
                {`✓ ${runResult.tc_id} → ${runResult.run?.status} (${fmtMs(runResult.run?.duration_ms)})`}
              </div>
              {(runResult.run?.correlation_id || runResult.run?.meta?.correlation_id) ? (
                <div style={{ fontSize: 12, color: "var(--text-3)" }}>
                  Correlation:{" "}
                  <code style={{ fontFamily: "monospace" }}>
                    {runResult.run?.correlation_id || runResult.run?.meta?.correlation_id}
                  </code>
                </div>
              ) : null}
              {runResult.run?.steps_count != null ? (
                <div style={{ fontSize: 12, color: "var(--text-3)" }}>
                  Steps:{" "}
                  <strong style={{ fontWeight: 600 }}>{runResult.run?.steps_count}</strong>
                </div>
              ) : null}
              {(
                runResult.run?.evidence_url ||
                runResult.run?.report_url ||
                runResult.run?.artifacts?.evidence_url ||
                runResult.run?.artifacts?.report_url
              ) ? (
                <div style={{ display: "flex", gap: 10, flexWrap: "wrap", marginTop: 2 }}>
                  {(
                    runResult.run?.evidence_url ||
                    runResult.run?.artifacts?.evidence_url
                  ) ? (
                    <a
                      href={runResult.run?.evidence_url || runResult.run?.artifacts?.evidence_url}
                      target="_blank"
                      rel="noreferrer"
                      className="btn btn-secondary btn-sm"
                      style={{ textDecoration: "none" }}
                    >
                      View Evidence ↗
                    </a>
                  ) : null}
                  {(
                    runResult.run?.report_url ||
                    runResult.run?.artifacts?.report_url
                  ) ? (
                    <a
                      href={runResult.run?.report_url || runResult.run?.artifacts?.report_url}
                      target="_blank"
                      rel="noreferrer"
                      className="btn btn-secondary btn-sm"
                      style={{ textDecoration: "none" }}
                    >
                      Download Report ↗
                    </a>
                  ) : null}
                </div>
              ) : null}
            </div>
          )}
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
          <div style={{ padding: "48px 32px", textAlign: "center" }}>
            <div style={{ fontSize: 40, marginBottom: 12 }}>☰</div>
            <div style={{ fontSize: 15, fontWeight: 600, color: "var(--text-1)", marginBottom: 8 }}>
              {currentProject ? t("catalog.empty.project_title") : t("catalog.empty.title")}
            </div>
            <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.7, maxWidth: 380, margin: "0 auto 20px" }}>
              {currentProject ? t("catalog.empty.project_desc") : t("catalog.empty.desc")}
            </div>
            <button className="btn btn-primary" onClick={() => navigate("/generate")}>
              ⊕ {t("catalog.empty.cta")}
            </button>
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
                    <td style={{ fontWeight: 600, fontSize: 13 }}>
                      <span>{tc.name}</span>
                      {tc.has_fragile_selectors && (
                        <span
                          className="badge badge-orange"
                          style={{ marginLeft: 6, fontSize: 10, fontWeight: 500, verticalAlign: "middle" }}
                          title="selector frágil"
                        >
                          selector frágil
                        </span>
                      )}
                      {tc.version != null && (
                        <span
                          className="badge badge-gray"
                          style={{ marginLeft: 7, fontSize: 10, fontWeight: 400, letterSpacing: "0.05em", verticalAlign: "middle" }}
                        >
                          v{tc.version}
                        </span>
                      )}
                      {tc.status && tc.status !== "active" && (
                        <span
                          className={`badge ${statusClass(tc.status)}`}
                          style={{ marginLeft: 4, fontSize: 10, verticalAlign: "middle" }}
                        >
                          {tc.status}
                        </span>
                      )}
                    </td>
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
                        style={{ marginRight: 4 }}
                      >
                        ⏱ {t("catalog.versions.btn")}
                      </button>
                      <button
                        className="btn btn-secondary btn-sm"
                        onClick={() => handleEdit(tc.test_case_id)}
                      >
                        ✏ {t("catalog.table.edit")}
                      </button>
                    </td>
                  </tr>
                  {expanded === tc.test_case_id && (
                    <tr>
                      <td colSpan={9} style={{ background: "var(--surface-2)", padding: "12px 20px" }}>
                        {editingId === tc.test_case_id && editForm ? (
                          <div style={{ display: "flex", flexDirection: "column", gap: 10, maxWidth: 680 }}>
                            <div style={{ fontWeight: 600, fontSize: 13, marginBottom: 2 }}>✏ Edit {tc.test_case_id}</div>
                            {editError && <div style={{ fontSize: 12, color: "var(--red)" }}>{editError}</div>}
                            <div style={{ display: "flex", gap: 10, flexWrap: "wrap" }}>
                              <div style={{ flex: "1 1 200px" }}>
                                <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 3 }}>Name</label>
                                <input className="input" style={{ width: "100%" }} value={editForm.name}
                                  onChange={e => setEditForm(f => ({ ...f, name: e.target.value }))} />
                              </div>
                              <div style={{ flex: "1 1 130px" }}>
                                <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 3 }}>Module</label>
                                <input className="input" style={{ width: "100%" }} value={editForm.module}
                                  onChange={e => setEditForm(f => ({ ...f, module: e.target.value }))} />
                              </div>
                              <div style={{ flex: "0 0 110px" }}>
                                <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 3 }}>Priority</label>
                                <select className="input" style={{ width: "100%" }} value={editForm.priority}
                                  onChange={e => setEditForm(f => ({ ...f, priority: e.target.value }))}>
                                  {["critical","high","medium","low"].map(p => <option key={p} value={p}>{p}</option>)}
                                </select>
                              </div>
                            </div>
                            <div>
                              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 3 }}>Steps (JSON)</label>
                              <textarea className="input" rows={6} style={{ width: "100%", fontFamily: "monospace", fontSize: 11 }}
                                value={editForm.stepsJson}
                                onChange={e => setEditForm(f => ({ ...f, stepsJson: e.target.value }))} />
                              {jsonStepsOrAssertionsHaveFragileSelectors(editForm.stepsJson, editForm.assertionsJson) && (
                                <div style={{ fontSize: 12, color: "var(--orange-text)", marginTop: 6, lineHeight: 1.45 }}>
                                  ⚠ Este test tiene selectores generados dinámicamente que pueden fallar entre builds. Usa Auto-fix para ver sugerencias.
                                </div>
                              )}
                            </div>
                            <div>
                              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 3 }}>Assertions (JSON)</label>
                              <textarea className="input" rows={4} style={{ width: "100%", fontFamily: "monospace", fontSize: 11 }}
                                value={editForm.assertionsJson}
                                onChange={e => setEditForm(f => ({ ...f, assertionsJson: e.target.value }))} />
                            </div>
                            <div>
                              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 3 }}>Change note (optional)</label>
                              <input className="input" style={{ width: "100%" }} placeholder="Describe what changed…"
                                value={editForm.changeNote}
                                onChange={e => setEditForm(f => ({ ...f, changeNote: e.target.value }))} />
                            </div>
                            <div style={{ display: "flex", gap: 8, alignItems: "center", flexWrap: "wrap" }}>
                              <button className="btn btn-primary btn-sm" onClick={() => handleSave(tc.test_case_id)} disabled={saving || fixPreviewLoading}>
                                {saving ? t("catalog.edit.saving") : t("catalog.edit.save")}
                              </button>
                              <button className="btn btn-secondary btn-sm" onClick={handleCancelEdit} disabled={saving}>
                                {t("catalog.edit.cancel")}
                              </button>
                              <button
                                className="btn btn-secondary btn-sm"
                                onClick={handleAutoFix}
                                disabled={saving || fixPreviewLoading}
                                style={{ marginLeft: 4 }}
                              >
                                {fixPreviewLoading ? t("catalog.edit.autofix_loading") : `⚙ ${t("catalog.edit.autofix")}`}
                              </button>
                            </div>

                            {/* Auto-fix preview panel */}
                            {fixPreviewResult && (
                              <div style={{ marginTop: 10, border: "1px solid var(--border)", borderRadius: 6, overflow: "hidden" }}>
                                <div style={{ background: "var(--surface-2)", padding: "6px 12px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between" }}>
                                  <span style={{ fontWeight: 600, fontSize: 12, color: "var(--text-2)" }}>
                                    ⚙ {t("catalog.edit.autofix_preview")}
                                  </span>
                                  <span style={{ fontSize: 11, color: "var(--text-3)" }}>
                                    Steps: {JSON.parse(editForm.stepsJson || "[]").length} → {fixPreviewResult.steps.length}
                                    {"  ·  "}
                                    Assertions: {JSON.parse(editForm.assertionsJson || "[]").length} → {fixPreviewResult.assertions.length}
                                  </span>
                                </div>
                                <div style={{ padding: "10px 12px" }}>
                                  {fixPreviewResult.changes.length === 0 ? (
                                    <div style={{ fontSize: 12, color: "var(--text-3)", fontStyle: "italic" }}>
                                      {t("catalog.edit.autofix_none")}
                                    </div>
                                  ) : (
                                    <ul style={{ margin: 0, padding: "0 0 0 16px", fontSize: 12, color: "var(--text-2)" }}>
                                      {fixPreviewResult.changes.map((c, i) => (
                                        <li key={i} style={{ marginBottom: 3, color: c.type === "warning" ? "var(--orange, #f59e0b)" : "var(--text-2)" }}>
                                          {c.type === "warning" ? "⚠ " : "✓ "}{c.message}
                                        </li>
                                      ))}
                                    </ul>
                                  )}
                                  <div style={{ display: "flex", gap: 8, marginTop: 10 }}>
                                    {fixPreviewResult.changes.length > 0 && (
                                      <button className="btn btn-primary btn-sm" onClick={handleApplyFix}>
                                        {t("catalog.edit.autofix_apply")}
                                      </button>
                                    )}
                                    <button className="btn btn-secondary btn-sm" onClick={() => setFixPreviewResult(null)}>
                                      {t("catalog.edit.autofix_dismiss")}
                                    </button>
                                  </div>
                                </div>
                              </div>
                            )}
                          </div>
                        ) : (
                          <div style={{ fontSize: 12, color: "var(--text-2)", display: "flex", gap: 24, flexWrap: "wrap" }}>
                            <span><b>ID:</b> {tc.id}</span>
                            <span><b>{t("catalog.row.status")}</b> <span className={`badge ${statusClass(tc.status)}`}>{tc.status}</span></span>
                            <span><b>{t("catalog.row.version")}</b> {tc.version}</span>
                            {tc.tags?.length > 0 && <span><b>{t("catalog.row.tags")}</b> {tc.tags.join(", ")}</span>}
                            <span><b>{t("catalog.row.steps")}</b> {tc.steps_count}</span>
                            <span><b>{t("catalog.row.updated")}</b> {tc.updated_at ? new Date(tc.updated_at).toLocaleDateString() : "—"}</span>
                          </div>
                        )}
                      </td>
                    </tr>
                  )}
                  {versionsOpen === tc.test_case_id && (
                    <tr>
                      <td colSpan={9} style={{ background: "var(--accent-light)", borderTop: "1px solid var(--border)", padding: "14px 20px" }}>
                        <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 10 }}>
                          <span style={{ fontWeight: 600, fontSize: 13, color: "var(--accent)" }}>
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
                        {versions.length >= 2 && (
                          <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 10, flexWrap: "wrap" }}>
                            <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)" }}>
                              {t("catalog.versions.compare_title")}:
                            </span>
                            <select
                              className="input"
                              style={{ width: 80, fontSize: 12, padding: "2px 6px" }}
                              value={compareFrom}
                              onChange={e => { setCompareFrom(e.target.value); setDiffResult(null); }}
                            >
                              {[...versions].sort((a, b) => a.version_number - b.version_number).map(v => (
                                <option key={v.version_number} value={String(v.version_number)}>v{v.version_number}</option>
                              ))}
                            </select>
                            <span style={{ fontSize: 12, color: "var(--text-3)" }}>→</span>
                            <select
                              className="input"
                              style={{ width: 80, fontSize: 12, padding: "2px 6px" }}
                              value={compareTo}
                              onChange={e => { setCompareTo(e.target.value); setDiffResult(null); }}
                            >
                              {[...versions].sort((a, b) => a.version_number - b.version_number).map(v => (
                                <option key={v.version_number} value={String(v.version_number)}>v{v.version_number}</option>
                              ))}
                            </select>
                            <button
                              className="btn btn-secondary btn-sm"
                              style={{ fontSize: 11 }}
                              onClick={() => handleCompare(tc.test_case_id)}
                              disabled={diffLoading || !compareFrom || !compareTo || compareFrom === compareTo}
                            >
                              {diffLoading ? t("catalog.versions.diff_loading") : t("catalog.versions.compare_btn")}
                            </button>
                          </div>
                        )}
                        {diffError && (
                          <div style={{ fontSize: 12, color: "var(--red)", marginBottom: 8 }}>✗ {diffError}</div>
                        )}
                        {diffResult && (
                          <DiffPanel diff={diffResult} t={t} />
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
