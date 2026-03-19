// src/pages/RiskSelectionPage.jsx
/**
 * Risk-Based Test Selection — surfaces the highest-risk tests to run
 * given a set of changed modules and other risk signals.
 * POST /risk-selection/select-tests | POST /risk-selection/select-and-run
 * POST /execution/run-batch         — batch run for manual selection
 * POST /tests/{id}/run              — per-row individual run
 * GET  /tests/{id}/versions         — lightweight history panel (list only)
 * Edit → navigates to /catalog with module pre-highlighted
 */
import React, { useState, useEffect, useRef } from "react";
import { useNavigate, useLocation } from "react-router-dom";
import { selectTests, selectAndRun, runBatch, runTest, listVersions } from "../api";
import { useLang } from "../i18n/LangContext";

// ── helpers ──────────────────────────────────────────────────────────────────

function scoreColor(score) {
  if (score >= 15) return "var(--red)";
  if (score >= 8)  return "var(--orange, #f59e0b)";
  return "var(--green)";
}

function priorityClass(p) {
  const v = String(p || "").toLowerCase();
  if (v === "critical") return "badge badge-red";
  if (v === "high")     return "badge badge-orange";
  if (v === "medium")   return "badge badge-blue";
  return "badge badge-gray";
}

function fmtMs(ms) {
  if (ms == null) return "—";
  return ms < 1000 ? `${ms}ms` : `${(ms / 1000).toFixed(1)}s`;
}

function ScoreBar({ score, max = 25 }) {
  const pct = Math.min(100, Math.round((score / max) * 100));
  return (
    <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
      <div style={{ flex: 1, height: 6, borderRadius: 3, background: "var(--border)", overflow: "hidden" }}>
        <div style={{ width: `${pct}%`, height: "100%", background: scoreColor(score), borderRadius: 3, transition: "width 0.3s" }} />
      </div>
      <span style={{ fontSize: 12, fontWeight: 700, color: scoreColor(score), minWidth: 22, textAlign: "right" }}>{score}</span>
    </div>
  );
}

// ── Page ─────────────────────────────────────────────────────────────────────

export default function RiskSelectionPage() {
  const { t } = useLang();
  const navigate  = useNavigate();
  const location  = useLocation();

  // Read prefill from navigation state (e.g. arriving from PR Analysis)
  const navState    = location.state || {};
  const prefillMods = Array.isArray(navState.modules) && navState.modules.length
    ? navState.modules.join(", ")
    : "";
  const fromPR      = !!navState.fromPR;

  // Form — initialize with prefill if available
  const [modules, setModules]   = useState(prefillMods);
  const [maxTests, setMaxTests] = useState(20);
  const [priority, setPriority] = useState("");

  // Results
  const [result, setResult]   = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError]     = useState("");

  // Manual selection — pre-filled with all recommended tests after each load
  const [selectedIds, setSelectedIds] = useState(new Set());

  // Run state — normalized shape: { ok, job_id, count } | null
  const [runLoading, setRunLoading] = useState(false);
  const [runResult, setRunResult]   = useState(null);
  const [runError, setRunError]     = useState("");

  // Ref to drive indeterminate state on the select-all checkbox
  const selectAllRef = useRef(null);

  // ── Per-row Run state ─────────────────────────────────────────────────────
  const [runningId,       setRunningId]       = useState(null);   // tc_id being run individually
  const [singleRunResult, setSingleRunResult] = useState(null);   // { tc_id, run } | { tc_id, error }

  // ── Lightweight version history state ────────────────────────────────────
  // Full edit/diff/rollback lives in CatalogPage — this is list-only.
  const [versionsOpen,    setVersionsOpen]    = useState(null);   // tc_id whose history panel is open
  const [versions,        setVersions]        = useState([]);
  const [versionsLoading, setVersionsLoading] = useState(false);
  const [versionsError,   setVersionsError]   = useState("");

  // Auto-trigger on mount — if prefill present, will use those modules automatically
  useEffect(() => {
    handleSelect();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // Keep select-all checkbox indeterminate when some (not all) are selected
  useEffect(() => {
    const el = selectAllRef.current;
    if (!el) return;
    const total = result?.selected_tests?.length ?? 0;
    el.indeterminate = selectedIds.size > 0 && selectedIds.size < total;
  }, [selectedIds, result]);

  // ── query helpers ───────────────────────────────────────────────────────────

  function buildBody() {
    const changed_modules = modules
      .split(/[,\n]/)
      .map(s => s.trim())
      .filter(Boolean);
    const body = { changed_modules, max_tests: Number(maxTests) || 20 };
    if (priority) body.priority = priority;
    return body;
  }

  // ── actions ─────────────────────────────────────────────────────────────────

  async function handleSelect() {
    setLoading(true);
    setError("");
    setResult(null);
    setRunResult(null);
    setRunError("");
    setSelectedIds(new Set());
    setSingleRunResult(null);
    try {
      const data = await selectTests(buildBody());
      setResult(data);
      // Pre-select ALL recommended tests so the default behaviour is unchanged
      setSelectedIds(new Set((data?.selected_tests ?? []).map(tc => tc.test_case_id)));
    } catch (e) {
      setError(e?.message || t("risk.error.select"));
    } finally {
      setLoading(false);
    }
  }

  function toggleTest(id) {
    setSelectedIds(prev => {
      const s = new Set(prev);
      s.has(id) ? s.delete(id) : s.add(id);
      return s;
    });
  }

  function toggleAll() {
    const total = result?.selected_tests?.length ?? 0;
    if (selectedIds.size === total) {
      setSelectedIds(new Set());
    } else {
      setSelectedIds(new Set((result?.selected_tests ?? []).map(tc => tc.test_case_id)));
    }
  }

  async function handleSelectAndRun() {
    if (selectedIds.size === 0) return;
    setRunLoading(true);
    setRunError("");
    setRunResult(null);
    try {
      const ids = [...selectedIds];
      // Always run the exact manual selection via runBatch.
      // Default (all pre-selected) → equivalent to the previous auto behaviour.
      const data = await runBatch({ test_case_ids: ids });
      setRunResult({ ok: true, job_id: data.job_id, count: data.total_count ?? ids.length });
    } catch (e) {
      setRunError(e?.message || t("risk.error.run"));
    } finally {
      setRunLoading(false);
    }
  }

  // Per-row Run — ignores selectedIds, runs only this test
  async function handleRunSingle(tc_id) {
    setRunningId(tc_id);
    setSingleRunResult(null);
    try {
      const run = await runTest(tc_id, { headless: true });
      setSingleRunResult({ tc_id, run });
    } catch (e) {
      setSingleRunResult({ tc_id, error: e?.message || t("catalog.error.run_failed") });
    } finally {
      setRunningId(null);
    }
  }

  // Lightweight history panel — list only, no diff/rollback (use CatalogPage for full flow)
  async function handleOpenVersions(tc_id) {
    if (versionsOpen === tc_id) {
      setVersionsOpen(null);
      setVersions([]);
      return;
    }
    setVersionsOpen(tc_id);
    setVersions([]);
    setVersionsError("");
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

  // Edit — delegates to CatalogPage where the full editor lives
  function handleEdit(module) {
    navigate("/catalog", { state: { highlightModule: module } });
  }

  // ── derived ─────────────────────────────────────────────────────────────────

  const tests        = result?.selected_tests ?? [];
  const allSelected  = tests.length > 0 && selectedIds.size === tests.length;
  const noneSelected = selectedIds.size === 0;

  return (
    <div className="page-wrap">

      {/* Hero */}
      <div style={{ marginBottom: 24 }}>
        <h1 className="page-title">{t("risk.title")}</h1>
        <p className="page-subtitle">{t("risk.subtitle")}</p>
        {fromPR && prefillMods && (
          <div style={{
            display: "inline-flex", alignItems: "center", gap: 8,
            marginTop: 10, padding: "6px 12px",
            background: "var(--accent-light)", borderRadius: "var(--r-sm)",
            border: "1px solid var(--accent-border)", fontSize: 12,
          }}>
            <span style={{ color: "var(--accent)", fontWeight: 700 }}>◎</span>
            <span style={{ color: "var(--text-2)" }}>{t("risk.prefill.hint")}</span>
            {navState.prTitle && (
              <span className="badge badge-gray" style={{ fontSize: 10 }}>{navState.prTitle}</span>
            )}
          </div>
        )}
      </div>

      {/* Filter form */}
      <div className="card" style={{ marginBottom: 24 }}>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 120px 160px", gap: 12, alignItems: "end", flexWrap: "wrap" }}>

          <div>
            <label style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", display: "block", marginBottom: 4 }}>
              {t("risk.form.modules_label")}
            </label>
            <input
              className="input"
              value={modules}
              onChange={e => setModules(e.target.value)}
              onKeyDown={e => e.key === "Enter" && handleSelect()}
              placeholder={t("risk.form.modules_ph")}
            />
          </div>

          <div>
            <label style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", display: "block", marginBottom: 4 }}>
              {t("risk.form.max_label")}
            </label>
            <input
              className="input"
              type="number"
              min={1}
              max={200}
              value={maxTests}
              onChange={e => setMaxTests(e.target.value)}
            />
          </div>

          <div>
            <label style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", display: "block", marginBottom: 4 }}>
              {t("risk.form.priority_label")}
            </label>
            <select className="input" value={priority} onChange={e => setPriority(e.target.value)}>
              <option value="">{t("risk.form.priority_all")}</option>
              <option value="critical">Critical</option>
              <option value="high">High</option>
              <option value="medium">Medium</option>
              <option value="low">Low</option>
            </select>
          </div>
        </div>

        <div style={{ display: "flex", gap: 10, marginTop: 14, flexWrap: "wrap", alignItems: "center" }}>
          <button className="btn btn-primary" onClick={handleSelect} disabled={loading}>
            {loading ? t("risk.form.selecting") : t("risk.form.select_btn")}
          </button>
          <button
            className="btn btn-secondary"
            onClick={handleSelectAndRun}
            disabled={runLoading || noneSelected}
            title={noneSelected ? t("risk.run.none_selected") : t("risk.form.run_tip")}
          >
            {runLoading ? t("risk.form.running") : t("risk.form.run_btn")}
          </button>
          {/* Selection counter */}
          {tests.length > 0 && (
            <span style={{ fontSize: 12, color: "var(--text-3)", marginLeft: 4 }}>
              <span style={{ fontWeight: 700, color: noneSelected ? "var(--red)" : "var(--text-2)" }}>
                {selectedIds.size}
              </span>
              {" "}{t("risk.selection.of")}{" "}
              {tests.length}
              {" "}{t("risk.selection.selected")}
            </span>
          )}
        </div>

        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {/* Run result (batch) */}
      {(runResult || runError) && (
        <div className="card" style={{ marginBottom: 24, borderLeft: runResult?.ok ? "3px solid var(--green)" : "3px solid var(--red)" }}>
          {runError ? (
            <div className="alert alert-error">{runError}</div>
          ) : runResult?.ok ? (
            <div style={{ display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap" }}>
              <span style={{ fontSize: 13, fontWeight: 700, color: "var(--green)" }}>
                ✓ {t("risk.run.enqueued")} — {runResult.count} {t("risk.run.tests_label")}
              </span>
              {runResult.job_id && (
                <code style={{ fontSize: 11, color: "var(--text-2)" }}>{runResult.job_id}</code>
              )}
              <button
                className="btn btn-secondary btn-sm"
                style={{ marginLeft: "auto" }}
                onClick={() => navigate("/execution")}
              >
                {t("risk.run.go_execution")}
              </button>
            </div>
          ) : (
            <div style={{ fontSize: 13, color: "var(--text-2)" }}>{t("risk.run.not_enqueued")}</div>
          )}
        </div>
      )}

      {/* Single run result */}
      {singleRunResult && (
        <div
          className={`alert ${singleRunResult.error ? "alert-error" : "alert-success"}`}
          style={{ marginBottom: 16 }}
        >
          {singleRunResult.error
            ? `✗ ${singleRunResult.tc_id}: ${singleRunResult.error}`
            : `✓ ${singleRunResult.tc_id} → ${singleRunResult.run?.status} (${fmtMs(singleRunResult.run?.duration_ms)})`
          }
        </div>
      )}

      {/* Reasoning */}
      {result?.reasoning && (
        <div style={{ marginBottom: 16, fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, fontStyle: "italic" }}>
          {result.reasoning}
        </div>
      )}

      {/* Results table */}
      {result && (
        <div className="card" style={{ padding: 0, overflow: "hidden" }}>
          <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", gap: 10 }}>
            <div className="section-title" style={{ margin: 0 }}>
              {t("risk.table.title")}
            </div>
            {tests.length > 0 && (
              <span className="badge badge-blue">{tests.length} {t("risk.table.tests_label")}</span>
            )}
            {tests.length > 0 && !allSelected && (
              <span className="badge badge-orange" style={{ fontSize: 10 }}>
                {selectedIds.size} {t("risk.selection.selected")}
              </span>
            )}
          </div>

          {tests.length === 0 ? (
            <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
              {t("risk.table.empty")}
            </div>
          ) : (
            <div style={{ overflowX: "auto" }}>
              <table className="data-table">
                <thead>
                  <tr>
                    <th style={{ width: 36 }}>
                      <input
                        ref={selectAllRef}
                        type="checkbox"
                        checked={allSelected}
                        onChange={toggleAll}
                        title={allSelected ? t("risk.selection.deselect_all") : t("risk.selection.select_all")}
                      />
                    </th>
                    <th style={{ width: 36 }}>#</th>
                    <th>{t("risk.col.test_id")}</th>
                    <th>{t("risk.col.name")}</th>
                    <th>{t("risk.col.module")}</th>
                    <th>{t("risk.col.priority")}</th>
                    <th style={{ width: 180 }}>{t("risk.col.score")}</th>
                    <th>{t("risk.col.reason")}</th>
                    <th style={{ width: 160 }}>{t("catalog.table.col.actions")}</th>
                  </tr>
                </thead>
                <tbody>
                  {tests.map((test, i) => {
                    const isSelected  = selectedIds.has(test.test_case_id);
                    const hasVersions = versionsOpen === test.test_case_id;
                    return (
                      <React.Fragment key={test.test_case_id}>
                        <tr
                          style={{
                            cursor: "pointer",
                            background: isSelected ? undefined : "rgba(0,0,0,0.02)",
                            opacity: isSelected ? 1 : 0.55,
                          }}
                          onClick={() => toggleTest(test.test_case_id)}
                        >
                          <td onClick={e => e.stopPropagation()}>
                            <input
                              type="checkbox"
                              checked={isSelected}
                              onChange={() => toggleTest(test.test_case_id)}
                            />
                          </td>
                          <td style={{ color: "var(--text-3)", fontWeight: 600, fontSize: 12 }}>{i + 1}</td>
                          <td style={{ fontFamily: "monospace", fontSize: 12, color: "var(--text-2)" }}>
                            {test.test_case_id}
                          </td>
                          <td style={{ fontWeight: 600, fontSize: 13, maxWidth: 240 }}>
                            {test.name}
                            {test.version != null && (
                              <span className="badge badge-gray" style={{ marginLeft: 7, fontSize: 10, fontWeight: 700, letterSpacing: "0.05em", verticalAlign: "middle" }}>
                                v{test.version}
                              </span>
                            )}
                          </td>
                          <td>
                            <span className="badge badge-gray" style={{ fontSize: 11 }}>{test.module}</span>
                          </td>
                          <td>
                            <span className={priorityClass(test.priority)} style={{ fontSize: 11 }}>{test.priority}</span>
                          </td>
                          <td>
                            <ScoreBar score={test.selection_score} />
                          </td>
                          <td style={{ fontSize: 12, color: "var(--text-2)", maxWidth: 280 }}>
                            {test.selection_reason || "—"}
                          </td>
                          <td onClick={e => e.stopPropagation()} style={{ whiteSpace: "nowrap" }}>
                            <button
                              className="btn btn-primary btn-sm"
                              disabled={runningId === test.test_case_id}
                              onClick={() => handleRunSingle(test.test_case_id)}
                              style={{ marginRight: 4 }}
                            >
                              {runningId === test.test_case_id ? "…" : t("catalog.table.run")}
                            </button>
                            <button
                              className={`btn btn-sm ${hasVersions ? "btn-primary" : "btn-secondary"}`}
                              onClick={() => handleOpenVersions(test.test_case_id)}
                              title={t("catalog.versions.title")}
                              style={{ marginRight: 4 }}
                            >
                              ⏱ {t("catalog.versions.btn")}
                            </button>
                            <button
                              className="btn btn-secondary btn-sm"
                              onClick={() => handleEdit(test.module)}
                              title={t("risk.edit.tooltip")}
                            >
                              ✏ Edit
                            </button>
                          </td>
                        </tr>

                        {/* Lightweight version history — list only, no diff/rollback */}
                        {hasVersions && (
                          <tr>
                            <td colSpan={9} style={{ background: "var(--accent-light)", borderTop: "1px solid var(--border)", padding: "12px 20px" }}>
                              <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 10 }}>
                                <span style={{ fontWeight: 700, fontSize: 13, color: "var(--accent)" }}>
                                  ⏱ {t("catalog.versions.title")} — {test.test_case_id}
                                </span>
                                <span style={{ fontSize: 11, color: "var(--text-3)", fontStyle: "italic" }}>
                                  {t("risk.history.full_hint")}
                                </span>
                                <button className="btn btn-secondary btn-sm" style={{ marginLeft: "auto", fontSize: 11 }} onClick={() => setVersionsOpen(null)}>
                                  {t("catalog.versions.close")}
                                </button>
                              </div>
                              {versionsLoading && <div style={{ fontSize: 12, color: "var(--text-3)" }}>{t("catalog.versions.loading")}</div>}
                              {versionsError  && <div style={{ fontSize: 12, color: "var(--red)" }}>{versionsError}</div>}
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
                                  </tr></thead>
                                  <tbody>
                                    {versions.map(v => {
                                      const isCurrent = v.version_number === test.version;
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
                                          <td style={{ color: "var(--text-2)", maxWidth: 280, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                                            {v.change_note || "—"}
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
                    );
                  })}
                </tbody>
              </table>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
