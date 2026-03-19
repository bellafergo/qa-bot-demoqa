// src/pages/RiskSelectionPage.jsx
/**
 * Risk-Based Test Selection — surfaces the highest-risk tests to run
 * given a set of changed modules and other risk signals.
 * POST /risk-selection/select-tests | POST /risk-selection/select-and-run
 */
import React, { useState, useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { selectTests, selectAndRun } from "../api";
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
  const navigate = useNavigate();

  // Form
  const [modules, setModules]     = useState("");
  const [maxTests, setMaxTests]   = useState(20);
  const [priority, setPriority]   = useState("");

  // Results
  const [result, setResult]           = useState(null);
  const [loading, setLoading]         = useState(false);
  const [error, setError]             = useState("");

  // Select & Run
  const [runLoading, setRunLoading]   = useState(false);
  const [runResult, setRunResult]     = useState(null);
  const [runError, setRunError]       = useState("");

  // Load default selection on mount (no filters)
  useEffect(() => {
    handleSelect();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  function buildBody() {
    const changed_modules = modules
      .split(/[,\n]/)
      .map(s => s.trim())
      .filter(Boolean);
    const body = { changed_modules, max_tests: Number(maxTests) || 20 };
    if (priority) body.priority = priority;
    return body;
  }

  async function handleSelect() {
    setLoading(true);
    setError("");
    setResult(null);
    setRunResult(null);
    setRunError("");
    try {
      const data = await selectTests(buildBody());
      setResult(data);
    } catch (e) {
      setError(e?.message || t("risk.error.select"));
    } finally {
      setLoading(false);
    }
  }

  async function handleSelectAndRun() {
    setRunLoading(true);
    setRunError("");
    setRunResult(null);
    try {
      const data = await selectAndRun(buildBody());
      setRunResult(data);
    } catch (e) {
      setRunError(e?.message || t("risk.error.run"));
    } finally {
      setRunLoading(false);
    }
  }

  const tests = result?.selected_tests ?? [];

  return (
    <div className="page-wrap">

      {/* Hero */}
      <div style={{ marginBottom: 24 }}>
        <h1 className="page-title">{t("risk.title")}</h1>
        <p className="page-subtitle">{t("risk.subtitle")}</p>
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

        <div style={{ display: "flex", gap: 10, marginTop: 14, flexWrap: "wrap" }}>
          <button className="btn btn-primary" onClick={handleSelect} disabled={loading}>
            {loading ? t("risk.form.selecting") : t("risk.form.select_btn")}
          </button>
          <button
            className="btn btn-secondary"
            onClick={handleSelectAndRun}
            disabled={runLoading || !result || tests.length === 0}
            title={t("risk.form.run_tip")}
          >
            {runLoading ? t("risk.form.running") : t("risk.form.run_btn")}
          </button>
        </div>

        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {/* Select & Run result */}
      {(runResult || runError) && (
        <div className="card" style={{ marginBottom: 24, borderLeft: runResult?.enqueued ? "3px solid var(--green)" : "3px solid var(--red)" }}>
          {runError ? (
            <div className="alert alert-error">{runError}</div>
          ) : runResult?.enqueued ? (
            <div style={{ display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap" }}>
              <span style={{ fontSize: 13, fontWeight: 700, color: "var(--green)" }}>
                ✓ {t("risk.run.enqueued")} — {runResult.selection?.total_selected ?? 0} {t("risk.run.tests_label")}
              </span>
              {runResult.orchestrator_job_id && (
                <code style={{ fontSize: 11, color: "var(--text-2)" }}>{runResult.orchestrator_job_id}</code>
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
                    <th style={{ width: 44 }}>#</th>
                    <th>{t("risk.col.test_id")}</th>
                    <th>{t("risk.col.name")}</th>
                    <th>{t("risk.col.module")}</th>
                    <th>{t("risk.col.priority")}</th>
                    <th style={{ width: 180 }}>{t("risk.col.score")}</th>
                    <th>{t("risk.col.reason")}</th>
                  </tr>
                </thead>
                <tbody>
                  {tests.map((test, i) => (
                    <tr key={test.test_case_id}>
                      <td style={{ color: "var(--text-3)", fontWeight: 600, fontSize: 12 }}>{i + 1}</td>
                      <td style={{ fontFamily: "monospace", fontSize: 12, color: "var(--text-2)" }}>
                        {test.test_case_id}
                      </td>
                      <td style={{ fontWeight: 600, fontSize: 13, maxWidth: 240 }}>
                        {test.name}
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
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
