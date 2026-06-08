// src/pages/PRAnalysisPage.jsx
/**
 * PR Impact Analysis — fetch PR data from GitHub, analyze impact, enqueue tests.
 * POST /github/pr/fetch          — fetch PR from GitHub (uses current catalog ``project_id`` + project GitHub settings)
 * POST /pr-analysis/analyze      — analyze impact and match catalog tests
 * POST /execution/run-batch      — enqueue matched test IDs via Execution Center
 */
import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { analyzePR, analyzeProjectPR, runBatch, fetchGithubPR, batchSaveDrafts, suggestModules } from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";

function RiskBadge({ level }) {
  const { t } = useLang();
  if (level === "high")   return <span className="badge badge-red">{t("pr.risk.high")}</span>;
  if (level === "medium") return <span className="badge badge-orange">{t("pr.risk.medium")}</span>;
  return <span className="badge badge-green">{t("pr.risk.low")}</span>;
}

function RiskLevelBadge({ level }) {
  const lv = (level || "LOW").toUpperCase();
  if (lv === "CRITICAL") return <span className="badge badge-red">CRITICAL</span>;
  if (lv === "HIGH") return <span className="badge badge-orange">HIGH</span>;
  if (lv === "MEDIUM") return <span className="badge badge-blue">MEDIUM</span>;
  return <span className="badge badge-gray">LOW</span>;
}

function parseChangedFiles(raw) {
  return (raw || "").split(/[\n,]+/).map((s) => s.trim()).filter(Boolean);
}

export default function PRAnalysisPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const navigate = useNavigate();
  const [form, setForm] = useState({
    title: "",
    description: "",
    changed_files: "",
    branch: "",
    pr_id: "",
  });
  const [generateDrafts, setGenerateDrafts] = useState(false);
  const [analyzing, setAnalyzing]         = useState(false);
  const [result, setResult]               = useState(null);
  const [v1Result, setV1Result]           = useState(null);
  const [error, setError]                 = useState("");

  const [enqueueing, setEnqueueing]       = useState(false);
  const [enqueueResult, setEnqueueResult] = useState(null);

  const [savingDrafts, setSavingDrafts]   = useState(false);
  const [saveDraftsResult, setSaveDraftsResult] = useState(null); // { count } | { error }

  // Risk Selection bridge
  const [sendingRisk, setSendingRisk] = useState(false);

  // GitHub fetch
  const [prUrl, setPrUrl]           = useState("");
  const [fetching, setFetching]     = useState(false);
  const [fetchError, setFetchError] = useState("");
  const [prDiff, setPrDiff]         = useState("");

  function set(key, val) { setForm(f => ({ ...f, [key]: val })); }

  async function handleFetchPR() {
    const url = prUrl.trim();
    if (!url) return;
    setFetching(true);
    setFetchError("");
    try {
      const pr = await fetchGithubPR(url, currentProject?.id ?? null);
      setForm({
        title:         pr.title        || "",
        description:   pr.description  || "",
        branch:        pr.branch       || "",
        pr_id:         pr.pr_id        || "",
        changed_files: (pr.changed_files || []).join("\n"),
      });
      setPrDiff(pr.diff || "");
      setResult(null);
      setV1Result(null);
      setEnqueueResult(null);
    } catch (e) {
      setFetchError(e?.message || t("pr.fetch.error"));
    } finally {
      setFetching(false);
    }
  }

  async function handleAnalyze() {
    const changedFiles = parseChangedFiles(form.changed_files);
    const useV1 = Boolean(currentProject?.id && changedFiles.length > 0);

    if (!useV1 && !form.title.trim() && !form.description.trim()) return;
    if (useV1 && !currentProject?.id) {
      setError(t("pr.v1.need_project"));
      return;
    }
    if (useV1 && !changedFiles.length) {
      setError(t("pr.v1.need_files"));
      return;
    }

    setAnalyzing(true);
    setError("");
    setResult(null);
    setV1Result(null);
    setEnqueueResult(null);
    try {
      if (useV1) {
        const r = await analyzeProjectPR(currentProject.id, {
          changed_files: changedFiles,
          repo: undefined,
          branch: form.branch || undefined,
          pr_id: form.pr_id || undefined,
          title: form.title || undefined,
          description: form.description || undefined,
        });
        setV1Result(r);
      } else {
        const body = {
          title: form.title || undefined,
          description: form.description || undefined,
          branch: form.branch || undefined,
          pr_id: form.pr_id || undefined,
          changed_files: changedFiles,
          diff_text: prDiff || undefined,
          generate_draft_tests: generateDrafts,
        };
        const r = await analyzePR(body);
        setResult(r);
      }
    } catch (e) {
      setError(e?.message || "Analysis failed");
    } finally {
      setAnalyzing(false);
    }
  }

  async function handleSaveDrafts() {
    const tests = result?.suggested_new_tests || [];
    if (!tests.length || savingDrafts) return;
    setSavingDrafts(true);
    setSaveDraftsResult(null);
    try {
      const drafts = tests.map(d => ({
        name:        d.name,
        module:      d.module,
        rationale:   d.rationale || "",
        confidence:  d.confidence || "medium",
        source:      "pr_analysis",
        steps:       d.suggested_steps      || [],
        assertions:  d.suggested_assertions || [],
      }));
      const res = await batchSaveDrafts(drafts);
      setSaveDraftsResult(res);
    } catch (e) {
      setSaveDraftsResult({ error: e?.message || "Save failed" });
    } finally {
      setSavingDrafts(false);
    }
  }

  async function handleSendToRisk() {
    const inferredModules = result?.inferred_modules || [];
    const changedFiles = form.changed_files.split(/[\n,]+/).map(s => s.trim()).filter(Boolean);

    setSendingRisk(true);
    try {
      // Resolve inferred domain names to real catalog module names
      const suggestion = await suggestModules({
        inferred_modules: inferredModules,
        changed_files:    changedFiles,
      });
      const modules = suggestion.suggested_modules?.length
        ? suggestion.suggested_modules
        : inferredModules;   // fallback: pass domain names directly (substring matching will handle them)
      navigate("/risk-selection", {
        state: {
          modules,
          fromPR:     true,
          prTitle:    form.title || undefined,
          prBranch:   form.branch || undefined,
        },
      });
    } catch {
      // If suggest-modules fails, navigate with raw inferred modules as fallback
      navigate("/risk-selection", {
        state: {
          modules:  result?.inferred_modules || [],
          fromPR:   true,
          prTitle:  form.title || undefined,
        },
      });
    } finally {
      setSendingRisk(false);
    }
  }

  async function handleEnqueueV1() {
    const ids = [...new Set((v1Result?.recommended_tests || []).map((t) => t.test_case_id).filter(Boolean))];
    if (!ids.length) return;
    setEnqueueing(true);
    setEnqueueResult(null);
    try {
      const context = {
        source: "pr_analysis",
        selection_type: "pr_v1_recommended",
        selected_test_ids: ids,
        selected_modules: (v1Result?.impacted_modules || []).map((m) => m.module),
        ...(form.title && { pr_title: form.title }),
        ...(form.branch && { pr_branch: form.branch }),
      };
      const r = await runBatch({ test_case_ids: ids, context });
      setEnqueueResult({ ok: true, ...r });
    } catch (e) {
      setEnqueueResult({ ok: false, error: e?.message || "Enqueue failed" });
    } finally {
      setEnqueueing(false);
    }
  }

  async function handleEnqueue() {
    const ids = [...new Set(result?.matched_test_case_ids || [])];
    if (!ids.length) return;
    setEnqueueing(true);
    setEnqueueResult(null);
    try {
      const context = {
        source:           "pr_analysis",
        selection_type:   "pr_matched",
        selected_test_ids: ids,
        selected_modules: result?.inferred_modules || [],
        ...(form.title  && { pr_title:  form.title }),
        ...(form.branch && { pr_branch: form.branch }),
      };
      const r = await runBatch({ test_case_ids: ids, context });
      setEnqueueResult({ ok: true, ...r });
    } catch (e) {
      setEnqueueResult({ ok: false, error: e?.message || "Enqueue failed" });
    } finally {
      setEnqueueing(false);
    }
  }

  return (
    <div className="page-wrap">

      {/* GitHub PR fetch */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 8 }}>{t("pr.fetch.title")}</div>
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 10 }}>
          {t("pr.fetch.desc")}
        </div>
        {!currentProject?.id ? (
          <div style={{ marginBottom: 10, fontSize: 12, color: "var(--text-3)" }}>
            {t("pr.fetch.hint_project")}
          </div>
        ) : null}
        <div style={{ display: "flex", gap: 10 }}>
          <input
            className="input"
            style={{ flex: 1, fontFamily: "monospace", fontSize: 12 }}
            placeholder={t("pr.fetch.placeholder")}
            value={prUrl}
            onChange={e => setPrUrl(e.target.value)}
            onKeyDown={e => e.key === "Enter" && handleFetchPR()}
          />
          <button
            className="btn btn-secondary"
            onClick={handleFetchPR}
            disabled={fetching || !prUrl.trim()}
          >
            {fetching ? t("pr.fetch.fetching") : t("pr.fetch.btn")}
          </button>
        </div>
        {fetchError && (
          <div className="alert alert-error" style={{ marginTop: 10, fontSize: 12 }}>
            {fetchError}
          </div>
        )}
      </div>

      {/* Diff indicator */}
      {prDiff && (
        <div style={{
          display: "flex", alignItems: "center", gap: 8,
          marginBottom: 20, padding: "8px 14px",
          background: "var(--accent-light)", borderRadius: "var(--r-sm)",
          border: "1px solid var(--border)", fontSize: 12,
        }}>
          <span style={{ color: "var(--accent)", fontWeight: 600 }}>◎</span>
          <span style={{ color: "var(--text-2)" }}>{t("pr.diff.loaded")}</span>
          <span className="badge badge-gray" style={{ fontSize: 10 }}>
            {(prDiff.length / 1000).toFixed(1)}k chars
          </span>
          {prDiff.length >= 15000 && (
            <span className="badge badge-orange" style={{ fontSize: 10 }}>
              {t("pr.diff.truncated")}
            </span>
          )}
        </div>
      )}

      {currentProject?.id ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 16 }}>
          {t("pr.v1.hint")} — <strong>{currentProject.name || currentProject.id}</strong>
        </div>
      ) : (
        <div className="alert" style={{ marginBottom: 16, fontSize: 12, padding: "10px 14px" }}>
          {t("pr.v1.need_project")}
        </div>
      )}

      {/* Form */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("pr.form.title")}</div>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 14, marginBottom: 14 }}>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.pr_title_label")}</label>
            <input className="input" style={{ width: "100%" }} placeholder={t("pr.form.pr_title_placeholder")} value={form.title} onChange={e => set("title", e.target.value)} />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.branch_label")}</label>
            <input className="input" style={{ width: "100%" }} placeholder={t("pr.form.branch_placeholder")} value={form.branch} onChange={e => set("branch", e.target.value)} />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.pr_id_label")}</label>
            <input className="input" style={{ width: "100%" }} placeholder={t("pr.form.pr_id_placeholder")} value={form.pr_id} onChange={e => set("pr_id", e.target.value)} />
          </div>
        </div>
        <div style={{ marginBottom: 14 }}>
          <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.changed_files_label")}</label>
          <textarea
            className="input"
            rows={4}
            style={{ width: "100%", resize: "vertical", fontFamily: "monospace", fontSize: 12 }}
            placeholder={"src/components/CandidateForm.tsx\nsrc/services/candidate_service.py"}
            value={form.changed_files}
            onChange={(e) => set("changed_files", e.target.value)}
          />
        </div>
        <div style={{ marginBottom: 14 }}>
          <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.desc_label")}</label>
          <textarea
            className="input"
            rows={3}
            style={{ width: "100%", resize: "vertical" }}
            placeholder={t("pr.form.desc_placeholder")}
            value={form.description}
            onChange={e => set("description", e.target.value)}
          />
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
          <label style={{ fontSize: 13, color: "var(--text-2)", display: "flex", alignItems: "center", gap: 6 }}>
            <input type="checkbox" checked={generateDrafts} onChange={e => setGenerateDrafts(e.target.checked)} />
            {t("pr.form.generate_drafts")}
          </label>
          <button
            className="btn btn-primary"
            onClick={handleAnalyze}
            disabled={
              analyzing
              || (
                currentProject?.id
                  ? !parseChangedFiles(form.changed_files).length
                  : (!form.title.trim() && !form.description.trim())
              )
            }
          >
            {analyzing ? t("pr.form.analyzing") : (currentProject?.id ? t("pr.form.analyze_pr") : t("pr.form.analyze"))}
          </button>
        </div>
        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {/* v1 Result (System Memory + Risk Engine) */}
      {v1Result && (
        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>
          <div className="card">
            <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "center", marginBottom: 12 }}>
              <span className="badge badge-gray">{v1Result.project_name || v1Result.project_id}</span>
              <span className="badge badge-gray">{t("pr.v1.files_analyzed")}: {v1Result.changed_files_count}</span>
              <RiskLevelBadge level={v1Result.risk_level} />
              <span className="badge badge-orange">{v1Result.risk_score ?? 0}/100</span>
            </div>
            {v1Result.summary && (
              <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.6, margin: "0 0 12px" }}>{v1Result.summary}</p>
            )}
          </div>

          <div className="card">
            <div className="section-title" style={{ marginBottom: 12 }}>{t("pr.v1.impacted_modules")}</div>
            {(v1Result.impacted_modules || []).length ? (
              <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
                {v1Result.impacted_modules.map((m) => (
                  <div key={m.module} style={{ borderLeft: "3px solid var(--accent)", paddingLeft: 12 }}>
                    <div style={{ display: "flex", gap: 8, alignItems: "center", flexWrap: "wrap" }}>
                      <strong>{m.module}</strong>
                      <RiskLevelBadge level={m.module_risk_level} />
                      <span className="badge badge-gray">{m.module_risk_score ?? 0}/100</span>
                    </div>
                    <ul style={{ margin: "6px 0 0", paddingLeft: 18, fontSize: 12, color: "var(--text-3)", lineHeight: 1.6 }}>
                      {(m.reasons || []).map((r, i) => <li key={i}>{r}</li>)}
                    </ul>
                  </div>
                ))}
              </div>
            ) : (
              <p style={{ fontSize: 13, color: "var(--text-3)" }}>{t("pr.v1.no_modules")}</p>
            )}
          </div>

          {(v1Result.file_mappings || []).length > 0 && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 12 }}>{t("pr.v1.file_mappings")}</div>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("pr.v1.col.file")}</th>
                    <th>{t("pr.v1.col.module")}</th>
                    <th>{t("pr.v1.col.confidence")}</th>
                    <th>{t("pr.v1.col.reason")}</th>
                  </tr>
                </thead>
                <tbody>
                  {v1Result.file_mappings.map((row) => (
                    <tr key={row.file_path}>
                      <td style={{ fontFamily: "monospace", fontSize: 11 }}>{row.file_path}</td>
                      <td>{row.module}</td>
                      <td>{Math.round((row.confidence || 0) * 100)}%</td>
                      <td style={{ fontSize: 12, color: "var(--text-3)" }}>{row.reason}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          <div className="card">
            <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12, flexWrap: "wrap", gap: 8 }}>
              <div className="section-title" style={{ margin: 0 }}>{t("pr.v1.recommended")}</div>
              {(v1Result.recommended_tests || []).length > 0 && !enqueueResult?.ok && (
                <button className="btn btn-primary btn-sm" onClick={handleEnqueueV1} disabled={enqueueing}>
                  {enqueueing ? t("pr.result.enqueueing") : `${t("pr.result.enqueue_prefix")} ${v1Result.recommended_tests.length} ${t("pr.result.enqueue_tests")}`}
                </button>
              )}
            </div>
            {(v1Result.recommended_tests || []).length ? (
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("knowledge.col.test")}</th>
                    <th>{t("knowledge.col.module")}</th>
                    <th>{t("pr.v1.col.reason")}</th>
                  </tr>
                </thead>
                <tbody>
                  {v1Result.recommended_tests.map((tc) => (
                    <tr key={tc.test_case_id}>
                      <td style={{ fontFamily: "monospace", fontSize: 12 }}>{tc.name || tc.test_case_id}</td>
                      <td>{tc.module || "—"}</td>
                      <td style={{ fontSize: 12 }}>{tc.reason || "—"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            ) : (
              <p style={{ fontSize: 13, color: "var(--text-3)" }}>{t("pr.v1.no_tests")}</p>
            )}
          </div>

          {(v1Result.reasoning || []).length > 0 && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 12 }}>{t("pr.v1.reasoning")}</div>
              <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
                {v1Result.reasoning.map((line, i) => <li key={i}>{line}</li>)}
              </ul>
            </div>
          )}
        </div>
      )}

      {/* Legacy Result */}
      {result && (
        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>

          {/* Summary card */}
          <div className="card">
            <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 14, flexWrap: "wrap" }}>
              <RiskBadge level={result.inferred_risk_level} />
              <span className="badge badge-gray">{t("pr.result.confidence")} {result.confidence}</span>
              {result.pr_id && <span className="badge badge-gray">{result.pr_id}</span>}
            </div>
            {result.summary && (
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.6, marginBottom: 14 }}>
                {result.summary}
              </div>
            )}
            <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 16 }}>
              <div>
                <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
                  {t("pr.result.inferred_modules")}
                </div>
                <div style={{ display: "flex", flexWrap: "wrap", gap: 6, marginBottom: 10 }}>
                  {result.inferred_modules?.length
                    ? result.inferred_modules.map(m => <span key={m} className="badge badge-blue">{m}</span>)
                    : <span style={{ color: "var(--text-3)", fontSize: 12 }}>{t("pr.result.none_detected")}</span>
                  }
                </div>
                <button
                  className="btn btn-secondary btn-sm"
                  onClick={handleSendToRisk}
                  disabled={sendingRisk || !result.inferred_modules?.length}
                  title={result.inferred_modules?.length ? undefined : t("pr.send_risk.no_modules")}
                  style={{ fontSize: 11 }}
                >
                  {sendingRisk ? "…" : t("pr.send_risk.btn")}
                </button>
                {!result.inferred_modules?.length && (
                  <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4, fontStyle: "italic" }}>
                    {t("pr.send_risk.no_modules")}
                  </div>
                )}
              </div>
              <div>
                <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
                  {t("pr.result.risk_reasons")}
                </div>
                <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
                  {result.risk_reasons?.length
                    ? result.risk_reasons.map((r, i) => <div key={i}>• {r}</div>)
                    : "—"
                  }
                </div>
              </div>
            </div>
          </div>

          {/* Matched tests */}
          <div className="card">
            <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 12, flexWrap: "wrap", gap: 10 }}>
              <div className="section-title" style={{ margin: 0 }}>
                {result.matched_tests_count} {result.matched_tests_count !== 1 ? t("pr.result.matched_plural") : t("pr.result.matched_single")}
              </div>
                {result.matched_test_case_ids?.length > 0 && !enqueueResult?.ok && (
                <button
                  className="btn btn-primary btn-sm"
                  onClick={handleEnqueue}
                  disabled={enqueueing}
                >
                  {enqueueing ? t("pr.result.enqueueing") : `${t("pr.result.enqueue_prefix")} ${result.matched_tests_count} ${t("pr.result.enqueue_tests")}`}
                </button>
              )}
              {result.orchestrator_job_id && !enqueueResult?.ok && (
                <span style={{ fontSize: 12, color: "var(--green)", fontWeight: 600 }}>
                  {t("pr.result.enqueued_prefix")} {result.orchestrator_job_id?.slice(0, 14)}…
                </span>
              )}
            </div>

            {enqueueResult && (
              <div
                className={`alert ${!enqueueResult.ok ? "alert-error" : "alert-success"}`}
                style={{ marginBottom: 12 }}
              >
                {!enqueueResult.ok
                  ? `✗ ${enqueueResult.error}`
                  : (
                    <div style={{ display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap" }}>
                      <span>
                        {t("pr.result.enqueued_manual_prefix")} {enqueueResult.job_id?.slice(0, 14)}… {t("pr.result.enqueued_manual_suffix")} — {enqueueResult.total_count} {t("pr.result.enqueue_tests")}
                      </span>
                      <button
                        className="btn btn-secondary btn-sm"
                        style={{ fontSize: 11 }}
                        onClick={() => navigate("/execution")}
                      >
                        {t("pr.result.go_execution")}
                      </button>
                    </div>
                  )
                }
              </div>
            )}

            {result.matched_test_case_ids?.length > 0 ? (
              <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fill, minmax(220px, 1fr))", gap: 10 }}>
                {result.matched_test_case_ids.map(id => (
                  <div key={id} style={{
                    background: "var(--surface)",
                    border: "1px solid var(--border)",
                    borderRadius: 6,
                    padding: "10px 12px",
                    display: "flex",
                    flexDirection: "column",
                    gap: 5,
                  }}>
                    <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", gap: 6 }}>
                      <span style={{ fontFamily: "monospace", fontSize: 12, fontWeight: 600, color: "var(--text-1)" }}>
                        {id}
                      </span>
                      <span style={{ fontSize: 10, color: "var(--accent)", background: "var(--accent-light)", borderRadius: 3, padding: "1px 6px", whiteSpace: "nowrap" }}>
                        {t("pr.test.suggested")}
                      </span>
                    </div>
                    {result.test_match_reasons?.[id] && (
                      <span style={{ fontSize: 11, color: "var(--text-3)", lineHeight: 1.5 }}>
                        {result.test_match_reasons[id]}
                      </span>
                    )}
                  </div>
                ))}
              </div>
            ) : (
              <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("pr.result.no_match")}</div>
            )}
          </div>

          {/* Draft suggestions */}
          {result.suggested_new_tests?.length > 0 && (
            <div className="card">
              <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", gap: 10, marginBottom: 12, flexWrap: "wrap" }}>
                <div className="section-title" style={{ margin: 0 }}>
                  {result.suggested_new_tests.length} {t("pr.result.draft_suggestions")}
                </div>
                <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
                  {!saveDraftsResult?.saved_count && !saveDraftsResult?.error && (
                    <button
                      className="btn btn-primary btn-sm"
                      onClick={handleSaveDrafts}
                      disabled={savingDrafts}
                    >
                      {savingDrafts ? t("pr.save_drafts.saving") : t("pr.save_drafts.btn")}
                    </button>
                  )}
                  <button className="btn btn-secondary btn-sm" onClick={() => navigate("/drafts")}>
                    {t("pr.save_drafts.open")}
                  </button>
                </div>
              </div>

              {saveDraftsResult && (
                <div
                  className={`alert ${saveDraftsResult.error ? "alert-error" : "alert-success"}`}
                  style={{ marginBottom: 12, fontSize: 12 }}
                >
                  {saveDraftsResult.error
                    ? `✗ ${saveDraftsResult.error}`
                    : (
                      <span>
                        ✓ {saveDraftsResult.saved_count ?? 0} {t("pr.save_drafts.saved")}
                        {saveDraftsResult.error_count > 0 && ` · ${saveDraftsResult.error_count} ${t("pr.save_drafts.batch_failed")}`}
                        {" "}<button className="btn btn-secondary btn-sm" style={{ fontSize: 11, marginLeft: 8 }} onClick={() => navigate("/drafts")}>{t("pr.save_drafts.open")}</button>
                      </span>
                    )
                  }
                </div>
              )}

              <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
                {result.suggested_new_tests.map(d => (
                  <div key={d.draft_id} style={{ borderLeft: "3px solid var(--accent)", paddingLeft: 12 }}>
                    <div style={{ fontWeight: 600, fontSize: 13, marginBottom: 4 }}>{d.name}</div>
                    <div style={{ display: "flex", gap: 6, flexWrap: "wrap", marginBottom: 6 }}>
                      <span className="badge badge-gray">{d.module}</span>
                      <span className="badge badge-gray">{t("pr.result.confidence")} {d.confidence}</span>
                    </div>
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>{d.rationale}</div>
                  </div>
                ))}
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
