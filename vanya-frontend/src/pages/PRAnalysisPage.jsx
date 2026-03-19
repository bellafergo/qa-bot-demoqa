// src/pages/PRAnalysisPage.jsx
/**
 * PR Impact Analysis — fetch PR data from GitHub, analyze impact, enqueue tests.
 * POST /github/pr/fetch          — fetch PR metadata + changed files from GitHub API
 * POST /pr-analysis/analyze      — analyze impact and match catalog tests
 * POST /execution/run-batch      — enqueue matched test IDs via Execution Center
 */
import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { analyzePR, runBatch, fetchGithubPR, batchSaveDrafts, suggestModules } from "../api";
import { useLang } from "../i18n/LangContext";

function RiskBadge({ level }) {
  const { t } = useLang();
  if (level === "high")   return <span className="badge badge-red">{t("pr.risk.high")}</span>;
  if (level === "medium") return <span className="badge badge-orange">{t("pr.risk.medium")}</span>;
  return <span className="badge badge-green">{t("pr.risk.low")}</span>;
}

export default function PRAnalysisPage() {
  const { t } = useLang();
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
      const pr = await fetchGithubPR(url);
      setForm({
        title:         pr.title        || "",
        description:   pr.description  || "",
        branch:        pr.branch       || "",
        pr_id:         pr.pr_id        || "",
        changed_files: (pr.changed_files || []).join("\n"),
      });
      setPrDiff(pr.diff || "");
      setResult(null);
      setEnqueueResult(null);
    } catch (e) {
      setFetchError(e?.message || t("pr.fetch.error"));
    } finally {
      setFetching(false);
    }
  }

  async function handleAnalyze() {
    if (!form.title.trim() && !form.description.trim()) return;
    setAnalyzing(true);
    setError("");
    setResult(null);
    setEnqueueResult(null);
    try {
      const body = {
        title:          form.title       || undefined,
        description:    form.description || undefined,
        branch:         form.branch      || undefined,
        pr_id:          form.pr_id       || undefined,
        changed_files:  form.changed_files.split(/[\n,]+/).map(s => s.trim()).filter(Boolean),
        diff_text:      prDiff           || undefined,
        generate_draft_tests: generateDrafts,
      };
      const r = await analyzePR(body);
      setResult(r);
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
          <span style={{ color: "var(--accent)", fontWeight: 700 }}>◎</span>
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
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.changed_files_label")}</label>
            <input className="input" style={{ width: "100%" }} placeholder={t("pr.form.changed_files_ph")} value={form.changed_files} onChange={e => set("changed_files", e.target.value)} />
          </div>
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
            disabled={analyzing || (!form.title.trim() && !form.description.trim())}
          >
            {analyzing ? t("pr.form.analyzing") : t("pr.form.analyze")}
          </button>
        </div>
        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {/* Result */}
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
                <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
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
                <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
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
