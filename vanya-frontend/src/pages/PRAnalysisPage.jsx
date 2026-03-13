// src/pages/PRAnalysisPage.jsx
/**
 * PR Impact Analysis — analyze a PR and optionally enqueue matched tests.
 * POST /pr-analysis/analyze, POST /pr-analysis/analyze-and-enqueue
 */
import React, { useState } from "react";
import { analyzePR, analyzePRAndEnqueue } from "../api";

function RiskBadge({ level }) {
  if (level === "high")   return <span className="badge badge-red">HIGH RISK</span>;
  if (level === "medium") return <span className="badge badge-orange">MEDIUM RISK</span>;
  return <span className="badge badge-green">LOW RISK</span>;
}

export default function PRAnalysisPage() {
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

  function set(key, val) { setForm(f => ({ ...f, [key]: val })); }

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

  async function handleEnqueue() {
    if (!result?.matched_test_case_ids?.length) return;
    setEnqueueing(true);
    setEnqueueResult(null);
    try {
      const body = {
        title:         form.title       || undefined,
        description:   form.description || undefined,
        branch:        form.branch      || undefined,
        pr_id:         form.pr_id       || undefined,
        changed_files: form.changed_files.split(/[\n,]+/).map(s => s.trim()).filter(Boolean),
      };
      const r = await analyzePRAndEnqueue(body);
      setEnqueueResult(r);
    } catch (e) {
      setEnqueueResult({ error: e?.message });
    } finally {
      setEnqueueing(false);
    }
  }

  return (
    <div className="page-wrap">

      {/* Form */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">PR Impact Analysis</div>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 14, marginBottom: 14 }}>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>PR Title</label>
            <input className="input" style={{ width: "100%" }} placeholder="Fix checkout timeout" value={form.title} onChange={e => set("title", e.target.value)} />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Branch</label>
            <input className="input" style={{ width: "100%" }} placeholder="fix/checkout-timeout" value={form.branch} onChange={e => set("branch", e.target.value)} />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>PR ID (optional)</label>
            <input className="input" style={{ width: "100%" }} placeholder="PR-123" value={form.pr_id} onChange={e => set("pr_id", e.target.value)} />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Changed Files (comma or newline)</label>
            <input className="input" style={{ width: "100%" }} placeholder="src/checkout.py, src/payments.py" value={form.changed_files} onChange={e => set("changed_files", e.target.value)} />
          </div>
        </div>
        <div style={{ marginBottom: 14 }}>
          <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Description / Diff Summary</label>
          <textarea
            className="input"
            rows={3}
            style={{ width: "100%", resize: "vertical" }}
            placeholder="Describe what changed in this PR…"
            value={form.description}
            onChange={e => set("description", e.target.value)}
          />
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
          <label style={{ fontSize: 13, color: "var(--text-2)", display: "flex", alignItems: "center", gap: 6 }}>
            <input type="checkbox" checked={generateDrafts} onChange={e => setGenerateDrafts(e.target.checked)} />
            Also generate draft test suggestions
          </label>
          <button
            className="btn btn-primary"
            onClick={handleAnalyze}
            disabled={analyzing || (!form.title.trim() && !form.description.trim())}
          >
            {analyzing ? "Analyzing…" : "◎ Analyze"}
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
              <span className="badge badge-gray">confidence: {result.confidence}</span>
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
                  Inferred Modules
                </div>
                <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
                  {result.inferred_modules?.length
                    ? result.inferred_modules.map(m => <span key={m} className="badge badge-blue">{m}</span>)
                    : <span style={{ color: "var(--text-3)", fontSize: 12 }}>None detected</span>
                  }
                </div>
              </div>
              <div>
                <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
                  Risk Reasons
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
                {result.matched_tests_count} Matched Test Case{result.matched_tests_count !== 1 ? "s" : ""}
              </div>
              {result.matched_test_case_ids?.length > 0 && !result.orchestrator_job_id && (
                <button
                  className="btn btn-primary btn-sm"
                  onClick={handleEnqueue}
                  disabled={enqueueing}
                >
                  {enqueueing ? "Enqueueing…" : `⚡ Enqueue ${result.matched_tests_count} tests`}
                </button>
              )}
              {result.orchestrator_job_id && (
                <span style={{ fontSize: 12, color: "var(--green)", fontWeight: 600 }}>
                  ✓ Enqueued — job {result.orchestrator_job_id?.slice(0, 14)}…
                </span>
              )}
            </div>

            {enqueueResult && (
              <div className={`alert ${enqueueResult.error ? "alert-error" : "alert-success"}`} style={{ marginBottom: 12 }}>
                {enqueueResult.error
                  ? `✗ ${enqueueResult.error}`
                  : `✓ Job ${enqueueResult.orchestrator_job_id?.slice(0, 14)}… enqueued`
                }
              </div>
            )}

            {result.matched_test_case_ids?.length > 0 ? (
              <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
                {result.matched_test_case_ids.map(id => (
                  <span key={id} style={{ fontFamily: "monospace", fontSize: 11, background: "var(--surface-2)", border: "1px solid var(--border)", borderRadius: 4, padding: "2px 8px", color: "var(--text-2)" }}>
                    {id}
                  </span>
                ))}
              </div>
            ) : (
              <div style={{ fontSize: 13, color: "var(--text-3)" }}>No matching tests in the catalog for these modules.</div>
            )}
          </div>

          {/* Draft suggestions */}
          {result.suggested_new_tests?.length > 0 && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 12 }}>
                {result.suggested_new_tests.length} Draft Suggestions
              </div>
              <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
                {result.suggested_new_tests.map(d => (
                  <div key={d.draft_id} style={{ borderLeft: "3px solid var(--accent)", paddingLeft: 12 }}>
                    <div style={{ fontWeight: 600, fontSize: 13, marginBottom: 4 }}>{d.name}</div>
                    <div style={{ display: "flex", gap: 6, flexWrap: "wrap", marginBottom: 6 }}>
                      <span className="badge badge-gray">{d.module}</span>
                      <span className="badge badge-gray">confidence: {d.confidence}</span>
                    </div>
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>{d.rationale}</div>
                  </div>
                ))}
              </div>
              <div style={{ marginTop: 12, fontSize: 12, color: "var(--text-3)" }}>
                Go to <a href="/drafts" style={{ color: "var(--accent)", fontWeight: 600 }}>Drafts</a> to generate and approve full test cases.
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
