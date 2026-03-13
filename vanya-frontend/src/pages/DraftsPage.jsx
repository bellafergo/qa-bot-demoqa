// src/pages/DraftsPage.jsx
/**
 * Draft Approval Center — generate AI test drafts and approve them into the catalog.
 * POST /test-generation/generate → drafts → POST /test-generation/approve
 */
import React, { useState } from "react";
import { generateTests, approveTests } from "../api";

function ConfidenceBadge({ c }) {
  if (c === "high")   return <span className="badge badge-green">high</span>;
  if (c === "medium") return <span className="badge badge-blue">medium</span>;
  return <span className="badge badge-gray">low</span>;
}

export default function DraftsPage() {
  // Generation form
  const [prompt, setPrompt]           = useState("");
  const [module, setModule]           = useState("");
  const [type, setType]               = useState("");
  const [priority, setPriority]       = useState("");
  const [maxDrafts, setMaxDrafts]     = useState(5);
  const [generating, setGenerating]   = useState(false);
  const [genError, setGenError]       = useState("");

  // Drafts state
  const [drafts, setDrafts]           = useState([]);
  const [genNotes, setGenNotes]       = useState([]);
  const [selectedDrafts, setSelected] = useState(new Set());

  // Approval
  const [approving, setApproving]     = useState(false);
  const [approveResult, setApproveResult] = useState(null);
  const [activate, setActivate]       = useState(true);

  // Expanded draft
  const [expanded, setExpanded]       = useState(null);

  async function handleGenerate() {
    if (!prompt.trim()) return;
    setGenerating(true);
    setGenError("");
    setDrafts([]);
    setGenNotes([]);
    setSelected(new Set());
    setApproveResult(null);
    try {
      const body = { prompt, max_drafts: maxDrafts };
      if (module)   body.module   = module;
      if (type)     body.type     = type;
      if (priority) body.priority = priority;
      const res = await generateTests(body);
      setDrafts(res.drafts || []);
      setGenNotes(res.notes || []);
    } catch (e) {
      setGenError(e?.message || "Generation failed");
    } finally {
      setGenerating(false);
    }
  }

  function toggleDraft(id) {
    setSelected(prev => {
      const s = new Set(prev);
      s.has(id) ? s.delete(id) : s.add(id);
      return s;
    });
  }

  function toggleAll() {
    if (selectedDrafts.size === drafts.length) setSelected(new Set());
    else setSelected(new Set(drafts.map(d => d.draft_id)));
  }

  async function handleApprove() {
    const toApprove = drafts.filter(d => selectedDrafts.has(d.draft_id));
    if (!toApprove.length) return;
    setApproving(true);
    setApproveResult(null);
    try {
      const res = await approveTests({ drafts: toApprove, activate });
      setApproveResult({ ok: true, ...res });
    } catch (e) {
      setApproveResult({ ok: false, error: e?.message || "Approval failed" });
    } finally {
      setApproving(false);
    }
  }

  return (
    <div className="page-wrap">

      {/* Generation form */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">Generate Test Drafts</div>
        <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>
              Prompt / requirement <span style={{ color: "var(--red)" }}>*</span>
            </label>
            <textarea
              className="input"
              rows={3}
              placeholder="Describe the feature or scenario to test, e.g. 'User adds item to cart and completes checkout with credit card'"
              value={prompt}
              onChange={e => setPrompt(e.target.value)}
              style={{ width: "100%", resize: "vertical" }}
            />
          </div>
          <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "flex-end" }}>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Module (optional)</label>
              <input className="input" placeholder="e.g. checkout" value={module} onChange={e => setModule(e.target.value)} style={{ width: 140 }} />
            </div>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Type</label>
              <select className="input" value={type} onChange={e => setType(e.target.value)} style={{ width: 140 }}>
                <option value="">Auto</option>
                {["smoke","regression","functional","negative","e2e"].map(v => <option key={v} value={v}>{v}</option>)}
              </select>
            </div>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Priority</label>
              <select className="input" value={priority} onChange={e => setPriority(e.target.value)} style={{ width: 120 }}>
                <option value="">Auto</option>
                {["critical","high","medium","low"].map(v => <option key={v} value={v}>{v}</option>)}
              </select>
            </div>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Max drafts</label>
              <select className="input" value={maxDrafts} onChange={e => setMaxDrafts(Number(e.target.value))} style={{ width: 90 }}>
                {[1,2,3,5,8,10].map(n => <option key={n} value={n}>{n}</option>)}
              </select>
            </div>
            <button
              className="btn btn-primary"
              onClick={handleGenerate}
              disabled={generating || !prompt.trim()}
              style={{ alignSelf: "flex-end" }}
            >
              {generating ? "Generating…" : "✦ Generate"}
            </button>
          </div>
        </div>
        {genError && <div className="alert alert-error" style={{ marginTop: 12 }}>{genError}</div>}
        {genNotes.length > 0 && (
          <div style={{ marginTop: 12, fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
            {genNotes.map((n, i) => <div key={i}>• {n}</div>)}
          </div>
        )}
      </div>

      {/* Drafts */}
      {drafts.length > 0 && (
        <>
          {/* Approve bar */}
          <div className="card" style={{
            marginBottom: 12, padding: "12px 18px",
            display: "flex", alignItems: "center", gap: 16, flexWrap: "wrap",
            background: selectedDrafts.size > 0 ? "var(--accent-light)" : undefined,
            borderColor: selectedDrafts.size > 0 ? "var(--accent-border)" : undefined,
          }}>
            <button className="btn btn-secondary btn-sm" onClick={toggleAll}>
              {selectedDrafts.size === drafts.length ? "Deselect all" : `Select all (${drafts.length})`}
            </button>
            {selectedDrafts.size > 0 && (
              <>
                <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
                  {selectedDrafts.size} selected
                </span>
                <label style={{ fontSize: 13, color: "var(--text-2)", display: "flex", alignItems: "center", gap: 6 }}>
                  <input type="checkbox" checked={activate} onChange={e => setActivate(e.target.checked)} />
                  Activate immediately
                </label>
                <button
                  className="btn btn-primary btn-sm"
                  onClick={handleApprove}
                  disabled={approving}
                >
                  {approving ? "Approving…" : `✓ Approve ${selectedDrafts.size}`}
                </button>
              </>
            )}
            {approveResult && (
              <div className={`alert ${approveResult.ok ? "alert-success" : "alert-error"}`} style={{ margin: 0, flex: 1 }}>
                {approveResult.ok
                  ? `✓ Created ${approveResult.total_created} test(s): ${(approveResult.created_test_case_ids || []).join(", ")}`
                  : `✗ ${approveResult.error}`
                }
              </div>
            )}
          </div>

          {/* Draft cards */}
          <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
            {drafts.map(d => (
              <div
                key={d.draft_id}
                className="card"
                style={{
                  borderColor: selectedDrafts.has(d.draft_id) ? "var(--accent)" : undefined,
                  background: selectedDrafts.has(d.draft_id) ? "var(--accent-light)" : undefined,
                }}
              >
                <div style={{ display: "flex", alignItems: "flex-start", gap: 12 }}>
                  <input
                    type="checkbox"
                    checked={selectedDrafts.has(d.draft_id)}
                    onChange={() => toggleDraft(d.draft_id)}
                    style={{ marginTop: 3, flexShrink: 0 }}
                  />
                  <div style={{ flex: 1 }}>
                    <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
                      <span style={{ fontWeight: 700, fontSize: 14, color: "var(--text)" }}>{d.name}</span>
                      <span className="badge badge-gray">{d.module}</span>
                      <span className="badge badge-blue">{d.type}</span>
                      <span className="badge badge-orange">{d.priority}</span>
                      <ConfidenceBadge c={d.confidence} />
                    </div>
                    <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 8, lineHeight: 1.5 }}>
                      {d.rationale}
                    </div>
                    <div style={{ display: "flex", gap: 12, fontSize: 11, color: "var(--text-3)" }}>
                      <span>{d.steps?.length ?? 0} step(s)</span>
                      <span>{d.assertions?.length ?? 0} assertion(s)</span>
                      <button
                        style={{ background: "none", border: "none", color: "var(--accent)", cursor: "pointer", fontSize: 11, padding: 0, fontWeight: 600 }}
                        onClick={() => setExpanded(expanded === d.draft_id ? null : d.draft_id)}
                      >
                        {expanded === d.draft_id ? "▲ Hide steps" : "▼ Show steps"}
                      </button>
                    </div>

                    {expanded === d.draft_id && d.steps?.length > 0 && (
                      <div style={{ marginTop: 10 }}>
                        <table className="data-table" style={{ fontSize: 11 }}>
                          <thead><tr><th>#</th><th>Action</th><th>Target / Value</th></tr></thead>
                          <tbody>
                            {d.steps.map((st, si) => (
                              <tr key={si}>
                                <td style={{ color: "var(--text-3)" }}>{si + 1}</td>
                                <td style={{ fontFamily: "monospace", fontWeight: 600 }}>{st.action}</td>
                                <td style={{ color: "var(--text-2)" }}>{st.target || st.value || st.url || "—"}</td>
                              </tr>
                            ))}
                          </tbody>
                        </table>
                      </div>
                    )}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </>
      )}

      {!generating && drafts.length === 0 && !genError && (
        <div className="card" style={{ textAlign: "center", padding: "40px 20px", color: "var(--text-3)" }}>
          <div style={{ fontSize: 32, marginBottom: 12 }}>⊕</div>
          <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 6 }}>No drafts yet</div>
          <div style={{ fontSize: 13 }}>Enter a prompt above and click Generate to get AI-drafted test cases.</div>
        </div>
      )}
    </div>
  );
}
