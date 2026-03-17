// src/pages/DraftsPage.jsx
import React, { useState, useEffect, useCallback } from "react";
import { generateTests, approveTests, generateDrafts, approveDrafts, listTests, runSuite, exploreApp } from "../api";

// ── Shared badge helpers ──────────────────────────────────────────────────────

function ConfidenceBadge({ c }) {
  if (c === "high")   return <span className="badge badge-green">high</span>;
  if (c === "medium") return <span className="badge badge-blue">medium</span>;
  return <span className="badge badge-gray">low</span>;
}

function PriorityBadge({ p }) {
  if (p === "high" || p === "critical") return <span className="badge badge-red">{p}</span>;
  if (p === "medium")                   return <span className="badge badge-orange">medium</span>;
  return <span className="badge badge-gray">{p || "low"}</span>;
}

function ReasonBadge({ r }) {
  const labels = {
    form_detected:           "form",
    required_field_detected: "missing field",
    search_button_detected:  "search",
    links_detected:          "navigation",
  };
  return <span className="badge badge-blue">{labels[r] || r}</span>;
}

// ── Tab bar ───────────────────────────────────────────────────────────────────

const TABS = [
  { id: "explorer", label: "⊕ Explorer Drafts"   },
  { id: "catalog",  label: "▶ Catalog Execution" },
  { id: "ai",       label: "✦ AI Generation"     },
  { id: "appmap",   label: "🗺 App Map"           },
];

// ══════════════════════════════════════════════════════════════════════════════
// Explorer Drafts Panel
// Source: Application Explorer → Suggested Tests → Draft Generator → Approve
// ══════════════════════════════════════════════════════════════════════════════

function ExplorerDraftsPanel() {
  const [url, setUrl]                 = useState("");
  const [loading, setLoading]         = useState(false);
  const [loadError, setLoadError]     = useState("");
  const [drafts, setDrafts]           = useState([]);
  const [selected, setSelected]       = useState(new Set());
  const [expanded, setExpanded]       = useState(null);
  const [approving, setApproving]     = useState(false);
  const [approveResult, setApproveResult] = useState(null);

  async function handleGenerate() {
    const trimmed = url.trim();
    if (!trimmed) return;
    setLoading(true);
    setLoadError("");
    setDrafts([]);
    setSelected(new Set());
    setApproveResult(null);
    try {
      const res = await generateDrafts(trimmed);
      setDrafts(res.drafts || []);
    } catch (e) {
      setLoadError(e?.message || "Generation failed");
    } finally {
      setLoading(false);
    }
  }

  function toggleOne(name) {
    setSelected(prev => {
      const s = new Set(prev);
      s.has(name) ? s.delete(name) : s.add(name);
      return s;
    });
  }

  function selectAll()   { setSelected(new Set(drafts.map(d => d.test_name))); }
  function deselectAll() { setSelected(new Set()); }
  const allSelected = drafts.length > 0 && selected.size === drafts.length;

  async function handleApprove(subset) {
    const toApprove = subset || drafts.filter(d => selected.has(d.test_name));
    if (!toApprove.length) return;
    setApproving(true);
    setApproveResult(null);
    try {
      const res = await approveDrafts(toApprove);
      setApproveResult({ ok: true, ...res });
    } catch (e) {
      setApproveResult({ ok: false, error: e?.message || "Approval failed" });
    } finally {
      setApproving(false);
    }
  }

  function handleKeyDown(e) {
    if (e.key === "Enter") handleGenerate();
  }

  return (
    <div>
      {/* URL input */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 12 }}>Generate Drafts from URL</div>
        <div style={{ display: "flex", gap: 10, alignItems: "center", flexWrap: "wrap" }}>
          <input
            className="input"
            style={{ flex: 1, minWidth: 260 }}
            placeholder="https://example.com"
            value={url}
            onChange={e => setUrl(e.target.value)}
            onKeyDown={handleKeyDown}
            disabled={loading}
          />
          <button
            className="btn btn-primary"
            onClick={handleGenerate}
            disabled={loading || !url.trim()}
          >
            {loading ? "Exploring…" : "⊕ Generate Drafts"}
          </button>
        </div>
        {loadError && (
          <div className="alert alert-error" style={{ marginTop: 12 }}>{loadError}</div>
        )}
      </div>

      {/* Action bar */}
      {drafts.length > 0 && (
        <div className="card" style={{
          marginBottom: 12,
          padding: "10px 16px",
          display: "flex",
          alignItems: "center",
          gap: 14,
          flexWrap: "wrap",
          background: selected.size > 0 ? "var(--accent-light)" : undefined,
          borderColor: selected.size > 0 ? "var(--accent-border)" : undefined,
        }}>
          <button
            className="btn btn-secondary btn-sm"
            onClick={allSelected ? deselectAll : selectAll}
          >
            {allSelected ? "Deselect all" : `Select all (${drafts.length})`}
          </button>

          {selected.size > 0 && (
            <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
              {selected.size} selected
            </span>
          )}

          <button
            className="btn btn-secondary btn-sm"
            onClick={() => handleApprove(drafts)}
            disabled={approving || drafts.length === 0}
          >
            {approving ? "Approving…" : `Approve All (${drafts.length})`}
          </button>

          {selected.size > 0 && (
            <button
              className="btn btn-primary btn-sm"
              onClick={() => handleApprove()}
              disabled={approving}
            >
              {approving ? "Approving…" : `✓ Approve Selected (${selected.size})`}
            </button>
          )}

          {approveResult && (
            <div
              className={`alert ${approveResult.ok ? "alert-success" : "alert-error"}`}
              style={{ margin: 0, flex: 1 }}
            >
              {approveResult.ok
                ? `✓ Saved ${(approveResult.saved || []).length} test(s)` +
                  (approveResult.skipped?.length
                    ? ` · ${approveResult.skipped.length} skipped`
                    : "")
                : `✗ ${approveResult.error}`
              }
            </div>
          )}
        </div>
      )}

      {/* Draft cards */}
      {drafts.length > 0 && (
        <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
          {drafts.map(d => {
            const isSelected = selected.has(d.test_name);
            const isExpanded = expanded === d.test_name;
            return (
              <div
                key={d.test_name}
                className="card"
                style={{
                  borderColor: isSelected ? "var(--accent)" : undefined,
                  background:  isSelected ? "var(--accent-light)" : undefined,
                }}
              >
                <div style={{ display: "flex", alignItems: "flex-start", gap: 12 }}>
                  <input
                    type="checkbox"
                    checked={isSelected}
                    onChange={() => toggleOne(d.test_name)}
                    style={{ marginTop: 3, flexShrink: 0 }}
                  />
                  <div style={{ flex: 1 }}>
                    <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
                      <span style={{ fontWeight: 700, fontSize: 14, fontFamily: "monospace", color: "var(--text)" }}>
                        {d.test_name}
                      </span>
                      <PriorityBadge p={d.priority} />
                      <ReasonBadge  r={d.reason} />
                      <span className="badge badge-gray">draft</span>
                    </div>

                    <div style={{ display: "flex", gap: 16, fontSize: 11, color: "var(--text-3)", alignItems: "center" }}>
                      <span>{d.steps?.length ?? 0} step(s)</span>
                      <button
                        style={{
                          background: "none", border: "none",
                          color: "var(--accent)", cursor: "pointer",
                          fontSize: 11, padding: 0, fontWeight: 600,
                        }}
                        onClick={() => setExpanded(isExpanded ? null : d.test_name)}
                      >
                        {isExpanded ? "▲ Hide steps" : "▼ Show steps"}
                      </button>
                    </div>

                    {isExpanded && d.steps?.length > 0 && (
                      <div style={{ marginTop: 10 }}>
                        <table className="data-table" style={{ fontSize: 11 }}>
                          <thead>
                            <tr>
                              <th>#</th>
                              <th>Action</th>
                              <th>Selector / URL</th>
                              <th>Value</th>
                            </tr>
                          </thead>
                          <tbody>
                            {d.steps.map((st, si) => (
                              <tr key={si}>
                                <td style={{ color: "var(--text-3)" }}>{si + 1}</td>
                                <td style={{ fontFamily: "monospace", fontWeight: 600 }}>{st.action}</td>
                                <td style={{ color: "var(--text-2)", fontFamily: "monospace" }}>
                                  {st.selector || st.url || "—"}
                                </td>
                                <td style={{ color: "var(--text-2)", fontFamily: "monospace" }}>
                                  {st.value || "—"}
                                </td>
                              </tr>
                            ))}
                          </tbody>
                        </table>
                      </div>
                    )}
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      )}

      {/* Empty state */}
      {!loading && drafts.length === 0 && !loadError && (
        <div className="card" style={{ textAlign: "center", padding: "40px 20px", color: "var(--text-3)" }}>
          <div style={{ fontSize: 32, marginBottom: 12 }}>⊕</div>
          <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 6 }}>No drafts yet</div>
          <div style={{ fontSize: 13 }}>
            Enter a URL above. Vanya will explore the page, detect patterns, and generate test drafts.
          </div>
        </div>
      )}
    </div>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// App Map Panel
// Source: POST /app-explorer/explore-app
// ══════════════════════════════════════════════════════════════════════════════

function PageCard({ page }) {
  const [open, setOpen] = useState(false);

  const counts = [
    { label: "inputs",  n: page.inputs?.length  ?? 0 },
    { label: "buttons", n: page.buttons?.length ?? 0 },
    { label: "links",   n: page.links?.length   ?? 0 },
    { label: "forms",   n: page.forms?.length   ?? 0 },
  ];

  return (
    <div className="card" style={{ marginBottom: 10 }}>
      {/* Header row */}
      <div style={{ display: "flex", alignItems: "flex-start", gap: 12, flexWrap: "wrap" }}>
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ fontWeight: 700, fontSize: 14 }}>{page.title || "(no title)"}</div>
          <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-3)", marginTop: 2, wordBreak: "break-all" }}>
            {page.url}
          </div>
        </div>
        <div style={{ display: "flex", gap: 6, flexWrap: "wrap", alignItems: "center" }}>
          {counts.map(c => (
            <span key={c.label} className={`badge ${c.n > 0 ? "badge-blue" : "badge-gray"}`}>
              {c.n} {c.label}
            </span>
          ))}
          <button
            style={{ background: "none", border: "none", color: "var(--accent)", cursor: "pointer", fontSize: 11, fontWeight: 600, padding: "0 4px" }}
            onClick={() => setOpen(v => !v)}
          >
            {open ? "▲ less" : "▼ detail"}
          </button>
        </div>
      </div>

      {/* Expanded detail */}
      {open && (
        <div style={{ marginTop: 12, display: "flex", flexDirection: "column", gap: 10 }}>

          {/* Inputs */}
          {page.inputs?.length > 0 && (
            <div>
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>Inputs</div>
              <table className="data-table" style={{ fontSize: 11 }}>
                <thead><tr><th>Name</th><th>Selector</th></tr></thead>
                <tbody>
                  {page.inputs.map((inp, i) => (
                    <tr key={i}>
                      <td style={{ fontWeight: 600 }}>{inp.name || "—"}</td>
                      <td style={{ fontFamily: "monospace", color: "var(--text-2)" }}>{inp.selector || "—"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {/* Buttons */}
          {page.buttons?.length > 0 && (
            <div>
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>Buttons</div>
              <table className="data-table" style={{ fontSize: 11 }}>
                <thead><tr><th>Name</th><th>Selector</th></tr></thead>
                <tbody>
                  {page.buttons.map((btn, i) => (
                    <tr key={i}>
                      <td style={{ fontWeight: 600 }}>{btn.name || "—"}</td>
                      <td style={{ fontFamily: "monospace", color: "var(--text-2)" }}>{btn.selector || "—"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {/* Links */}
          {page.links?.length > 0 && (
            <div>
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>Links</div>
              <table className="data-table" style={{ fontSize: 11 }}>
                <thead><tr><th>Text</th><th>Selector</th></tr></thead>
                <tbody>
                  {page.links.map((lnk, i) => (
                    <tr key={i}>
                      <td>{lnk.text || "—"}</td>
                      <td style={{ fontFamily: "monospace", color: "var(--text-2)" }}>{lnk.selector || "—"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {/* Forms */}
          {page.forms?.length > 0 && (
            <div>
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>Forms</div>
              {page.forms.map((frm, i) => (
                <div key={i} style={{ fontSize: 12, padding: "6px 10px", background: "var(--bg)", borderRadius: 6, marginBottom: 4 }}>
                  <span style={{ fontWeight: 700 }}>{frm.name || `form_${i + 1}`}</span>
                  {frm.fields?.length > 0 && (
                    <span style={{ marginLeft: 8, color: "var(--text-3)" }}>
                      fields: {frm.fields.join(", ")}
                    </span>
                  )}
                  {frm.buttons?.length > 0 && (
                    <span style={{ marginLeft: 8, color: "var(--text-3)" }}>
                      · buttons: {frm.buttons.join(", ")}
                    </span>
                  )}
                </div>
              ))}
            </div>
          )}

        </div>
      )}
    </div>
  );
}

function AppMapPanel() {
  const [url, setUrl]         = useState("");
  const [maxPages, setMax]    = useState(5);
  const [loading, setLoading] = useState(false);
  const [error, setError]     = useState("");
  const [result, setResult]   = useState(null);

  async function handleExplore() {
    const trimmed = url.trim();
    if (!trimmed) return;
    setLoading(true);
    setError("");
    setResult(null);
    try {
      const res = await exploreApp(trimmed, maxPages);
      setResult(res);
    } catch (e) {
      setError(e?.message || "Exploration failed");
    } finally {
      setLoading(false);
    }
  }

  function handleKeyDown(e) {
    if (e.key === "Enter") handleExplore();
  }

  return (
    <div>
      {/* Input row */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 12 }}>Explore Application</div>
        <div style={{ display: "flex", gap: 10, alignItems: "flex-end", flexWrap: "wrap" }}>
          <div style={{ flex: 1, minWidth: 240 }}>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Start URL</label>
            <input
              className="input"
              style={{ width: "100%" }}
              placeholder="https://example.com"
              value={url}
              onChange={e => setUrl(e.target.value)}
              onKeyDown={handleKeyDown}
              disabled={loading}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Max pages</label>
            <select className="input" value={maxPages} onChange={e => setMax(Number(e.target.value))} style={{ width: 90 }}>
              {[1, 2, 3, 5, 8, 10, 15, 20].map(n => <option key={n} value={n}>{n}</option>)}
            </select>
          </div>
          <button
            className="btn btn-primary"
            onClick={handleExplore}
            disabled={loading || !url.trim()}
            style={{ alignSelf: "flex-end" }}
          >
            {loading ? "Exploring…" : "🗺 Explore App"}
          </button>
        </div>
        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {/* Loading */}
      {loading && (
        <div className="card" style={{ textAlign: "center", padding: "32px 20px", color: "var(--text-3)" }}>
          Exploring app…
        </div>
      )}

      {/* Results */}
      {result && !loading && (
        <>
          {/* Summary card */}
          <div className="card" style={{ marginBottom: 16, display: "flex", gap: 20, flexWrap: "wrap", alignItems: "center" }}>
            <div>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 2 }}>Start URL</div>
              <div style={{ fontFamily: "monospace", fontSize: 12, wordBreak: "break-all" }}>{result.start_url}</div>
            </div>
            <div style={{ marginLeft: "auto", display: "flex", gap: 10 }}>
              <div style={{ textAlign: "center" }}>
                <div style={{ fontSize: 22, fontWeight: 800, color: "var(--accent)" }}>{result.visited_count ?? 0}</div>
                <div style={{ fontSize: 11, color: "var(--text-3)" }}>pages visited</div>
              </div>
              <div style={{ textAlign: "center" }}>
                <div style={{ fontSize: 22, fontWeight: 800, color: result.errors?.length > 0 ? "var(--red)" : "var(--text-3)" }}>
                  {result.errors?.length ?? 0}
                </div>
                <div style={{ fontSize: 11, color: "var(--text-3)" }}>errors</div>
              </div>
            </div>
          </div>

          {/* Pages */}
          {result.pages?.length > 0 && (
            <div style={{ marginBottom: 16 }}>
              <div style={{ fontSize: 12, fontWeight: 700, color: "var(--text-2)", marginBottom: 8 }}>
                Pages ({result.pages.length})
              </div>
              {result.pages.map((page, i) => (
                <PageCard key={page.url || i} page={page} />
              ))}
            </div>
          )}

          {/* Errors */}
          {result.errors?.length > 0 && (
            <div>
              <div style={{ fontSize: 12, fontWeight: 700, color: "var(--red)", marginBottom: 8 }}>
                Errors ({result.errors.length})
              </div>
              <div className="card" style={{ padding: 0, overflow: "hidden" }}>
                <table className="data-table" style={{ fontSize: 12 }}>
                  <thead><tr><th>URL</th><th>Error</th></tr></thead>
                  <tbody>
                    {result.errors.map((e, i) => (
                      <tr key={i}>
                        <td style={{ fontFamily: "monospace", wordBreak: "break-all" }}>{e.url}</td>
                        <td style={{ color: "var(--red)" }}>{e.error}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}
        </>
      )}

      {/* Empty state */}
      {!loading && !result && !error && (
        <div className="card" style={{ textAlign: "center", padding: "40px 20px", color: "var(--text-3)" }}>
          <div style={{ fontSize: 32, marginBottom: 12 }}>🗺</div>
          <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 6 }}>No app map generated yet</div>
          <div style={{ fontSize: 13 }}>Enter a URL above to discover pages, inputs, buttons, and forms.</div>
        </div>
      )}
    </div>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// Catalog Execution Panel
// Source: GET /tests + POST /tests/run-suite
// ══════════════════════════════════════════════════════════════════════════════

const TYPE_BADGE   = { smoke: "badge-gray", regression: "badge-blue", functional: "badge-blue", negative: "badge-orange", e2e: "badge-blue" };
const STATUS_BADGE = { active: "badge-green", inactive: "badge-gray" };
const RUN_BADGE    = { pass: "badge-green", fail: "badge-red", error: "badge-orange", running: "badge-blue" };

function CatalogExecutionPanel() {
  const [tests, setTests]               = useState([]);
  const [loading, setLoading]           = useState(false);
  const [loadError, setLoadError]       = useState("");

  // Filters
  const [search, setSearch]             = useState("");
  const [filterModule, setFilterModule] = useState("");
  const [filterPriority, setFilterPri]  = useState("");
  const [filterStatus, setFilterStatus] = useState("active");

  // Selection
  const [selected, setSelected]         = useState(new Set());

  // Execution
  const [executing, setExecuting]       = useState(false);
  const [execResult, setExecResult]     = useState(null);
  const [execError, setExecError]       = useState("");

  const loadTests = useCallback(async () => {
    setLoading(true);
    setLoadError("");
    setSelected(new Set());
    setExecResult(null);
    setExecError("");
    try {
      const params = { limit: 200 };
      if (filterModule)   params.module   = filterModule;
      if (filterPriority) params.priority = filterPriority;
      // pass empty string to get all statuses, or the selected status value
      params.status = filterStatus;
      const res = await listTests(params);
      setTests(res || []);
    } catch (e) {
      setLoadError(e?.message || "Failed to load tests");
    } finally {
      setLoading(false);
    }
  }, [filterModule, filterPriority, filterStatus]);

  useEffect(() => { loadTests(); }, [loadTests]);

  // Client-side text search
  const visible = search.trim()
    ? tests.filter(t =>
        t.name?.toLowerCase().includes(search.toLowerCase()) ||
        t.test_case_id?.toLowerCase().includes(search.toLowerCase())
      )
    : tests;

  const allVisible = visible.length > 0 && visible.every(t => selected.has(t.test_case_id));

  function toggleOne(id) {
    setSelected(prev => {
      const s = new Set(prev);
      s.has(id) ? s.delete(id) : s.add(id);
      return s;
    });
  }

  function toggleAll() {
    if (allVisible) {
      setSelected(prev => {
        const s = new Set(prev);
        visible.forEach(t => s.delete(t.test_case_id));
        return s;
      });
    } else {
      setSelected(prev => {
        const s = new Set(prev);
        visible.forEach(t => s.add(t.test_case_id));
        return s;
      });
    }
  }

  async function handleExecute(ids) {
    if (!ids?.length) return;
    setExecuting(true);
    setExecResult(null);
    setExecError("");
    try {
      const res = await runSuite({ test_case_ids: ids, headless: true });
      setExecResult(res);
    } catch (e) {
      setExecError(e?.message || "Execution failed");
    } finally {
      setExecuting(false);
    }
  }

  const selectedIds = [...selected];
  const visibleIds  = visible.map(t => t.test_case_id);

  return (
    <div>
      {/* Filters */}
      <div className="card" style={{ marginBottom: 16 }}>
        <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "flex-end" }}>
          <div style={{ flex: 1, minWidth: 200 }}>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Search</label>
            <input
              className="input"
              placeholder="Filter by name or ID…"
              value={search}
              onChange={e => setSearch(e.target.value)}
              style={{ width: "100%" }}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Module</label>
            <input
              className="input"
              placeholder="e.g. login"
              value={filterModule}
              onChange={e => setFilterModule(e.target.value)}
              onBlur={loadTests}
              onKeyDown={e => e.key === "Enter" && loadTests()}
              style={{ width: 130 }}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Priority</label>
            <select className="input" value={filterPriority} onChange={e => setFilterPri(e.target.value)} style={{ width: 120 }}>
              <option value="">All</option>
              {["critical","high","medium","low"].map(v => <option key={v} value={v}>{v}</option>)}
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>Status</label>
            <select className="input" value={filterStatus} onChange={e => setFilterStatus(e.target.value)} style={{ width: 110 }}>
              <option value="active">active</option>
              <option value="inactive">inactive</option>
              <option value="">all</option>
            </select>
          </div>
          <button className="btn btn-secondary" onClick={loadTests} disabled={loading} style={{ alignSelf: "flex-end" }}>
            {loading ? "Loading…" : "↺ Refresh"}
          </button>
        </div>
        {loadError && <div className="alert alert-error" style={{ marginTop: 10 }}>{loadError}</div>}
      </div>

      {/* Action bar */}
      {visible.length > 0 && (
        <div className="card" style={{
          marginBottom: 12, padding: "10px 16px",
          display: "flex", alignItems: "center", gap: 14, flexWrap: "wrap",
          background: selectedIds.length > 0 ? "var(--accent-light)" : undefined,
          borderColor: selectedIds.length > 0 ? "var(--accent-border)" : undefined,
        }}>
          <button className="btn btn-secondary btn-sm" onClick={toggleAll}>
            {allVisible ? "Deselect all" : `Select all visible (${visible.length})`}
          </button>

          {selectedIds.length > 0 && (
            <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
              {selectedIds.length} selected
            </span>
          )}

          {selectedIds.length > 0 && (
            <button
              className="btn btn-primary btn-sm"
              onClick={() => handleExecute(selectedIds)}
              disabled={executing}
            >
              {executing ? "Running…" : `▶ Execute Selected (${selectedIds.length})`}
            </button>
          )}

          <button
            className="btn btn-secondary btn-sm"
            onClick={() => handleExecute(visibleIds)}
            disabled={executing || visibleIds.length === 0}
            style={{ marginLeft: "auto" }}
          >
            {executing ? "Running…" : `▶ Execute All Filtered (${visibleIds.length})`}
          </button>
        </div>
      )}

      {/* Execution error */}
      {execError && <div className="alert alert-error" style={{ marginBottom: 12 }}>{execError}</div>}

      {/* Execution result summary */}
      {execResult && (
        <div className="card" style={{ marginBottom: 16, borderColor: "var(--accent-border)" }}>
          <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 10, flexWrap: "wrap" }}>
            <span style={{ fontWeight: 700, fontSize: 14 }}>Suite result</span>
            <span className="badge badge-gray">{execResult.total ?? 0} total</span>
            <span className="badge badge-green">{execResult.passed ?? 0} passed</span>
            {(execResult.failed ?? 0) > 0 && <span className="badge badge-red">{execResult.failed} failed</span>}
            {(execResult.errors ?? 0) > 0 && <span className="badge badge-orange">{execResult.errors} errors</span>}
            {execResult.duration_ms != null && (
              <span style={{ fontSize: 12, color: "var(--text-3)", marginLeft: "auto" }}>
                {(execResult.duration_ms / 1000).toFixed(1)}s
              </span>
            )}
          </div>
          {execResult.runs?.length > 0 && (
            <table className="data-table" style={{ fontSize: 12 }}>
              <thead>
                <tr>
                  <th>Test</th>
                  <th>Status</th>
                  <th>Duration</th>
                  <th>Run ID</th>
                </tr>
              </thead>
              <tbody>
                {execResult.runs.map(r => (
                  <tr key={r.run_id}>
                    <td style={{ fontWeight: 600 }}>{r.test_name || r.test_case_id}</td>
                    <td>
                      <span className={`badge ${RUN_BADGE[r.status] || "badge-gray"}`}>{r.status}</span>
                    </td>
                    <td style={{ color: "var(--text-3)" }}>
                      {r.duration_ms != null ? `${(r.duration_ms / 1000).toFixed(1)}s` : "—"}
                    </td>
                    <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>
                      {r.run_id ? r.run_id.slice(0, 16) + "…" : "—"}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
          <div style={{ marginTop: 10, fontSize: 12, color: "var(--text-3)" }}>
            View full history in the{" "}
            <a href="/runs" style={{ color: "var(--accent)", textDecoration: "none", fontWeight: 600 }}>Runs</a>
            {" "}page.
          </div>
        </div>
      )}

      {/* Tests table */}
      {!loading && visible.length > 0 && (
        <div className="card" style={{ padding: 0, overflow: "hidden" }}>
          <table className="data-table">
            <thead>
              <tr>
                <th style={{ width: 32 }}>
                  <input type="checkbox" checked={allVisible} onChange={toggleAll} />
                </th>
                <th>Name</th>
                <th>Module</th>
                <th>Type</th>
                <th>Priority</th>
                <th>Status</th>
              </tr>
            </thead>
            <tbody>
              {visible.map(t => (
                <tr
                  key={t.test_case_id}
                  style={{
                    background: selected.has(t.test_case_id) ? "var(--accent-light)" : undefined,
                    cursor: "pointer",
                  }}
                  onClick={() => toggleOne(t.test_case_id)}
                >
                  <td onClick={e => e.stopPropagation()}>
                    <input
                      type="checkbox"
                      checked={selected.has(t.test_case_id)}
                      onChange={() => toggleOne(t.test_case_id)}
                    />
                  </td>
                  <td>
                    <div style={{ fontWeight: 600, fontSize: 13 }}>{t.name}</div>
                    <div style={{ fontFamily: "monospace", fontSize: 10, color: "var(--text-3)" }}>{t.test_case_id}</div>
                  </td>
                  <td><span className="badge badge-gray">{t.module || "—"}</span></td>
                  <td><span className={`badge ${TYPE_BADGE[t.type] || "badge-gray"}`}>{t.type || "—"}</span></td>
                  <td><PriorityBadge p={t.priority} /></td>
                  <td><span className={`badge ${STATUS_BADGE[t.status] || "badge-gray"}`}>{t.status}</span></td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {loading && (
        <div className="card" style={{ textAlign: "center", padding: "32px 20px", color: "var(--text-3)" }}>
          Loading catalog…
        </div>
      )}

      {!loading && visible.length === 0 && !loadError && (
        <div className="card" style={{ textAlign: "center", padding: "40px 20px", color: "var(--text-3)" }}>
          <div style={{ fontSize: 32, marginBottom: 12 }}>▶</div>
          <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 6 }}>No tests found</div>
          <div style={{ fontSize: 13 }}>Adjust the filters or approve drafts to populate the catalog.</div>
        </div>
      )}
    </div>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// AI Generation Panel (existing prompt-based flow)
// ══════════════════════════════════════════════════════════════════════════════

function AIGenerationPanel() {
  const [prompt, setPrompt]           = useState("");
  const [module, setModule]           = useState("");
  const [type, setType]               = useState("");
  const [priority, setPriority]       = useState("");
  const [maxDrafts, setMaxDrafts]     = useState(5);
  const [generating, setGenerating]   = useState(false);
  const [genError, setGenError]       = useState("");
  const [drafts, setDrafts]           = useState([]);
  const [genNotes, setGenNotes]       = useState([]);
  const [selectedDrafts, setSelected] = useState(new Set());
  const [approving, setApproving]     = useState(false);
  const [approveResult, setApproveResult] = useState(null);
  const [activate, setActivate]       = useState(true);
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
    <div>
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

          <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
            {drafts.map(d => (
              <div
                key={d.draft_id}
                className="card"
                style={{
                  borderColor: selectedDrafts.has(d.draft_id) ? "var(--accent)" : undefined,
                  background:  selectedDrafts.has(d.draft_id) ? "var(--accent-light)" : undefined,
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
          <div style={{ fontSize: 32, marginBottom: 12 }}>✦</div>
          <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 6 }}>No drafts yet</div>
          <div style={{ fontSize: 13 }}>Enter a prompt above and click Generate to get AI-drafted test cases.</div>
        </div>
      )}
    </div>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// Page root
// ══════════════════════════════════════════════════════════════════════════════

export default function DraftsPage() {
  const [tab, setTab] = useState("explorer");

  return (
    <div className="page-wrap">
      {/* Tab bar */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "1px solid var(--border)", paddingBottom: 0 }}>
        {TABS.map(t => (
          <button
            key={t.id}
            onClick={() => setTab(t.id)}
            style={{
              background: "none",
              border: "none",
              padding: "8px 18px",
              cursor: "pointer",
              fontSize: 13,
              fontWeight: tab === t.id ? 700 : 400,
              color: tab === t.id ? "var(--accent)" : "var(--text-2)",
              borderBottom: tab === t.id ? "2px solid var(--accent)" : "2px solid transparent",
              marginBottom: -1,
              transition: "color 0.15s",
            }}
          >
            {t.label}
          </button>
        ))}
      </div>

      {tab === "explorer" && <ExplorerDraftsPanel />}
      {tab === "catalog"  && <CatalogExecutionPanel />}
      {tab === "ai"       && <AIGenerationPanel />}
      {tab === "appmap"   && <AppMapPanel />}
    </div>
  );
}
