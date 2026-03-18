// src/pages/DraftsPage.jsx
import React, { useState, useEffect, useCallback } from "react";
import { generateTests, approveTests, generateDrafts, generateDraftsFromPages, approveDrafts, listTests, runSuite, exploreApp } from "../api";
import { useLang } from "../i18n/LangContext";

// ── Shared badge helpers ──────────────────────────────────────────────────────

function ConfidenceBadge({ c }) {
  const { t } = useLang();
  if (c === "high")   return <span className="badge badge-green">{t("drafts.badge.high")}</span>;
  if (c === "medium") return <span className="badge badge-blue">{t("drafts.badge.medium")}</span>;
  return <span className="badge badge-gray">{t("drafts.badge.low")}</span>;
}

function PriorityBadge({ p }) {
  const { t } = useLang();
  if (p === "high" || p === "critical") return <span className="badge badge-red">{p}</span>;
  if (p === "medium")                   return <span className="badge badge-orange">{p}</span>;
  return <span className="badge badge-gray">{p || t("drafts.badge.low")}</span>;
}

function ReasonBadge({ r }) {
  const { t } = useLang();
  const labels = {
    form_detected:           t("drafts.reason.form"),
    required_field_detected: t("drafts.reason.missing_field"),
    search_button_detected:  t("drafts.reason.search"),
    links_detected:          t("drafts.reason.navigation"),
  };
  return <span className="badge badge-blue">{labels[r] || r}</span>;
}

// ── Tab bar ───────────────────────────────────────────────────────────────────

const TAB_DEFS = [
  { id: "appmap",   labelKey: "drafts.tab.appmap"   },
  { id: "explorer", labelKey: "drafts.tab.explorer" },
  { id: "catalog",  labelKey: "drafts.tab.catalog"  },
  { id: "ai",       labelKey: "drafts.tab.ai"       },
];

// ══════════════════════════════════════════════════════════════════════════════
// Explorer Drafts Panel
// Source: Application Explorer → Suggested Tests → Draft Generator → Approve
// ══════════════════════════════════════════════════════════════════════════════

function ExplorerDraftsPanel({ onGoToCatalog }) {
  const { t } = useLang();
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
      setLoadError(e?.message || t("drafts.explorer.gen_error"));
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
      setApproveResult({ ok: false, error: e?.message || t("drafts.explorer.approve_error") });
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
        <div className="section-title" style={{ marginBottom: 12 }}>{t("drafts.explorer.title")}</div>
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
            {loading ? t("drafts.explorer.discovering") : t("drafts.explorer.discover_btn")}
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
            {allSelected ? t("drafts.common.deselect_all") : `${t("drafts.common.select_all")} (${drafts.length})`}
          </button>

          {selected.size > 0 && (
            <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
              {selected.size} {t("drafts.common.n_selected")}
            </span>
          )}

          <button
            className="btn btn-secondary btn-sm"
            onClick={() => handleApprove(drafts)}
            disabled={approving || drafts.length === 0}
          >
            {approving ? t("drafts.common.approving") : `${t("drafts.explorer.approve_all")} (${drafts.length})`}
          </button>

          {selected.size > 0 && (
            <button
              className="btn btn-primary btn-sm"
              onClick={() => handleApprove()}
              disabled={approving}
            >
              {approving ? t("drafts.common.approving") : `${t("drafts.explorer.approve_selected")} (${selected.size})`}
            </button>
          )}

          {approveResult && (
            <div style={{ flex: 1, display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
              <div className={`alert ${approveResult.ok ? "alert-success" : "alert-error"}`} style={{ margin: 0 }}>
                {approveResult.ok
                  ? `✓ ${(approveResult.saved || []).length} ${t("drafts.explorer.tests_added")}` +
                    (approveResult.skipped?.length ? ` · ${approveResult.skipped.length} ${t("drafts.explorer.skipped")}` : "")
                  : `✗ ${approveResult.error}`
                }
              </div>
              {approveResult.ok && onGoToCatalog && (
                <button className="btn btn-secondary btn-sm" onClick={onGoToCatalog}>
                  {t("drafts.explorer.run_catalog")}
                </button>
              )}
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
                      <span className="badge badge-gray">{t("drafts.badge.draft")}</span>
                    </div>

                    <div style={{ display: "flex", gap: 16, fontSize: 11, color: "var(--text-3)", alignItems: "center" }}>
                      <span>{d.steps?.length ?? 0} {t("drafts.common.steps")}</span>
                      <button
                        style={{
                          background: "none", border: "none",
                          color: "var(--accent)", cursor: "pointer",
                          fontSize: 11, padding: 0, fontWeight: 600,
                        }}
                        onClick={() => setExpanded(isExpanded ? null : d.test_name)}
                      >
                        {isExpanded ? t("drafts.common.hide_steps") : t("drafts.common.show_steps")}
                      </button>
                    </div>

                    {isExpanded && d.steps?.length > 0 && (
                      <div style={{ marginTop: 10 }}>
                        <table className="data-table" style={{ fontSize: 11 }}>
                          <thead>
                            <tr>
                              <th>{t("drafts.col.num")}</th>
                              <th>{t("drafts.col.action")}</th>
                              <th>{t("drafts.col.selector_url")}</th>
                              <th>{t("drafts.col.value")}</th>
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
        <div className="card" style={{ padding: "40px 32px" }}>
          <div style={{ fontSize: 28, marginBottom: 12 }}>⊕</div>
          <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text)", marginBottom: 8 }}>{t("drafts.explorer.empty_title")}</div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.8 }}>
            {t("drafts.explorer.empty_desc")}
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

function PageCard({ page, selected, onToggle }) {
  const { t } = useLang();
  const [open, setOpen] = useState(false);

  const counts = [
    { labelKey: "drafts.page.inputs_badge",  n: page.inputs?.length  ?? 0 },
    { labelKey: "drafts.page.buttons_badge", n: page.buttons?.length ?? 0 },
    { labelKey: "drafts.page.links_badge",   n: page.links?.length   ?? 0 },
    { labelKey: "drafts.page.forms_badge",   n: page.forms?.length   ?? 0 },
  ];

  return (
    <div
      className="card"
      style={{
        marginBottom: 10,
        borderColor: selected ? "var(--accent)" : undefined,
        background:  selected ? "var(--accent-light)" : undefined,
      }}
    >
      {/* Header row */}
      <div style={{ display: "flex", alignItems: "flex-start", gap: 12, flexWrap: "wrap" }}>
        {onToggle && (
          <input
            type="checkbox"
            checked={!!selected}
            onChange={onToggle}
            style={{ marginTop: 3, flexShrink: 0 }}
          />
        )}
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ fontWeight: 700, fontSize: 14 }}>{page.title || "(no title)"}</div>
          <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-3)", marginTop: 2, wordBreak: "break-all" }}>
            {page.url}
          </div>
        </div>
        <div style={{ display: "flex", gap: 6, flexWrap: "wrap", alignItems: "center" }}>
          {counts.map(c => (
            <span key={c.labelKey} className={`badge ${c.n > 0 ? "badge-blue" : "badge-gray"}`}>
              {c.n} {t(c.labelKey)}
            </span>
          ))}
          <button
            style={{ background: "none", border: "none", color: "var(--accent)", cursor: "pointer", fontSize: 11, fontWeight: 600, padding: "0 4px" }}
            onClick={() => setOpen(v => !v)}
          >
            {open ? t("drafts.common.less") : t("drafts.common.detail")}
          </button>
        </div>
      </div>

      {/* Expanded detail */}
      {open && (
        <div style={{ marginTop: 12, display: "flex", flexDirection: "column", gap: 10 }}>

          {/* Inputs */}
          {page.inputs?.length > 0 && (
            <div>
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>{t("drafts.page.inputs")}</div>
              <table className="data-table" style={{ fontSize: 11 }}>
                <thead><tr><th>{t("drafts.col.name")}</th><th>{t("drafts.col.selector")}</th></tr></thead>
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
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>{t("drafts.page.buttons")}</div>
              <table className="data-table" style={{ fontSize: 11 }}>
                <thead><tr><th>{t("drafts.col.name")}</th><th>{t("drafts.col.selector")}</th></tr></thead>
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
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>{t("drafts.page.links")}</div>
              <table className="data-table" style={{ fontSize: 11 }}>
                <thead><tr><th>{t("drafts.col.text")}</th><th>{t("drafts.col.selector")}</th></tr></thead>
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
              <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", marginBottom: 4, textTransform: "uppercase", letterSpacing: "0.05em" }}>{t("drafts.page.forms")}</div>
              {page.forms.map((frm, i) => (
                <div key={i} style={{ fontSize: 12, padding: "6px 10px", background: "var(--bg)", borderRadius: 6, marginBottom: 4 }}>
                  <span style={{ fontWeight: 700 }}>{frm.name || `form_${i + 1}`}</span>
                  {frm.fields?.length > 0 && (
                    <span style={{ marginLeft: 8, color: "var(--text-3)" }}>
                      {t("drafts.page.fields_prefix")} {frm.fields.join(", ")}
                    </span>
                  )}
                  {frm.buttons?.length > 0 && (
                    <span style={{ marginLeft: 8, color: "var(--text-3)" }}>
                      {t("drafts.page.buttons_prefix")} {frm.buttons.join(", ")}
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

function AppMapDraftCard({ draft }) {
  const { t } = useLang();
  const [open, setOpen] = useState(false);
  return (
    <div className="card" style={{ marginBottom: 8 }}>
      <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
        <span style={{ fontWeight: 700, fontSize: 13, fontFamily: "monospace", flex: 1 }}>{draft.test_name}</span>
        <PriorityBadge p={draft.priority} />
        <ReasonBadge   r={draft.reason} />
        <span className="badge badge-gray">{t("drafts.badge.suggested")}</span>
        <span style={{ fontSize: 11, color: "var(--text-3)" }}>{draft.steps?.length ?? 0} {t("drafts.appmap.actions_count")}</span>
        <button
          style={{ background: "none", border: "none", color: "var(--accent)", cursor: "pointer", fontSize: 11, fontWeight: 600, padding: 0 }}
          onClick={() => setOpen(v => !v)}
        >
          {open ? "▲" : "▼"}
        </button>
      </div>
      {open && draft.steps?.length > 0 && (
        <div style={{ marginTop: 8 }}>
          <table className="data-table" style={{ fontSize: 11 }}>
            <thead><tr>
              <th>{t("drafts.col.num")}</th>
              <th>{t("drafts.col.action")}</th>
              <th>{t("drafts.col.target_url")}</th>
              <th>{t("drafts.col.value")}</th>
            </tr></thead>
            <tbody>
              {draft.steps.map((st, i) => (
                <tr key={i}>
                  <td style={{ color: "var(--text-3)" }}>{i + 1}</td>
                  <td style={{ fontFamily: "monospace", fontWeight: 600 }}>{st.action}</td>
                  <td style={{ fontFamily: "monospace", color: "var(--text-2)" }}>{st.selector || st.url || "—"}</td>
                  <td style={{ fontFamily: "monospace", color: "var(--text-2)" }}>{st.value || "—"}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}

function AppMapPanel({ onGoToExplorer }) {
  const { t } = useLang();
  // ── Exploration state ──────────────────────────────────────────────────────
  const [url, setUrl]             = useState("");
  const [maxPages, setMax]        = useState(5);
  const [exploring, setExploring] = useState(false);
  const [exploreErr, setExpErr]   = useState("");
  const [result, setResult]       = useState(null);

  // ── Page selection ─────────────────────────────────────────────────────────
  const [selectedPages, setSelPages] = useState(new Set());

  // ── Draft generation ───────────────────────────────────────────────────────
  const [generating, setGenerating]   = useState(false);
  const [genErr, setGenErr]           = useState("");
  const [generatedDrafts, setDrafts]  = useState([]);

  async function handleExplore() {
    const trimmed = url.trim();
    if (!trimmed) return;
    setExploring(true);
    setExpErr("");
    setResult(null);
    setSelPages(new Set());
    setDrafts([]);
    setGenErr("");
    try {
      const res = await exploreApp(trimmed, maxPages);
      setResult(res);
    } catch (e) {
      setExpErr(e?.message || t("drafts.appmap.explore_error"));
    } finally {
      setExploring(false);
    }
  }

  function togglePage(pageUrl) {
    setSelPages(prev => {
      const s = new Set(prev);
      s.has(pageUrl) ? s.delete(pageUrl) : s.add(pageUrl);
      return s;
    });
  }

  const pages = result?.pages || [];
  const allSelected = pages.length > 0 && pages.every(p => selectedPages.has(p.url));

  function selectAll()   { setSelPages(new Set(pages.map(p => p.url))); }
  function clearSel()    { setSelPages(new Set()); }

  async function handleGenerateDrafts() {
    const chosen = pages.filter(p => selectedPages.has(p.url));
    if (!chosen.length) return;
    setGenerating(true);
    setGenErr("");
    setDrafts([]);
    try {
      const res = await generateDraftsFromPages(chosen);
      setDrafts(res.drafts || []);
    } catch (e) {
      setGenErr(e?.message || t("drafts.appmap.draft_gen_error"));
    } finally {
      setGenerating(false);
    }
  }

  return (
    <div>
      {/* Hero header */}
      <div style={{ marginBottom: 20 }}>
        <div style={{ fontSize: 20, fontWeight: 800, color: "var(--text)", marginBottom: 4 }}>
          {t("drafts.appmap.title")}
        </div>
        <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
          {t("drafts.appmap.subtitle")}
        </div>
      </div>

      {/* Input row */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 12 }}>{t("drafts.appmap.section_title")}</div>
        <div style={{ display: "flex", gap: 10, alignItems: "flex-end", flexWrap: "wrap" }}>
          <div style={{ flex: 1, minWidth: 240 }}>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.appmap.start_url")}</label>
            <input
              className="input"
              style={{ width: "100%" }}
              placeholder="https://example.com"
              value={url}
              onChange={e => setUrl(e.target.value)}
              onKeyDown={e => e.key === "Enter" && handleExplore()}
              disabled={exploring}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.appmap.max_pages")}</label>
            <select className="input" value={maxPages} onChange={e => setMax(Number(e.target.value))} style={{ width: 90 }}>
              {[1, 2, 3, 5, 8, 10, 15, 20].map(n => <option key={n} value={n}>{n}</option>)}
            </select>
          </div>
          <button
            className="btn btn-primary"
            onClick={handleExplore}
            disabled={exploring || !url.trim()}
            style={{ alignSelf: "flex-end" }}
          >
            {exploring ? t("drafts.appmap.discovering") : t("drafts.appmap.discover_btn")}
          </button>
        </div>
        {exploreErr && <div className="alert alert-error" style={{ marginTop: 12 }}>{exploreErr}</div>}
      </div>

      {/* Discovery loading */}
      {exploring && (
        <div className="card" style={{ textAlign: "center", padding: "32px 20px", color: "var(--text-3)" }}>
          {t("drafts.appmap.loading")}
        </div>
      )}

      {/* Results */}
      {result && !exploring && (
        <>
          {/* Summary */}
          <div className="card" style={{ marginBottom: 16 }}>
            <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 8 }}>{t("drafts.appmap.start_url")}</div>
            <div style={{ fontFamily: "monospace", fontSize: 12, wordBreak: "break-all", marginBottom: 16 }}>{result.start_url}</div>
            <div style={{ display: "flex", gap: 24, flexWrap: "wrap" }}>
              <div style={{ textAlign: "center" }}>
                <div style={{ fontSize: 26, fontWeight: 800, color: "var(--accent)" }}>{result.visited_count ?? 0}</div>
                <div style={{ fontSize: 11, color: "var(--text-3)" }}>{t("drafts.appmap.pages_discovered_label")}</div>
              </div>
              <div style={{ textAlign: "center" }}>
                <div style={{ fontSize: 26, fontWeight: 800, color: "var(--accent)" }}>
                  {pages.reduce((sum, p) => sum + (p.inputs?.length ?? 0) + (p.buttons?.length ?? 0) + (p.links?.length ?? 0), 0)}
                </div>
                <div style={{ fontSize: 11, color: "var(--text-3)" }}>{t("drafts.appmap.interactive_elements")}</div>
              </div>
              <div style={{ textAlign: "center" }}>
                <div style={{ fontSize: 26, fontWeight: 800, color: generatedDrafts.length > 0 ? "var(--green, #22c55e)" : "var(--text-3)" }}>
                  {generatedDrafts.length > 0 ? generatedDrafts.length : "—"}
                </div>
                <div style={{ fontSize: 11, color: "var(--text-3)" }}>{t("drafts.appmap.test_scenarios_label")}</div>
              </div>
              <div style={{ textAlign: "center" }}>
                <div style={{ fontSize: 26, fontWeight: 800, color: result.errors?.length > 0 ? "var(--red)" : "var(--text-3)" }}>
                  {result.errors?.length ?? 0}
                </div>
                <div style={{ fontSize: 11, color: "var(--text-3)" }}>{t("drafts.appmap.exploration_issues_label")}</div>
              </div>
            </div>
          </div>

          {/* Page selection action bar */}
          {pages.length > 0 && (
            <div className="card" style={{
              marginBottom: 12,
              padding: "10px 16px",
              display: "flex",
              alignItems: "center",
              gap: 12,
              flexWrap: "wrap",
              background: selectedPages.size > 0 ? "var(--accent-light)" : undefined,
              borderColor: selectedPages.size > 0 ? "var(--accent-border)" : undefined,
            }}>
              <button className="btn btn-secondary btn-sm" onClick={allSelected ? clearSel : selectAll}>
                {allSelected ? t("drafts.common.clear_selection") : `${t("drafts.common.select_all")} (${pages.length})`}
              </button>

              {selectedPages.size > 0 && (
                <>
                  <span className="badge badge-blue">
                    {t("drafts.appmap.selected_badge")} {selectedPages.size} page{selectedPages.size > 1 ? "s" : ""}
                  </span>
                  <button
                    className="btn btn-primary btn-sm"
                    onClick={handleGenerateDrafts}
                    disabled={generating}
                  >
                    {generating ? t("drafts.common.generating") : t("drafts.appmap.generate_btn")}
                  </button>
                </>
              )}

              {genErr && (
                <div className="alert alert-error" style={{ margin: 0, flex: 1 }}>{genErr}</div>
              )}
            </div>
          )}

          {/* Page cards */}
          {pages.length > 0 && (
            <div style={{ marginBottom: 16 }}>
              <div style={{ fontSize: 12, fontWeight: 700, color: "var(--text-2)", marginBottom: 8 }}>
                {t("drafts.appmap.discovered_pages")} ({pages.length})
              </div>
              {pages.map((page, i) => (
                <PageCard
                  key={page.url || i}
                  page={page}
                  selected={selectedPages.has(page.url)}
                  onToggle={() => togglePage(page.url)}
                />
              ))}
            </div>
          )}

          {/* Errors */}
          {result.errors?.length > 0 && (
            <div style={{ marginBottom: 16 }}>
              <div style={{ fontSize: 12, fontWeight: 700, color: "var(--red)", marginBottom: 8 }}>
                {t("drafts.appmap.exploration_issues_title")} ({result.errors.length})
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

          {/* Generated Drafts */}
          {generatedDrafts.length > 0 && (
            <div>
              <div style={{ display: "flex", alignItems: "center", gap: 10, marginBottom: 10 }}>
                <div style={{ fontSize: 12, fontWeight: 700, color: "var(--text-2)" }}>
                  {t("drafts.appmap.suggested_scenarios")} ({generatedDrafts.length})
                </div>
                <span className="badge badge-green">{generatedDrafts.length} {t("drafts.appmap.ready_to_approve")}</span>
                <button
                  className="btn btn-secondary btn-sm"
                  onClick={onGoToExplorer}
                >
                  {t("drafts.appmap.go_to_explorer")}
                </button>
              </div>
              {generatedDrafts.map(d => (
                <AppMapDraftCard key={d.test_name} draft={d} />
              ))}
            </div>
          )}

          {generating && (
            <div className="card" style={{ textAlign: "center", padding: "20px", color: "var(--text-3)" }}>
              {t("drafts.appmap.generating_drafts")}
            </div>
          )}
        </>
      )}

      {/* Empty state */}
      {!exploring && !result && !exploreErr && (
        <div className="card" style={{ padding: "40px 32px" }}>
          <div style={{ fontSize: 28, marginBottom: 14 }}>🗺</div>
          <div style={{ fontSize: 16, fontWeight: 700, color: "var(--text)", marginBottom: 10 }}>
            {t("drafts.appmap.empty_title")}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.8, maxWidth: 500 }}>
            <div>{t("drafts.appmap.empty_desc1")}</div>
            <div>{t("drafts.appmap.empty_desc2")}</div>
            <div>{t("drafts.appmap.empty_desc3")}</div>
            <div>{t("drafts.appmap.empty_desc4")}</div>
          </div>
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
  const { t } = useLang();
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
      setLoadError(e?.message || t("drafts.catalog.load_error"));
    } finally {
      setLoading(false);
    }
  }, [filterModule, filterPriority, filterStatus, t]);

  useEffect(() => { loadTests(); }, [loadTests]);

  // Client-side text search
  const visible = search.trim()
    ? tests.filter(tc =>
        tc.name?.toLowerCase().includes(search.toLowerCase()) ||
        tc.test_case_id?.toLowerCase().includes(search.toLowerCase())
      )
    : tests;

  const allVisible = visible.length > 0 && visible.every(tc => selected.has(tc.test_case_id));

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
        visible.forEach(tc => s.delete(tc.test_case_id));
        return s;
      });
    } else {
      setSelected(prev => {
        const s = new Set(prev);
        visible.forEach(tc => s.add(tc.test_case_id));
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
      setExecError(e?.message || t("drafts.catalog.exec_error"));
    } finally {
      setExecuting(false);
    }
  }

  const selectedIds = [...selected];
  const visibleIds  = visible.map(tc => tc.test_case_id);

  return (
    <div>
      {/* Filters */}
      <div className="card" style={{ marginBottom: 16 }}>
        <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "flex-end" }}>
          <div style={{ flex: 1, minWidth: 200 }}>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.catalog.search_label")}</label>
            <input
              className="input"
              placeholder={t("drafts.catalog.search_placeholder")}
              value={search}
              onChange={e => setSearch(e.target.value)}
              style={{ width: "100%" }}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.catalog.module_label")}</label>
            <input
              className="input"
              placeholder={t("drafts.catalog.module_placeholder")}
              value={filterModule}
              onChange={e => setFilterModule(e.target.value)}
              onBlur={loadTests}
              onKeyDown={e => e.key === "Enter" && loadTests()}
              style={{ width: 130 }}
            />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.catalog.priority_label")}</label>
            <select className="input" value={filterPriority} onChange={e => setFilterPri(e.target.value)} style={{ width: 120 }}>
              <option value="">{t("drafts.catalog.all")}</option>
              {["critical","high","medium","low"].map(v => <option key={v} value={v}>{v}</option>)}
            </select>
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.catalog.status_label")}</label>
            <select className="input" value={filterStatus} onChange={e => setFilterStatus(e.target.value)} style={{ width: 110 }}>
              <option value="active">active</option>
              <option value="inactive">inactive</option>
              <option value="">{t("drafts.catalog.all")}</option>
            </select>
          </div>
          <button className="btn btn-secondary" onClick={loadTests} disabled={loading} style={{ alignSelf: "flex-end" }}>
            {loading ? t("common.loading") : t("drafts.catalog.reload")}
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
            {allVisible ? t("drafts.common.deselect_all") : `${t("drafts.catalog.select_all_visible")} (${visible.length})`}
          </button>

          {selectedIds.length > 0 && (
            <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
              {selectedIds.length} {t("drafts.common.n_selected")}
            </span>
          )}

          {selectedIds.length > 0 && (
            <button
              className="btn btn-primary btn-sm"
              onClick={() => handleExecute(selectedIds)}
              disabled={executing}
            >
              {executing ? t("drafts.common.running") : `${t("drafts.catalog.run_selected")} (${selectedIds.length})`}
            </button>
          )}

          <button
            className="btn btn-secondary btn-sm"
            onClick={() => handleExecute(visibleIds)}
            disabled={executing || visibleIds.length === 0}
            style={{ marginLeft: "auto" }}
          >
            {executing ? t("drafts.common.running") : `${t("drafts.catalog.run_all_filtered")} (${visibleIds.length})`}
          </button>
        </div>
      )}

      {/* Execution error */}
      {execError && <div className="alert alert-error" style={{ marginBottom: 12 }}>{execError}</div>}

      {/* Execution result summary */}
      {execResult && (
        <div className="card" style={{ marginBottom: 16, borderColor: "var(--accent-border)" }}>
          <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 10, flexWrap: "wrap" }}>
            <span style={{ fontWeight: 700, fontSize: 14 }}>{t("drafts.catalog.exec_complete")}</span>
            <span className="badge badge-gray">{execResult.total ?? 0} {t("drafts.catalog.total")}</span>
            <span className="badge badge-green">{execResult.passed ?? 0} {t("drafts.catalog.passed")}</span>
            {(execResult.failed ?? 0) > 0 && <span className="badge badge-red">{execResult.failed} {t("drafts.catalog.failed")}</span>}
            {(execResult.errors ?? 0) > 0 && <span className="badge badge-orange">{execResult.errors} {t("drafts.catalog.errors")}</span>}
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
                  <th>{t("drafts.catalog.col.test")}</th>
                  <th>{t("drafts.catalog.col.status")}</th>
                  <th>{t("drafts.catalog.col.duration")}</th>
                  <th>{t("drafts.catalog.col.run_id")}</th>
                </tr>
              </thead>
              <tbody>
                {execResult.runs.map(r => (
                  <tr key={r.run_id}>
                    <td style={{ fontWeight: 600 }}>{r.test_name || r.test_id || r.test_case_id}</td>
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
          <div style={{ marginTop: 12, display: "flex", alignItems: "center", gap: 10 }}>
            <span style={{ fontSize: 12, color: "var(--text-3)" }}>{t("drafts.catalog.history_note")}</span>
            <a href="/runs" style={{ fontSize: 12, color: "var(--accent)", textDecoration: "none", fontWeight: 600 }}>
              {t("drafts.catalog.view_runs")}
            </a>
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
                <th>{t("drafts.catalog.col.name")}</th>
                <th>{t("drafts.catalog.col.module")}</th>
                <th>{t("drafts.catalog.col.type")}</th>
                <th>{t("drafts.catalog.col.priority")}</th>
                <th>{t("drafts.catalog.col.status")}</th>
              </tr>
            </thead>
            <tbody>
              {visible.map(tc => (
                <tr
                  key={tc.test_case_id}
                  style={{
                    background: selected.has(tc.test_case_id) ? "var(--accent-light)" : undefined,
                    cursor: "pointer",
                  }}
                  onClick={() => toggleOne(tc.test_case_id)}
                >
                  <td onClick={e => e.stopPropagation()}>
                    <input
                      type="checkbox"
                      checked={selected.has(tc.test_case_id)}
                      onChange={() => toggleOne(tc.test_case_id)}
                    />
                  </td>
                  <td>
                    <div style={{ fontWeight: 600, fontSize: 13 }}>{tc.name}</div>
                    <div style={{ fontFamily: "monospace", fontSize: 10, color: "var(--text-3)" }}>{tc.test_case_id}</div>
                  </td>
                  <td><span className="badge badge-gray">{tc.module || "—"}</span></td>
                  <td><span className={`badge ${TYPE_BADGE[tc.type] || "badge-gray"}`}>{tc.type || "—"}</span></td>
                  <td><PriorityBadge p={tc.priority} /></td>
                  <td><span className={`badge ${STATUS_BADGE[tc.status] || "badge-gray"}`}>{tc.status}</span></td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {loading && (
        <div className="card" style={{ textAlign: "center", padding: "32px 20px", color: "var(--text-3)" }}>
          {t("drafts.catalog.loading")}
        </div>
      )}

      {!loading && visible.length === 0 && !loadError && (
        <div className="card" style={{ padding: "40px 32px" }}>
          <div style={{ fontSize: 28, marginBottom: 12 }}>▶</div>
          <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text)", marginBottom: 8 }}>{t("drafts.catalog.empty_title")}</div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.8 }}>
            {t("drafts.catalog.empty_desc")}
          </div>
        </div>
      )}
    </div>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// AI Generation Panel (existing prompt-based flow)
// ══════════════════════════════════════════════════════════════════════════════

function AIGenerationPanel() {
  const { t } = useLang();
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
      setGenError(e?.message || t("drafts.ai.gen_error"));
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
      setApproveResult({ ok: false, error: e?.message || t("drafts.ai.approve_error") });
    } finally {
      setApproving(false);
    }
  }

  return (
    <div>
      {/* Generation form */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("drafts.ai.title")}</div>
        <div style={{ display: "flex", flexDirection: "column", gap: 12 }}>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>
              {t("drafts.ai.prompt_label")} <span style={{ color: "var(--red)" }}>*</span>
            </label>
            <textarea
              className="input"
              rows={3}
              placeholder={t("drafts.ai.prompt_placeholder")}
              value={prompt}
              onChange={e => setPrompt(e.target.value)}
              style={{ width: "100%", resize: "vertical" }}
            />
          </div>
          <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "flex-end" }}>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.ai.module_label")}</label>
              <input className="input" placeholder={t("drafts.ai.module_placeholder")} value={module} onChange={e => setModule(e.target.value)} style={{ width: 140 }} />
            </div>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.ai.type_label")}</label>
              <select className="input" value={type} onChange={e => setType(e.target.value)} style={{ width: 140 }}>
                <option value="">{t("drafts.ai.auto")}</option>
                {["smoke","regression","functional","negative","e2e"].map(v => <option key={v} value={v}>{v}</option>)}
              </select>
            </div>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.ai.priority_label")}</label>
              <select className="input" value={priority} onChange={e => setPriority(e.target.value)} style={{ width: 120 }}>
                <option value="">{t("drafts.ai.auto")}</option>
                {["critical","high","medium","low"].map(v => <option key={v} value={v}>{v}</option>)}
              </select>
            </div>
            <div>
              <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("drafts.ai.max_drafts_label")}</label>
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
              {generating ? t("drafts.common.generating") : t("drafts.ai.generate_btn")}
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
              {selectedDrafts.size === drafts.length ? t("drafts.common.deselect_all") : `${t("drafts.common.select_all")} (${drafts.length})`}
            </button>
            {selectedDrafts.size > 0 && (
              <>
                <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
                  {selectedDrafts.size} {t("drafts.common.n_selected")}
                </span>
                <label style={{ fontSize: 13, color: "var(--text-2)", display: "flex", alignItems: "center", gap: 6 }}>
                  <input type="checkbox" checked={activate} onChange={e => setActivate(e.target.checked)} />
                  {t("drafts.ai.activate")}
                </label>
                <button
                  className="btn btn-primary btn-sm"
                  onClick={handleApprove}
                  disabled={approving}
                >
                  {approving ? t("drafts.ai.adding") : `${t("drafts.ai.add_to_catalog")} (${selectedDrafts.size})`}
                </button>
              </>
            )}
            {approveResult && (
              <div className={`alert ${approveResult.ok ? "alert-success" : "alert-error"}`} style={{ margin: 0, flex: 1 }}>
                {approveResult.ok
                  ? `✓ ${t("drafts.ai.created")} ${approveResult.total_created} ${t("drafts.ai.tests_created_label")} ${(approveResult.created_test_case_ids || []).join(", ")}`
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
                      <span>{d.steps?.length ?? 0} {t("drafts.common.steps")}</span>
                      <span>{d.assertions?.length ?? 0} {t("drafts.ai.assertions")}</span>
                      <button
                        style={{ background: "none", border: "none", color: "var(--accent)", cursor: "pointer", fontSize: 11, padding: 0, fontWeight: 600 }}
                        onClick={() => setExpanded(expanded === d.draft_id ? null : d.draft_id)}
                      >
                        {expanded === d.draft_id ? t("drafts.common.hide_steps") : t("drafts.common.show_steps")}
                      </button>
                    </div>
                    {expanded === d.draft_id && d.steps?.length > 0 && (
                      <div style={{ marginTop: 10 }}>
                        <table className="data-table" style={{ fontSize: 11 }}>
                          <thead><tr>
                            <th>{t("drafts.col.num")}</th>
                            <th>{t("drafts.col.action")}</th>
                            <th>{t("drafts.col.target_value")}</th>
                          </tr></thead>
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
        <div className="card" style={{ padding: "40px 32px" }}>
          <div style={{ fontSize: 28, marginBottom: 12 }}>✦</div>
          <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text)", marginBottom: 8 }}>{t("drafts.ai.empty_title")}</div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.8 }}>
            {t("drafts.ai.empty_desc")}
          </div>
        </div>
      )}
    </div>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// Page root
// ══════════════════════════════════════════════════════════════════════════════

function KpiBar() {
  const { t } = useLang();
  return (
    <div style={{
      display: "flex", gap: 0,
      borderBottom: "1px solid var(--border)",
      marginBottom: 20,
      paddingBottom: 16,
    }}>
      {[
        { labelKey: "drafts.kpi.pages_discovered", value: "—", hintKey: "drafts.kpi.hint_appmap"         },
        { labelKey: "drafts.kpi.test_scenarios",   value: "—", hintKey: "drafts.kpi.hint_suggested"      },
        { labelKey: "drafts.kpi.approved_tests",   value: "—", hintKey: "drafts.kpi.hint_catalog"        },
        { labelKey: "drafts.kpi.recent_runs",      value: "—", hintKey: "drafts.kpi.hint_last_execution" },
      ].map((kpi, i, arr) => (
        <div key={kpi.labelKey} style={{
          flex: 1,
          padding: "0 20px",
          borderRight: i < arr.length - 1 ? "1px solid var(--border)" : "none",
        }}>
          <div style={{ fontSize: 22, fontWeight: 800, color: "var(--accent)" }}>{kpi.value}</div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginTop: 2 }}>{t(kpi.labelKey)}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)" }}>{t(kpi.hintKey)}</div>
        </div>
      ))}
    </div>
  );
}

export default function DraftsPage() {
  const { t } = useLang();
  const [tab, setTab] = useState("appmap");

  return (
    <div className="page-wrap">
      {/* KPI summary bar */}
      <KpiBar />

      {/* Tab bar */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "1px solid var(--border)", paddingBottom: 0 }}>
        {TAB_DEFS.map(tabDef => (
          <button
            key={tabDef.id}
            onClick={() => setTab(tabDef.id)}
            style={{
              background: "none",
              border: "none",
              padding: "8px 18px",
              cursor: "pointer",
              fontSize: 13,
              fontWeight: tab === tabDef.id ? 700 : 400,
              color: tab === tabDef.id ? "var(--accent)" : "var(--text-2)",
              borderBottom: tab === tabDef.id ? "2px solid var(--accent)" : "2px solid transparent",
              marginBottom: -1,
              transition: "color 0.15s",
            }}
          >
            {t(tabDef.labelKey)}
          </button>
        ))}
      </div>

      {tab === "explorer" && <ExplorerDraftsPanel onGoToCatalog={() => setTab("catalog")} />}
      {tab === "catalog"  && <CatalogExecutionPanel />}
      {tab === "ai"       && <AIGenerationPanel />}
      {tab === "appmap"   && <AppMapPanel onGoToExplorer={() => setTab("explorer")} />}
    </div>
  );
}
