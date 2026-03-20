// src/pages/GeneratePage.jsx
/**
 * Generate Tests — unified entry point for all test-generation sources.
 *
 * Tabs:
 *   From Description → PlannerPage (embedded, no header)
 *   From URL         → inline FromUrlPanel (App Map exploration flow)
 *   From PR          → PRAnalysisPage (no page header, directly embeds content)
 *   From API         → ApiTestingPage (no page header, stepper only)
 *
 * Legacy routes /planner, /drafts, /pr-analysis, /api-testing remain active
 * in App.jsx for backward compatibility.
 */
import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { useLang } from "../i18n/LangContext";
import {
  exploreApp,
  generateDraftsFromPages,
  approveDrafts,
  batchSaveDrafts,
} from "../api";
import PlannerPage    from "./PlannerPage";
import PRAnalysisPage from "./PRAnalysisPage";
import ApiTestingPage from "./ApiTestingPage";

// ── Summary cards ─────────────────────────────────────────────────────────────

const SOURCE_CARDS = [
  {
    icon: "⚡",
    titleKey: "gen.card.desc_title",
    descKey:  "gen.card.desc_desc",
    tab: 0,
  },
  {
    icon: "⊞",
    titleKey: "gen.card.url_title",
    descKey:  "gen.card.url_desc",
    tab: 1,
  },
  {
    icon: "◎",
    titleKey: "gen.card.pr_title",
    descKey:  "gen.card.pr_desc",
    tab: 2,
  },
  {
    icon: "⌥",
    titleKey: "gen.card.api_title",
    descKey:  "gen.card.api_desc",
    tab: 3,
  },
];

function SourceCard({ card, active, onClick, t }) {
  return (
    <div
      onClick={onClick}
      style={{
        flex: "1 1 180px",
        minWidth: 150,
        padding: "14px 16px",
        borderRadius: 10,
        border: `1px solid ${active ? "var(--accent)" : "var(--border)"}`,
        background: active ? "rgba(79,107,255,0.06)" : "var(--surface)",
        cursor: "pointer",
        transition: "border-color 0.15s, background 0.15s",
      }}
    >
      <div style={{
        width: 34, height: 34, borderRadius: 8,
        background: active ? "rgba(79,107,255,0.15)" : "var(--bg)",
        display: "flex", alignItems: "center", justifyContent: "center",
        fontSize: 16, marginBottom: 10,
      }}>
        {card.icon}
      </div>
      <div style={{ fontSize: 13, fontWeight: 700, color: "var(--text-1)", marginBottom: 4 }}>
        {t(card.titleKey)}
      </div>
      <div style={{ fontSize: 11, color: "var(--text-3)", lineHeight: 1.5 }}>
        {t(card.descKey)}
      </div>
    </div>
  );
}

// ── Tab bar ───────────────────────────────────────────────────────────────────

const TABS = [
  { key: "gen.tab.description", icon: "⚡" },
  { key: "gen.tab.url",         icon: "⊞" },
  { key: "gen.tab.pr",          icon: "◎" },
  { key: "gen.tab.api",         icon: "⌥" },
];

// ── From URL Panel ────────────────────────────────────────────────────────────

function PriorityBadge({ p }) {
  if (p === "high" || p === "critical") return <span className="badge badge-red">{p}</span>;
  if (p === "medium")                   return <span className="badge badge-orange">{p}</span>;
  return <span className="badge badge-gray">{p || "low"}</span>;
}

function FromUrlPanel() {
  const { t } = useLang();
  const navigate = useNavigate();

  // Explore state
  const [url, setUrl]           = useState("");
  const [maxPages, setMax]      = useState(5);
  const [exploring, setExpl]    = useState(false);
  const [exploreErr, setExpErr] = useState("");
  const [pages, setPages]       = useState([]);

  // Page selection
  const [selPages, setSelPages] = useState(new Set());

  // Draft generation state
  const [generating, setGen]   = useState(false);
  const [genErr, setGenErr]     = useState("");
  const [drafts, setDrafts]     = useState([]);
  const [selDrafts, setSelD]    = useState(new Set());
  const [expanded, setExpanded] = useState(null);

  // Save / approve state
  const [saving, setSaving]         = useState(false);
  const [saveResult, setSaveResult] = useState(null);
  const [approving, setApproving]   = useState(false);
  const [approveResult, setApproveResult] = useState(null);

  const allPagesSelected = pages.length > 0 && pages.every(p => selPages.has(p.url));
  const allDraftsSelected = drafts.length > 0 && selDrafts.size === drafts.length;

  async function handleExplore() {
    const trimmed = url.trim();
    if (!trimmed) return;
    setExpl(true);
    setExpErr("");
    setPages([]);
    setSelPages(new Set());
    setDrafts([]);
    setSelD(new Set());
    setSaveResult(null);
    setApproveResult(null);
    try {
      const res = await exploreApp(trimmed, maxPages);
      const found = res?.pages || [];
      setPages(found);
      setSelPages(new Set(found.map(p => p.url)));
    } catch (e) {
      setExpErr(e?.message || t("gen.url.explore_error"));
    } finally {
      setExpl(false);
    }
  }

  async function handleGenerateDrafts() {
    const chosen = pages.filter(p => selPages.has(p.url));
    if (!chosen.length) return;
    setGen(true);
    setGenErr("");
    setDrafts([]);
    setSelD(new Set());
    setSaveResult(null);
    setApproveResult(null);
    try {
      const res = await generateDraftsFromPages(chosen);
      const generated = res?.drafts || [];
      setDrafts(generated);
      setSelD(new Set(generated.map(d => d.test_name)));
    } catch (e) {
      setGenErr(e?.message || t("gen.url.gen_error"));
    } finally {
      setGen(false);
    }
  }

  function togglePage(pageUrl) {
    setSelPages(prev => {
      const s = new Set(prev);
      s.has(pageUrl) ? s.delete(pageUrl) : s.add(pageUrl);
      return s;
    });
  }

  function toggleDraft(name) {
    setSelD(prev => {
      const s = new Set(prev);
      s.has(name) ? s.delete(name) : s.add(name);
      return s;
    });
  }

  async function handleSaveDrafts() {
    const toSave = drafts.filter(d => selDrafts.has(d.test_name));
    if (!toSave.length || saving) return;
    setSaving(true);
    setSaveResult(null);
    try {
      const payload = toSave.map(d => ({
        test_name:   d.test_name,
        steps:       d.steps || [],
        priority:    d.priority || "medium",
        description: d.reason || "",
        module:      (() => {
          try { return new URL(url).pathname.split("/").filter(Boolean)[0] || "explorer"; }
          catch { return "explorer"; }
        })(),
      }));
      const res = await batchSaveDrafts(payload);
      setSaveResult(res);
    } catch (e) {
      setSaveResult({ error: e?.message || "Save failed" });
    } finally {
      setSaving(false);
    }
  }

  async function handleApprove() {
    const toApprove = drafts.filter(d => selDrafts.has(d.test_name));
    if (!toApprove.length || approving) return;
    setApproving(true);
    setApproveResult(null);
    try {
      const res = await approveDrafts(toApprove);
      setApproveResult({ ok: true, ...res });
    } catch (e) {
      setApproveResult({ ok: false, error: e?.message || "Approve failed" });
    } finally {
      setApproving(false);
    }
  }

  return (
    <div className="page-wrap">

      {/* ── URL + config ─────────────────────────────────────────────────── */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 12 }}>{t("gen.url.section_title")}</div>
        <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
          <input
            className="input"
            style={{ flex: 1, minWidth: 260 }}
            placeholder="https://example.com"
            value={url}
            onChange={e => setUrl(e.target.value)}
            onKeyDown={e => e.key === "Enter" && handleExplore()}
            disabled={exploring}
          />
          <div style={{ display: "flex", alignItems: "center", gap: 6, flexShrink: 0 }}>
            <span style={{ fontSize: 12, color: "var(--text-3)", whiteSpace: "nowrap" }}>{t("gen.url.max_pages")}:</span>
            <input
              type="number"
              className="input"
              style={{ width: 60 }}
              value={maxPages}
              min={1}
              max={20}
              onChange={e => setMax(Math.max(1, Math.min(20, Number(e.target.value))))}
              disabled={exploring}
            />
          </div>
          <button
            className="btn btn-primary"
            onClick={handleExplore}
            disabled={exploring || !url.trim()}
            style={{ flexShrink: 0 }}
          >
            {exploring ? t("gen.url.exploring") : t("gen.url.explore_btn")}
          </button>
        </div>
        {exploreErr && (
          <div className="alert alert-error" style={{ marginTop: 12 }}>{exploreErr}</div>
        )}
      </div>

      {/* ── Page map ─────────────────────────────────────────────────────── */}
      {pages.length > 0 && !drafts.length && (
        <div className="card" style={{ marginBottom: 20 }}>
          <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 12, flexWrap: "wrap" }}>
            <div className="section-title" style={{ margin: 0 }}>
              {pages.length} {t("gen.url.pages_found")}
            </div>
            <button
              className="btn btn-secondary btn-sm"
              onClick={() => allPagesSelected ? setSelPages(new Set()) : setSelPages(new Set(pages.map(p => p.url)))}
            >
              {allPagesSelected ? t("gen.url.deselect_all") : t("gen.url.select_all")}
            </button>
            {selPages.size > 0 && (
              <button
                className="btn btn-primary btn-sm"
                onClick={handleGenerateDrafts}
                disabled={generating}
                style={{ marginLeft: "auto" }}
              >
                {generating ? t("gen.url.generating") : `${t("gen.url.generate_btn")} (${selPages.size})`}
              </button>
            )}
          </div>

          {genErr && <div className="alert alert-error" style={{ marginBottom: 12 }}>{genErr}</div>}

          <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
            {pages.map(page => (
              <div
                key={page.url}
                className="card"
                style={{
                  padding: "10px 14px",
                  borderColor: selPages.has(page.url) ? "var(--accent)" : undefined,
                  background:  selPages.has(page.url) ? "var(--accent-light)" : undefined,
                }}
              >
                <div style={{ display: "flex", alignItems: "flex-start", gap: 10 }}>
                  <input
                    type="checkbox"
                    checked={selPages.has(page.url)}
                    onChange={() => togglePage(page.url)}
                    style={{ marginTop: 3, flexShrink: 0 }}
                  />
                  <div style={{ flex: 1, minWidth: 0 }}>
                    <div style={{ fontWeight: 700, fontSize: 13 }}>{page.title || "(no title)"}</div>
                    <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-3)", wordBreak: "break-all", marginTop: 2 }}>
                      {page.url}
                    </div>
                  </div>
                  <div style={{ display: "flex", gap: 4, flexShrink: 0, flexWrap: "wrap" }}>
                    {page.inputs?.length > 0   && <span className="badge badge-blue">{page.inputs.length} {t("gen.url.badge_inputs")}</span>}
                    {page.buttons?.length > 0  && <span className="badge badge-blue">{page.buttons.length} {t("gen.url.badge_buttons")}</span>}
                    {page.forms?.length > 0    && <span className="badge badge-green">{page.forms.length} {t("gen.url.badge_forms")}</span>}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* ── Draft results ─────────────────────────────────────────────────── */}
      {drafts.length > 0 && (
        <>
          {/* Action bar */}
          <div className="card" style={{
            marginBottom: 12, padding: "10px 16px",
            display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap",
            background: selDrafts.size > 0 ? "var(--accent-light)" : undefined,
            borderColor: selDrafts.size > 0 ? "var(--accent-border)" : undefined,
          }}>
            <button
              className="btn btn-secondary btn-sm"
              onClick={() => allDraftsSelected ? setSelD(new Set()) : setSelD(new Set(drafts.map(d => d.test_name)))}
            >
              {allDraftsSelected ? t("gen.url.deselect_all") : `${t("gen.url.select_all")} (${drafts.length})`}
            </button>
            {selDrafts.size > 0 && (
              <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
                {selDrafts.size} {t("gen.url.selected")}
              </span>
            )}
            <div style={{ marginLeft: "auto", display: "flex", gap: 8, flexWrap: "wrap" }}>
              {selDrafts.size > 0 && !saveResult?.saved_count && !saveResult?.error && (
                <button
                  className="btn btn-secondary btn-sm"
                  onClick={handleSaveDrafts}
                  disabled={saving}
                >
                  {saving ? t("gen.url.saving") : `${t("gen.url.save_btn")} (${selDrafts.size})`}
                </button>
              )}
              {selDrafts.size > 0 && !approveResult && (
                <button
                  className="btn btn-primary btn-sm"
                  onClick={handleApprove}
                  disabled={approving}
                >
                  {approving ? t("gen.url.approving") : `${t("gen.url.approve_btn")} (${selDrafts.size})`}
                </button>
              )}
            </div>

            {saveResult && (
              <div style={{ display: "flex", gap: 8, alignItems: "center", width: "100%", marginTop: 6 }}>
                <div className={`alert ${saveResult.error ? "alert-error" : "alert-success"}`} style={{ margin: 0, fontSize: 12 }}>
                  {saveResult.error
                    ? `✗ ${saveResult.error}`
                    : `✓ ${saveResult.saved_count ?? 0} ${t("gen.url.saved_count")}`}
                </div>
                {!saveResult.error && (
                  <button className="btn btn-secondary btn-sm" style={{ fontSize: 11 }} onClick={() => navigate("/drafts")}>
                    {t("gen.url.open_drafts")}
                  </button>
                )}
              </div>
            )}

            {approveResult && (
              <div style={{ display: "flex", gap: 8, alignItems: "center", width: "100%", marginTop: 6 }}>
                <div className={`alert ${approveResult.ok ? "alert-success" : "alert-error"}`} style={{ margin: 0, fontSize: 12 }}>
                  {approveResult.ok
                    ? `✓ ${(approveResult.saved || []).length} ${t("gen.url.added_to_catalog")}`
                    : `✗ ${approveResult.error}`}
                </div>
                {approveResult.ok && (
                  <button className="btn btn-secondary btn-sm" style={{ fontSize: 11 }} onClick={() => navigate("/catalog")}>
                    {t("gen.url.open_catalog")}
                  </button>
                )}
              </div>
            )}
          </div>

          {/* Draft cards */}
          <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
            {drafts.map(d => {
              const isSel = selDrafts.has(d.test_name);
              const isExp = expanded === d.test_name;
              return (
                <div
                  key={d.test_name}
                  className="card"
                  style={{
                    borderColor: isSel ? "var(--accent)" : undefined,
                    background:  isSel ? "var(--accent-light)" : undefined,
                  }}
                >
                  <div style={{ display: "flex", alignItems: "flex-start", gap: 12 }}>
                    <input
                      type="checkbox"
                      checked={isSel}
                      onChange={() => toggleDraft(d.test_name)}
                      style={{ marginTop: 3, flexShrink: 0 }}
                    />
                    <div style={{ flex: 1 }}>
                      <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
                        <span style={{ fontWeight: 700, fontSize: 14, fontFamily: "monospace", color: "var(--text)" }}>
                          {d.test_name}
                        </span>
                        <PriorityBadge p={d.priority} />
                        <span className="badge badge-gray">{t("gen.url.badge_draft")}</span>
                      </div>
                      <div style={{ display: "flex", gap: 16, fontSize: 11, color: "var(--text-3)", alignItems: "center" }}>
                        <span>{d.steps?.length ?? 0} {t("gen.url.steps")}</span>
                        <button
                          style={{ background: "none", border: "none", color: "var(--accent)", cursor: "pointer", fontSize: 11, padding: 0, fontWeight: 600 }}
                          onClick={() => setExpanded(isExp ? null : d.test_name)}
                        >
                          {isExp ? t("gen.url.hide_steps") : t("gen.url.show_steps")}
                        </button>
                      </div>
                      {isExp && d.steps?.length > 0 && (
                        <div style={{ marginTop: 10 }}>
                          <table className="data-table" style={{ fontSize: 11 }}>
                            <thead>
                              <tr>
                                <th>#</th>
                                <th>{t("gen.url.col_action")}</th>
                                <th>{t("gen.url.col_selector")}</th>
                                <th>{t("gen.url.col_value")}</th>
                              </tr>
                            </thead>
                            <tbody>
                              {d.steps.map((st, si) => (
                                <tr key={si}>
                                  <td style={{ color: "var(--text-3)" }}>{si + 1}</td>
                                  <td style={{ fontFamily: "monospace", fontWeight: 600 }}>{st.action}</td>
                                  <td style={{ color: "var(--text-2)", fontFamily: "monospace" }}>{st.selector || st.url || "—"}</td>
                                  <td style={{ color: "var(--text-2)", fontFamily: "monospace" }}>{st.value || "—"}</td>
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
        </>
      )}

      {/* ── Empty state ──────────────────────────────────────────────────── */}
      {!exploring && pages.length === 0 && !exploreErr && (
        <div className="card" style={{ padding: "48px 32px", textAlign: "center" }}>
          <div style={{ fontSize: 32, marginBottom: 12 }}>⊞</div>
          <div style={{ fontSize: 15, fontWeight: 700, color: "var(--text)", marginBottom: 8 }}>{t("gen.url.empty_title")}</div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.8, maxWidth: 380, margin: "0 auto" }}>
            {t("gen.url.empty_desc")}
          </div>
        </div>
      )}
    </div>
  );
}

// ── Main page ─────────────────────────────────────────────────────────────────

export default function GeneratePage() {
  const { t } = useLang();
  const [tab, setTab] = useState(0);

  return (
    <div>
      {/* ── Header + source cards ─────────────────────────────────────────── */}
      <div style={{ padding: "24px 24px 0", background: "var(--bg)" }}>
        <h1 style={{ fontSize: 20, fontWeight: 800, margin: 0, color: "var(--text-1)" }}>
          {t("gen.title")}
        </h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", margin: "4px 0 16px" }}>
          {t("gen.subtitle")}
        </p>

        {/* Source cards */}
        <div style={{ display: "flex", gap: 10, flexWrap: "wrap", marginBottom: 16 }}>
          {SOURCE_CARDS.map(card => (
            <SourceCard
              key={card.tab}
              card={card}
              active={tab === card.tab}
              onClick={() => setTab(card.tab)}
              t={t}
            />
          ))}
        </div>

        {/* Tab bar */}
        <div style={{ display: "flex", gap: 4, flexWrap: "wrap" }}>
          {TABS.map((tb, i) => (
            <button
              key={tb.key}
              onClick={() => setTab(i)}
              style={{
                padding: "7px 16px",
                borderRadius: 8,
                border: "none",
                cursor: "pointer",
                fontSize: 13,
                fontWeight: tab === i ? 700 : 500,
                background: tab === i ? "var(--accent)" : "var(--surface)",
                color: tab === i ? "#fff" : "var(--text-2)",
                boxShadow: tab === i ? "0 2px 8px rgba(79,107,255,0.25)" : "none",
                transition: "all 0.15s",
                display: "flex",
                alignItems: "center",
                gap: 6,
              }}
            >
              <span style={{ fontSize: 11 }}>{tb.icon}</span>
              {t(tb.key)}
            </button>
          ))}
        </div>

        <div style={{ height: 1, background: "var(--border)", margin: "12px -24px 0" }} />
      </div>

      {/* ── Tab content ──────────────────────────────────────────────────── */}
      {tab === 0 && <PlannerPage embedded />}
      {tab === 1 && <FromUrlPanel />}
      {tab === 2 && <PRAnalysisPage />}
      {tab === 3 && <ApiTestingPage />}
    </div>
  );
}
