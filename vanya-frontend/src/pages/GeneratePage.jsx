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
import { Link, useNavigate } from "react-router-dom";
import { useLang } from "../i18n/LangContext";
import { PageHeader, Card, EmptyState } from "../ui";
import {
  exploreApp,
  generateDraftsFromPages,
  approveDrafts,
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
    <Card
      role="button"
      tabIndex={0}
      onClick={onClick}
      onKeyDown={e => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          onClick?.();
        }
      }}
      padding="sm"
      interactive
      className="gen-source-card"
      style={{
        flex: "1 1 180px",
        minWidth: 150,
        borderColor: active ? "var(--accent-border)" : undefined,
        background: active ? "var(--accent-light)" : undefined,
        boxShadow: active ? "var(--shadow-2), var(--shadow-glow)" : undefined,
      }}
    >
      <div style={{
        width: 34, height: 34, borderRadius: 8,
        background: active ? "var(--zu-brand-muted)" : "rgba(255,255,255,0.04)",
        display: "flex", alignItems: "center", justifyContent: "center",
        fontSize: 16, marginBottom: 10,
        border: "1px solid var(--border-light)",
      }}>
        {card.icon}
      </div>
      <div style={{ fontSize: 13, fontWeight: 600, color: active ? "var(--accent)" : "var(--text-1)", marginBottom: 4, letterSpacing: "-0.01em" }}>
        {t(card.titleKey)}
      </div>
      <div style={{ fontSize: 11, fontWeight: 400, color: "var(--text-3)", lineHeight: 1.5 }}>
        {t(card.descKey)}
      </div>
    </Card>
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

const REASON_TO_TYPE = {
  form_detected: "functional",
  required_field_detected: "negative",
  search_button_detected: "functional",
  links_detected: "smoke",
};

function tr(t, key, vars) {
  let s = t(key);
  Object.entries(vars || {}).forEach(([k, v]) => {
    s = s.split(`{{${k}}}`).join(String(v));
  });
  return s;
}

function defaultModuleFromUrl(urlStr) {
  try {
    const seg = new URL(urlStr).pathname.split("/").filter(Boolean)[0];
    return seg || "explorer";
  } catch {
    return "explorer";
  }
}

function draftTypeLabel(reason) {
  return REASON_TO_TYPE[reason] || "smoke";
}

function PriorityBadge({ p }) {
  if (p === "high" || p === "critical") return <span className="badge badge-red">{p}</span>;
  if (p === "medium")                   return <span className="badge badge-orange">{p}</span>;
  return <span className="badge badge-gray">{p || "low"}</span>;
}

function TypeBadge({ typeLabel }) {
  return <span className="badge badge-blue">{typeLabel}</span>;
}

const EXPLORE_TIMEOUT_MS = 30_000;

function FromUrlPanel() {
  const { t } = useLang();
  const navigate = useNavigate();

  const [url, setUrl] = useState("");
  const [maxPagesStr, setMaxPagesStr] = useState("5");
  /** @type {Array<{ key: string, sourceDraft: object, name: string, module: string }>} */
  const [draftRows, setDraftRows] = useState([]);
  const [selectedKeys, setSelectedKeys] = useState(() => new Set());
  const [loadingPipeline, setLoadingPipeline] = useState(false);
  const [pipelineHint, setPipelineHint] = useState("");
  const [loadingApprove, setLoadingApprove] = useState(false);
  const [error, setError] = useState("");
  const [successResult, setSuccessResult] = useState(null);
  const [expandedKey, setExpandedKey] = useState(null);

  function parseMaxPages() {
    const raw = String(maxPagesStr).trim();
    if (raw === "") return 5;
    const n = parseInt(raw, 10);
    if (!Number.isFinite(n)) return 5;
    return Math.min(20, Math.max(1, n));
  }

  function discardAll() {
    setDraftRows([]);
    setSelectedKeys(new Set());
    setError("");
    setSuccessResult(null);
    setPipelineHint("");
    setExpandedKey(null);
    setLoadingPipeline(false);
    setLoadingApprove(false);
  }

  function toggleKey(key) {
    setSelectedKeys(prev => {
      const s = new Set(prev);
      if (s.has(key)) s.delete(key);
      else s.add(key);
      return s;
    });
  }

  function updateRow(key, patch) {
    setDraftRows(rows => rows.map(r => (r.key === key ? { ...r, ...patch } : r)));
  }

  const allSelected = draftRows.length > 0 && draftRows.every(r => selectedKeys.has(r.key));

  async function handleExplore() {
    const trimmed = url.trim();
    if (!trimmed) {
      setError(t("gen.url.err_url_required"));
      return;
    }
    const maxPages = parseMaxPages();

    setDraftRows([]);
    setSelectedKeys(new Set());
    setError("");
    setSuccessResult(null);
    setExpandedKey(null);
    setPipelineHint(tr(t, "gen.url.exploring_detail", { url: trimmed }));
    setLoadingPipeline(true);

    try {
      const explorePromise = exploreApp(trimmed, maxPages);
      const timeoutPromise = new Promise((_, rej) => {
        setTimeout(() => rej(new Error("EXPLORE_TIMEOUT")), EXPLORE_TIMEOUT_MS);
      });
      let res;
      try {
        res = await Promise.race([explorePromise, timeoutPromise]);
      } catch (e) {
        if (e?.message === "EXPLORE_TIMEOUT") {
          setError(t("gen.url.explore_failed_timeout"));
        } else {
          setError(e?.message || t("gen.url.explore_error"));
        }
        return;
      }

      const found = Array.isArray(res?.pages) ? res.pages : [];
      if (!found.length) {
        setError(t("gen.url.explore_no_pages"));
        return;
      }

      setPipelineHint(tr(t, "gen.url.pages_then_generate", { n: found.length }));

      let genRes;
      try {
        genRes = await generateDraftsFromPages(found);
      } catch (e) {
        setError(e?.message || t("gen.url.gen_error"));
        return;
      }

      const generated = Array.isArray(genRes?.drafts) ? genRes.drafts : [];
      if (!generated.length) {
        setError(t("gen.url.no_flows"));
        return;
      }

      const modDefault = defaultModuleFromUrl(trimmed);
      const rows = generated.map((d, i) => ({
        key: `d-${i}`,
        sourceDraft: d,
        name: d.test_name || `test_${i}`,
        module: modDefault,
      }));
      setDraftRows(rows);
      setSelectedKeys(new Set(rows.map(r => r.key)));
    } finally {
      setLoadingPipeline(false);
      setPipelineHint("");
    }
  }

  async function handleApprove() {
    if (selectedKeys.size === 0 || loadingApprove) return;
    const byKey = Object.fromEntries(draftRows.map(r => [r.key, r]));
    const trimmedUrl = url.trim();
    const fallbackMod = trimmedUrl ? defaultModuleFromUrl(trimmedUrl) : "discovered";

    const toApprove = [...selectedKeys].map(k => {
      const row = byKey[k];
      if (!row) return null;
      const d = { ...row.sourceDraft };
      d.test_name = row.name.trim() || row.sourceDraft.test_name;
      d.module = row.module.trim() || fallbackMod;
      return d;
    }).filter(Boolean);

    if (!toApprove.length) return;

    setLoadingApprove(true);
    setError("");
    try {
      const res = await approveDrafts(toApprove, { activate: true });
      const saved = Array.isArray(res?.saved) ? res.saved : [];
      const skipped = Array.isArray(res?.skipped) ? res.skipped : [];
      setSuccessResult({
        totalCreated: saved.length,
        totalSkipped: skipped.length,
        saved,
        skipped,
      });
      setDraftRows([]);
      setSelectedKeys(new Set());
    } catch (e) {
      setError(e?.message || t("gen.url.approve_error"));
    } finally {
      setLoadingApprove(false);
    }
  }

  const showEmpty =
    !loadingPipeline &&
    draftRows.length === 0 &&
    !error &&
    !successResult;

  return (
    <div className="page-wrap">

      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 12 }}>{t("gen.url.section_title")}</div>
        <div style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
          <input
            className="input"
            style={{ flex: 1, minWidth: 260 }}
            placeholder="https://example.com"
            value={url}
            onChange={e => setUrl(e.target.value)}
            onKeyDown={e => e.key === "Enter" && !loadingPipeline && handleExplore()}
            disabled={loadingPipeline}
          />
          <div style={{ display: "flex", alignItems: "center", gap: 6, flexShrink: 0 }}>
            <span style={{ fontSize: 12, color: "var(--text-3)", whiteSpace: "nowrap" }}>{t("gen.url.max_pages")}:</span>
            <input
              type="number"
              className="input"
              style={{ width: 72 }}
              value={maxPagesStr}
              min={1}
              max={20}
              placeholder="5"
              onChange={e => setMaxPagesStr(e.target.value)}
              disabled={loadingPipeline}
            />
          </div>
          <button
            className="btn btn-primary"
            onClick={handleExplore}
            disabled={loadingPipeline || !url.trim()}
            style={{ flexShrink: 0 }}
          >
            {loadingPipeline ? t("gen.url.exploring") : t("gen.url.explore_btn")}
          </button>
        </div>
      </div>

      {successResult && (
        <div className="card" style={{ marginBottom: 16, borderColor: "rgba(34,197,94,0.35)", background: "rgba(34,197,94,0.06)" }}>
          <div className="alert alert-success" style={{ margin: 0, fontSize: 13 }}>
            {tr(t, "gen.url.success_catalog", {
              created: successResult.totalCreated,
              skipped: successResult.totalSkipped,
            })}
          </div>
          <button
            type="button"
            className="btn btn-secondary btn-sm"
            style={{ marginTop: 10 }}
            onClick={() => navigate("/catalog")}
          >
            {t("gen.url.view_catalog_arrow")}
          </button>
        </div>
      )}

      {loadingPipeline && (
        <div className="card" style={{ marginBottom: 20, padding: "24px 20px", textAlign: "center" }}>
          <div style={{ fontSize: 14, fontWeight: 500, color: "var(--text-1)", marginBottom: 6 }}>
            {t("gen.url.exploring")}
          </div>
          <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.6 }}>
            {pipelineHint}
          </div>
        </div>
      )}

      {error && !loadingPipeline && (
        <div className="alert alert-error" style={{ marginBottom: 16 }}>{error}</div>
      )}

      {draftRows.length > 0 && (
        <>
          <div className="card" style={{
            marginBottom: 12, padding: "10px 16px",
            display: "flex", alignItems: "center", gap: 12, flexWrap: "wrap",
            background: selectedKeys.size > 0 ? "var(--accent-light)" : undefined,
            borderColor: selectedKeys.size > 0 ? "var(--accent-border)" : undefined,
          }}>
            <button
              type="button"
              className="btn btn-secondary btn-sm"
              onClick={() => (allSelected
                ? setSelectedKeys(new Set())
                : setSelectedKeys(new Set(draftRows.map(r => r.key))))}
            >
              {allSelected ? t("gen.url.deselect_all") : t("gen.url.select_all")}
            </button>
            {selectedKeys.size > 0 && (
              <span style={{ fontSize: 13, fontWeight: 600, color: "var(--accent)" }}>
                {selectedKeys.size} {t("gen.url.selected")}
              </span>
            )}
            <div style={{ marginLeft: "auto", display: "flex", gap: 8, flexWrap: "wrap" }}>
              <button
                type="button"
                className="btn btn-primary btn-sm"
                onClick={handleApprove}
                disabled={loadingApprove || selectedKeys.size === 0}
              >
                {loadingApprove
                  ? t("gen.url.approving")
                  : tr(t, "gen.url.approve_selected", { n: selectedKeys.size })}
              </button>
              <button type="button" className="btn btn-secondary btn-sm" onClick={discardAll}>
                {t("gen.url.discard_btn")}
              </button>
            </div>
          </div>

          <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
            {draftRows.map(row => {
              const d = row.sourceDraft;
              const isSel = selectedKeys.has(row.key);
              const isExp = expandedKey === row.key;
              const nSteps = d.steps?.length ?? 0;
              return (
                <div
                  key={row.key}
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
                      onChange={() => toggleKey(row.key)}
                      style={{ marginTop: 3, flexShrink: 0 }}
                    />
                    <div style={{ flex: 1, minWidth: 0 }}>
                      <div style={{ marginBottom: 8 }}>
                        <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 4 }}>{t("gen.url.field_name")}</div>
                        <input
                          className="input"
                          style={{ width: "100%", fontFamily: "monospace", fontSize: 13 }}
                          value={row.name}
                          onChange={e => updateRow(row.key, { name: e.target.value })}
                        />
                      </div>
                      <div style={{ marginBottom: 10 }}>
                        <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 4 }}>{t("gen.url.field_module")}</div>
                        <input
                          className="input"
                          style={{ width: "100%", fontSize: 13 }}
                          value={row.module}
                          onChange={e => updateRow(row.key, { module: e.target.value })}
                        />
                      </div>
                      <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap", marginBottom: 6 }}>
                        <TypeBadge typeLabel={draftTypeLabel(d.reason)} />
                        <PriorityBadge p={d.priority} />
                        <span className="badge badge-gray">
                          {nSteps} {nSteps === 1 ? t("gen.url.step_one") : t("gen.url.steps")}
                        </span>
                      </div>
                      <div style={{ display: "flex", gap: 16, fontSize: 11, color: "var(--text-3)", alignItems: "center" }}>
                        <button
                          type="button"
                          style={{ background: "none", border: "none", color: "var(--accent)", cursor: "pointer", fontSize: 11, padding: 0, fontWeight: 600 }}
                          onClick={() => setExpandedKey(isExp ? null : row.key)}
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

      {showEmpty && (
        <EmptyState
          icon="⊞"
          title={t("gen.url.empty_title")}
          description={t("gen.url.empty_desc")}
        />
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
        <PageHeader
          className="gen-page-header"
          title={t("gen.title")}
          subtitle={t("gen.subtitle")}
          actions={
            <div className="zu-action-row">
              <Link to="/projects?new=1" className="btn btn-secondary btn-lg">
                {t("projects.create_new")}
              </Link>
            </div>
          }
        />

        {/* Source cards */}
        <div style={{ display: "flex", gap: 12, flexWrap: "wrap", marginBottom: 16 }}>
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
                border: tab === i ? "1px solid var(--accent-border)" : "1px solid var(--border)",
                cursor: "pointer",
                fontSize: 13,
                fontWeight: 500,
                background: tab === i ? "var(--accent-light)" : "var(--surface)",
                color: tab === i ? "var(--accent)" : "var(--text-3)",
                boxShadow: tab === i ? "var(--shadow-1)" : "none",
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
