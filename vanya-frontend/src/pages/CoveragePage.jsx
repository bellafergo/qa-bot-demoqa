// src/pages/CoveragePage.jsx
/**
 * Coverage & Exploration — module-level coverage metrics, discovered pages, uncovered flows.
 * GET /coverage/summary
 */
import React, { useState, useEffect, useCallback } from "react";
import { useNavigate } from "react-router-dom";
import { getCoverageSummary, generateCoverageTests } from "../api";
import { useLang } from "../i18n/LangContext";

function ScoreBar({ value, max = 100 }) {
  const pct = Math.min(100, Math.max(0, ((value ?? 0) / (max || 1)) * 100));
  const color = pct >= 80 ? "var(--green)" : pct >= 50 ? "var(--orange)" : "var(--red)";
  return (
    <div style={{ display: "flex", alignItems: "center", gap: 8, minWidth: 160 }}>
      <div style={{ flex: 1, height: 6, background: "var(--surface-2)", borderRadius: 3, overflow: "hidden" }}>
        <div style={{ height: "100%", width: `${pct}%`, background: color, borderRadius: 3, transition: "width 0.4s" }} />
      </div>
      <span style={{ fontSize: 11, fontWeight: 700, color, minWidth: 32, textAlign: "right" }}>
        {Math.round(pct)}%
      </span>
    </div>
  );
}

function CoverageGrade({ pct }) {
  const { t } = useLang();
  if (pct >= 80) return <span className="badge badge-green">{t("cov.grade.good")}</span>;
  if (pct >= 50) return <span className="badge badge-orange">{t("cov.grade.partial")}</span>;
  return <span className="badge badge-red">{t("cov.grade.low")}</span>;
}

export default function CoveragePage() {
  const { t } = useLang();
  const [summary, setSummary]   = useState(null);
  const [loading, setLoading]   = useState(true);
  const [error, setError]       = useState("");
  const [expanded, setExpanded] = useState(null); // module name
  const [suggestions, setSuggestions]       = useState(null);  // { module, gap_summary, suggested_tests }
  const [suggestingFor, setSuggestingFor]   = useState(null);  // module currently loading
  const [suggestError, setSuggestError]     = useState("");
  const navigate = useNavigate();

  async function handleGenerateTests(e, moduleName) {
    e.stopPropagation();
    setSuggestingFor(moduleName);
    setSuggestError("");
    setSuggestions(null);
    try {
      const data = await generateCoverageTests(moduleName);
      setSuggestions(data);
    } catch (err) {
      setSuggestError(err?.message || "Failed to generate suggestions");
    } finally {
      setSuggestingFor(null);
    }
  }

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const data = await getCoverageSummary();
      setSummary(data);
    } catch (e) {
      setError(e?.message || t("cov.error.load"));
    } finally {
      setLoading(false);
    }
  }, [t]);

  useEffect(() => { load(); }, [load]);

  const modules = summary?.modules ?? [];
  const overall = summary?.overall_coverage_pct ?? 0;
  const totalTests = summary?.total_test_cases ?? 0;
  const totalFlows = summary?.total_flows_discovered ?? 0;
  const uncoveredCount = summary?.uncovered_flows_count ?? 0;
  const discoveredPages = summary?.discovered_pages ?? [];
  const uncoveredFlows  = summary?.uncovered_flow_suggestions ?? [];

  const kpiItems = [
    { labelKey: "cov.kpi.overall",  value: loading ? "…" : `${Math.round(overall)}%`, accent: overall >= 80 ? "var(--green)" : overall >= 50 ? "var(--orange)" : "var(--red)" },
    { labelKey: "cov.kpi.modules",  value: loading ? "…" : modules.length },
    { labelKey: "cov.kpi.tests",    value: loading ? "…" : totalTests },
    { labelKey: "cov.kpi.flows",    value: loading ? "…" : totalFlows },
    { labelKey: "cov.kpi.uncovered",value: loading ? "…" : uncoveredCount, accent: uncoveredCount > 0 ? "var(--orange)" : undefined },
  ];

  return (
    <div className="page-wrap">

      {/* Header KPIs */}
      <div className="kpi-grid" style={{ gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))", marginBottom: 24 }}>
        {kpiItems.map(({ labelKey, value, accent }) => (
          <div key={labelKey} className="kpi-card">
            <div className="kpi-label">{t(labelKey)}</div>
            <div className="kpi-value" style={accent ? { color: accent } : {}}>{value}</div>
          </div>
        ))}
      </div>

      {error && <div className="alert alert-error" style={{ marginBottom: 16 }}>{error}</div>}

      {/* Module coverage table */}
      <div className="card" style={{ marginBottom: 24, padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between" }}>
          <div className="section-title" style={{ margin: 0 }}>{t("cov.table.title")}</div>
          <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>{t("cov.table.refresh")}</button>
        </div>

        {loading ? (
          <div style={{ padding: 24, color: "var(--text-3)", fontSize: 13 }}>{t("cov.table.loading")}</div>
        ) : modules.length === 0 ? (
          <div style={{ padding: 24, color: "var(--text-3)", fontSize: 13 }}>{t("cov.table.none")}</div>
        ) : (
          <>
            <table className="data-table">
              <thead><tr>
                <th>{t("cov.table.col.module")}</th>
                <th>{t("cov.table.col.coverage")}</th>
                <th>{t("cov.table.col.grade")}</th>
                <th>{t("cov.table.col.tests")}</th>
                <th>{t("cov.table.col.flows")}</th>
                <th>{t("cov.table.col.gaps")}</th>
              </tr></thead>
              <tbody>
                {modules.map(m => (
                  <React.Fragment key={m.module}>
                    <tr
                      style={{ cursor: "pointer" }}
                      onClick={() => setExpanded(expanded === m.module ? null : m.module)}
                    >
                      <td style={{ fontWeight: 600, fontSize: 13 }}>{m.module}</td>
                      <td style={{ minWidth: 180 }}>
                        <ScoreBar value={m.coverage_pct} />
                      </td>
                      <td><CoverageGrade pct={m.coverage_pct ?? 0} /></td>
                      <td style={{ fontSize: 12, color: "var(--text-2)" }}>{m.test_case_count ?? "—"}</td>
                      <td style={{ fontSize: 12, color: "var(--text-2)" }}>{m.flows_covered ?? "—"}</td>
                      <td>
                        {(m.gaps?.length ?? 0) > 0
                          ? (
                            <span
                              className="badge badge-orange"
                              style={{ cursor: "pointer" }}
                              title={t("cov.table.gaps_generate_tip")}
                              onClick={(e) => handleGenerateTests(e, m.module)}
                            >
                              {suggestingFor === m.module
                                ? "…"
                                : `${m.gaps.length} ${m.gaps.length !== 1 ? t("cov.table.gap_plural") : t("cov.table.gap_single")} ↗`
                              }
                            </span>
                          )
                          : <span className="badge badge-green">{t("cov.table.gaps_none")}</span>
                        }
                      </td>
                    </tr>
                    {expanded === m.module && (
                      <tr>
                        <td colSpan={6} style={{ background: "var(--surface-2)", padding: "12px 20px" }}>
                          <div style={{ display: "flex", gap: 32, flexWrap: "wrap" }}>
                            {m.gaps?.length > 0 && (
                              <div>
                                <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
                                  {t("cov.section.gaps")}
                                </div>
                                {m.gaps.map((g, i) => (
                                  <div key={i} style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 3 }}>• {g}</div>
                                ))}
                              </div>
                            )}
                            {m.notes && (
                              <div>
                                <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
                                  {t("cov.section.notes")}
                                </div>
                                <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>{m.notes}</div>
                              </div>
                            )}
                          </div>
                        </td>
                      </tr>
                    )}
                  </React.Fragment>
                ))}
              </tbody>
            </table>
          </>
        )}
      </div>

      {/* Suggestions panel */}
      {suggestError && (
        <div className="alert alert-error" style={{ marginBottom: 16 }}>{suggestError}</div>
      )}
      {suggestions && (
        <div className="card" style={{ marginBottom: 24 }}>
          <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 14, flexWrap: "wrap", gap: 8 }}>
            <div className="section-title" style={{ margin: 0 }}>
              {t("cov.suggestions.title")} — <span style={{ color: "var(--accent)" }}>{suggestions.module}</span>
            </div>
            <div style={{ display: "flex", gap: 8 }}>
              <button className="btn btn-secondary btn-sm" onClick={() => navigate("/drafts")}>
                {t("cov.suggestions.open_drafts")}
              </button>
              <button className="btn btn-secondary btn-sm" onClick={() => setSuggestions(null)}>✕</button>
            </div>
          </div>

          {suggestions.gap_summary?.length > 0 && (
            <div style={{ marginBottom: 14, padding: "8px 12px", background: "var(--surface-2)", borderRadius: 6, fontSize: 12, color: "var(--text-2)", lineHeight: 1.6 }}>
              {suggestions.gap_summary.map((g, i) => <div key={i}>• {g}</div>)}
            </div>
          )}

          {suggestions.suggested_tests?.length === 0 ? (
            <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("cov.suggestions.empty")}</div>
          ) : (
            <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
              {suggestions.suggested_tests.map((s, i) => (
                <div key={i} style={{ borderLeft: "3px solid var(--accent)", paddingLeft: 12, paddingTop: 4, paddingBottom: 4 }}>
                  <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 4, flexWrap: "wrap" }}>
                    <span style={{ fontWeight: 600, fontSize: 13 }}>{s.name}</span>
                    <span className="badge badge-gray" style={{ fontSize: 10 }}>{s.confidence}</span>
                    <span className="badge badge-gray" style={{ fontSize: 10 }}>
                      {s.suggested_steps?.length ?? 0} {t("cov.suggestions.steps")}
                    </span>
                  </div>
                  {s.rationale && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.5 }}>{s.rationale}</div>
                  )}
                </div>
              ))}
            </div>
          )}
        </div>
      )}

      <div style={{ display: "grid", gridTemplateColumns: "minmax(0,1fr) minmax(0,1fr)", gap: 24, alignItems: "start" }}>

        {/* Discovered pages */}
        {discoveredPages.length > 0 && (
          <div className="card">
            <div className="section-title">{t("cov.section.discovered")}</div>
            <div style={{ display: "flex", flexDirection: "column", gap: 6 }}>
              {discoveredPages.map((p, i) => (
                <div key={i} style={{ display: "flex", alignItems: "center", gap: 8 }}>
                  <span style={{ fontSize: 11, color: "var(--green)", fontWeight: 700 }}>✓</span>
                  <span style={{ fontFamily: "monospace", fontSize: 12, color: "var(--text-2)", wordBreak: "break-all" }}>
                    {typeof p === "string" ? p : p.url || p.path || JSON.stringify(p)}
                  </span>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Uncovered flow suggestions */}
        {uncoveredFlows.length > 0 && (
          <div className="card">
            <div className="section-title">{t("cov.section.uncovered")}</div>
            <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
              {uncoveredFlows.map((f, i) => (
                <div key={i} style={{ borderLeft: "3px solid var(--accent)", paddingLeft: 12 }}>
                  <div style={{ fontWeight: 600, fontSize: 13, marginBottom: 2 }}>
                    {typeof f === "string" ? f : f.name || f.flow || f.description || JSON.stringify(f)}
                  </div>
                  {typeof f === "object" && f.rationale && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>{f.rationale}</div>
                  )}
                  {typeof f === "object" && f.module && (
                    <span className="badge badge-gray" style={{ marginTop: 4, display: "inline-block" }}>{f.module}</span>
                  )}
                </div>
              ))}
            </div>
          </div>
        )}
      </div>

      {/* Empty state */}
      {!loading && !error && modules.length === 0 && discoveredPages.length === 0 && uncoveredFlows.length === 0 && (
        <div className="card" style={{ textAlign: "center", padding: "40px 20px", color: "var(--text-3)" }}>
          <div style={{ fontSize: 32, marginBottom: 12 }}>◎</div>
          <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 6 }}>{t("cov.empty.title")}</div>
          <div style={{ fontSize: 13 }}>{t("cov.empty.desc")}</div>
        </div>
      )}
    </div>
  );
}
