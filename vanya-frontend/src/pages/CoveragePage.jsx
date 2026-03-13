// src/pages/CoveragePage.jsx
/**
 * Coverage & Exploration — module-level coverage metrics, discovered pages, uncovered flows.
 * GET /coverage/summary
 */
import React, { useState, useEffect, useCallback } from "react";
import { getCoverageSummary } from "../api";

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
  if (pct >= 80) return <span className="badge badge-green">Good</span>;
  if (pct >= 50) return <span className="badge badge-orange">Partial</span>;
  return <span className="badge badge-red">Low</span>;
}

export default function CoveragePage() {
  const [summary, setSummary]   = useState(null);
  const [loading, setLoading]   = useState(true);
  const [error, setError]       = useState("");
  const [expanded, setExpanded] = useState(null); // module name

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const data = await getCoverageSummary();
      setSummary(data);
    } catch (e) {
      setError(e?.message || "Failed to load coverage data");
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => { load(); }, [load]);

  const modules = summary?.modules ?? [];
  const overall = summary?.overall_coverage_pct ?? 0;
  const totalTests = summary?.total_test_cases ?? 0;
  const totalFlows = summary?.total_flows_discovered ?? 0;
  const uncoveredCount = summary?.uncovered_flows_count ?? 0;
  const discoveredPages = summary?.discovered_pages ?? [];
  const uncoveredFlows  = summary?.uncovered_flow_suggestions ?? [];

  return (
    <div className="page-wrap">

      {/* Header KPIs */}
      <div className="kpi-grid" style={{ gridTemplateColumns: "repeat(auto-fit, minmax(140px, 1fr))", marginBottom: 24 }}>
        {[
          { label: "Overall Coverage", value: loading ? "…" : `${Math.round(overall)}%`, accent: overall >= 80 ? "var(--green)" : overall >= 50 ? "var(--orange)" : "var(--red)" },
          { label: "Modules Covered",  value: loading ? "…" : modules.length },
          { label: "Total Test Cases", value: loading ? "…" : totalTests },
          { label: "Flows Discovered", value: loading ? "…" : totalFlows },
          { label: "Uncovered Flows",  value: loading ? "…" : uncoveredCount, accent: uncoveredCount > 0 ? "var(--orange)" : undefined },
        ].map(({ label, value, accent }) => (
          <div key={label} className="kpi-card">
            <div className="kpi-label">{label}</div>
            <div className="kpi-value" style={accent ? { color: accent } : {}}>{value}</div>
          </div>
        ))}
      </div>

      {error && <div className="alert alert-error" style={{ marginBottom: 16 }}>{error}</div>}

      {/* Module coverage table */}
      <div className="card" style={{ marginBottom: 24, padding: 0, overflow: "hidden" }}>
        <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between" }}>
          <div className="section-title" style={{ margin: 0 }}>Module Coverage</div>
          <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>↻ Refresh</button>
        </div>

        {loading ? (
          <div style={{ padding: 24, color: "var(--text-3)", fontSize: 13 }}>Loading…</div>
        ) : modules.length === 0 ? (
          <div style={{ padding: 24, color: "var(--text-3)", fontSize: 13 }}>No module coverage data available.</div>
        ) : (
          <>
            <table className="data-table">
              <thead><tr>
                <th>Module</th>
                <th>Coverage</th>
                <th>Grade</th>
                <th>Test Cases</th>
                <th>Flows Covered</th>
                <th>Gaps</th>
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
                          ? <span className="badge badge-orange">{m.gaps.length} gap{m.gaps.length !== 1 ? "s" : ""}</span>
                          : <span className="badge badge-green">None</span>
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
                                  Coverage Gaps
                                </div>
                                {m.gaps.map((g, i) => (
                                  <div key={i} style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 3 }}>• {g}</div>
                                ))}
                              </div>
                            )}
                            {m.notes && (
                              <div>
                                <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.08em", marginBottom: 6 }}>
                                  Notes
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

      <div style={{ display: "grid", gridTemplateColumns: "minmax(0,1fr) minmax(0,1fr)", gap: 24, alignItems: "start" }}>

        {/* Discovered pages */}
        {discoveredPages.length > 0 && (
          <div className="card">
            <div className="section-title">Discovered Pages / Flows</div>
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
            <div className="section-title">Uncovered Flow Suggestions</div>
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
          <div style={{ fontSize: 14, fontWeight: 600, marginBottom: 6 }}>No coverage data yet</div>
          <div style={{ fontSize: 13 }}>Run some tests and explore flows to populate coverage metrics.</div>
        </div>
      )}
    </div>
  );
}
