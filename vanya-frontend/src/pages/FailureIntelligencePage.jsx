// src/pages/FailureIntelligencePage.jsx
/**
 * Failure Intelligence — Flaky Test Detection & Recurrent Regression Analysis
 *
 * Reads from:
 *   GET /failure-intelligence/flaky-tests    → FlakyTestSignal[]
 *   GET /failure-intelligence/regressions    → RegressionPattern[]
 *
 * No AI. All metrics are deterministic heuristics computed over run history.
 */
import React, { useState, useEffect, useCallback } from "react";
import { getFlakyTests, getRegressions } from "../api";
import { useLang } from "../i18n/LangContext";

// ── Helpers ───────────────────────────────────────────────────────────────────

function pct(value) {
  if (value == null) return "—";
  return `${(value * 100).toFixed(0)}%`;
}

function scoreColor(score) {
  if (score >= 0.6) return "var(--red)";
  if (score >= 0.3) return "var(--orange, #f59e0b)";
  return "var(--text-2)";
}

function flipBar(flipRate) {
  const w = Math.round((flipRate || 0) * 100);
  return (
    <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
      <div style={{ flex: 1, height: 5, background: "var(--border)", borderRadius: 3, overflow: "hidden", minWidth: 50 }}>
        <div style={{ width: `${w}%`, height: "100%", background: w >= 60 ? "var(--red)" : w >= 30 ? "var(--orange, #f59e0b)" : "var(--accent)", borderRadius: 3 }} />
      </div>
      <span style={{ fontSize: 11, color: "var(--text-2)", minWidth: 28, textAlign: "right" }}>{w}%</span>
    </div>
  );
}

// P/F/P pattern derived from pass_count / fail_count / total
function statusPattern(pass_count, fail_count, error_count) {
  const total = pass_count + fail_count + error_count;
  if (total === 0) return "—";
  const parts = [];
  if (pass_count  > 0) parts.push(<span key="p" style={{ color: "var(--green)", fontWeight: 700 }}>P×{pass_count}</span>);
  if (fail_count  > 0) parts.push(<span key="f" style={{ color: "var(--red)",   fontWeight: 700 }}>F×{fail_count}</span>);
  if (error_count > 0) parts.push(<span key="e" style={{ color: "var(--orange, #f59e0b)", fontWeight: 700 }}>E×{error_count}</span>);
  return parts.reduce((acc, el, i) => [...acc, i > 0 ? <span key={`sep-${i}`} style={{ color: "var(--text-3)", margin: "0 3px" }}>/</span> : null, el], []);
}

// ── Flaky Tests tab ───────────────────────────────────────────────────────────

function FlakyTab({ data, loading, error, t }) {
  const suspected = (data || []).filter(f => f.suspected_flaky);
  const watching  = (data || []).filter(f => !f.suspected_flaky && f.flip_rate > 0);

  if (loading) return <div style={{ padding: 24, color: "var(--text-3)", fontSize: 13 }}>{t("fi.loading")}</div>;
  if (error)   return <div style={{ padding: 16 }} className="alert alert-error">{error}</div>;

  return (
    <div>
      <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 14, fontStyle: "italic" }}>
        {t("fi.flaky.hint")}
      </div>

      {suspected.length === 0 && watching.length === 0 && (
        <div style={{ padding: "20px 0", color: "var(--text-3)", fontSize: 13 }}>{t("fi.flaky.empty")}</div>
      )}

      {suspected.length > 0 && (
        <div style={{ marginBottom: 20 }}>
          <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 8 }}>
            <span style={{ fontSize: 11, fontWeight: 800, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-3)" }}>
              {t("fi.flaky.badge.suspected")}
            </span>
            <span className="badge badge-red" style={{ fontSize: 10 }}>{suspected.length}</span>
          </div>
          <table className="data-table">
            <thead><tr>
              <th>{t("fi.col.test")}</th>
              <th style={{ width: 100 }}>{t("fi.col.flip_rate")}</th>
              <th style={{ width: 64, textAlign: "right" }}>{t("fi.col.score")}</th>
              <th style={{ width: 56, textAlign: "right" }}>{t("fi.col.passes")}</th>
              <th style={{ width: 60, textAlign: "right" }}>{t("fi.col.failures")}</th>
              <th style={{ width: 72, textAlign: "right" }}>{t("fi.col.total")}</th>
              <th>{t("fi.col.notes")}</th>
            </tr></thead>
            <tbody>
              {suspected.map(f => (
                <tr key={f.test_case_id}>
                  <td>
                    <span style={{ fontFamily: "monospace", fontSize: 11, fontWeight: 600 }}>{f.test_case_id}</span>
                    <div style={{ marginTop: 3, fontSize: 11, fontFamily: "monospace" }}>
                      {statusPattern(f.pass_count, f.fail_count, f.error_count)}
                    </div>
                  </td>
                  <td>{flipBar(f.flip_rate)}</td>
                  <td style={{ textAlign: "right" }}>
                    <span style={{ fontWeight: 700, fontSize: 12, color: scoreColor(f.flaky_score) }}>
                      {Math.round(f.flaky_score * 100)}
                    </span>
                  </td>
                  <td style={{ textAlign: "right", color: "var(--green)", fontWeight: 600 }}>{f.pass_count}</td>
                  <td style={{ textAlign: "right", color: "var(--red)",   fontWeight: 600 }}>{f.fail_count + f.error_count}</td>
                  <td style={{ textAlign: "right", color: "var(--text-2)" }}>{f.total_runs}</td>
                  <td style={{ fontSize: 11, color: "var(--text-3)", maxWidth: 240, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                    {f.notes}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {watching.length > 0 && (
        <div>
          <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 8 }}>
            <span style={{ fontSize: 11, fontWeight: 800, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-3)" }}>
              {t("fi.flaky.badge.watch")}
            </span>
            <span className="badge badge-gray" style={{ fontSize: 10 }}>{watching.length}</span>
          </div>
          <table className="data-table" style={{ fontSize: 12 }}>
            <thead><tr>
              <th>{t("fi.col.test")}</th>
              <th style={{ width: 100 }}>{t("fi.col.flip_rate")}</th>
              <th style={{ width: 64, textAlign: "right" }}>{t("fi.col.score")}</th>
              <th style={{ width: 56, textAlign: "right" }}>{t("fi.col.passes")}</th>
              <th style={{ width: 60, textAlign: "right" }}>{t("fi.col.failures")}</th>
              <th style={{ width: 72, textAlign: "right" }}>{t("fi.col.total")}</th>
            </tr></thead>
            <tbody>
              {watching.slice(0, 10).map(f => (
                <tr key={f.test_case_id}>
                  <td style={{ fontFamily: "monospace", fontSize: 11 }}>{f.test_case_id}</td>
                  <td>{flipBar(f.flip_rate)}</td>
                  <td style={{ textAlign: "right", color: scoreColor(f.flaky_score), fontWeight: 600 }}>
                    {Math.round(f.flaky_score * 100)}
                  </td>
                  <td style={{ textAlign: "right", color: "var(--green)", fontWeight: 600 }}>{f.pass_count}</td>
                  <td style={{ textAlign: "right", color: "var(--red)",   fontWeight: 600 }}>{f.fail_count + f.error_count}</td>
                  <td style={{ textAlign: "right", color: "var(--text-2)" }}>{f.total_runs}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}

// ── Regressions tab ───────────────────────────────────────────────────────────

function RegressionsTab({ data, loading, error, t }) {
  if (loading) return <div style={{ padding: 24, color: "var(--text-3)", fontSize: 13 }}>{t("fi.loading")}</div>;
  if (error)   return <div style={{ padding: 16 }} className="alert alert-error">{error}</div>;
  if (!data || data.length === 0) return (
    <div>
      <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 14, fontStyle: "italic" }}>{t("fi.reg.hint")}</div>
      <div style={{ color: "var(--text-3)", fontSize: 13 }}>{t("fi.reg.empty")}</div>
    </div>
  );

  // Derive regression_signal from repeated_failures
  function regSignal(n) {
    if (n >= 5) return { label: "high",  bg: "var(--red)",    text: "#fff" };
    if (n >= 3) return { label: "watch", bg: "var(--orange, #f59e0b)", text: "#fff" };
    return              { label: "none",  bg: "var(--border)", text: "var(--text-2)" };
  }

  return (
    <div>
      <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 14, fontStyle: "italic" }}>{t("fi.reg.hint")}</div>
      <table className="data-table">
        <thead><tr>
          <th>{t("fi.col.test")}</th>
          <th style={{ width: 130 }}>{t("fi.col.module")}</th>
          <th style={{ width: 100, textAlign: "right" }}>{t("fi.col.repeated")}</th>
          <th style={{ width: 130 }}>{t("fi.col.root_cause")}</th>
          <th style={{ width: 80, textAlign: "center" }}>Signal</th>
          <th>{t("fi.col.summary")}</th>
        </tr></thead>
        <tbody>
          {data.map(r => {
            const sig = regSignal(r.repeated_failures);
            return (
              <tr key={r.pattern_id}>
                <td style={{ fontFamily: "monospace", fontSize: 11, fontWeight: 600 }}>{r.test_case_id}</td>
                <td><span className="badge badge-gray" style={{ fontSize: 10 }}>{r.module || "—"}</span></td>
                <td style={{ textAlign: "right" }}>
                  <span style={{ color: "var(--red)", fontWeight: 700 }}>{r.repeated_failures}</span>
                </td>
                <td>
                  <span className="badge badge-gray" style={{ fontSize: 10 }}>
                    {(r.latest_root_cause || "unknown").replace(/_/g, " ")}
                  </span>
                </td>
                <td style={{ textAlign: "center" }}>
                  <span style={{
                    fontSize: 10, fontWeight: 700, padding: "2px 8px",
                    borderRadius: 4, background: sig.bg, color: sig.text,
                  }}>
                    {sig.label}
                  </span>
                </td>
                <td style={{ fontSize: 11, color: "var(--text-3)", maxWidth: 260, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                  {r.summary}
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

// ── Main page ─────────────────────────────────────────────────────────────────

export default function FailureIntelligencePage() {
  const { t } = useLang();

  const [tab, setTab]               = useState("flaky");
  const [flaky, setFlaky]           = useState(null);
  const [regressions, setRegressions] = useState(null);
  const [loadingFlaky, setLoadingFlaky]     = useState(false);
  const [loadingReg,   setLoadingReg]       = useState(false);
  const [errorFlaky, setErrorFlaky]         = useState("");
  const [errorReg,   setErrorReg]           = useState("");

  const loadFlaky = useCallback(async () => {
    setLoadingFlaky(true);
    setErrorFlaky("");
    try {
      const data = await getFlakyTests();
      setFlaky(Array.isArray(data) ? data : []);
    } catch (e) {
      setErrorFlaky(e?.message || "Failed to load flaky tests");
    } finally {
      setLoadingFlaky(false);
    }
  }, []);

  const loadReg = useCallback(async () => {
    setLoadingReg(true);
    setErrorReg("");
    try {
      const data = await getRegressions();
      setRegressions(Array.isArray(data) ? data : []);
    } catch (e) {
      setErrorReg(e?.message || "Failed to load regressions");
    } finally {
      setLoadingReg(false);
    }
  }, []);

  useEffect(() => {
    loadFlaky();
    loadReg();
  }, [loadFlaky, loadReg]);

  const suspectedCount   = (flaky || []).filter(f => f.suspected_flaky).length;
  const regressionCount  = (regressions || []).length;

  return (
    <div className="page-wrap">

      {/* Header */}
      <div style={{ marginBottom: 20 }}>
        <h2 style={{ margin: 0, fontSize: 20, fontWeight: 800, color: "var(--text)" }}>
          {t("fi.title")}
        </h2>
        <p style={{ margin: "4px 0 0", fontSize: 13, color: "var(--text-3)" }}>
          {t("fi.subtitle")}
        </p>
      </div>

      {/* Tabs */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "1px solid var(--border)" }}>
        {[
          { key: "flaky",       label: t("fi.tab.flaky"),       count: suspectedCount  },
          { key: "regressions", label: t("fi.tab.regressions"), count: regressionCount },
        ].map(({ key, label, count }) => (
          <button
            key={key}
            onClick={() => setTab(key)}
            style={{
              background: "none", border: "none", cursor: "pointer",
              padding: "8px 16px", fontSize: 13, fontWeight: tab === key ? 700 : 500,
              color: tab === key ? "var(--accent)" : "var(--text-2)",
              borderBottom: tab === key ? "2px solid var(--accent)" : "2px solid transparent",
              marginBottom: -1, transition: "color 0.15s",
              display: "flex", alignItems: "center", gap: 6,
            }}
          >
            {label}
            {count > 0 && (
              <span className={`badge ${key === "flaky" ? "badge-red" : "badge-orange"}`} style={{ fontSize: 10 }}>
                {count}
              </span>
            )}
          </button>
        ))}

        {/* Refresh */}
        <button
          className="btn btn-secondary btn-sm"
          style={{ marginLeft: "auto", marginBottom: 4, alignSelf: "center" }}
          onClick={() => { loadFlaky(); loadReg(); }}
          disabled={loadingFlaky || loadingReg}
        >
          {loadingFlaky || loadingReg ? "…" : "↻ Refresh"}
        </button>
      </div>

      {/* Tab content */}
      <div className="card" style={{ padding: "16px 20px" }}>
        {tab === "flaky" && (
          <FlakyTab data={flaky} loading={loadingFlaky} error={errorFlaky} t={t} />
        )}
        {tab === "regressions" && (
          <RegressionsTab data={regressions} loading={loadingReg} error={errorReg} t={t} />
        )}
      </div>

    </div>
  );
}
