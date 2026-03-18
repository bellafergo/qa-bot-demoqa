// src/pages/ApiTestingPage.jsx
/**
 * API Testing — parse OpenAPI spec, generate API test drafts, approve, run.
 * POST /api-testing/parse-spec → /api-testing/generate-tests → /api-testing/approve → /api-testing/run
 */
import React, { useState } from "react";
import { parseSpec, generateApiTests, approveApiTests, runApiTests } from "../api";
import { useLang } from "../i18n/LangContext";

// Tab labels are defined as keys — resolved via t() inside the component
const TAB_KEYS = [
  { labelKey: "api.tab.parse" },
  { labelKey: "api.tab.generate" },
  { labelKey: "api.tab.approve" },
];

function MethodBadge({ method }) {
  const colors = { GET: "badge-green", POST: "badge-blue", PUT: "badge-orange", PATCH: "badge-orange", DELETE: "badge-red" };
  return <span className={`badge ${colors[method?.toUpperCase()] || "badge-gray"}`}>{method || "?"}</span>;
}

function statusClass(s) {
  const v = String(s || "").toLowerCase();
  if (v === "pass" || v === "true")  return "badge-green";
  if (v === "fail" || v === "false") return "badge-red";
  if (v === "error")                 return "badge-red";
  return "badge-gray";
}

export default function ApiTestingPage() {
  const { t } = useLang();
  const [activeTab, setActiveTab] = useState(0);

  // Step 1: Parse
  const [specText, setSpecText]       = useState("");
  const [baseUrl, setBaseUrl]         = useState("");
  const [parsing, setParsing]         = useState(false);
  const [parseError, setParseError]   = useState("");
  const [endpoints, setEndpoints]     = useState([]);
  const [selectedEps, setSelectedEps] = useState(new Set());

  // Step 2: Generate
  const [generating, setGenerating]   = useState(false);
  const [genError, setGenError]       = useState("");
  const [drafts, setDrafts]           = useState([]);
  const [selectedDrafts, setSelDrafts] = useState(new Set());

  // Step 3: Approve & Run
  const [approving, setApproving]     = useState(false);
  const [approveResult, setApproveResult] = useState(null);
  const [running, setRunning]         = useState(false);
  const [runResult, setRunResult]     = useState(null);
  const [activate, setActivate]       = useState(true);
  const [runMode, setRunMode]         = useState("draft"); // "draft" | "catalog"

  async function handleParse() {
    if (!specText.trim()) return;
    setParsing(true);
    setParseError("");
    setEndpoints([]);
    setSelectedEps(new Set());
    try {
      const body = { spec_text: specText };
      if (baseUrl.trim()) body.base_url = baseUrl.trim();
      const eps = await parseSpec(body);
      setEndpoints(Array.isArray(eps) ? eps : []);
      if (eps?.length > 0) setActiveTab(1);
    } catch (e) {
      setParseError(e?.message || "Parse failed");
    } finally {
      setParsing(false);
    }
  }

  function toggleEp(idx) {
    setSelectedEps(prev => { const s = new Set(prev); s.has(idx) ? s.delete(idx) : s.add(idx); return s; });
  }

  async function handleGenerate() {
    const selected = endpoints.filter((_, i) => selectedEps.has(i));
    if (!selected.length && !specText.trim()) return;
    setGenerating(true);
    setGenError("");
    setDrafts([]);
    setSelDrafts(new Set());
    try {
      const body = selected.length
        ? { endpoints: selected, base_url: baseUrl || undefined }
        : { spec_text: specText, base_url: baseUrl || undefined };
      const res = await generateApiTests(body);
      setDrafts(res.drafts || []);
      if (res.drafts?.length > 0) setActiveTab(2);
    } catch (e) {
      setGenError(e?.message || "Generation failed");
    } finally {
      setGenerating(false);
    }
  }

  async function handleApprove() {
    const toApprove = drafts.filter(d => selectedDrafts.has(d.draft_id));
    if (!toApprove.length) return;
    setApproving(true);
    setApproveResult(null);
    try {
      const res = await approveApiTests({ drafts: toApprove, activate });
      setApproveResult({ ok: true, ...res });
    } catch (e) {
      setApproveResult({ ok: false, error: e?.message });
    } finally {
      setApproving(false);
    }
  }

  async function handleRun() {
    setRunning(true);
    setRunResult(null);
    try {
      const body = runMode === "catalog" && approveResult?.created_test_case_ids?.length
        ? { test_case_ids: approveResult.created_test_case_ids }
        : { drafts: drafts.filter(d => selectedDrafts.has(d.draft_id)), base_url: baseUrl || undefined };
      const res = await runApiTests(body);
      setRunResult(res);
    } catch (e) {
      setRunResult({ error: e?.message });
    } finally {
      setRunning(false);
    }
  }

  return (
    <div className="page-wrap">

      {/* Tab bar */}
      <div style={{ display: "flex", gap: 4, marginBottom: 20, borderBottom: "2px solid var(--border)", paddingBottom: 0 }}>
        {TAB_KEYS.map((tab, i) => (
          <button
            key={i}
            onClick={() => setActiveTab(i)}
            style={{
              padding: "8px 16px",
              background: "none",
              border: "none",
              borderBottom: activeTab === i ? "2px solid var(--accent)" : "2px solid transparent",
              marginBottom: -2,
              fontWeight: activeTab === i ? 700 : 500,
              fontSize: 13,
              color: activeTab === i ? "var(--accent)" : "var(--text-2)",
              cursor: "pointer",
            }}
          >
            {t(tab.labelKey)}
          </button>
        ))}
      </div>

      {/* Tab 1: Parse */}
      {activeTab === 0 && (
        <div className="card">
          <div className="section-title">{t("api.parse.title")}</div>
          <div style={{ marginBottom: 12 }}>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("api.parse.base_url_label")}</label>
            <input className="input" style={{ width: "100%", maxWidth: 400 }} placeholder="https://api.example.com/v1" value={baseUrl} onChange={e => setBaseUrl(e.target.value)} />
          </div>
          <div style={{ marginBottom: 12 }}>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>
              {t("api.parse.spec_label")} <span style={{ color: "var(--red)" }}>*</span>
            </label>
            <textarea
              className="input"
              rows={12}
              style={{ width: "100%", fontFamily: "monospace", fontSize: 11, resize: "vertical" }}
              placeholder={'{\n  "openapi": "3.0.0",\n  "info": { "title": "My API", "version": "1.0.0" },\n  "paths": { ... }\n}'}
              value={specText}
              onChange={e => setSpecText(e.target.value)}
            />
          </div>
          <button className="btn btn-primary" onClick={handleParse} disabled={parsing || !specText.trim()}>
            {parsing ? t("api.parse.parsing") : t("api.parse.btn")}
          </button>
          {parseError && <div className="alert alert-error" style={{ marginTop: 12 }}>{parseError}</div>}
          {endpoints.length > 0 && (
            <div className="alert alert-success" style={{ marginTop: 12 }}>
              {t("api.parse.found_prefix")} {endpoints.length} {endpoints.length !== 1 ? t("api.parse.endpoint_plural") : t("api.parse.endpoint_single")}. {t("api.parse.found_suffix")}
            </div>
          )}
        </div>
      )}

      {/* Tab 2: Generate */}
      {activeTab === 1 && (
        <div>
          {endpoints.length === 0 ? (
            <div className="alert alert-error">{t("api.gen.no_endpoints")}</div>
          ) : (
            <>
              <div className="card" style={{ marginBottom: 16 }}>
                <div style={{ display: "flex", alignItems: "center", justifyContent: "space-between", marginBottom: 12 }}>
                  <div className="section-title" style={{ margin: 0 }}>
                    {endpoints.length} {endpoints.length !== 1 ? t("api.parse.endpoint_plural") : t("api.parse.endpoint_single")} — {t("api.gen.target_label")}
                  </div>
                  <div style={{ display: "flex", gap: 8 }}>
                    <button className="btn btn-secondary btn-sm" onClick={() => setSelectedEps(new Set(endpoints.map((_, i) => i)))}>
                      {t("api.gen.select_all")}
                    </button>
                    <button
                      className="btn btn-primary btn-sm"
                      onClick={handleGenerate}
                      disabled={generating}
                    >
                      {generating ? t("api.gen.generating") : `${t("api.gen.generate_prefix")} (${selectedEps.size || t("api.gen.all")})`}
                    </button>
                  </div>
                </div>
                <table className="data-table">
                  <thead><tr>
                    <th style={{ width: 36 }}></th>
                    <th>{t("api.gen.col.method")}</th>
                    <th>{t("api.gen.col.path")}</th>
                    <th>{t("api.gen.col.summary")}</th>
                    <th>{t("api.gen.col.tag")}</th>
                  </tr></thead>
                  <tbody>
                    {endpoints.map((ep, i) => (
                      <tr key={i} style={{ background: selectedEps.has(i) ? "var(--accent-light)" : undefined }}>
                        <td>
                          <input type="checkbox" checked={selectedEps.has(i)} onChange={() => toggleEp(i)} />
                        </td>
                        <td><MethodBadge method={ep.method} /></td>
                        <td style={{ fontFamily: "monospace", fontSize: 11 }}>{ep.path}</td>
                        <td style={{ fontSize: 12, color: "var(--text-2)" }}>{ep.summary || "—"}</td>
                        <td><span className="badge badge-gray">{ep.tag || "—"}</span></td>
                      </tr>
                    ))}
                  </tbody>
                </table>
                {genError && <div className="alert alert-error" style={{ marginTop: 12 }}>{genError}</div>}
              </div>

              {drafts.length > 0 && (
                <div className="alert alert-success">
                  {t("api.gen.success_prefix")} {drafts.length} {drafts.length !== 1 ? t("api.gen.draft_plural") : t("api.gen.draft_single")}. {t("api.gen.success_suffix")}
                </div>
              )}
            </>
          )}
        </div>
      )}

      {/* Tab 3: Approve & Run */}
      {activeTab === 2 && (
        <div>
          {drafts.length === 0 ? (
            <div className="alert alert-error">{t("api.approve.no_drafts")}</div>
          ) : (
            <>
              {/* Drafts list */}
              <div className="card" style={{ marginBottom: 16, padding: 0, overflow: "hidden" }}>
                <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between" }}>
                  <div className="section-title" style={{ margin: 0 }}>{drafts.length} {t("api.approve.drafts_title")}</div>
                  <button
                    className="btn btn-secondary btn-sm"
                    onClick={() => setSelDrafts(selectedDrafts.size === drafts.length ? new Set() : new Set(drafts.map(d => d.draft_id)))}
                  >
                    {selectedDrafts.size === drafts.length ? t("api.approve.deselect_all") : t("api.approve.select_all")}
                  </button>
                </div>
                <table className="data-table">
                  <thead><tr>
                    <th style={{ width: 36 }}></th>
                    <th>{t("api.approve.col.name")}</th>
                    <th>{t("api.approve.col.method")}</th>
                    <th>{t("api.approve.col.endpoint")}</th>
                    <th>{t("api.approve.col.steps")}</th>
                  </tr></thead>
                  <tbody>
                    {drafts.map(d => (
                      <tr key={d.draft_id} style={{ background: selectedDrafts.has(d.draft_id) ? "var(--accent-light)" : undefined }}>
                        <td>
                          <input type="checkbox" checked={selectedDrafts.has(d.draft_id)} onChange={() => setSelDrafts(prev => { const s = new Set(prev); s.has(d.draft_id) ? s.delete(d.draft_id) : s.add(d.draft_id); return s; })} />
                        </td>
                        <td style={{ fontWeight: 600, fontSize: 13 }}>{d.name}</td>
                        <td><MethodBadge method={d.method} /></td>
                        <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>{d.endpoint}</td>
                        <td style={{ fontSize: 12 }}>{d.steps?.length ?? 0}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              {/* Actions */}
              <div className="card">
                <div className="section-title">{t("api.approve.section")}</div>
                <div style={{ display: "flex", gap: 16, flexWrap: "wrap", marginBottom: 14, alignItems: "center" }}>
                  <label style={{ fontSize: 13, color: "var(--text-2)", display: "flex", alignItems: "center", gap: 6 }}>
                    <input type="checkbox" checked={activate} onChange={e => setActivate(e.target.checked)} />
                    {t("api.approve.activate")}
                  </label>
                  <button
                    className="btn btn-secondary"
                    onClick={handleApprove}
                    disabled={approving || selectedDrafts.size === 0}
                  >
                    {approving ? t("api.approve.approving") : `${t("api.approve.approve_prefix")} ${selectedDrafts.size} ${t("api.approve.approve_drafts")}`}
                  </button>
                </div>
                {approveResult && (
                  <div className={`alert ${approveResult.ok ? "alert-success" : "alert-error"}`} style={{ marginBottom: 14 }}>
                    {approveResult.ok
                      ? `${t("api.approve.ok_prefix")} ${approveResult.total_created} ${t("api.approve.ok_suffix")} ${(approveResult.created_test_case_ids || []).join(", ")}`
                      : `✗ ${approveResult.error}`
                    }
                  </div>
                )}

                <div style={{ borderTop: "1px solid var(--border)", paddingTop: 14 }}>
                  <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text)", marginBottom: 10 }}>{t("api.run.title")}</div>
                  <div style={{ display: "flex", gap: 12, marginBottom: 10, flexWrap: "wrap", alignItems: "center" }}>
                    <label style={{ fontSize: 13, display: "flex", alignItems: "center", gap: 6 }}>
                      <input type="radio" name="runMode" value="draft" checked={runMode === "draft"} onChange={() => setRunMode("draft")} />
                      {t("api.run.mode_draft")}
                    </label>
                    <label style={{ fontSize: 13, display: "flex", alignItems: "center", gap: 6 }}>
                      <input type="radio" name="runMode" value="catalog" checked={runMode === "catalog"} onChange={() => setRunMode("catalog")} />
                      {t("api.run.mode_catalog")}
                    </label>
                  </div>
                  <button
                    className="btn btn-primary"
                    onClick={handleRun}
                    disabled={running || selectedDrafts.size === 0}
                  >
                    {running ? t("api.run.running") : t("api.run.run_btn")}
                  </button>
                </div>

                {runResult && !runResult.error && runResult.results && (
                  <div style={{ marginTop: 14 }}>
                    <div style={{ fontSize: 13, fontWeight: 600, marginBottom: 8 }}>
                      {t("api.run.results_prefix")} {runResult.passed ?? 0} {t("api.run.results_passed")} / {runResult.failed ?? 0} {t("api.run.results_failed")}
                    </div>
                    <table className="data-table" style={{ fontSize: 12 }}>
                      <thead><tr>
                        <th>{t("api.run.col.name")}</th>
                        <th>{t("api.run.col.status")}</th>
                        <th>{t("api.run.col.duration")}</th>
                      </tr></thead>
                      <tbody>
                        {runResult.results.map((r, i) => (
                          <tr key={i}>
                            <td style={{ fontWeight: 600 }}>{r.name || r.draft_id}</td>
                            <td><span className={`badge ${statusClass(r.ok ? "pass" : "fail")}`}>{r.ok ? "pass" : "fail"}</span></td>
                            <td style={{ color: "var(--text-3)" }}>{r.duration_ms != null ? `${r.duration_ms}ms` : "—"}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                )}
                {runResult?.mode === "catalog" && !runResult.error && (
                  <div className="alert alert-success" style={{ marginTop: 14 }}>
                    {t("api.run.catalog_prefix")} {runResult.orchestrator_job_id?.slice(0, 14)}… {t("api.run.catalog_enqueued")} — {runResult.test_case_count} {t("api.run.catalog_tests")}
                  </div>
                )}
                {runResult?.error && (
                  <div className="alert alert-error" style={{ marginTop: 14 }}>✗ {runResult.error}</div>
                )}
              </div>
            </>
          )}
        </div>
      )}
    </div>
  );
}
