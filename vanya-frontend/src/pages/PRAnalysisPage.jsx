// src/pages/PRAnalysisPage.jsx
/**
 * PR Impact Analysis — fetch PR data from GitHub, analyze impact, enqueue tests.
 * POST /projects/{id}/github/status   — read GitHub connector status (configure in Integrations)
 * POST /pr-analysis/analyze      — analyze impact and match catalog tests
 * POST /execution/run-batch      — enqueue matched test IDs via Execution Center
 */
import React, { useCallback, useEffect, useRef, useState } from "react";
import { useNavigate, useSearchParams } from "react-router-dom";
import {
  analyzePR,
  analyzeProjectPR,
  analyzeProjectGitHubPR,
  getProjectGitHubStatus,
  getProjectAzureDevOpsStatus,
  listProjectAzureDevOpsPRs,
  analyzeProjectAzureDevOpsPR,
  getProjectKnowledge,
  listProjectGitHubPRs,
  runBatch,
  fetchGithubPR,
  batchSaveDrafts,
  suggestModules,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import PRAnalysisEnterpriseView from "../components/pr/PRAnalysisEnterpriseView";
import PREmptyState from "../components/pr/PREmptyState";
import PrRiskFlowGuide from "../components/platform/PrRiskFlowGuide.jsx";
import { parseChangedFilesList } from "../utils/prAnalysisViewUtils";
import {
  isAzureDrilldownProvider,
  isGithubDrilldownProvider,
  parsePrAnalysisDrilldown,
  resolvePrAnalysisDrilldown,
} from "../utils/prAnalysisDrilldownUtils.js";

export default function PRAnalysisPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const drilldownTarget = parsePrAnalysisDrilldown(searchParams);
  const drilldownResolvedRef = useRef(false);
  const [drilldownHighlightPr, setDrilldownHighlightPr] = useState(null);
  const [drilldownNotFound, setDrilldownNotFound] = useState(false);
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
  const [v1Result, setV1Result]           = useState(null);
  const [error, setError]                 = useState("");

  const [enqueueing, setEnqueueing]       = useState(false);
  const [enqueueResult, setEnqueueResult] = useState(null);

  const [savingDrafts, setSavingDrafts]   = useState(false);
  const [saveDraftsResult, setSaveDraftsResult] = useState(null); // { count } | { error }

  // Risk Selection bridge
  const [sendingRisk, setSendingRisk] = useState(false);

  // GitHub — read-only status from project connector (configured in Integrations)
  const [ghStatus, setGhStatus] = useState(null);
  const [ghLoading, setGhLoading] = useState(false);
  const [ghError, setGhError] = useState("");
  const [ghPRs, setGhPRs] = useState([]);
  const [ghPRsLoading, setGhPRsLoading] = useState(false);
  const [ghAnalyzingPR, setGhAnalyzingPR] = useState(null);
  const [ghAnalyzePayload, setGhAnalyzePayload] = useState(null);
  const [analyzedAt, setAnalyzedAt] = useState(null);
  const [hasKnowledge, setHasKnowledge] = useState(false);
  const [knowledgeLoading, setKnowledgeLoading] = useState(false);

  // Azure DevOps — read-only status from project connector (configured in Integrations)
  const [azStatus, setAzStatus] = useState(null);
  const [azLoading, setAzLoading] = useState(false);
  const [azError, setAzError] = useState("");
  const [azPRs, setAzPRs] = useState([]);
  const [azPRsLoading, setAzPRsLoading] = useState(false);
  const [azAnalyzingPR, setAzAnalyzingPR] = useState(null);
  const [azAnalyzePayload, setAzAnalyzePayload] = useState(null);

  const gitHubReady = Boolean(ghStatus?.connected);
  const azureReady = Boolean(azStatus?.connected);
  const changedFilesList = parseChangedFilesList(form.changed_files);

  // GitHub legacy URL fetch
  const [prUrl, setPrUrl]           = useState("");
  const [fetching, setFetching]     = useState(false);
  const [fetchError, setFetchError] = useState("");
  const [prDiff, setPrDiff]         = useState("");

  const loadGhStatus = useCallback(async () => {
    if (!currentProject?.id) {
      setGhStatus(null);
      setGhPRs([]);
      return;
    }
    setGhLoading(true);
    setGhError("");
    try {
      const st = await getProjectGitHubStatus(currentProject.id, false);
      setGhStatus(st);
      if (st?.connected) {
        setGhPRsLoading(true);
        const prs = await listProjectGitHubPRs(currentProject.id);
        setGhPRs(prs.pull_requests || []);
      } else {
        setGhPRs([]);
      }
    } catch (e) {
      setGhError(e?.message || t("gh.error.status"));
      setGhStatus(null);
      setGhPRs([]);
    } finally {
      setGhLoading(false);
      setGhPRsLoading(false);
    }
  }, [currentProject?.id, t]);

  const loadAzStatus = useCallback(async () => {
    if (!currentProject?.id) {
      setAzStatus(null);
      setAzPRs([]);
      return;
    }
    setAzLoading(true);
    setAzError("");
    try {
      const st = await getProjectAzureDevOpsStatus(currentProject.id, false);
      setAzStatus(st);
      if (st?.connected) {
        setAzPRsLoading(true);
        const prs = await listProjectAzureDevOpsPRs(currentProject.id);
        setAzPRs(prs.pull_requests || []);
      } else {
        setAzPRs([]);
      }
    } catch (e) {
      setAzError(e?.message || t("az.error.status"));
      setAzStatus(null);
      setAzPRs([]);
    } finally {
      setAzLoading(false);
      setAzPRsLoading(false);
    }
  }, [currentProject?.id, t]);

  useEffect(() => {
    loadGhStatus();
  }, [loadGhStatus]);

  useEffect(() => {
    loadAzStatus();
  }, [loadAzStatus]);

  useEffect(() => {
    drilldownResolvedRef.current = false;
    setDrilldownHighlightPr(null);
    setDrilldownNotFound(false);
  }, [drilldownTarget?.provider, drilldownTarget?.prNumber]);

  useEffect(() => {
    if (!drilldownTarget || drilldownResolvedRef.current) return;

    const waitingGithub = isGithubDrilldownProvider(drilldownTarget.provider) && ghPRsLoading;
    const waitingAzure = isAzureDrilldownProvider(drilldownTarget.provider) && azPRsLoading;
    if (waitingGithub || waitingAzure) return;

    const resolved = resolvePrAnalysisDrilldown({
      provider: drilldownTarget.provider,
      prNumber: drilldownTarget.prNumber,
      ghPRs,
      azPRs,
    });
    drilldownResolvedRef.current = true;

    if (resolved.found && resolved.formPatch) {
      setDrilldownHighlightPr(String(drilldownTarget.prNumber));
      setDrilldownNotFound(false);
      setForm((f) => ({ ...f, ...resolved.formPatch }));
      return;
    }

    setDrilldownHighlightPr(null);
    setDrilldownNotFound(true);
  }, [drilldownTarget, ghPRs, azPRs, ghPRsLoading, azPRsLoading]);

  useEffect(() => {
    if (!currentProject?.id) {
      setHasKnowledge(false);
      setKnowledgeLoading(false);
      return;
    }
    let cancelled = false;
    setKnowledgeLoading(true);
    getProjectKnowledge(currentProject.id)
      .then((k) => { if (!cancelled) setHasKnowledge(!!k); })
      .catch((e) => { if (!cancelled) setHasKnowledge(e?.status === 404 ? false : false); })
      .finally(() => { if (!cancelled) setKnowledgeLoading(false); });
    return () => { cancelled = true; };
  }, [currentProject?.id]);

  async function handleGhAnalyzePR(prNumber) {
    if (!currentProject?.id) return;
    setGhAnalyzingPR(prNumber);
    setError("");
    setV1Result(null);
    setResult(null);
    setGhAnalyzePayload(null);
    setAzAnalyzePayload(null);
    setEnqueueResult(null);
    try {
      const res = await analyzeProjectGitHubPR(currentProject.id, prNumber);
      setV1Result(res.analysis);
      setGhAnalyzePayload(res);
      setAnalyzedAt(new Date().toISOString());
      setForm((f) => ({
        ...f,
        title: res.pull_request?.title || f.title,
        branch: res.pull_request?.branch || f.branch,
        pr_id: String(prNumber),
        changed_files: (res.pull_request?.changed_files || []).join("\n"),
      }));
    } catch (e) {
      setError(e?.message || t("gh.error.analyze"));
    } finally {
      setGhAnalyzingPR(null);
    }
  }

  async function handleAzAnalyzePR(pullRequestId) {
    if (!currentProject?.id) return;
    setAzAnalyzingPR(pullRequestId);
    setError("");
    setV1Result(null);
    setResult(null);
    setGhAnalyzePayload(null);
    setAzAnalyzePayload(null);
    setEnqueueResult(null);
    try {
      const res = await analyzeProjectAzureDevOpsPR(currentProject.id, pullRequestId);
      setV1Result(res.analysis);
      setAzAnalyzePayload(res);
      setAnalyzedAt(new Date().toISOString());
      setForm((f) => ({
        ...f,
        title: res.pull_request?.title || f.title,
        branch: res.pull_request?.branch || f.branch,
        pr_id: String(pullRequestId),
        changed_files: (res.pull_request?.changed_files || []).join("\n"),
      }));
    } catch (e) {
      setError(e?.message || t("az.error.analyze"));
    } finally {
      setAzAnalyzingPR(null);
    }
  }

  function set(key, val) { setForm(f => ({ ...f, [key]: val })); }

  async function handleFetchPR() {
    const url = prUrl.trim();
    if (!url || !gitHubReady) return;
    setFetching(true);
    setFetchError("");
    try {
      const pr = await fetchGithubPR(url, currentProject?.id ?? null);
      setForm({
        title:         pr.title        || "",
        description:   pr.description  || "",
        branch:        pr.branch       || "",
        pr_id:         pr.pr_id        || "",
        changed_files: (pr.changed_files || []).join("\n"),
      });
      setPrDiff(pr.diff || "");
      setResult(null);
      setV1Result(null);
      setEnqueueResult(null);
    } catch (e) {
      setFetchError(e?.message || t("pr.fetch.error"));
    } finally {
      setFetching(false);
    }
  }

  async function handleAnalyze() {
    const changedFiles = changedFilesList;
    const useV1 = Boolean(currentProject?.id && changedFiles.length > 0);

    if (!useV1 && !form.title.trim() && !form.description.trim()) return;
    if (useV1 && !currentProject?.id) {
      setError(t("pr.v1.need_project"));
      return;
    }
    if (useV1 && !changedFiles.length) {
      setError(t("pr.v1.need_files"));
      return;
    }

    setAnalyzing(true);
    setError("");
    setResult(null);
    setV1Result(null);
    setGhAnalyzePayload(null);
    setEnqueueResult(null);
    try {
      if (useV1) {
        const r = await analyzeProjectPR(currentProject.id, {
          changed_files: changedFiles,
          repo: undefined,
          branch: form.branch || undefined,
          pr_id: form.pr_id || undefined,
          title: form.title || undefined,
          description: form.description || undefined,
        });
        setV1Result(r);
        setAnalyzedAt(new Date().toISOString());
      } else {
        const body = {
          title: form.title || undefined,
          description: form.description || undefined,
          branch: form.branch || undefined,
          pr_id: form.pr_id || undefined,
          changed_files: changedFiles,
          diff_text: prDiff || undefined,
          generate_draft_tests: generateDrafts,
        };
        const r = await analyzePR(body);
        setResult(r);
        setAnalyzedAt(new Date().toISOString());
      }
    } catch (e) {
      setError(e?.message || "Analysis failed");
    } finally {
      setAnalyzing(false);
    }
  }

  async function handleSaveDrafts() {
    const tests = result?.suggested_new_tests || [];
    if (!tests.length || savingDrafts) return;
    setSavingDrafts(true);
    setSaveDraftsResult(null);
    try {
      const drafts = tests.map(d => ({
        name:        d.name,
        module:      d.module,
        rationale:   d.rationale || "",
        confidence:  d.confidence || "medium",
        source:      "pr_analysis",
        steps:       d.suggested_steps      || [],
        assertions:  d.suggested_assertions || [],
      }));
      const res = await batchSaveDrafts(drafts);
      setSaveDraftsResult(res);
    } catch (e) {
      setSaveDraftsResult({ error: e?.message || "Save failed" });
    } finally {
      setSavingDrafts(false);
    }
  }

  async function handleSendToRisk() {
    const inferredModules = result?.inferred_modules || [];
    const changedFiles = form.changed_files.split(/[\n,]+/).map(s => s.trim()).filter(Boolean);

    setSendingRisk(true);
    try {
      // Resolve inferred domain names to real catalog module names
      const suggestion = await suggestModules({
        inferred_modules: inferredModules,
        changed_files:    changedFiles,
      });
      const modules = suggestion.suggested_modules?.length
        ? suggestion.suggested_modules
        : inferredModules;   // fallback: pass domain names directly (substring matching will handle them)
      navigate("/risk-selection", {
        state: {
          modules,
          fromPR:     true,
          prTitle:    form.title || undefined,
          prBranch:   form.branch || undefined,
        },
      });
    } catch {
      // If suggest-modules fails, navigate with raw inferred modules as fallback
      navigate("/risk-selection", {
        state: {
          modules:  result?.inferred_modules || [],
          fromPR:   true,
          prTitle:  form.title || undefined,
        },
      });
    } finally {
      setSendingRisk(false);
    }
  }

  async function handleEnqueueV1() {
    const ids = [...new Set((v1Result?.recommended_tests || []).map((t) => t.test_case_id).filter(Boolean))];
    if (!ids.length) return;
    setEnqueueing(true);
    setEnqueueResult(null);
    try {
      const context = {
        source: "pr_analysis",
        selection_type: "pr_v1_recommended",
        selected_test_ids: ids,
        selected_modules: (v1Result?.impacted_modules || []).map((m) => m.module),
        ...(form.title && { pr_title: form.title }),
        ...(form.branch && { pr_branch: form.branch }),
      };
      const r = await runBatch({ test_case_ids: ids, context });
      setEnqueueResult({ ok: true, ...r });
    } catch (e) {
      setEnqueueResult({ ok: false, error: e?.message || "Enqueue failed" });
    } finally {
      setEnqueueing(false);
    }
  }

  async function handleEnqueue() {
    const ids = [...new Set(result?.matched_test_case_ids || [])];
    if (!ids.length) return;
    setEnqueueing(true);
    setEnqueueResult(null);
    try {
      const context = {
        source:           "pr_analysis",
        selection_type:   "pr_matched",
        selected_test_ids: ids,
        selected_modules: result?.inferred_modules || [],
        ...(form.title  && { pr_title:  form.title }),
        ...(form.branch && { pr_branch: form.branch }),
      };
      const r = await runBatch({ test_case_ids: ids, context });
      setEnqueueResult({ ok: true, ...r });
    } catch (e) {
      setEnqueueResult({ ok: false, error: e?.message || "Enqueue failed" });
    } finally {
      setEnqueueing(false);
    }
  }

  return (
    <div className="page-wrap">

      <PrRiskFlowGuide variant="pr" />

      {drilldownTarget ? (
        drilldownNotFound ? (
          <div className="alert alert-warning" style={{ marginBottom: 16, fontSize: 13, lineHeight: 1.5 }}>
            {t("incident.qa.drilldown.pr_analysis_not_found")}
          </div>
        ) : drilldownHighlightPr ? (
          <div className="alert alert-info" style={{ marginBottom: 16, fontSize: 13, lineHeight: 1.5 }}>
            {t("incident.qa.drilldown.pr_analysis_selected", {
              provider: drilldownTarget.provider,
              pr: drilldownHighlightPr,
            })}
          </div>
        ) : null
      ) : null}

      {/* GitHub — read-only integration status (configure in Integrations) */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 8 }}>{t("gh.title")}</div>
        <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12 }}>{t("gh.read_only_subtitle")}</p>
        {!currentProject?.id ? (
          <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("pr.v1.need_project")}</p>
        ) : ghLoading ? (
          <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("gh.loading")}</p>
        ) : gitHubReady ? (
          <>
            <div style={{ marginBottom: 14, display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
              <span className="badge badge-green">{t("gh.connected_badge")}</span>
              {ghStatus?.owner ? (
                <span className="badge badge-gray">{t("gh.workspace")}: {ghStatus.owner}</span>
              ) : null}
              {ghStatus?.full_name ? <span className="badge badge-gray">{ghStatus.full_name}</span> : null}
              {ghStatus?.default_branch ? (
                <span className="badge badge-gray">{t("gh.branch")}: {ghStatus.default_branch}</span>
              ) : null}
              {ghStatus?.provider === "github_app" ? (
                <span className="badge badge-gray">{t("gh.provider_app")}</span>
              ) : null}
              {ghStatus?.validation_ok ? (
                <span className="badge badge-green">{t("gh.health_ok")}</span>
              ) : ghStatus?.validation_message ? (
                <span className="badge badge-orange">{ghStatus.validation_message}</span>
              ) : null}
              {ghStatus?.needs_migration ? (
                <span className="badge badge-orange">{t("gh.needs_migration")}</span>
              ) : null}
            </div>
            <div style={{ marginTop: 16 }}>
              <div className="section-title" style={{ fontSize: 13, marginBottom: 8 }}>{t("gh.open_prs")}</div>
              {ghPRsLoading ? (
                <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("gh.loading_prs")}</p>
              ) : ghPRs.length === 0 ? (
                <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("gh.no_prs")}</p>
              ) : (
                <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                  {ghPRs.map((pr) => (
                    <div
                      key={pr.number}
                      style={{
                        display: "flex",
                        justifyContent: "space-between",
                        alignItems: "center",
                        gap: 10,
                        padding: "10px 12px",
                        border: String(drilldownHighlightPr) === String(pr.number)
                          ? "2px solid var(--accent)"
                          : "1px solid var(--border)",
                        borderRadius: 6,
                        background: String(drilldownHighlightPr) === String(pr.number)
                          ? "var(--blue-bg, rgba(59,130,246,0.08))"
                          : undefined,
                      }}
                    >
                      <div>
                        <div style={{ fontSize: 13, fontWeight: 600 }}>#{pr.number} {pr.title}</div>
                        <div style={{ fontSize: 11, color: "var(--text-3)" }}>{pr.branch} → {pr.base_branch} · {pr.author}</div>
                      </div>
                      <button type="button" className="btn btn-primary btn-sm" onClick={() => handleGhAnalyzePR(pr.number)} disabled={ghAnalyzingPR === pr.number}>
                        {ghAnalyzingPR === pr.number ? t("gh.analyzing") : t("gh.analyze_pr")}
                      </button>
                    </div>
                  ))}
                </div>
              )}
            </div>
          </>
        ) : (
          <div style={{ padding: "14px 16px", border: "1px solid var(--border)", borderRadius: 8, background: "var(--surface)" }}>
            <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.6, margin: "0 0 12px" }}>
              {t("gh.not_configured_desc")}
            </p>
            <button type="button" className="btn btn-primary btn-sm" onClick={() => navigate("/integrations")}>
              {t("gh.go_integrations")}
            </button>
          </div>
        )}
        {ghError ? <div className="alert alert-error" style={{ marginTop: 10, fontSize: 12 }}>{ghError}</div> : null}
      </div>

      {/* Azure DevOps — read-only integration status (configure in Integrations) */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title" style={{ marginBottom: 8 }}>{t("az.title")}</div>
        <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12 }}>{t("az.read_only_subtitle")}</p>
        {!currentProject?.id ? (
          <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("pr.v1.need_project")}</p>
        ) : azLoading ? (
          <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("az.loading")}</p>
        ) : azureReady ? (
          <>
            <div style={{ marginBottom: 14, display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
              <span className="badge badge-green">{t("az.connected_badge")}</span>
              {azStatus?.organization ? (
                <span className="badge badge-gray">{t("az.organization")}: {azStatus.organization}</span>
              ) : null}
              {azStatus?.full_name ? <span className="badge badge-gray">{azStatus.full_name}</span> : null}
              {azStatus?.default_branch ? (
                <span className="badge badge-gray">{t("az.branch")}: {azStatus.default_branch}</span>
              ) : null}
              {azStatus?.validation_ok ? (
                <span className="badge badge-green">{t("az.health_ok")}</span>
              ) : azStatus?.validation_message ? (
                <span className="badge badge-orange">{azStatus.validation_message}</span>
              ) : null}
            </div>
            <div style={{ marginTop: 16 }}>
              <div className="section-title" style={{ fontSize: 13, marginBottom: 8 }}>{t("az.open_prs")}</div>
              {azPRsLoading ? (
                <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("az.loading_prs")}</p>
              ) : azPRs.length === 0 ? (
                <p style={{ fontSize: 12, color: "var(--text-3)" }}>{t("az.no_prs")}</p>
              ) : (
                <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
                  {azPRs.map((pr) => (
                    <div
                      key={pr.pull_request_id}
                      style={{
                        display: "flex",
                        justifyContent: "space-between",
                        alignItems: "center",
                        gap: 10,
                        padding: "10px 12px",
                        border: String(drilldownHighlightPr) === String(pr.pull_request_id)
                          ? "2px solid var(--accent)"
                          : "1px solid var(--border)",
                        borderRadius: 6,
                        background: String(drilldownHighlightPr) === String(pr.pull_request_id)
                          ? "var(--blue-bg, rgba(59,130,246,0.08))"
                          : undefined,
                      }}
                    >
                      <div>
                        <div style={{ fontSize: 13, fontWeight: 600 }}>#{pr.pull_request_id} {pr.title}</div>
                        <div style={{ fontSize: 11, color: "var(--text-3)" }}>{pr.branch} → {pr.base_branch} · {pr.author}</div>
                      </div>
                      <button type="button" className="btn btn-primary btn-sm" onClick={() => handleAzAnalyzePR(pr.pull_request_id)} disabled={azAnalyzingPR === pr.pull_request_id}>
                        {azAnalyzingPR === pr.pull_request_id ? t("az.analyzing") : t("az.analyze_pr")}
                      </button>
                    </div>
                  ))}
                </div>
              )}
            </div>
          </>
        ) : (
          <div style={{ padding: "14px 16px", border: "1px solid var(--border)", borderRadius: 8, background: "var(--surface)" }}>
            <p style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.6, margin: "0 0 12px" }}>
              {t("az.not_configured_desc")}
            </p>
            <button type="button" className="btn btn-primary btn-sm" onClick={() => navigate("/integrations")}>
              {t("az.go_integrations")}
            </button>
          </div>
        )}
        {azError ? <div className="alert alert-error" style={{ marginTop: 10, fontSize: 12 }}>{azError}</div> : null}
      </div>

      {/* GitHub PR fetch by URL — requires active GitHub integration */}
      <div className="card" style={{ marginBottom: 20, opacity: gitHubReady ? 1 : 0.72 }}>
        <div className="section-title" style={{ marginBottom: 8 }}>{t("pr.fetch.title")}</div>
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 10 }}>
          {t("pr.fetch.desc")}
        </div>
        {!currentProject?.id ? (
          <div style={{ marginBottom: 10, fontSize: 12, color: "var(--text-3)" }}>
            {t("pr.fetch.hint_project")}
          </div>
        ) : !gitHubReady ? (
          <div className="alert" style={{ marginBottom: 10, fontSize: 12, padding: "10px 14px" }}>
            {t("gh.requires_integration")}
          </div>
        ) : null}
        <div style={{ display: "flex", gap: 10 }}>
          <input
            className="input"
            style={{ flex: 1, fontFamily: "monospace", fontSize: 12 }}
            placeholder={t("pr.fetch.placeholder")}
            value={prUrl}
            onChange={e => setPrUrl(e.target.value)}
            onKeyDown={e => e.key === "Enter" && gitHubReady && handleFetchPR()}
            disabled={!gitHubReady}
          />
          <button
            className="btn btn-secondary"
            onClick={handleFetchPR}
            disabled={fetching || !prUrl.trim() || !gitHubReady}
            title={!gitHubReady ? t("gh.requires_integration") : undefined}
          >
            {fetching ? t("pr.fetch.fetching") : t("pr.fetch.btn")}
          </button>
        </div>
        {fetchError && (
          <div className="alert alert-error" style={{ marginTop: 10, fontSize: 12 }}>
            {fetchError}
          </div>
        )}
      </div>

      {/* Diff indicator */}
      {prDiff && (
        <div style={{
          display: "flex", alignItems: "center", gap: 8,
          marginBottom: 20, padding: "8px 14px",
          background: "var(--accent-light)", borderRadius: "var(--r-sm)",
          border: "1px solid var(--border)", fontSize: 12,
        }}>
          <span style={{ color: "var(--accent)", fontWeight: 600 }}>◎</span>
          <span style={{ color: "var(--text-2)" }}>{t("pr.diff.loaded")}</span>
          <span className="badge badge-gray" style={{ fontSize: 10 }}>
            {(prDiff.length / 1000).toFixed(1)}k chars
          </span>
          {prDiff.length >= 15000 && (
            <span className="badge badge-orange" style={{ fontSize: 10 }}>
              {t("pr.diff.truncated")}
            </span>
          )}
        </div>
      )}

      {currentProject?.id ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 16 }}>
          {t("pr.v1.hint")} — <strong>{currentProject.name || currentProject.id}</strong>
        </div>
      ) : (
        <div className="alert" style={{ marginBottom: 16, fontSize: 12, padding: "10px 14px" }}>
          {t("pr.v1.need_project")}
        </div>
      )}

      {/* Form */}
      <div className="card" style={{ marginBottom: 20 }}>
        <div className="section-title">{t("pr.form.title")}</div>
        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 14, marginBottom: 14 }}>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.pr_title_label")}</label>
            <input className="input" style={{ width: "100%" }} placeholder={t("pr.form.pr_title_placeholder")} value={form.title} onChange={e => set("title", e.target.value)} />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.branch_label")}</label>
            <input className="input" style={{ width: "100%" }} placeholder={t("pr.form.branch_placeholder")} value={form.branch} onChange={e => set("branch", e.target.value)} />
          </div>
          <div>
            <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.pr_id_label")}</label>
            <input className="input" style={{ width: "100%" }} placeholder={t("pr.form.pr_id_placeholder")} value={form.pr_id} onChange={e => set("pr_id", e.target.value)} />
          </div>
        </div>
        <div style={{ marginBottom: 14 }}>
          <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.changed_files_label")}</label>
          <textarea
            className="input"
            rows={4}
            style={{ width: "100%", resize: "vertical", fontFamily: "monospace", fontSize: 12 }}
            placeholder={t("pr.form.changed_files_ph")}
            value={form.changed_files}
            onChange={(e) => set("changed_files", e.target.value)}
          />
        </div>
        <div style={{ marginBottom: 14 }}>
          <label style={{ fontSize: 11, color: "var(--text-3)", display: "block", marginBottom: 4 }}>{t("pr.form.desc_label")}</label>
          <textarea
            className="input"
            rows={3}
            style={{ width: "100%", resize: "vertical" }}
            placeholder={t("pr.form.desc_placeholder")}
            value={form.description}
            onChange={e => set("description", e.target.value)}
          />
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
          <label style={{ fontSize: 13, color: "var(--text-2)", display: "flex", alignItems: "center", gap: 6 }}>
            <input type="checkbox" checked={generateDrafts} onChange={e => setGenerateDrafts(e.target.checked)} />
            {t("pr.form.generate_drafts")}
          </label>
          <button
            className="btn btn-primary"
            onClick={handleAnalyze}
            disabled={
              analyzing
              || (
                currentProject?.id
                  ? !changedFilesList.length
                  : (!form.title.trim() && !form.description.trim())
              )
            }
          >
            {analyzing ? t("pr.form.analyzing") : (currentProject?.id ? t("pr.form.analyze_pr") : t("pr.form.analyze"))}
          </button>
        </div>
        {error && <div className="alert alert-error" style={{ marginTop: 12 }}>{error}</div>}
      </div>

      {v1Result ? (
        <PRAnalysisEnterpriseView
          mode="v1"
          v1={v1Result}
          form={form}
          ghFiles={ghAnalyzePayload?.pull_request?.files || azAnalyzePayload?.pull_request?.files || []}
          changedFilesList={changedFilesList}
          analyzedAt={analyzedAt}
          hasKnowledge={hasKnowledge}
          knowledgeLoading={knowledgeLoading}
          enqueueResult={enqueueResult}
          enqueueing={enqueueing}
          onEnqueue={handleEnqueueV1}
        />
      ) : null}

      {result ? (
        <PRAnalysisEnterpriseView
          mode="legacy"
          legacy={result}
          form={form}
          changedFilesList={changedFilesList}
          analyzedAt={analyzedAt}
          hasKnowledge={hasKnowledge}
          knowledgeLoading={knowledgeLoading}
          enqueueResult={enqueueResult}
          enqueueing={enqueueing}
          onEnqueue={handleEnqueue}
          onSendToRisk={handleSendToRisk}
          sendingRisk={sendingRisk}
          onSaveDrafts={handleSaveDrafts}
          savingDrafts={savingDrafts}
          saveDraftsResult={saveDraftsResult}
        />
      ) : null}

      {!v1Result && !result && !analyzing && !ghAnalyzingPR && !azAnalyzingPR ? (
        <PREmptyState
          gitHubReady={gitHubReady || azureReady}
          hasKnowledge={hasKnowledge}
          knowledgeLoading={knowledgeLoading}
          hasProject={Boolean(currentProject?.id)}
        />
      ) : null}
    </div>
  );
}
