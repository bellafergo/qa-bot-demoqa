import React, { useState, useEffect, useCallback, useRef } from "react";
import { useNavigate } from "react-router-dom";
import {
  listTestRuns,
  getTestRun,
  runTest,
  analyzeRCA,
  analyzeRisk,
  getRunClusters,
  enqueueSingle,
  apiErrorMessage,
} from "../../api";
import { useLang } from "../../i18n/LangContext";
import { useProject } from "../../context/ProjectContext.jsx";
import {
  runPersistedLookupId,
  statusBadgeClass,
  statusBadgeText,
  fmtDate,
  fmtMs,
  getStepsCount,
  hasEvidenceForList,
  runStatusIsFailure,
  resolveTestCaseIdFromRun,
  inferRunType,
  inferBackend,
} from "../../utils/runHelpers.js";
import RunTypeBadge from "../../components/runs/RunTypeBadge.jsx";
import BackendBadge from "../../components/runs/BackendBadge.jsx";
import RiskBadge from "../../components/runs/RiskBadge.jsx";
import CorrelationIdChip from "../../components/runs/CorrelationIdChip.jsx";
import { formatTestDisplayNameWithMeta } from "../../utils/humanizeTestNameUtils.js";
import DebugAccordion from "../../components/runs/DebugAccordion.jsx";
import ConfirmModal from "../../components/runs/ConfirmModal.jsx";
import EvidenceCard from "../../components/runs/EvidenceCard.jsx";
import DesktopContextCard from "../../components/runs/DesktopContextCard.jsx";
import FailureAnalysisPanel from "../../components/runs/FailureAnalysisPanel.jsx";
import FailureClustersPanel from "../../components/runs/FailureClustersPanel.jsx";
import AiFixModal from "../../components/runs/AiFixModal.jsx";

export default function RunHistoryTab({ initialRunId }) {
  const { t } = useLang();
  const { currentProject } = useProject();
  const navigate = useNavigate();
  const [runs, setRuns]             = useState([]);
  const [loading, setLoading]       = useState(true);
  const [error, setError]           = useState("");
  const [selected, setSelected]     = useState(null); // evidence_id || run_id (API lookup)
  const [detail, setDetail]         = useState(null);
  const [detailLoading, setDetailLoading] = useState(false);
  const autoOpenedRef    = useRef(false);
  const selectedRowRef   = useRef(null);
  const rcaResultRef     = useRef(null);

  // RCA / risk panels
  const [rcaResult, setRcaResult]   = useState(null);
  const [rcaLoading, setRcaLoading] = useState(false);
  const [riskResult, setRiskResult] = useState(null);
  const [riskLoading, setRiskLoading] = useState(false);

  // Failure clusters
  const [clusters, setClusters]           = useState([]);
  const [clustersLoading, setClustersLoading] = useState(true);

  // Operational filters (frontend-only; no backend changes)
  const [statusFilter, setStatusFilter] = useState("all");
  const [flakyFilter, setFlakyFilter] = useState("all");
  const [quarantineFilter, setQuarantineFilter] = useState("all");
  const [retryFilter, setRetryFilter] = useState("all");

  // Human confirmation for operational actions
  const [confirmRetryOpen, setConfirmRetryOpen] = useState(false);
  const [retryBusy, setRetryBusy] = useState(false);
  const [retryError, setRetryError] = useState("");
  const [lastEnqueuedJobId, setLastEnqueuedJobId] = useState(null);

  // AI-assisted catalog fix (from failed run)
  const [aiFixModalOpen, setAiFixModalOpen] = useState(false);
  const [postAiApplyRerunTcId, setPostAiApplyRerunTcId] = useState(null);
  const [aiRerunBusy, setAiRerunBusy] = useState(false);
  const [aiRerunError, setAiRerunError] = useState("");

  const loadClusters = useCallback(async () => {
    setClustersLoading(true);
    try {
      const data = await getRunClusters(100);
      setClusters(Array.isArray(data) ? data : []);
    } catch {
      setClusters([]);
    } finally {
      setClustersLoading(false);
    }
  }, []);

  const load = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const opts = { limit: 100 };
      if (currentProject?.id) opts.project_id = currentProject.id;
      const data = await listTestRuns(opts);
      setRuns(Array.isArray(data) ? data : (data?.runs ?? []));
    } catch (e) {
      setError(apiErrorMessage(e) || t("runs.history.error"));
    } finally {
      setLoading(false);
    }
  }, [t, currentProject?.id]);

  useEffect(() => {
    load();
    loadClusters();
  }, [load, loadClusters]);

  // Auto-open a specific run when navigated from Execution Center
  useEffect(() => {
    if (initialRunId && !autoOpenedRef.current) {
      autoOpenedRef.current = true;
      openDetail(initialRunId);
    }
  }, [initialRunId]);

  // Scroll selected row into view once runs are loaded and a row is selected
  useEffect(() => {
    if (selected && selectedRowRef.current) {
      selectedRowRef.current.scrollIntoView({ block: "nearest", behavior: "smooth" });
    }
  }, [selected, runs]);

  useEffect(() => {
    if (rcaResult && rcaResultRef.current) {
      rcaResultRef.current.scrollIntoView({ behavior: "smooth", block: "nearest" });
    }
  }, [rcaResult]);

  async function openDetail(lookupId) {
    setSelected(lookupId);
    setDetail(null);
    setRcaResult(null);
    setRiskResult(null);
    setAiFixModalOpen(false);
    setPostAiApplyRerunTcId(null);
    setDetailLoading(true);
    try {
      const d = await getTestRun(lookupId);
      setDetail(d);
    } catch (e) {
      setDetail({ error: apiErrorMessage(e) });
    } finally {
      setDetailLoading(false);
    }
  }

  async function handleRCA() {
    if (!detail || detail.error) return;
    setRcaLoading(true);
    setRcaResult(null);
    try {
      const res = await analyzeRCA({ run_id: selected, run: detail });
      setRcaResult(res);
    } catch (e) {
      setRcaResult({ error: apiErrorMessage(e) });
    } finally {
      setRcaLoading(false);
    }
  }

  async function handleRisk() {
    if (!detail || detail.error) return;
    setRiskLoading(true);
    setRiskResult(null);
    try {
      const res = await analyzeRisk({ run_id: selected, run: detail, rca: rcaResult });
      setRiskResult(res);
    } catch (e) {
      setRiskResult({ error: apiErrorMessage(e) });
    } finally {
      setRiskLoading(false);
    }
  }

  async function handleAiRerunAfterFix(tcId) {
    if (!tcId) return;
    setAiRerunBusy(true);
    setAiRerunError("");
    try {
      const meta = detail?.meta || {};
      const env = meta.environment || detail?.environment || "default";
      await runTest(tcId, { headless: true, environment: env });
      setPostAiApplyRerunTcId(null);
    } catch (e) {
      setAiRerunError(apiErrorMessage(e) || t("runs.ai_fix.rerun_failed"));
    } finally {
      setAiRerunBusy(false);
    }
  }

  // Apply operational filters in the UI (frontend-only, no backend changes)
  const filteredRuns = runs.filter(r => {
    if (statusFilter !== "all" && r.status !== statusFilter) return false;

    const meta = r.meta || {};
    const flakyPresent = Boolean(meta.flaky_signal);
    const quarantineRecommended = meta.quarantine_recommended === true;
    const retryPolicyApplied = meta.retry_policy_applied === true;

    if (flakyFilter !== "all" && !flakyPresent) return false;
    if (quarantineFilter !== "all" && !quarantineRecommended) return false;
    if (retryFilter !== "all" && !retryPolicyApplied) return false;

    return true;
  });

  const filtersActive =
    statusFilter !== "all" || flakyFilter !== "all" || quarantineFilter !== "all" || retryFilter !== "all";

  return (
    <div>
      <FailureClustersPanel clusters={clusters} loading={clustersLoading} />

      <div style={{ display: "flex", flexDirection: "column", gap: 20, width: "100%" }}>

      {/* Runs list */}
      <div>
        {error && <div className="alert alert-error" style={{ marginBottom: 12 }}>{error}</div>}
        <div className="card" style={{ padding: 0, overflow: "hidden" }}>
          <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)", display: "flex", alignItems: "center", justifyContent: "space-between", flexWrap: "wrap", gap: 10 }}>
            <div style={{ display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
              <div className="section-title" style={{ margin: 0 }}>
                {loading
                  ? t("runs.history.loading")
                  : `${filteredRuns.length} run${filteredRuns.length !== 1 ? "s" : ""}`}
              </div>
              {currentProject && (
                <span className="badge badge-gray" style={{ fontSize: 11 }}>
                  {t("runs.active_project", { name: currentProject.name })}
                </span>
              )}
            </div>
            <button className="btn btn-secondary btn-sm" onClick={load} disabled={loading}>{t("runs.history.refresh")}</button>
          </div>

          {/* Filters (compact, frontend-only) */}
          <div style={{ padding: "12px 20px", display: "flex", gap: 16, flexWrap: "wrap", alignItems: "center" }}>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.status")}:</span>
              <select
                className="input"
                value={statusFilter}
                onChange={e => setStatusFilter(e.target.value)}
                style={{ minWidth: 130 }}
                aria-label={t("runs.filter.status")}
              >
                <option value="all">{t("runs.filter.status_all")}</option>
                <option value="passed">{t("runs.filter.status_passed")}</option>
                <option value="failed">{t("runs.filter.status_failed")}</option>
                <option value="error">{t("runs.filter.status_error")}</option>
                <option value="running">{t("runs.filter.status_running")}</option>
              </select>
            </div>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.flaky")}:</span>
              <select
                className="input"
                value={flakyFilter}
                onChange={e => setFlakyFilter(e.target.value)}
                style={{ minWidth: 110 }}
                aria-label={t("runs.filter.flaky")}
              >
                <option value="all">{t("runs.filter.flaky_all")}</option>
                <option value="only">{t("runs.filter.flaky_only")}</option>
              </select>
            </div>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.quarantine")}:</span>
              <select
                className="input"
                value={quarantineFilter}
                onChange={e => setQuarantineFilter(e.target.value)}
                style={{ minWidth: 130 }}
                aria-label={t("runs.filter.quarantine")}
              >
                <option value="all">{t("runs.filter.quarantine_all")}</option>
                <option value="only">{t("runs.filter.quarantine_only")}</option>
              </select>
            </div>
            <div style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
              <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-2)", whiteSpace: "nowrap" }}>{t("runs.filter.retry")}:</span>
              <select
                className="input"
                value={retryFilter}
                onChange={e => setRetryFilter(e.target.value)}
                style={{ minWidth: 120 }}
                aria-label={t("runs.filter.retry")}
              >
                <option value="all">{t("runs.filter.retry_all")}</option>
                <option value="only">{t("runs.filter.retry_only")}</option>
              </select>
            </div>
          </div>

          {!loading && runs.length === 0 ? (
            <div style={{ padding: "32px 24px", textAlign: "center" }}>
              <div style={{ fontSize: 28, marginBottom: 10 }}>◈</div>
              <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>{t("runs.empty.title")}</div>
              <div style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.7, marginBottom: 16, maxWidth: 320, margin: "0 auto 16px" }}>
                {currentProject ? t("runs.empty.project_desc") : t("runs.empty.desc")}
              </div>
              <div style={{ display: "flex", gap: 8, justifyContent: "center", flexWrap: "wrap" }}>
                <button className="btn btn-primary btn-sm" onClick={() => navigate("/catalog")}>{t("runs.empty.cta_catalog")}</button>
                <button className="btn btn-secondary btn-sm" onClick={() => navigate("/generate")}>{t("runs.empty.cta_generate")}</button>
              </div>
            </div>
          ) : loading ? (
            <div style={{ padding: "32px 24px", textAlign: "center", color: "var(--text-3)", fontSize: 13 }}>
              {t("runs.history.loading")}
            </div>
          ) : filteredRuns.length === 0 ? (
            <div style={{ padding: "32px 24px", textAlign: "center" }}>
              <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 6 }}>
                {filtersActive ? t("runs.filter.no_results") : t("runs.history.none")}
              </div>
              {filtersActive && (
                <div style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.7, margin: "0 auto 16px", maxWidth: 320 }}>
                  {t("runs.filter.adjust_desc")}
                </div>
              )}
            </div>
          ) : (
            <table className="data-table">
              <thead><tr>
                <th>{t("runs.history.col.run_id")}</th>
                <th>{t("runs.history.col.test_case")}</th>
                <th>{t("runs.history.col.status")}</th>
                <th>{t("runs.history.col.duration")}</th>
                <th>{t("runs.history.col.created")}</th>
              </tr></thead>
              <tbody>
                {filteredRuns.map((r, i) => {
                  const lookupId = runPersistedLookupId(r);
                  return (
                  <tr
                    key={lookupId || r.run_id || i}
                    ref={lookupId === selected ? selectedRowRef : null}
                    style={{
                      cursor: "pointer",
                      background: selected === lookupId ? "var(--accent-light)" : undefined,
                      borderLeft: selected === lookupId ? "3px solid var(--accent)" : "3px solid transparent",
                    }}
                    onClick={() => lookupId && openDetail(lookupId)}
                  >
                    <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>
                      {(lookupId || "").slice(0, 14)}…
                    </td>
                    <td style={{ fontWeight: 600, fontSize: 13 }}>
                      <span style={{ display: "flex", alignItems: "center", gap: 6, flexWrap: "wrap" }}>
                        {(() => {
                          const testMeta = formatTestDisplayNameWithMeta({
                            testId: r.test_id || r.test_case_id,
                            testName: r.test_name,
                          }, t);
                          return (
                            <span title={testMeta.showTechnicalId ? testMeta.technicalId : undefined}>
                              {testMeta.display || "—"}
                            </span>
                          );
                        })()}
                        <RunTypeBadge runType={inferRunType(r)} />
                        {getStepsCount(r) > 0 && (
                          <span className="badge badge-gray" style={{ fontSize: 10 }} title="Executed steps">
                            {getStepsCount(r)} steps
                          </span>
                        )}
                      </span>
                    </td>
                    <td>
                      <span style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
                        <span className={statusBadgeClass(r.status)}>{statusBadgeText(r.status)}</span>
                        {r.meta?.retry_policy_applied && (
                          <span
                            className="badge badge-blue"
                            style={{ fontSize: 10 }}
                            title="Auto retry applied"
                          >
                            Auto retry: {r.meta?.retry_count ?? 0}
                          </span>
                        )}
                        {r.meta?.flaky_signal && (
                          <span
                            className="badge badge-gray"
                            style={{ fontSize: 10 }}
                            title={
                              r.meta?.flaky_score != null
                                ? `Historically unstable (score: ${Math.round(Number(r.meta?.flaky_score) * 100)}%)`
                                : "Historically unstable"
                            }
                          >
                            Flaky signal
                          </span>
                        )}
                        {r.meta?.quarantine_recommended && (
                          <span
                            className="badge badge-orange"
                            style={{ fontSize: 10 }}
                            title={t("runs.quarantine.badge_tooltip")}
                          >
                            {t("runs.quarantine.badge")}
                          </span>
                        )}
                        {hasEvidenceForList(r) && (
                          <span className="badge badge-green" style={{ fontSize: 10 }} title="Evidence available">
                            E
                          </span>
                        )}
                      </span>
                    </td>
                    <td style={{ fontSize: 12, color: "var(--text-3)" }}>{fmtMs(r.duration_ms)}</td>
                    <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(r.started_at || r.created_at)}</td>
                  </tr>
                );
                })}
              </tbody>
            </table>
          )}
        </div>
      </div>

      {/* Detail panel */}
      {selected && (
        <div style={{ display: "flex", flexDirection: "column", gap: 16 }}>

          {/* Run summary */}
          <div className="card">
            <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 10, flexWrap: "wrap" }}>
              <div className="section-title" style={{ margin: 0 }}>{t("runs.history.run_detail")}</div>
              {initialRunId && selected === initialRunId && (
                <span
                  className="badge badge-gray"
                  style={{ fontSize: 10, letterSpacing: "0.04em", opacity: 0.75 }}
                  title={t("runs.from_execution_tip")}
                >
                  {t("runs.from_execution")}
                </span>
              )}
            </div>
            {detailLoading ? (
              <div style={{ fontSize: 13, color: "var(--text-3)" }}>{t("runs.history.loading")}</div>
            ) : detail?.error ? (
              <div className="alert alert-error">{detail.error}</div>
            ) : detail ? (
              <>
                <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-3)", marginBottom: 8, wordBreak: "break-all" }}>
                  {runPersistedLookupId(detail)}
                  {detail.run_id && detail.evidence_id && String(detail.run_id) !== String(detail.evidence_id) ? (
                    <span style={{ color: "var(--text-4)", marginLeft: 6 }} title="run_id">· {detail.run_id}</span>
                  ) : null}
                </div>
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
                  <span className={statusBadgeClass(detail.status)}>{statusBadgeText(detail.status)}</span>
                  {detail.duration_ms != null && <span className="badge badge-gray">{fmtMs(detail.duration_ms)}</span>}
                  {getStepsCount(detail) > 0 && (
                    <span className="badge badge-gray" title="Executed steps">{getStepsCount(detail)} steps</span>
                  )}
                  {(() => {
                    const testMeta = formatTestDisplayNameWithMeta({
                      testId: detail.test_id || detail.test_case_id,
                      testName: detail.test_name,
                    }, t);
                    return testMeta.display ? (
                      <span className="badge badge-gray" title={testMeta.showTechnicalId ? testMeta.technicalId : undefined}>
                        {testMeta.display}
                      </span>
                    ) : null;
                  })()}
                  {detail.started_at || detail.created_at ? (
                    <span className="badge badge-gray" title="Start time">{fmtDate(detail.started_at || detail.created_at)}</span>
                  ) : null}
                  <RunTypeBadge runType={inferRunType(detail)} />
                  <BackendBadge backend={inferBackend(detail)} />
                  <CorrelationIdChip value={detail.correlation_id || detail.meta?.correlation_id} />
              {detail.meta?.retry_policy_applied && (
                <span className="badge badge-blue" style={{ fontSize: 10 }} title="Auto retry applied">
                  Auto retry: {detail.meta?.retry_count ?? 0}
                </span>
              )}
              {detail.meta?.flaky_signal && (
                <span
                  className="badge badge-gray"
                  style={{ fontSize: 10 }}
                  title={
                    detail.meta?.flaky_score != null
                      ? `Historically unstable (score: ${Math.round(Number(detail.meta?.flaky_score) * 100)}%)`
                      : "Historically unstable"
                  }
                >
                  Flaky signal
                </span>
              )}
              {detail.meta?.quarantine_recommended && (
                <span className="badge badge-orange" style={{ fontSize: 10 }} title={t("runs.quarantine.badge_tooltip")}>
                  {t("runs.quarantine.badge_recommended")}
                </span>
              )}
                </div>
                {(detail.error_summary || detail.reason || detail.message || detail.error_message) && (
                  <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 8 }}>
                    <strong>{t("runs.history.reason")}</strong>{" "}
                    {detail.error_summary || detail.reason || detail.message || detail.error_message}
                    {(detail.hint || detail.meta?.hint) && (
                      <div style={{ marginTop: 6, color: "var(--text-3)" }}>
                        <strong>Hint:</strong> {detail.hint || detail.meta?.hint}
                      </div>
                    )}
                  </div>
                )}

                {postAiApplyRerunTcId &&
                  resolveTestCaseIdFromRun(detail) === postAiApplyRerunTcId && (
                    <div
                      className="alert alert-success"
                      style={{ marginBottom: 12, fontSize: 12, display: "flex", flexWrap: "wrap", gap: 10, alignItems: "center" }}
                    >
                      <span>{t("runs.ai_fix.apply_ok_banner")}</span>
                      <button
                        type="button"
                        className="btn btn-primary btn-sm"
                        onClick={() => handleAiRerunAfterFix(postAiApplyRerunTcId)}
                        disabled={aiRerunBusy}
                      >
                        {aiRerunBusy ? t("common.working") : t("runs.ai_fix.rerun")}
                      </button>
                      <button
                        type="button"
                        className="btn btn-secondary btn-sm"
                        onClick={() => {
                          setPostAiApplyRerunTcId(null);
                          setAiRerunError("");
                        }}
                        disabled={aiRerunBusy}
                      >
                        {t("runs.ai_fix.dismiss_banner")}
                      </button>
                    </div>
                  )}
                {aiRerunError && (
                  <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 12 }}>
                    {aiRerunError}
                  </div>
                )}

                <DebugAccordion detail={detail} />

                {/* Action Panel: recommended next steps with explicit confirmation for operations */}
                {detail && (() => {
                  const meta = detail.meta || {};
                  const statusNorm = String(detail.status || "").toLowerCase();
                  const quarantineRecommended = meta.quarantine_recommended === true;
                  const retryPolicyApplied = meta.retry_policy_applied === true;
                  const flakySignalPresent = !!meta.flaky_signal;
                  const failedOrError =
                    statusNorm === "failed" || statusNorm === "fail" || statusNorm === "error";

                  const shouldShowPanel = quarantineRecommended || retryPolicyApplied || flakySignalPresent || failedOrError;
                  if (!shouldShowPanel) return null;

                  const testCaseId = detail.test_id || detail.test_case_id;
                  const correlationId = detail.correlation_id || meta.correlation_id;
                  const evidenceUrl = detail?.evidence_url || detail?.artifacts?.evidence_url;
                  const reportUrl = detail?.report_url || detail?.artifacts?.report_url;

                  const canRetry =
                    !!testCaseId &&
                    failedOrError &&
                    (retryPolicyApplied || quarantineRecommended || flakySignalPresent);

                  return (
                    <div style={{ marginTop: 14 }}>
                      <div className="section-title" style={{ marginBottom: 10 }}>
                        {t("runs.action_panel.recommended_actions")}{" "}
                        <span style={{ color: "var(--text-3)", fontWeight: 500, fontSize: 12 }}>
                          {t("runs.action_panel.requires_confirmation")}
                        </span>
                      </div>
                      <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
                        {evidenceUrl && (
                          <a className="btn btn-secondary btn-sm" href={evidenceUrl} target="_blank" rel="noreferrer">
                            {t("runs.action_panel.open_evidence")}
                          </a>
                        )}
                        {reportUrl && (
                          <a className="btn btn-secondary btn-sm" href={reportUrl} target="_blank" rel="noreferrer">
                            {t("runs.action_panel.open_report")}
                          </a>
                        )}
                        {correlationId && (
                          <button
                            className="btn btn-secondary btn-sm"
                            onClick={async () => {
                              try {
                                await navigator.clipboard.writeText(String(correlationId));
                              } catch {
                                // No-op: clipboard may be unavailable; avoids side effects.
                              }
                            }}
                            title={t("runs.action_panel.copy_correlation_id")}
                          >
                            {t("runs.action_panel.copy_correlation_id")}
                          </button>
                        )}

                        {canRetry && (
                          <button
                            className="btn btn-primary btn-sm"
                            onClick={() => {
                              setRetryError("");
                              setConfirmRetryOpen(true);
                            }}
                            disabled={retryBusy}
                          >
                            {t("runs.action_panel.retry_test_case")}
                          </button>
                        )}

                        {quarantineRecommended && (
                          <button
                            className="btn btn-secondary btn-sm"
                            disabled
                            title={t("runs.action_panel.quarantine_recommend_disabled_title")}
                          >
                            {t("runs.action_panel.quarantine_recommend")}
                          </button>
                        )}

                        <button
                          className="btn btn-secondary btn-sm"
                          disabled
                          title={t("runs.action_panel.create_ticket_disabled_title")}
                        >
                          {t("runs.action_panel.create_ticket")}
                        </button>
                        <button
                          className="btn btn-secondary btn-sm"
                          disabled
                          title={t("runs.action_panel.assign_owner_disabled_title")}
                        >
                          {t("runs.action_panel.assign_owner")}
                        </button>
                      </div>

                      {lastEnqueuedJobId && (
                        <div style={{ marginTop: 10, fontSize: 12, color: "var(--text-2)" }}>
                          {t("runs.action_panel.retry_job_enqueued_prefix")} <span style={{ fontFamily: "monospace" }}>{lastEnqueuedJobId}</span>
                        </div>
                      )}
                    </div>
                  );
                })()}

                {detail.steps?.length > 0 && (
                  <div style={{ fontSize: 12, color: "var(--text-3)" }}>
                    {detail.steps.length} {t("runs.history.steps_label")} — {detail.steps.filter(s => String(s.status || "").toLowerCase().includes("pass")).length} {t("runs.history.passed_label")}
                  </div>
                )}

                {/* RCA / Risk / AI catalog fix */}
                <div style={{ display: "flex", gap: 8, marginTop: 14, flexWrap: "wrap", alignItems: "center" }}>
                  <button className="btn btn-secondary btn-sm" onClick={handleRCA} disabled={rcaLoading}>
                    {rcaLoading ? t("runs.history.analyzing") : t("runs.history.rca")}
                  </button>
                  <button className="btn btn-secondary btn-sm" onClick={handleRisk} disabled={riskLoading}>
                    {riskLoading ? t("runs.history.analyzing") : t("runs.history.risk")}
                  </button>
                  {runStatusIsFailure(detail.status) && resolveTestCaseIdFromRun(detail) && (
                    <button
                      type="button"
                      className="btn btn-secondary btn-sm"
                      onClick={() => setAiFixModalOpen(true)}
                    >
                      ✨ {t("runs.ai_fix.open_btn")}
                    </button>
                  )}
                </div>
              </>
            ) : null}
          </div>

          {/* Evidence */}
          {detail && !detail.error && (
            <>
              <EvidenceCard detail={detail} runType={inferRunType(detail)} />
              <div style={{ textAlign: "right", marginTop: -8 }}>
                <button
                  className="btn btn-secondary btn-sm"
                  style={{ fontSize: 11 }}
                  onClick={() => navigate("/evidence")}
                >
                  {t("runs.view_evidence")}
                </button>
              </div>
            </>
          )}

          {/* Desktop context */}
          {detail && !detail.error && <DesktopContextCard run={detail} />}

          {/* Failure Analysis */}
          <FailureAnalysisPanel fa={detail?.failure_analysis} />

          {/* RCA result */}
          {rcaResult && (
            <div className="card" ref={rcaResultRef}>
              <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.rca.title")}</div>
              {rcaResult.error ? (
                <div className="alert alert-error">{rcaResult.error}</div>
              ) : (
                <>
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
                    {rcaResult.root_cause_category && <span className="badge badge-blue">{rcaResult.root_cause_category}</span>}
                    {rcaResult.impacted_layer && <span className="badge badge-gray">{rcaResult.impacted_layer}</span>}
                    {rcaResult.confidence && <span className="badge badge-gray">confidence: {rcaResult.confidence}</span>}
                  </div>
                  {rcaResult.summary && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginBottom: 8 }}>{rcaResult.summary}</div>
                  )}
                  {rcaResult.probable_cause && (
                    <div style={{ fontSize: 12, color: "var(--text-1)", fontWeight: 600, marginBottom: 6 }}>
                      {t("runs.rca.probable_cause")} {rcaResult.probable_cause}
                    </div>
                  )}
                  {rcaResult.recommended_action && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                      <strong>{t("runs.rca.recommended")}</strong> {rcaResult.recommended_action}
                    </div>
                  )}
                </>
              )}
            </div>
          )}

          {/* Business risk result */}
          {riskResult && (
            <div className="card">
              <div className="section-title" style={{ marginBottom: 10 }}>{t("runs.risk.title")}</div>
              {riskResult.error ? (
                <div className="alert alert-error">{riskResult.error}</div>
              ) : (
                <>
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 10 }}>
                    <RiskBadge level={riskResult.risk_level} />
                    {riskResult.confidence && <span className="badge badge-gray">confidence: {riskResult.confidence}</span>}
                  </div>
                  {riskResult.affected_business_flow && riskResult.affected_business_flow !== "unknown" && (
                    <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 6 }}>
                      <span className="badge badge-orange">{riskResult.affected_business_flow}</span>
                    </div>
                  )}
                  {riskResult.impact_summary && (
                    <div style={{ fontSize: 12, color: "var(--text-2)", lineHeight: 1.6, marginBottom: 8 }}>{riskResult.impact_summary}</div>
                  )}
                  {riskResult.priority_recommendation && (
                    <div style={{ fontSize: 12, color: "var(--text-2)" }}>
                      <strong>{t("runs.risk.recommended")}</strong> {riskResult.priority_recommendation}
                    </div>
                  )}
                </>
              )}
            </div>
          )}
        </div>
      )}
      <AiFixModal
        open={aiFixModalOpen}
        onClose={() => setAiFixModalOpen(false)}
        detail={detail}
        onApplied={(tcId) => setPostAiApplyRerunTcId(tcId)}
      />


      <ConfirmModal
        open={confirmRetryOpen}
        busy={retryBusy}
        error={retryError}
        title={t("runs.action_panel.retry_confirmation_title")}
        cancelLabel={t("common.cancel")}
        confirmLabel={t("runs.action_panel.retry_confirmation_confirm")}
        description={`${t("runs.action_panel.retry_confirmation_desc_prefix")} ${
          detail && (detail.test_id || detail.test_case_id)
            ? `"${detail.test_id || detail.test_case_id}"`
            : t("common.unknown")
        }${t("runs.action_panel.retry_confirmation_desc_suffix")}`}
        onCancel={() => {
          setConfirmRetryOpen(false);
          setRetryError("");
        }}
        onConfirm={async () => {
          if (!detail) return;
          const meta = detail.meta || {};
          const testCaseId = detail.test_id || detail.test_case_id;
          if (!testCaseId) {
            setRetryError(t("runs.action_panel.retry_error_missing_id"));
            return;
          }
          setRetryBusy(true);
          setRetryError("");
          try {
            const env = meta.environment || detail.environment || "default";
            const job = await enqueueSingle({ test_case_id: testCaseId, environment: env });
            setLastEnqueuedJobId(job?.job_id || job?.id || null);
            setConfirmRetryOpen(false);
          } catch (e) {
            setRetryError(e?.message || t("runs.action_panel.retry_error_enqueue_failed"));
          } finally {
            setRetryBusy(false);
          }
        }}
      />
      </div>
    </div>
  );
}
