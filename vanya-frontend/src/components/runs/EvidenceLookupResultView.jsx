import React from "react";
import { useLang } from "../../i18n/LangContext";
import {
  collectHealingRows,
  getStepsCount,
  getScreenshotSrc,
  statusBadgeClass,
  statusBadgeText,
  inferRunType,
  inferBackend,
  fmtDate,
  formatHealingConfidence,
  healingConfidenceColor,
} from "../../utils/runHelpers.js";
import RunTypeBadge from "./RunTypeBadge.jsx";
import BackendBadge from "./BackendBadge.jsx";
import CorrelationIdChip from "./CorrelationIdChip.jsx";
import DebugAccordion from "./DebugAccordion.jsx";
import FailureAnalysisPanel from "./FailureAnalysisPanel.jsx";
import DesktopContextCard from "./DesktopContextCard.jsx";
import RunExecutionStepsTable from "./RunExecutionStepsTable.jsx";

export default function EvidenceLookupResultView({ run }) {
  const { t } = useLang();
  const healedRows = collectHealingRows(run);
  const hasHealing = healedRows.length > 0;
  const healedIndexSet = new Set(
    healedRows.map((r) => r.stepIndex).filter((i) => typeof i === "number")
  );

  const passedSteps = run?.steps?.filter(s => String(s.status || "").toLowerCase().includes("pass")).length ?? 0;
  const failedSteps = run?.steps?.filter(s => {
    const s2 = String(s.status || "").toLowerCase();
    return s2.includes("fail") || s2.includes("error");
  }).length ?? 0;
  const totalSteps = run?.steps?.length ?? 0;
  const stepsCount = getStepsCount(run);
  const screenshotSrc = getScreenshotSrc(run);

  return (
    <>
          <div style={{ display: "flex", alignItems: "center", gap: 12, marginBottom: 20, flexWrap: "wrap" }}>
            <h2 style={{ margin: 0, fontSize: 18, fontWeight: 600, color: "var(--text-1)" }}>{t("runs.detail.title")}</h2>
            <span className={statusBadgeClass(run.status)}>{statusBadgeText(run.status || "unknown")}</span>
            <RunTypeBadge runType={inferRunType(run)} />
            <BackendBadge backend={inferBackend(run)} />
            {run.evidence_id && <code style={{ fontSize: 12, color: "var(--text-2)" }}>{run.evidence_id}</code>}
            {stepsCount > 0 && <span className="badge badge-gray">{stepsCount} steps</span>}
            {(run.started_at || run.created_at) && (
              <span className="badge badge-gray" title="Start time">
                {fmtDate(run.started_at || run.created_at)}
              </span>
            )}
            <CorrelationIdChip value={run.correlation_id || run.meta?.correlation_id} />
            {hasHealing && (
              <span
                className="badge badge-orange"
                style={{ fontWeight: 600, letterSpacing: "0.02em" }}
                title={t("runs.detail.healing_chip_title", { count: healedRows.length })}
              >
                ⚡ {healedRows.length} {t("runs.detail.auto_healed")}
              </span>
            )}
          </div>

          {/* Trigger context — present when run originated from Risk Selection or PR Analysis */}
          {run.meta?.trigger_context && (() => {
            const ctx = run.meta.trigger_context;
            const sourceLabel = ctx.source === "pr_analysis"
              ? "◎ PR Analysis"
              : ctx.source === "risk_selection"
                ? "◈ Risk Selection"
                : ctx.source;
            return (
              <div className="card" style={{ marginBottom: 16, padding: "10px 16px" }}>
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center" }}>
                  <span style={{ fontSize: 11, fontWeight: 500, color: "var(--text-3)", textTransform: "uppercase", letterSpacing: "0.06em", marginRight: 4 }}>
                    {t("runs.trigger.label")}
                  </span>
                  <span className="badge badge-blue" style={{ fontSize: 11 }}>{sourceLabel}</span>
                  {ctx.selection_type && (
                    <span className="badge badge-gray" style={{ fontSize: 10 }}>{ctx.selection_type}</span>
                  )}
                  {ctx.pr_title && (
                    <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-1)" }}>{ctx.pr_title}</span>
                  )}
                  {ctx.pr_branch && (
                    <span className="badge badge-gray" style={{ fontSize: 10 }}>⎇ {ctx.pr_branch}</span>
                  )}
                  {ctx.selected_modules?.map(m => (
                    <span key={m} className="badge badge-gray" style={{ fontSize: 10 }}>{m}</span>
                  ))}
                  {ctx.selected_test_ids?.length > 0 && (
                    <span style={{ fontSize: 11, color: "var(--text-3)" }}>
                      {ctx.selected_test_ids.length} {t("runs.trigger.tests_label")}
                    </span>
                  )}
                </div>
              </div>
            );
          })()}

          <div className="kpi-grid">
            {run.duration_ms != null && (
              <div className="kpi-card">
                <div className="kpi-label">{t("runs.detail.duration")}</div>
                <div className="kpi-value">{run.duration_ms}<span style={{ fontSize: 14, fontWeight: 600, color: "var(--text-2)" }}>ms</span></div>
              </div>
            )}
            {totalSteps > 0 && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.total_steps")}</div><div className="kpi-value">{totalSteps}</div></div>}
            {totalSteps > 0 && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.passed")}</div><div className="kpi-value" style={{ color: "var(--green)" }}>{passedSteps}</div></div>}
            {totalSteps > 0 && failedSteps > 0 && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.failed")}</div><div className="kpi-value" style={{ color: "var(--red)" }}>{failedSteps}</div></div>}
            {hasHealing && (
              <div
                className="kpi-card"
                style={{
                  borderLeft: "3px solid var(--orange-border)",
                  background: "var(--orange-bg)",
                }}
                title={t("runs.detail.healing_chip_title", { count: healedRows.length })}
              >
                <div className="kpi-label">{t("runs.detail.healing_kpi")}</div>
                <div className="kpi-value" style={{ color: "var(--orange-text)" }}>{healedRows.length}</div>
              </div>
            )}
            {run.expected && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.expected")}</div><div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginTop: 4 }}>{run.expected}</div></div>}
            {run.outcome && <div className="kpi-card"><div className="kpi-label">{t("runs.detail.outcome")}</div><div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginTop: 4 }}>{run.outcome}</div></div>}
          </div>

          {/* Evidence links — support both canonical (artifacts.*) and legacy (flat) fields */}
          {(() => {
            const arts = run?.artifacts || {};
            const evUrl = arts.evidence_url || run.evidence_url;
            const rpUrl = arts.report_url   || run.report_url;
            const baseUrl = run.meta?.base_url;
            if (!baseUrl && !evUrl && !rpUrl) return null;
            return (
              <div className="card" style={{ marginBottom: 20 }}>
                <div style={{ display: "flex", gap: 20, flexWrap: "wrap", alignItems: "center" }}>
                  {baseUrl && (
                    <div>
                      <div className="section-title" style={{ marginBottom: 2 }}>{t("runs.detail.base_url")}</div>
                      <div style={{ fontSize: 13, color: "var(--text-2)", wordBreak: "break-all" }}>{baseUrl}</div>
                    </div>
                  )}
                  <div style={{ marginLeft: "auto", display: "flex", gap: 10, flexShrink: 0 }}>
                    {evUrl && <a href={evUrl} target="_blank" rel="noreferrer" className="btn btn-secondary btn-sm">{t("runs.detail.view_evidence")}</a>}
                    {rpUrl && <a href={rpUrl} target="_blank" rel="noreferrer" className="btn btn-primary btn-sm">{t("runs.detail.download_report")}</a>}
                  </div>
                </div>
              </div>
            );
          })()}

          {(run.error_summary || run.reason || run.message || run.error_message) && (() => {
            const rawErr = run.error_summary ?? run.reason ?? run.message ?? run.error_message;
            const errText =
              typeof rawErr === "string"
                ? rawErr
                : typeof rawErr?.message === "string"
                  ? rawErr.message
                  : rawErr
                    ? JSON.stringify(rawErr, null, 2)
                    : "Execution failed";
            return (
              <div className="card" style={{ marginBottom: 20 }}>
                <div className="section-title">{t("runs.detail.failure_reason")}</div>
                <div style={{ fontSize: 13, color: "var(--text-1)", lineHeight: 1.6 }}>
                  {errText}
                </div>
                {(run.hint || run.meta?.hint) && (
                  <div style={{ marginTop: 8, fontSize: 12, color: "var(--text-3)", lineHeight: 1.6 }}>
                    <strong>Hint:</strong> {run.hint || run.meta?.hint}
                  </div>
                )}
              </div>
            );
          })()}

          <DebugAccordion detail={run} />

          <FailureAnalysisPanel fa={run.failure_analysis} style={{ marginBottom: 20 }} />

          <DesktopContextCard run={run} />

          {hasHealing && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
                <div style={{ display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
                  <div className="section-title" style={{ margin: 0 }}>{t("runs.detail.selector_healing")}</div>
                  <span className="badge badge-orange" style={{ fontWeight: 600 }}>⚡ {healedRows.length} {t("runs.detail.auto_healed")}</span>
                </div>
                <p style={{ margin: "10px 0 0", fontSize: 12, color: "var(--text-3)", lineHeight: 1.55, maxWidth: 720 }}>
                  {t("runs.detail.healing_subtitle")}
                </p>
              </div>
              <div style={{ overflowX: "auto" }}>
                <table className="data-table">
                  <thead><tr>
                    <th style={{ width: 44 }}>{t("runs.detail.col.step")}</th>
                    <th>{t("runs.detail.col.action")}</th>
                    <th>{t("runs.detail.col.orig_sel")}</th>
                    <th>{t("runs.detail.col.healed_sel")}</th>
                    <th style={{ minWidth: 120 }}>{t("runs.detail.col.strategy")}</th>
                    <th style={{ width: 96, textAlign: "right" }}>{t("runs.detail.col.confidence")}</th>
                  </tr></thead>
                  <tbody>
                    {healedRows.map((row, i) => {
                      const fd = formatHealingConfidence(row.confidence);
                      const cColor = healingConfidenceColor(fd.tier);
                      return (
                        <tr key={`${row.stepIndex ?? "x"}-${i}`}>
                          <td style={{ color: "var(--text-3)", fontWeight: 600 }}>
                            {row.stepIndex != null ? row.stepIndex + 1 : "—"}
                          </td>
                          <td><code style={{ fontSize: 12 }}>{row.action || "—"}</code></td>
                          <td style={{ fontSize: 12, color: "var(--red)", wordBreak: "break-all", maxWidth: 220 }}>{row.originalSelector}</td>
                          <td style={{ fontSize: 12, color: "var(--green)", wordBreak: "break-all", maxWidth: 220 }}>{row.healedSelector}</td>
                          <td><span className="badge badge-gray" style={{ fontSize: 10, fontFamily: "monospace" }}>{row.strategy}</span></td>
                          <td style={{ textAlign: "right", fontSize: 13, fontWeight: 600, color: cColor }} title={row.healingReason || (fd.detail ? `${fd.detail} (ratio)` : "")}>
                            <span style={{ display: "block" }}>{fd.label}</span>
                            {fd.detail != null && fd.tier !== "none" && (
                              <span style={{ display: "block", fontSize: 10, fontWeight: 500, color: "var(--text-3)", marginTop: 2 }}>{fd.detail}</span>
                            )}
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {run.steps?.length > 0 && (
            <RunExecutionStepsTable steps={run.steps} healedIndexSet={healedIndexSet} t={t} />
          )}

          {run.logs?.length > 0 && (
            <div className="card" style={{ marginBottom: 20, padding: 0, overflow: "hidden" }}>
              <div style={{ padding: "14px 20px", borderBottom: "1px solid var(--border)" }}>
                <div className="section-title" style={{ margin: 0 }}>{t("runs.detail.logs")}</div>
              </div>
              <pre className="code-block" style={{ margin: 0, borderRadius: 0, border: "none", maxHeight: 320, overflow: "auto" }}>
                {run.logs.join("\n")}
              </pre>
            </div>
          )}

          {screenshotSrc && (
            <div className="card">
              <div className="section-title">{t("runs.detail.screenshot")}</div>
              <img
                src={screenshotSrc}
                alt={t("runs.detail.screenshot")}
                style={{ maxWidth: "100%", borderRadius: "var(--r-sm)", border: "1px solid var(--border)", marginTop: 4 }}
              />
            </div>
          )}
    </>
  );
}
