// src/pages/IncidentInvestigatorPage.jsx
/**
 * Autonomous Incident Investigator — describe a problem, Vanya probes with Playwright
 * and returns heuristic diagnosis + evidence.
 */
import React, { useCallback, useEffect, useState } from "react";
import {
  investigateProjectIncident,
  listProjectIncidentHistory,
  getProjectIncidentReport,
  listIncidentRuns,
  getIncidentRun,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import {
  buildIncidentReportViewModel,
  correlationReasonText,
  isEvidenceCorrelationEmpty,
} from "../utils/incidentReportViewUtils.js";
import { buildStorylineViewModel } from "../utils/incidentStorylineViewUtils.js";
import { buildImpactMapViewModel } from "../utils/incidentImpactMapViewUtils.js";
import { buildDeploymentRiskViewModel } from "../utils/deploymentRiskAssessmentViewUtils.js";
import { buildTestRecommendationsViewModel } from "../utils/testRecommendationViewUtils.js";
import { buildDecisionCenterViewModel } from "../utils/qualityDecisionCenterViewUtils.js";
import { buildHistoricalLearningViewModel } from "../utils/historicalLearningViewUtils.js";
import { buildApprovalWorkflowViewModel } from "../utils/approvalWorkflowViewUtils.js";
import { buildRecommendedActionsViewModel } from "../utils/incidentRecommendedActionsViewUtils.js";
import EvidenceCorrelationDrilldownCell from "../components/incident/EvidenceCorrelationDrilldownCell.jsx";
import RecommendedActionCard from "../components/incident/RecommendedActionCard.jsx";
import RecommendedTestCard from "../components/incident/RecommendedTestCard.jsx";
import SimilarIncidentCard from "../components/incident/SimilarIncidentCard.jsx";
import ApprovalRequestCard from "../components/incident/ApprovalRequestCard.jsx";

function fmtTs(iso) {
  if (!iso) return "—";
  try {
    return new Date(iso).toLocaleString(undefined, {
      month: "short", day: "numeric", hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return "—";
  }
}

function severityBadge(sev) {
  const v = String(sev || "info").toLowerCase();
  if (v === "critical") return "badge badge-red";
  if (v === "high") return "badge badge-orange";
  if (v === "medium") return "badge badge-orange";
  if (v === "low") return "badge badge-blue";
  return "badge badge-gray";
}

function reproducedLabel(rep, t) {
  const v = String(rep || "unknown").toLowerCase();
  if (v === "true") return t("incident.reproduced.yes");
  if (v === "false") return t("incident.reproduced.no");
  return t("incident.reproduced.unknown");
}

function confidencePct(v) {
  const n = Number(v);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

function hypothesisBasisLabel(basis, t) {
  if (basis === "evidence") return t("incident.qa.basis.evidence");
  if (basis === "assumption") return t("incident.qa.basis.assumption");
  return t("incident.qa.basis.inference");
}

function strengthBadge(strength) {
  const v = String(strength || "medium").toLowerCase();
  if (v === "high") return "badge badge-red";
  if (v === "low") return "badge badge-gray";
  return "badge badge-orange";
}

function emptyStateText(message) {
  return (
    <p style={{ fontSize: 13, color: "var(--text-3)", lineHeight: 1.6, margin: 0, fontStyle: "italic" }}>
      {message}
    </p>
  );
}

function QaInvestigationReport({ report, t }) {
  if (!report) return null;
  const vm = buildIncidentReportViewModel(report, t);
  const storylineVm = buildStorylineViewModel(report, t);
  const impactMapVm = buildImpactMapViewModel(report, t);
  const deploymentRiskVm = buildDeploymentRiskViewModel(report, t);
  const testRecommendationsVm = buildTestRecommendationsViewModel(report, t);
  const decisionCenterVm = buildDecisionCenterViewModel(report, t);
  const historicalLearningVm = buildHistoricalLearningViewModel(report, t, fmtTs);
  const recommendedActionsVm = buildRecommendedActionsViewModel(report, t);
  const approvalWorkflowVm = buildApprovalWorkflowViewModel(report, t);
  const es = report.evidence_strength;
  const temporal = report.temporal_correlation;
  return (
    <div className="card" style={{ padding: "20px 24px", marginTop: 20 }}>
      {vm.showLegacyBanner ? (
        <div className="alert alert-warning" style={{ marginBottom: 16, fontSize: 13, lineHeight: 1.5 }}>
          {vm.legacyBannerMessage}
        </div>
      ) : null}
      {report.meta?.analyze_only ? (
        <div className="alert alert-info" style={{ marginBottom: 16, fontSize: 13, lineHeight: 1.5 }}>
          <strong>{t("incident.qa.analyze_only_banner")}</strong>
          {" "}{t("incident.qa.approval_notice")}
        </div>
      ) : null}
      {vm.showEngineMeta ? (
        <div style={{ marginBottom: 16, display: "flex", gap: 12, flexWrap: "wrap", fontSize: 12, color: "var(--text-3)" }}>
          <span><strong>{t("incident.qa.engine_label")}:</strong> {vm.engineVersion}</span>
          <span><strong>{t("incident.qa.analyze_only_label")}:</strong> {String(vm.analyzeOnly)}</span>
        </div>
      ) : null}
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 12, flexWrap: "wrap", marginBottom: 16 }}>
        <div className="section-title" style={{ margin: 0 }}>{t("incident.qa.title")}</div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
          <span className={severityBadge(report.severity)}>{report.severity}</span>
          <span className="badge badge-blue">{t("incident.qa.confidence")}: {confidencePct(report.confidence)}</span>
        </div>
      </div>

      {report.summary ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>{t("incident.qa.summary")}</div>
          <div style={{ fontSize: 14, color: "var(--text-1)", lineHeight: 1.6 }}>{report.summary}</div>
        </div>
      ) : null}

      {report.hypotheses?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.hypotheses")}</div>
          <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
            {report.hypotheses.map((h) => (
              <li key={h.id || h.rank || h.statement} style={{ marginBottom: 10, padding: "10px 12px", background: "var(--bg-2)", borderRadius: 8, fontSize: 13, lineHeight: 1.5 }}>
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 4 }}>
                  {h.id === report.primary_hypothesis_id ? (
                    <span className="badge badge-orange">{t("incident.qa.primary_hypothesis")}</span>
                  ) : null}
                  {h.rank ? <span className="badge badge-gray">#{h.rank}</span> : null}
                  <span className="badge badge-gray">{hypothesisBasisLabel(h.basis, t)}</span>
                  <span className="badge badge-blue">{confidencePct(h.confidence)}</span>
                </div>
                {h.statement}
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {Array.isArray(report.investigation_plan) ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {t("incident.qa.investigation_plan")}
          </div>
          {report.investigation_plan.length > 0 ? (
            <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
              {report.investigation_plan.map((step, i) => (
                <li
                  key={`${step.title}-${i}`}
                  style={{
                    marginBottom: 10,
                    padding: "12px 14px",
                    background: "var(--bg-2)",
                    borderRadius: 8,
                    fontSize: 13,
                    lineHeight: 1.5,
                  }}
                >
                  <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 6 }}>
                    <span className="badge badge-orange">
                      {t("incident.qa.investigation_plan_priority")}: {step.priority ?? 0}
                    </span>
                    <strong style={{ color: "var(--text-1)" }}>{step.title}</strong>
                  </div>
                  <div style={{ color: "var(--text-3)", marginBottom: 8 }}>{step.reason}</div>
                  {step.related_entity_type && step.related_entity_id ? (
                    <EvidenceCorrelationDrilldownCell
                      item={{
                        source: step.related_entity_type,
                        related_entity_type: step.related_entity_type,
                        related_entity_id: step.related_entity_id,
                        reason: step.reason,
                        detail: step.title,
                        title: step.title,
                      }}
                    />
                  ) : null}
                </li>
              ))}
            </ul>
          ) : (
            emptyStateText(t("incident.qa.investigation_plan_empty"))
          )}
        </div>
      ) : null}

      {storylineVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {storylineVm.title}
          </div>
          {storylineVm.empty ? (
            emptyStateText(storylineVm.emptyMessage)
          ) : (
            <ol style={{ margin: 0, padding: 0, listStyle: "none" }}>
              {storylineVm.steps.map((step) => (
                <li
                  key={`storyline-${step.step_number}-${step.title}`}
                  style={{
                    display: "flex",
                    gap: 12,
                    marginBottom: step.isLast ? 0 : 4,
                  }}
                >
                  <div
                    style={{
                      display: "flex",
                      flexDirection: "column",
                      alignItems: "center",
                      flexShrink: 0,
                      width: 28,
                    }}
                  >
                    <div
                      style={{
                        width: 28,
                        height: 28,
                        borderRadius: "50%",
                        background: "var(--accent, #f97316)",
                        color: "#fff",
                        fontSize: 12,
                        fontWeight: 700,
                        display: "flex",
                        alignItems: "center",
                        justifyContent: "center",
                      }}
                    >
                      {step.step_number}
                    </div>
                    {!step.isLast ? (
                      <div
                        style={{
                          flex: 1,
                          width: 2,
                          minHeight: 24,
                          background: "var(--border, rgba(255,255,255,0.12))",
                          marginTop: 4,
                        }}
                      />
                    ) : null}
                  </div>
                  <div
                    style={{
                      flex: 1,
                      marginBottom: step.isLast ? 0 : 14,
                      padding: "12px 14px",
                      background: "var(--bg-2)",
                      borderRadius: 8,
                      fontSize: 13,
                      lineHeight: 1.5,
                    }}
                  >
                    <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 6 }}>
                      <span className="badge badge-blue">
                        {storylineVm.stepLabel} {step.step_number}
                      </span>
                      <span className="badge badge-orange">
                        {storylineVm.confidenceLabel}: {step.confidenceText}
                      </span>
                      {step.timestamp ? (
                        <span style={{ fontSize: 12, color: "var(--text-3)" }}>
                          {storylineVm.timestampLabel}: {fmtTs(step.timestamp)}
                        </span>
                      ) : null}
                    </div>
                    <strong style={{ color: "var(--text-1)", display: "block", marginBottom: 6 }}>
                      {step.title}
                    </strong>
                    <div style={{ color: "var(--text-3)", marginBottom: step.drilldownItem ? 8 : 0 }}>
                      {step.description}
                    </div>
                    {step.drilldownItem ? (
                      <EvidenceCorrelationDrilldownCell item={step.drilldownItem} />
                    ) : null}
                  </div>
                </li>
              ))}
            </ol>
          )}
        </div>
      ) : null}

      {impactMapVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {impactMapVm.title}
          </div>
          {impactMapVm.empty ? (
            emptyStateText(impactMapVm.emptyMessage)
          ) : (
            <div
              style={{
                display: "grid",
                gridTemplateColumns: "repeat(auto-fill, minmax(240px, 1fr))",
                gap: 12,
              }}
            >
              {impactMapVm.nodes.map((node) => (
                <div
                  key={`impact-${node.title}`}
                  style={{
                    padding: "14px 16px",
                    background: "var(--bg-2)",
                    borderRadius: 8,
                    fontSize: 13,
                    lineHeight: 1.5,
                    border: "1px solid var(--border, rgba(255,255,255,0.08))",
                  }}
                >
                  <strong style={{ color: "var(--text-1)", display: "block", marginBottom: 8, fontSize: 14 }}>
                    {node.title}
                  </strong>
                  <div style={{ display: "flex", gap: 6, flexWrap: "wrap", marginBottom: 8 }}>
                    <span className={node.severityBadgeClass}>
                      {impactMapVm.severityLabel}: {node.severityLabel}
                    </span>
                    <span className="badge badge-blue">
                      {node.related_entity_count} {impactMapVm.signalsLabel.toLowerCase()}
                    </span>
                    <span className="badge badge-orange">
                      {impactMapVm.confidenceLabel}: {node.confidenceText}
                    </span>
                  </div>
                  <div style={{ color: "var(--text-3)", marginBottom: node.drilldownItem ? 10 : 0 }}>
                    {node.description}
                  </div>
                  {node.drilldownItem ? (
                    <EvidenceCorrelationDrilldownCell item={node.drilldownItem} />
                  ) : null}
                </div>
              ))}
            </div>
          )}
        </div>
      ) : null}

      {testRecommendationsVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {testRecommendationsVm.title}
          </div>
          {testRecommendationsVm.empty ? (
            emptyStateText(testRecommendationsVm.emptyMessage)
          ) : (
            <>
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
                {testRecommendationsVm.summary}
              </div>
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {testRecommendationsVm.recommendations.map((rec) => (
                  <RecommendedTestCard
                    key={rec.recommendation_id}
                    recommendation={rec}
                    labels={{
                      approvalRequiredLabel: testRecommendationsVm.approvalRequiredLabel,
                      priorityLabel: testRecommendationsVm.priorityLabel,
                      confidenceLabel: testRecommendationsVm.confidenceLabel,
                      riskReductionLabel: testRecommendationsVm.riskReductionLabel,
                      previewLabel: testRecommendationsVm.previewLabel,
                    }}
                  />
                ))}
              </ul>
            </>
          )}
        </div>
      ) : null}

      {decisionCenterVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {decisionCenterVm.title}
          </div>
          {decisionCenterVm.empty ? (
            emptyStateText(decisionCenterVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "center", marginBottom: 12 }}>
                <span className={decisionCenterVm.center.statusBadgeClass}>
                  {decisionCenterVm.overallStatusLabel}: {decisionCenterVm.center.statusLabel}
                </span>
                {decisionCenterVm.center.top_risk_score > 0 ? (
                  <div
                    style={{
                      minWidth: 72,
                      padding: "8px 12px",
                      borderRadius: 8,
                      background: "var(--bg-3, rgba(255,255,255,0.04))",
                      textAlign: "center",
                    }}
                  >
                    <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 2 }}>
                      {decisionCenterVm.topRiskScoreLabel}
                    </div>
                    <div style={{ fontSize: 18, fontWeight: 700, color: "var(--text-1)" }}>
                      {decisionCenterVm.center.top_risk_score}
                      <span style={{ fontSize: 12, fontWeight: 500, color: "var(--text-3)" }}> / 100</span>
                    </div>
                  </div>
                ) : null}
                <span className="badge badge-orange">
                  {decisionCenterVm.confidenceLabel}: {decisionCenterVm.center.confidenceText}
                </span>
                <span className="badge badge-blue">
                  {decisionCenterVm.recommendedTestsLabel}: {decisionCenterVm.center.recommended_test_count}
                </span>
                <span className="badge badge-blue">
                  {decisionCenterVm.recommendedActionsLabel}: {decisionCenterVm.center.recommended_action_count}
                </span>
              </div>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                {decisionCenterVm.executiveSummaryLabel}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 14 }}>
                {decisionCenterVm.center.executive_summary}
              </div>
              {(decisionCenterVm.center.top_impacted_area || decisionCenterVm.center.top_hypothesis) ? (
                <div style={{ display: "grid", gap: 10, marginBottom: 14 }}>
                  {decisionCenterVm.center.top_impacted_area ? (
                    <div style={{ fontSize: 13, lineHeight: 1.5 }}>
                      <span style={{ fontWeight: 600, color: "var(--text-3)" }}>
                        {decisionCenterVm.topImpactedAreaLabel}:
                      </span>{" "}
                      <span style={{ color: "var(--text-2)" }}>{decisionCenterVm.center.top_impacted_area}</span>
                    </div>
                  ) : null}
                  {decisionCenterVm.center.top_hypothesis ? (
                    <div style={{ fontSize: 13, lineHeight: 1.5 }}>
                      <span style={{ fontWeight: 600, color: "var(--text-3)" }}>
                        {decisionCenterVm.topHypothesisLabel}:
                      </span>{" "}
                      <span style={{ color: "var(--text-2)" }}>{decisionCenterVm.center.top_hypothesis}</span>
                    </div>
                  ) : null}
                </div>
              ) : null}
              {decisionCenterVm.center.takeaways?.length > 0 ? (
                <>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
                    {decisionCenterVm.keyTakeawaysLabel}
                  </div>
                  <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                    {decisionCenterVm.center.takeaways.map((insight) => (
                      <li
                        key={`${insight.priority}-${insight.title}`}
                        style={{
                          marginBottom: 10,
                          padding: "10px 12px",
                          background: "var(--bg-3, rgba(255,255,255,0.03))",
                          borderRadius: 6,
                          fontSize: 13,
                          lineHeight: 1.5,
                        }}
                      >
                        <strong style={{ color: "var(--text-1)", display: "block", marginBottom: 4 }}>
                          {insight.title}
                        </strong>
                        <div style={{ color: "var(--text-3)", marginBottom: insight.drilldownItem ? 8 : 0 }}>
                          {insight.description}
                        </div>
                        {insight.drilldownItem ? (
                          <EvidenceCorrelationDrilldownCell item={insight.drilldownItem} />
                        ) : null}
                      </li>
                    ))}
                  </ul>
                </>
              ) : null}
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {decisionCenterVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {historicalLearningVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {historicalLearningVm.title}
          </div>
          {historicalLearningVm.empty ? (
            emptyStateText(historicalLearningVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "center", marginBottom: 12 }}>
                <span className="badge badge-orange">
                  {historicalLearningVm.confidenceLabel}: {historicalLearningVm.learning.confidenceText}
                </span>
              </div>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                {historicalLearningVm.patternSummaryLabel}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 14 }}>
                {historicalLearningVm.learning.pattern_summary}
              </div>
              {historicalLearningVm.learning.incidents?.length > 0 ? (
                <>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
                    {historicalLearningVm.similarIncidentsLabel}
                  </div>
                  <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                    {historicalLearningVm.learning.incidents.map((incident) => (
                      <SimilarIncidentCard
                        key={incident.incident_id}
                        incident={incident}
                        labels={{
                          similarityLabel: historicalLearningVm.similarityLabel,
                          previewLabel: historicalLearningVm.previewLabel,
                        }}
                      />
                    ))}
                  </ul>
                </>
              ) : null}
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {historicalLearningVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {deploymentRiskVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {deploymentRiskVm.title}
          </div>
          {deploymentRiskVm.empty ? (
            emptyStateText(deploymentRiskVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ display: "flex", gap: 12, flexWrap: "wrap", alignItems: "center", marginBottom: 12 }}>
                <div
                  style={{
                    minWidth: 88,
                    padding: "10px 14px",
                    borderRadius: 8,
                    background: "var(--bg-3, rgba(255,255,255,0.04))",
                    textAlign: "center",
                  }}
                >
                  <div style={{ fontSize: 11, color: "var(--text-3)", marginBottom: 4 }}>
                    {deploymentRiskVm.riskScoreLabel}
                  </div>
                  <div style={{ fontSize: 22, fontWeight: 700, color: "var(--text-1)" }}>
                    {deploymentRiskVm.assessment.risk_score}
                    <span style={{ fontSize: 13, fontWeight: 500, color: "var(--text-3)" }}> / 100</span>
                  </div>
                </div>
                <span className={deploymentRiskVm.assessment.riskLevelBadgeClass}>
                  {deploymentRiskVm.riskLevelLabel}: {deploymentRiskVm.assessment.riskLevelLabel}
                </span>
                <span className="badge badge-orange">
                  {deploymentRiskVm.confidenceLabel}: {deploymentRiskVm.assessment.confidenceText}
                </span>
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 14 }}>
                {deploymentRiskVm.assessment.summary}
              </div>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
                {deploymentRiskVm.factorsLabel}
              </div>
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {deploymentRiskVm.assessment.factors.map((factor) => (
                  <li
                    key={factor.title}
                    style={{
                      marginBottom: 10,
                      padding: "10px 12px",
                      background: "var(--bg-3, rgba(255,255,255,0.03))",
                      borderRadius: 6,
                      fontSize: 13,
                      lineHeight: 1.5,
                    }}
                  >
                    <div style={{ display: "flex", gap: 8, flexWrap: "wrap", alignItems: "center", marginBottom: 4 }}>
                      <strong style={{ color: "var(--text-1)" }}>{factor.title}</strong>
                      <span className="badge badge-blue">
                        {deploymentRiskVm.weightLabel}: {factor.weightText}
                      </span>
                    </div>
                    <div style={{ color: "var(--text-3)", marginBottom: factor.drilldownItem ? 8 : 0 }}>
                      {factor.description}
                    </div>
                    {factor.drilldownItem ? (
                      <EvidenceCorrelationDrilldownCell item={factor.drilldownItem} />
                    ) : null}
                  </li>
                ))}
              </ul>
            </div>
          )}
        </div>
      ) : null}

      {recommendedActionsVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {recommendedActionsVm.title}
          </div>
          {recommendedActionsVm.empty ? (
            emptyStateText(recommendedActionsVm.emptyMessage)
          ) : (
            <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
              {recommendedActionsVm.actions.map((action) => (
                <RecommendedActionCard
                  key={action.action_id}
                  action={action}
                  labels={{
                    approvalRequiredLabel: recommendedActionsVm.approvalRequiredLabel,
                    priorityLabel: recommendedActionsVm.priorityLabel,
                    confidenceLabel: recommendedActionsVm.confidenceLabel,
                    actionTypeLabel: recommendedActionsVm.actionTypeLabel,
                  }}
                />
              ))}
            </ul>
          )}
        </div>
      ) : null}

      {approvalWorkflowVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {approvalWorkflowVm.title}
          </div>
          {approvalWorkflowVm.empty ? (
            emptyStateText(approvalWorkflowVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ display: "flex", gap: 12, flexWrap: "wrap", marginBottom: 14 }}>
                <span className="badge badge-orange">
                  {approvalWorkflowVm.pendingLabel}: {approvalWorkflowVm.workflow.pending_count}
                </span>
                <span className="badge badge-gray">
                  {approvalWorkflowVm.approvedLabel}: {approvalWorkflowVm.workflow.approved_count}
                </span>
                <span className="badge badge-red">
                  {approvalWorkflowVm.rejectedLabel}: {approvalWorkflowVm.workflow.rejected_count}
                </span>
              </div>
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {approvalWorkflowVm.workflow.requests.map((request) => (
                  <ApprovalRequestCard
                    key={request.approval_id}
                    request={request}
                    labels={{
                      statusLabel: approvalWorkflowVm.statusLabel,
                      approvalTypeLabel: approvalWorkflowVm.approvalTypeLabel,
                      approveLabel: approvalWorkflowVm.approveLabel,
                      rejectLabel: approvalWorkflowVm.rejectLabel,
                    }}
                  />
                ))}
              </ul>
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {approvalWorkflowVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {vm.evidenceStrength.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.evidence_strength")}</div>
          <div style={{ marginBottom: 12 }}>
            <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 6, textTransform: "uppercase", letterSpacing: "0.05em" }}>{t("incident.qa.evidence_bucket")}</div>
            {es?.evidence?.length > 0 ? (
              <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
                {es.evidence.map((item, i) => (
                  <li key={`ev-${i}`} style={{ marginBottom: 6, fontSize: 13, lineHeight: 1.5, padding: "8px 10px", background: "var(--bg-2)", borderRadius: 6 }}>
                    <strong>{item.label}</strong>: {item.detail}
                  </li>
                ))}
              </ul>
            ) : (
              emptyStateText(vm.evidenceStrength.evidenceEmptyMessage)
            )}
          </div>
          {es?.inference?.length > 0 ? (
            <div style={{ marginBottom: 12 }}>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 6, textTransform: "uppercase", letterSpacing: "0.05em" }}>{t("incident.qa.inference_bucket")}</div>
              <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
                {es.inference.map((item, i) => (
                  <li key={`inf-${i}`} style={{ marginBottom: 6, fontSize: 13, lineHeight: 1.5, padding: "8px 10px", background: "var(--bg-2)", borderRadius: 6 }}>
                    <strong>{item.label}</strong>: {item.detail}
                  </li>
                ))}
              </ul>
            </div>
          ) : null}
          {es?.assumptions?.length > 0 ? (
            <div>
              <div style={{ fontSize: 11, fontWeight: 600, color: "var(--text-2)", marginBottom: 6, textTransform: "uppercase", letterSpacing: "0.05em" }}>{t("incident.qa.assumptions_bucket")}</div>
              <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
                {es.assumptions.map((item, i) => (
                  <li key={`as-${i}`} style={{ marginBottom: 6, fontSize: 13, lineHeight: 1.5, padding: "8px 10px", background: "var(--bg-2)", borderRadius: 6 }}>
                    <strong>{item.label}</strong>: {item.detail}
                  </li>
                ))}
              </ul>
            </div>
          ) : null}
        </div>
      ) : null}

      {report.evidence_correlation != null ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.evidence_correlation")}</div>
          <div style={{ display: "flex", gap: 16, flexWrap: "wrap", fontSize: 12, color: "var(--text-3)", marginBottom: 10 }}>
            <span><strong>{t("incident.qa.correlation_total")}:</strong> {report.evidence_correlation.total_correlations ?? 0}</span>
            {report.evidence_correlation.strongest_source ? (
              <span><strong>{t("incident.qa.correlation_strongest")}:</strong> {report.evidence_correlation.strongest_source}</span>
            ) : null}
          </div>
          <p style={{ fontSize: 12, color: "var(--text-3)", margin: "0 0 10px", lineHeight: 1.5 }}>
            <strong>{t("incident.qa.correlation_global_confidence")}:</strong> {confidencePct(report.confidence)}
            {" · "}{t("incident.qa.correlation_ranked_independently")}
          </p>
          {!isEvidenceCorrelationEmpty(report.evidence_correlation) ? (
            <div className="card" style={{ overflow: "hidden", padding: 0 }}>
              <p style={{ fontSize: 11, color: "var(--text-3)", margin: "10px 12px 0", lineHeight: 1.5, fontStyle: "italic" }}>
                {t("incident.qa.drilldown.navigation_tooltip")}
              </p>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("incident.qa.correlation_source")}</th>
                    <th title={t("incident.qa.correlation_weight_tooltip")}>{t("incident.qa.correlation_evidence_weight")}</th>
                    <th>{t("incident.qa.correlation_reason")}</th>
                    <th>{t("incident.qa.correlation_evidence")}</th>
                    <th>{t("incident.qa.drilldown.action")}</th>
                  </tr>
                </thead>
                <tbody>
                  {report.evidence_correlation.evidence.map((item, i) => (
                    <tr key={i}>
                      <td style={{ fontSize: 12 }}>{item.source}</td>
                      <td style={{ fontSize: 12 }} title={t("incident.qa.correlation_weight_tooltip")}>{confidencePct(item.confidence)}</td>
                      <td style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5 }}>{correlationReasonText(item, t)}</td>
                      <td style={{ fontSize: 12 }}>
                        <strong>{item.title}</strong>
                        {String(item.reason || "").trim() ? (
                          <span style={{ display: "block", color: "var(--text-3)", marginTop: 2, lineHeight: 1.5 }}>
                            {correlationReasonText(item, t)}
                          </span>
                        ) : (
                          <span style={{ display: "block", color: "var(--text-3)", marginTop: 2, lineHeight: 1.5, fontStyle: "italic" }}>
                            {t("incident.qa.correlation_reason_unavailable")}
                          </span>
                        )}
                        <span style={{ display: "block", color: "var(--text-3)", marginTop: 2 }}>{item.detail}</span>
                      </td>
                      <td style={{ fontSize: 12 }}>
                        <EvidenceCorrelationDrilldownCell item={item} />
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
              <p style={{ fontSize: 11, color: "var(--text-3)", margin: "8px 12px 12px", lineHeight: 1.5, fontStyle: "italic" }}>
                {t("incident.qa.correlation_weight_tooltip")}
              </p>
            </div>
          ) : (
            <div>
              {emptyStateText(t("incident.qa.evidence_correlation_empty"))}
              <p style={{ fontSize: 13, color: "var(--text-3)", lineHeight: 1.6, margin: "8px 0 0", fontStyle: "italic" }}>
                {t("incident.qa.evidence_correlation_empty_hint")}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {report.actions_available?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>{t("incident.qa.actions_available")}</div>
          <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 10, lineHeight: 1.5 }}>{t("incident.qa.approval_notice")}</p>
          <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
            {report.actions_available.map((a) => (
              <li key={a.action} style={{ marginBottom: 8, display: "flex", gap: 10, alignItems: "flex-start", flexWrap: "wrap" }}>
                <button
                  type="button"
                  className="btn btn-ghost btn-sm"
                  disabled
                  title={t("incident.qa.action_disabled_tooltip")}
                  style={{ opacity: 0.65, cursor: "not-allowed" }}
                >
                  {a.label}
                </button>
                <span style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, flex: 1, minWidth: 180 }}>{a.reason}</span>
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {report.evidence_found?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.evidence")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {report.evidence_found.map((e, i) => <li key={i}>{e}</li>)}
          </ul>
        </div>
      ) : null}

      {report.data_gaps?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.data_gaps")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-3)", lineHeight: 1.7 }}>
            {report.data_gaps.map((g, i) => <li key={i}>{g}</li>)}
          </ul>
        </div>
      ) : null}

      {report.related_runs?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.runs")}</div>
          <div className="card" style={{ overflow: "hidden", padding: 0 }}>
            <table className="data-table">
              <thead>
                <tr>
                  <th>Test</th>
                  <th>Module</th>
                  <th>Status</th>
                  <th>Error</th>
                </tr>
              </thead>
              <tbody>
                {report.related_runs.map((r) => (
                  <tr key={r.run_id}>
                    <td style={{ fontSize: 12 }}>{r.test_name || r.test_id}</td>
                    <td style={{ fontSize: 12 }}>{r.module || "—"}</td>
                    <td style={{ fontSize: 12 }}>{r.status}</td>
                    <td style={{ fontSize: 12, maxWidth: 280, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                      {r.error_summary || r.rca_summary || "—"}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      ) : null}

      {report.related_evidence?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.evidence")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, lineHeight: 1.7 }}>
            {report.related_evidence.map((e, i) => (
              <li key={i}>
                {e.test_name || e.run_id}
                {e.evidence_url ? (
                  <> — <a href={e.evidence_url} target="_blank" rel="noreferrer">{e.evidence_url}</a></>
                ) : null}
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {report.confidence_breakdown?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.confidence_breakdown")}</div>
          <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
            {report.confidence_breakdown.map((f, i) => (
              <li key={i} style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 6, display: "flex", gap: 8, flexWrap: "wrap" }}>
                <span className="badge badge-gray" style={{ fontFamily: "monospace", fontSize: 10 }}>{f.label}</span>
                <span style={{ color: f.delta >= 0 ? "var(--green, #16a34a)" : "var(--red, #dc2626)" }}>
                  {f.delta >= 0 ? "+" : ""}{Math.round(f.delta * 100)}%
                </span>
                <span>{f.reason}</span>
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {(report.timeline?.length > 0 || vm.temporal.showEmpty) ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", gap: 8, flexWrap: "wrap", marginBottom: 8 }}>
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)" }}>{t("incident.qa.timeline")}</div>
            {temporal?.signal && temporal.signal !== "none" ? (
              <span className="badge badge-blue">{t("incident.qa.temporal_signal")}: {temporal.signal}</span>
            ) : null}
          </div>
          {vm.temporal.showEmpty ? (
            <div style={{ marginBottom: 8 }}>{emptyStateText(vm.temporal.emptyMessage)}</div>
          ) : null}
          {!vm.temporal.showEmpty && temporal?.reason ? (
            <p style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 8, lineHeight: 1.5 }}>{temporal.reason}</p>
          ) : null}
          {temporal?.event_chain?.length > 0 ? (
            <div style={{ fontSize: 12, color: "var(--text-2)", marginBottom: 10, fontFamily: "monospace", lineHeight: 1.6 }}>
              {temporal.event_chain.join(" → ")}
            </div>
          ) : null}
          {report.timeline?.length > 0 ? (
            <ol style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
              {report.timeline.map((ev, i) => (
                <li key={i} style={{ marginBottom: 8 }}>
                  <span style={{ fontSize: 11, color: "var(--text-3)", marginRight: 8 }}>{fmtTs(ev.timestamp)}</span>
                  {ev.time_distance_minutes != null && ev.relative_to_previous ? (
                    <span style={{ fontSize: 11, color: "var(--text-3)", marginRight: 8 }}>
                      (+{ev.time_distance_minutes} min after {ev.relative_to_previous})
                    </span>
                  ) : null}
                  <strong>{ev.title}</strong>
                  {ev.details ? <span style={{ display: "block", fontSize: 12, color: "var(--text-3)", marginTop: 2 }}>{ev.details}</span> : null}
                </li>
              ))}
            </ol>
          ) : null}
        </div>
      ) : null}

      {report.related_pr_analysis?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.pr_analysis")}</div>
          <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
            {report.related_pr_analysis.map((pr, i) => (
              <li key={i} style={{ marginBottom: 10, padding: "10px 12px", background: "var(--bg-2)", borderRadius: 8, fontSize: 13 }}>
                <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 4 }}>
                  <span className="badge badge-gray">#{pr.pr_number}</span>
                  <span className="badge badge-orange">{pr.risk_level}</span>
                  <span className="badge badge-blue">{Math.round(pr.pr_risk_score)}/100</span>
                </div>
                <div>{pr.reason}</div>
                {pr.impacted_modules?.length > 0 ? (
                  <div style={{ marginTop: 6, fontSize: 12, color: "var(--text-3)" }}>
                    Modules: {pr.impacted_modules.join(", ")}
                  </div>
                ) : null}
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {report.related_prs?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.prs")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, lineHeight: 1.7 }}>
            {report.related_prs.map((pr, i) => (
              <li key={i}>
                [{pr.provider}] #{pr.pr_id} {pr.title}
                {pr.html_url ? <> — <a href={pr.html_url} target="_blank" rel="noreferrer">{pr.branch}</a></> : null}
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {vm.blastRadius.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.blast_radius")}</div>
          {vm.blastRadius.empty ? (
            emptyStateText(vm.blastRadius.emptyMessage)
          ) : (
            <div className="card" style={{ overflow: "hidden", padding: 0 }}>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("incident.qa.blast_module")}</th>
                    <th>{t("incident.qa.blast_score")}</th>
                    <th>{t("incident.qa.blast_reason")}</th>
                  </tr>
                </thead>
                <tbody>
                  {report.impacted_modules_ranked.map((m, i) => (
                    <tr key={i}>
                      <td style={{ fontSize: 12 }}>{m.module}</td>
                      <td style={{ fontSize: 12 }}>{Math.round(m.score)}/100</td>
                      <td style={{ fontSize: 12 }}>{m.reason}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      ) : report.impacted_modules?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.modules")}</div>
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {report.impacted_modules.map((m, i) => (
              <span key={i} className="badge badge-gray">{m}</span>
            ))}
          </div>
        </div>
      ) : null}

      {vm.recommendedTests.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.tests")}</div>
          {vm.recommendedTests.empty ? (
            emptyStateText(vm.recommendedTests.emptyMessage)
          ) : (
            <div className="card" style={{ overflow: "hidden", padding: 0 }}>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("incident.qa.test_id")}</th>
                    <th>{t("incident.qa.test_strength")}</th>
                    <th>{t("incident.qa.test_reason")}</th>
                  </tr>
                </thead>
                <tbody>
                  {report.recommended_tests_v2.map((rec, i) => (
                    <tr key={i}>
                      <td style={{ fontSize: 12, fontFamily: "monospace" }}>{rec.test_case_id}</td>
                      <td><span className={strengthBadge(rec.recommendation_strength)}>{rec.recommendation_strength}</span></td>
                      <td style={{ fontSize: 12 }}>{rec.reason}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      ) : report.recommended_tests?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.tests")}</div>
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {report.recommended_tests.map((tid, i) => (
              <span key={i} className="badge badge-gray" style={{ fontFamily: "monospace", fontSize: 10 }}>{tid}</span>
            ))}
          </div>
        </div>
      ) : null}

      {report.next_steps?.length > 0 ? (
        <div>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.next_steps")}</div>
          <ol style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {report.next_steps.map((s, i) => <li key={i}>{s}</li>)}
          </ol>
        </div>
      ) : null}
    </div>
  );
}

function InvestigationResult({ run, t, titleKey = "incident.result.title" }) {
  if (!run) return null;
  const screenshot = run.screenshot_url || (run.screenshot_b64 ? `data:image/png;base64,${run.screenshot_b64}` : null);

  return (
    <div className="card" style={{ padding: "20px 24px", marginTop: 20 }}>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "flex-start", gap: 12, flexWrap: "wrap", marginBottom: 16 }}>
        <div>
          <div className="section-title" style={{ margin: 0 }}>{t(titleKey)}</div>
          <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 4 }}>
            {fmtTs(run.created_at)} · {run.id?.slice(0, 8)}…
          </div>
        </div>
        <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
          <span className={severityBadge(run.severity)}>{run.severity}</span>
          <span className="badge badge-gray">{reproducedLabel(run.reproduced, t)}</span>
          <span className="badge badge-gray">{run.suspected_area || "unknown"}</span>
        </div>
      </div>

      {run.error_message ? (
        <div className="alert alert-error" style={{ marginBottom: 16, fontSize: 13 }}>{run.error_message}</div>
      ) : null}

      <div style={{ fontSize: 14, fontWeight: 600, color: "var(--text-1)", marginBottom: 8 }}>
        {run.diagnosis_summary || t("incident.result.no_diagnosis")}
      </div>

      {run.symptom_observed ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
          <strong>{t("incident.result.symptom")}:</strong> {run.symptom_observed}
        </div>
      ) : null}

      {run.probable_cause ? (
        <div style={{ fontSize: 13, color: "var(--text-2)", marginBottom: 12, lineHeight: 1.6 }}>
          <strong>{t("incident.result.cause")}:</strong> {run.probable_cause}
        </div>
      ) : null}

      {run.suspected_endpoint ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, fontFamily: "monospace", wordBreak: "break-all" }}>
          {t("incident.result.endpoint")}: {run.suspected_endpoint}
        </div>
      ) : null}

      {run.target_url ? (
        <div style={{ fontSize: 12, color: "var(--text-3)", marginBottom: 12, wordBreak: "break-all" }}>
          {t("incident.form.target_url")}: {run.target_url}
        </div>
      ) : null}

      {screenshot ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8, textTransform: "uppercase", letterSpacing: "0.06em" }}>
            {t("incident.result.screenshot")}
          </div>
          <img
            src={screenshot}
            alt="investigation screenshot"
            style={{ maxWidth: "100%", borderRadius: 8, border: "1px solid var(--border)" }}
          />
        </div>
      ) : null}

      {run.recommendations?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.recommendations")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {run.recommendations.map((r, i) => <li key={i}>{r}</li>)}
          </ul>
        </div>
      ) : null}

      {run.reproduction_steps?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.repro_steps")}</div>
          <ol style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-2)", lineHeight: 1.7 }}>
            {run.reproduction_steps.map((s, i) => <li key={i}>{s}</li>)}
          </ol>
        </div>
      ) : null}

      {run.steps_executed?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.result.steps")}</div>
          <div style={{ display: "flex", flexWrap: "wrap", gap: 6 }}>
            {run.steps_executed.map((s, i) => (
              <span key={i} className="badge badge-gray" style={{ fontFamily: "monospace", fontSize: 10 }}>{s}</span>
            ))}
          </div>
        </div>
      ) : null}

      {(run.console_errors?.length > 0 || run.network_errors?.length > 0 || run.http_errors?.length > 0) ? (
        <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(240px, 1fr))", gap: 16 }}>
          {run.console_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.console")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.console_errors.map((e, i) => `${i + 1}. ${e.text || JSON.stringify(e)}`).join("\n")}
              </pre>
            </div>
          ) : null}
          {run.http_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.http")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.http_errors.map((e, i) => `${i + 1}. ${e.status} ${e.method || "GET"} ${e.url}`).join("\n")}
              </pre>
            </div>
          ) : null}
          {run.network_errors?.length > 0 ? (
            <div>
              <div style={{ fontSize: 12, fontWeight: 600, marginBottom: 8 }}>{t("incident.result.network")}</div>
              <pre style={{ fontSize: 11, background: "var(--bg-2)", padding: 12, borderRadius: 8, overflow: "auto", maxHeight: 200 }}>
                {run.network_errors.map((e, i) => `${i + 1}. ${e.url} — ${e.failure || e.error || ""}`).join("\n")}
              </pre>
            </div>
          ) : null}
        </div>
      ) : null}
    </div>
  );
}

export default function IncidentInvestigatorPage() {
  const { t } = useLang();
  const { currentProject } = useProject();
  const projectId = currentProject?.id;

  const [description, setDescription] = useState("");
  const [targetUrl, setTargetUrl] = useState("");
  const [moduleHint, setModuleHint] = useState("");
  const [severity, setSeverity] = useState("medium");
  const [timeWindowHours, setTimeWindowHours] = useState(72);
  const [includeBrowserProbe, setIncludeBrowserProbe] = useState(false);
  const [investigating, setInvestigating] = useState(false);
  const [error, setError] = useState("");
  const [qaReport, setQaReport] = useState(null);
  const [result, setResult] = useState(null);

  const [history, setHistory] = useState([]);
  const [historyLoading, setHistoryLoading] = useState(true);
  const [historyError, setHistoryError] = useState("");

  const [qaHistory, setQaHistory] = useState([]);
  const [qaHistoryLoading, setQaHistoryLoading] = useState(true);
  const [qaHistoryError, setQaHistoryError] = useState("");

  const loadBrowserHistory = useCallback(async () => {
    setHistoryLoading(true);
    setHistoryError("");
    try {
      const data = await listIncidentRuns({
        limit: 30,
        project_id: projectId || undefined,
      });
      setHistory(Array.isArray(data?.items) ? data.items : []);
    } catch (e) {
      setHistory([]);
      setHistoryError(apiErrorMessage(e) || t("incident.history.error"));
    } finally {
      setHistoryLoading(false);
    }
  }, [projectId, t]);

  const loadQaHistory = useCallback(async () => {
    if (!projectId) {
      setQaHistory([]);
      setQaHistoryLoading(false);
      return;
    }
    setQaHistoryLoading(true);
    setQaHistoryError("");
    try {
      const data = await listProjectIncidentHistory(projectId, { limit: 30 });
      setQaHistory(Array.isArray(data?.items) ? data.items : []);
    } catch (e) {
      setQaHistory([]);
      setQaHistoryError(apiErrorMessage(e) || t("incident.qa.history.error"));
    } finally {
      setQaHistoryLoading(false);
    }
  }, [projectId, t]);

  useEffect(() => {
    loadBrowserHistory();
    loadQaHistory();
  }, [loadBrowserHistory, loadQaHistory]);

  const handleInvestigate = async (e) => {
    e.preventDefault();
    const desc = description.trim();
    if (desc.length < 3) {
      setError(t("incident.form.desc_required"));
      return;
    }
    if (!projectId) {
      setError(t("incident.form.no_project"));
      return;
    }
    setInvestigating(true);
    setError("");
    setQaReport(null);
    setResult(null);
    try {
      const body = {
        description: desc,
        severity,
        time_window_hours: Number(timeWindowHours) || 72,
        include_browser_probe: includeBrowserProbe && !!targetUrl.trim(),
      };
      if (targetUrl.trim()) body.target_url = targetUrl.trim();
      if (moduleHint.trim()) body.module = moduleHint.trim();
      const report = await investigateProjectIncident(projectId, body);
      setQaReport(report);
      await loadQaHistory();
      if (report.browser_investigation) {
        setResult(report.browser_investigation);
        await loadBrowserHistory();
      }
    } catch (err) {
      setError(apiErrorMessage(err) || t("incident.form.error"));
    } finally {
      setInvestigating(false);
    }
  };

  const openQaHistoryItem = async (id) => {
    if (!projectId) return;
    setError("");
    try {
      const report = await getProjectIncidentReport(projectId, id);
      setQaReport(report);
      setDescription(report.description || "");
      setSeverity(report.severity || "medium");
      setTimeWindowHours(report.time_window_hours || 72);
      if (report.browser_investigation) {
        setResult(report.browser_investigation);
      } else {
        setResult(null);
      }
      window.scrollTo({ top: 0, behavior: "smooth" });
    } catch (e) {
      setError(apiErrorMessage(e) || t("incident.qa.history.error"));
    }
  };

  const openHistoryItem = async (id) => {
    setError("");
    try {
      const run = await getIncidentRun(id);
      setResult(run);
      setDescription(run.incident_description || "");
      setTargetUrl(run.target_url || "");
      setModuleHint(run.module || "");
      window.scrollTo({ top: 0, behavior: "smooth" });
    } catch (e) {
      setError(apiErrorMessage(e) || t("incident.history.load_one_error"));
    }
  };

  return (
    <div style={{ padding: "32px 40px", maxWidth: 960, margin: "0 auto" }}>
      <div style={{ marginBottom: 28 }}>
        <h1 style={{ fontSize: 22, fontWeight: 700, margin: 0, color: "var(--text-1)" }}>{t("incident.title")}</h1>
        <p style={{ fontSize: 13, color: "var(--text-3)", marginTop: 6, lineHeight: 1.6 }}>{t("incident.subtitle")}</p>
        {currentProject ? (
          <p style={{ fontSize: 12, color: "var(--text-3)", marginTop: 4 }}>{t("incident.scope", { name: currentProject.name })}</p>
        ) : null}
      </div>

      <form className="card" style={{ padding: "20px 24px" }} onSubmit={handleInvestigate}>
        <div style={{ marginBottom: 14 }}>
          <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
            {t("incident.form.description")}
          </label>
          <textarea
            className="input"
            rows={4}
            value={description}
            onChange={(e) => setDescription(e.target.value)}
            placeholder={t("incident.form.description_ph")}
            disabled={investigating}
            style={{ width: "100%", resize: "vertical", minHeight: 96 }}
          />
        </div>

        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 12, marginBottom: 12 }}>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.target_url")} ({t("incident.form.optional")})
            </label>
            <input
              className="input"
              type="url"
              value={targetUrl}
              onChange={(e) => setTargetUrl(e.target.value)}
              placeholder="https://app.example.com/dashboard"
              disabled={investigating}
              style={{ width: "100%" }}
            />
          </div>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.module")} ({t("incident.form.optional")})
            </label>
            <input
              className="input"
              value={moduleHint}
              onChange={(e) => setModuleHint(e.target.value)}
              placeholder="/vacancies"
              disabled={investigating}
              style={{ width: "100%" }}
            />
          </div>
        </div>

        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 12, marginBottom: 16 }}>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.severity")}
            </label>
            <select className="input" value={severity} onChange={(e) => setSeverity(e.target.value)} disabled={investigating} style={{ width: "100%" }}>
              <option value="low">low</option>
              <option value="medium">medium</option>
              <option value="high">high</option>
              <option value="critical">critical</option>
            </select>
          </div>
          <div>
            <label style={{ display: "block", fontSize: 12, fontWeight: 600, color: "var(--text-2)", marginBottom: 6 }}>
              {t("incident.form.time_window")}
            </label>
            <input
              className="input"
              type="number"
              min={1}
              max={720}
              value={timeWindowHours}
              onChange={(e) => setTimeWindowHours(e.target.value)}
              disabled={investigating}
              style={{ width: "100%" }}
            />
          </div>
          <div style={{ display: "flex", alignItems: "flex-end", paddingBottom: 4 }}>
            <label style={{ display: "flex", alignItems: "center", gap: 8, fontSize: 12, color: "var(--text-2)", cursor: "pointer" }}>
              <input
                type="checkbox"
                checked={includeBrowserProbe}
                onChange={(e) => setIncludeBrowserProbe(e.target.checked)}
                disabled={investigating || !targetUrl.trim()}
              />
              {t("incident.form.browser_probe")}
            </label>
          </div>
        </div>

        {error ? (
          <div className="alert alert-error" style={{ marginBottom: 12, fontSize: 13 }}>{error}</div>
        ) : null}

        <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
          <button type="submit" className="btn btn-primary" disabled={investigating}>
            {investigating ? t("incident.form.investigating") : t("incident.form.submit")}
          </button>
          <span style={{ fontSize: 11, color: "var(--text-3)" }}>{t("incident.form.safe_note")}</span>
        </div>
      </form>

      {investigating ? (
        <div style={{ padding: "24px 0", textAlign: "center", color: "var(--text-3)", fontSize: 13 }}>
          {includeBrowserProbe && targetUrl.trim()
            ? t("incident.form.investigating_hint")
            : t("incident.form.investigating")}
        </div>
      ) : null}

      <QaInvestigationReport report={qaReport} t={t} />
      {result ? <InvestigationResult run={result} t={t} titleKey="incident.qa.browser_title" /> : null}

      {projectId ? (
        <div style={{ marginTop: 32 }}>
          <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12 }}>
            <div className="section-title" style={{ margin: 0 }}>{t("incident.qa.history")}</div>
            <button type="button" className="btn btn-ghost btn-sm" onClick={loadQaHistory} disabled={qaHistoryLoading}>
              {t("dash.refresh")}
            </button>
          </div>
          {qaHistoryLoading ? (
            <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.qa.history.loading")}</div>
          ) : qaHistoryError ? (
            <div className="alert alert-error" style={{ fontSize: 13 }}>{qaHistoryError}</div>
          ) : qaHistory.length === 0 ? (
            <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.qa.history.empty")}</div>
          ) : (
            <div className="card" style={{ overflow: "hidden" }}>
              <table className="data-table">
                <thead>
                  <tr>
                    <th>{t("incident.history.col.date")}</th>
                    <th>{t("incident.history.col.description")}</th>
                    <th>{t("incident.history.col.severity")}</th>
                    <th>{t("incident.qa.confidence")}</th>
                  </tr>
                </thead>
                <tbody>
                  {qaHistory.map((h) => (
                    <tr key={h.id} style={{ cursor: "pointer" }} onClick={() => openQaHistoryItem(h.id)}>
                      <td style={{ fontSize: 12, whiteSpace: "nowrap" }}>{fmtTs(h.created_at)}</td>
                      <td style={{ fontSize: 13, maxWidth: 320, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                        {h.description}
                      </td>
                      <td><span className={severityBadge(h.severity)}>{h.severity}</span></td>
                      <td style={{ fontSize: 12 }}>{confidencePct(h.confidence)}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      ) : null}

      <div style={{ marginTop: 32 }}>
        <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 12 }}>
          <div className="section-title" style={{ margin: 0 }}>{t("incident.history.title")}</div>
          <button type="button" className="btn btn-ghost btn-sm" onClick={loadBrowserHistory} disabled={historyLoading}>
            {t("dash.refresh")}
          </button>
        </div>

        {historyLoading ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.history.loading")}</div>
        ) : historyError ? (
          <div className="alert alert-error" style={{ fontSize: 13 }}>{historyError}</div>
        ) : history.length === 0 ? (
          <div style={{ fontSize: 13, color: "var(--text-3)", padding: "16px 0" }}>{t("incident.history.empty")}</div>
        ) : (
          <div className="card" style={{ overflow: "hidden" }}>
            <table className="data-table">
              <thead>
                <tr>
                  <th>{t("incident.history.col.date")}</th>
                  <th>{t("incident.history.col.description")}</th>
                  <th>{t("incident.history.col.severity")}</th>
                  <th>{t("incident.history.col.status")}</th>
                  <th>{t("incident.history.col.reproduced")}</th>
                </tr>
              </thead>
              <tbody>
                {history.map((h) => (
                  <tr
                    key={h.id}
                    style={{ cursor: "pointer" }}
                    onClick={() => openHistoryItem(h.id)}
                  >
                    <td style={{ fontSize: 12, whiteSpace: "nowrap" }}>{fmtTs(h.created_at)}</td>
                    <td style={{ fontSize: 13, maxWidth: 320, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                      {h.incident_description}
                    </td>
                    <td><span className={severityBadge(h.severity)}>{h.severity}</span></td>
                    <td style={{ fontSize: 12 }}>{h.status}</td>
                    <td style={{ fontSize: 12 }}>{reproducedLabel(h.reproduced, t)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  );
}
