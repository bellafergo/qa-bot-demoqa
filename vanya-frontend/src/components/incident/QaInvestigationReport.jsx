import React, { useEffect, useState } from "react";
import {
  executeDatabaseValidation,
  listDatabaseConnections,
  simulateDatabaseValidationApproval,
} from "../../api";
import {
  buildIncidentReportViewModel,
  correlationReasonText,
  isEvidenceCorrelationEmpty,
} from "../../utils/incidentReportViewUtils.js";
import { buildStorylineViewModel } from "../../utils/incidentStorylineViewUtils.js";
import { buildImpactMapViewModel } from "../../utils/incidentImpactMapViewUtils.js";
import { buildDeploymentRiskViewModel } from "../../utils/deploymentRiskAssessmentViewUtils.js";
import { buildTestRecommendationsViewModel } from "../../utils/testRecommendationViewUtils.js";
import { buildDecisionCenterViewModel } from "../../utils/qualityDecisionCenterViewUtils.js";
import { buildHistoricalLearningViewModel } from "../../utils/historicalLearningViewUtils.js";
import { buildApprovalWorkflowViewModel } from "../../utils/approvalWorkflowViewUtils.js";
import { buildDatabaseValidationViewModel } from "../../utils/databaseValidationViewUtils.js";
import { buildApiContractIntelligenceViewModel } from "../../utils/apiContractIntelligenceViewUtils.js";
import { buildContractRiskAssessmentViewModel } from "../../utils/contractRiskAssessmentViewUtils.js";
import { buildDataJourneyValidationViewModel } from "../../utils/dataJourneyValidationViewUtils.js";
import { buildEnterpriseDependencyMapViewModel } from "../../utils/enterpriseDependencyMapViewUtils.js";
import { buildExecutiveQualityReportViewModel } from "../../utils/executiveQualityReportViewUtils.js";
import { buildQualityHealthScoreViewModel } from "../../utils/qualityHealthScoreViewUtils.js";
import { buildQualityTrendViewModel } from "../../utils/qualityTrendViewUtils.js";
import { buildEarlyDegradationViewModel } from "../../utils/earlyDegradationViewUtils.js";
import { buildMultiEnvironmentIntelligenceViewModel } from "../../utils/environmentIntelligenceViewUtils.js";
import { buildRecommendedActionsViewModel } from "../../utils/incidentRecommendedActionsViewUtils.js";
import { buildExecuteValidationPreviewPayload, pickConnectionForCheck } from "../../utils/databaseConnectorViewUtils.js";
import { buildReleaseReadinessViewModel } from "../../utils/releaseReadinessViewUtils.js";
import { buildJiraIssueIntelligenceViewModel } from "../../utils/jiraIssueIntelligenceViewUtils.js";
import { buildServiceNowIntelligenceViewModel } from "../../utils/servicenowIntelligenceViewUtils.js";
import { buildCoverageIntelligenceViewModel } from "../../utils/qmetryCoverageViewUtils.js";
import { buildRecommendationCorrelationViewModel } from "../../utils/qmetryRecommendationViewUtils.js";
import {
  buildQMetryIntegrationGroupState,
  isCapabilityGated,
  shouldConsolidateQMetryIntegration,
} from "../../utils/capabilityStateViewUtils.js";
import {
  fmtTs,
  severityBadge,
  confidencePct,
  hypothesisBasisLabel,
  strengthBadge,
  emptyStateText,
} from "../../utils/incidentInvestigatorHelpers.js";
import EvidenceCorrelationDrilldownCell from "./EvidenceCorrelationDrilldownCell.jsx";
import RecommendedActionCard from "./RecommendedActionCard.jsx";
import RecommendedTestCard from "./RecommendedTestCard.jsx";
import SimilarIncidentCard from "./SimilarIncidentCard.jsx";
import ApprovalRequestCard from "./ApprovalRequestCard.jsx";
import DatabaseValidationCheckCard from "./DatabaseValidationCheckCard.jsx";
import ApiContractCard from "./ApiContractCard.jsx";
import ContractRiskAssessmentCard from "./ContractRiskAssessmentCard.jsx";
import DataJourneyCard from "./DataJourneyCard.jsx";
import EnterpriseDependencyMapView from "./EnterpriseDependencyMapView.jsx";
import ExecutiveQualityReportView from "./ExecutiveQualityReportView.jsx";
import QualityHealthReportView from "./QualityHealthReportView.jsx";
import QualityTrendReportView from "./QualityTrendReportView.jsx";
import EarlyDegradationReportView from "./EarlyDegradationReportView.jsx";
import MultiEnvironmentIntelligenceView from "./MultiEnvironmentIntelligenceView.jsx";
import ReleaseReadinessView from "../release-readiness/ReleaseReadinessView.jsx";
import JiraIssueIntelligenceView from "./JiraIssueIntelligenceView.jsx";
import ServiceNowIntelligenceView from "../servicenow-intelligence/ServiceNowIntelligenceView.jsx";
import CoverageIntelligenceView from "../coverage-intelligence/CoverageIntelligenceView.jsx";
import QMetryRecommendationView from "../qmetry-recommendations/QMetryRecommendationView.jsx";
import IncidentInsightTracePanel from "../incidents/IncidentInsightTracePanel.jsx";
import CapabilityStateCard from "../capability-state/CapabilityStateCard.jsx";
import ReportCollapsibleSection from "./ReportCollapsibleSection.jsx";
import {
  buildQaInvestigationReportLayoutViewModel,
  partitionImpactMapNodes,
  sliceTopItems,
} from "../../utils/qaInvestigationReportLayoutUtils.js";

export default function QaInvestigationReport({ report, t }) {
  const [dbConnections, setDbConnections] = useState([]);
  const [approvalStatusByCheckId, setApprovalStatusByCheckId] = useState({});
  const [executeBusyId, setExecuteBusyId] = useState(null);

  useEffect(() => {
    if (!report?.database_validation) {
      setDbConnections([]);
      return;
    }
    let cancelled = false;
    (async () => {
      try {
        const data = await listDatabaseConnections({ limit: 100 });
        if (!cancelled) setDbConnections(Array.isArray(data) ? data : []);
      } catch {
        if (!cancelled) setDbConnections([]);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, [report?.database_validation]);

  if (!report) return null;
  const vm = buildIncidentReportViewModel(report, t);
  const storylineVm = buildStorylineViewModel(report, t);
  const impactMapVm = buildImpactMapViewModel(report, t);
  const deploymentRiskVm = buildDeploymentRiskViewModel(report, t);
  const testRecommendationsVm = buildTestRecommendationsViewModel(report, t);
  const databaseValidationVm = buildDatabaseValidationViewModel(report, t, {
    connections: dbConnections,
    approvalStatusByCheckId,
  });
  const apiContractIntelligenceVm = buildApiContractIntelligenceViewModel(report, t);
  const contractRiskAssessmentVm = buildContractRiskAssessmentViewModel(report, t);
  const dataJourneyValidationVm = buildDataJourneyValidationViewModel(report, t);
  const enterpriseDependencyMapVm = buildEnterpriseDependencyMapViewModel(report, t);
  const executiveQualityReportVm = buildExecutiveQualityReportViewModel(report, t);
  const qualityHealthVm = buildQualityHealthScoreViewModel(report, t);
  const qualityTrendVm = buildQualityTrendViewModel(report, t);
  const earlyDegradationVm = buildEarlyDegradationViewModel(report, t);
  const multiEnvironmentVm = buildMultiEnvironmentIntelligenceViewModel(report, t);
  const decisionCenterVm = buildDecisionCenterViewModel(report, t);
  const historicalLearningVm = buildHistoricalLearningViewModel(report, t, fmtTs);
  const recommendedActionsVm = buildRecommendedActionsViewModel(report, t);
  const releaseReadinessVm = buildReleaseReadinessViewModel(report, t);
  const jiraIssueIntelligenceVm = buildJiraIssueIntelligenceViewModel(report, t);
  const servicenowIntelligenceVm = buildServiceNowIntelligenceViewModel(report, t);
  const coverageIntelligenceVm = buildCoverageIntelligenceViewModel(report, t);
  const recommendationCorrelationVm = buildRecommendationCorrelationViewModel(report, t);
  const approvalWorkflowVm = buildApprovalWorkflowViewModel(report, t);
  const layoutVm = buildQaInvestigationReportLayoutViewModel(report, t);
  const impactPartition = partitionImpactMapNodes(impactMapVm.nodes);
  const recommendedActionSlices = sliceTopItems(recommendedActionsVm.actions, 3);
  const testRecommendationSlices = sliceTopItems(testRecommendationsVm.recommendations, 3);
  const es = report.evidence_strength;
  const temporal = report.temporal_correlation;
  const qmetryIntegrationConsolidated = shouldConsolidateQMetryIntegration(
    coverageIntelligenceVm,
    recommendationCorrelationVm,
  );
  const qmetryGroupState = qmetryIntegrationConsolidated
    ? buildQMetryIntegrationGroupState(t)
    : null;
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

      {layoutVm.showLowConfidenceWarning ? (
        <div className="alert alert-warning" style={{ marginBottom: 16, fontSize: 13, lineHeight: 1.55 }}>
          {layoutVm.lowConfidenceMessage}
        </div>
      ) : null}

      <div style={{ fontSize: 11, fontWeight: 700, color: "var(--text-3)", letterSpacing: "0.08em", textTransform: "uppercase", marginBottom: 12 }}>
        {layoutVm.executiveOverviewTitle}
      </div>

      {layoutVm.showNoisySections && executiveQualityReportVm.show ? (
        <div style={{ marginBottom: 20 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {executiveQualityReportVm.title}
          </div>
          {executiveQualityReportVm.empty ? (
            emptyStateText(executiveQualityReportVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <ExecutiveQualityReportView vm={executiveQualityReportVm} />
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {executiveQualityReportVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {layoutVm.showNoisySections && qualityHealthVm.show ? (
        <div style={{ marginBottom: 20 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {qualityHealthVm.title}
          </div>
          {qualityHealthVm.empty ? (
            emptyStateText(qualityHealthVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <QualityHealthReportView vm={qualityHealthVm} />
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {qualityHealthVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {layoutVm.showNoisySections && deploymentRiskVm.show ? (
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
              {deploymentRiskVm.assessment.showTrace ? (
                <IncidentInsightTracePanel trace={deploymentRiskVm.assessment.trace} />
              ) : null}
            </div>
          )}
        </div>
      ) : null}

      {report.summary ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>{t("incident.qa.summary")}</div>
          <div style={{ fontSize: 14, color: "var(--text-1)", lineHeight: 1.6 }}>{report.summary}</div>
        </div>
      ) : null}

      {layoutVm.showNoisySections && decisionCenterVm.show ? (
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
              </div>
              <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 6 }}>
                {decisionCenterVm.executiveSummaryLabel}
              </div>
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 14 }}>
                {decisionCenterVm.center.executive_summary}
              </div>
              {decisionCenterVm.center.showTrace ? (
                <IncidentInsightTracePanel trace={decisionCenterVm.center.trace} />
              ) : null}
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
                    {decisionCenterVm.center.takeaways.slice(0, 3).map((insight) => (
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

      {layoutVm.showNoisySections && recommendedActionsVm.show && !recommendedActionsVm.empty ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {layoutVm.recommendedActionsTitle}
          </div>
          <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
            {recommendedActionSlices.top.map((action) => (
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
          {recommendedActionSlices.hasRemainder ? (
            <ReportCollapsibleSection
              title={layoutVm.allRecommendedActionsTitle}
              defaultOpen={layoutVm.expandAllRecommendedActions}
            >
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {recommendedActionSlices.remainder.map((action) => (
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
            </ReportCollapsibleSection>
          ) : null}
        </div>
      ) : null}

      {layoutVm.lowConfidence && Array.isArray(report.investigation_plan) && report.investigation_plan.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {layoutVm.suggestedNextStepsTitle}
          </div>
          <ul style={{ margin: 0, paddingLeft: 0, listStyle: "none" }}>
            {report.investigation_plan.slice(0, 3).map((step, i) => (
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
                <strong style={{ color: "var(--text-1)" }}>{step.title}</strong>
                <div style={{ color: "var(--text-3)", marginTop: 6 }}>{step.reason}</div>
              </li>
            ))}
          </ul>
        </div>
      ) : null}

      {layoutVm.lowConfidence && report.data_gaps?.length > 0 ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>{t("incident.qa.data_gaps")}</div>
          <ul style={{ margin: 0, paddingLeft: 18, fontSize: 13, color: "var(--text-3)", lineHeight: 1.7 }}>
            {report.data_gaps.slice(0, 5).map((g, i) => <li key={i}>{g}</li>)}
          </ul>
        </div>
      ) : null}

      {layoutVm.showNoisySections ? (
      <ReportCollapsibleSection
        title={layoutVm.operationalAnalysisTitle}
        defaultOpen={layoutVm.expandOperationalAnalysis}
      >
      {qualityTrendVm.show ? (
        <div style={{ marginBottom: 20 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {qualityTrendVm.title}
          </div>
          {qualityTrendVm.empty ? (
            emptyStateText(qualityTrendVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <QualityTrendReportView vm={qualityTrendVm} />
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {qualityTrendVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {earlyDegradationVm.show ? (
        <div style={{ marginBottom: 20 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {earlyDegradationVm.title}
          </div>
          {earlyDegradationVm.empty ? (
            emptyStateText(earlyDegradationVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <EarlyDegradationReportView vm={earlyDegradationVm} />
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {earlyDegradationVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {multiEnvironmentVm.show ? (
        <div style={{ marginBottom: 20 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {multiEnvironmentVm.title}
          </div>
          {multiEnvironmentVm.empty ? (
            emptyStateText(multiEnvironmentVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <MultiEnvironmentIntelligenceView vm={multiEnvironmentVm} />
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {multiEnvironmentVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {releaseReadinessVm.show ? (
        <div style={{ marginBottom: 20 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {releaseReadinessVm.title}
          </div>
          {releaseReadinessVm.empty ? (
            emptyStateText(releaseReadinessVm.emptyMessage)
          ) : (
            <ReleaseReadinessView vm={releaseReadinessVm} />
          )}
        </div>
      ) : null}

      {historicalLearningVm.show ? (
        <div style={{ marginBottom: 20 }}>
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
      </ReportCollapsibleSection>
      ) : null}

      {layoutVm.showNoisySections && impactMapVm.show ? (
        <ReportCollapsibleSection
          title={`${layoutVm.operationalAnalysisTitle} · ${impactMapVm.title}`}
          defaultOpen={layoutVm.expandOperationalAnalysis}
        >
          {impactMapVm.empty ? (
            emptyStateText(impactMapVm.emptyMessage)
          ) : (
            <>
              {impactPartition.confirmed.length > 0 ? (
                <>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
                    {layoutVm.confirmedImpactedAreasTitle}
                  </div>
                  <div
                    style={{
                      display: "grid",
                      gridTemplateColumns: "repeat(auto-fill, minmax(240px, 1fr))",
                      gap: 12,
                      marginBottom: impactPartition.inferred.length > 0 ? 16 : 0,
                    }}
                  >
                    {impactPartition.confirmed.map((node) => (
                      <div
                        key={`impact-confirmed-${node.title}`}
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
                </>
              ) : null}
              {impactPartition.inferred.length > 0 ? (
                <ReportCollapsibleSection
                  title={layoutVm.potentialInferredAreasTitle}
                  defaultOpen={layoutVm.expandInferredImpactAreas}
                >
                  <div
                    style={{
                      display: "grid",
                      gridTemplateColumns: "repeat(auto-fill, minmax(240px, 1fr))",
                      gap: 12,
                    }}
                  >
                    {impactPartition.inferred.map((node) => (
                      <div
                        key={`impact-inferred-${node.title}`}
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
                        <div style={{ color: "var(--text-3)", marginBottom: node.drilldownItem ? 10 : 0 }}>
                          {node.description}
                        </div>
                        {node.drilldownItem ? (
                          <EvidenceCorrelationDrilldownCell item={node.drilldownItem} />
                        ) : null}
                      </div>
                    ))}
                  </div>
                </ReportCollapsibleSection>
              ) : null}
            </>
          )}
        </ReportCollapsibleSection>
      ) : null}

      {layoutVm.showNoisySections ? (
      <ReportCollapsibleSection
        title={layoutVm.dependencyIntelligenceTitle}
        defaultOpen={layoutVm.expandDependencyIntelligence}
      >
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
                {testRecommendationSlices.top.map((rec) => (
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
              {testRecommendationSlices.hasRemainder ? (
                <ReportCollapsibleSection
                  title={layoutVm.additionalTestRecommendationsTitle}
                  defaultOpen={layoutVm.expandAdditionalTestRecommendations}
                >
                  <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                    {testRecommendationSlices.remainder.map((rec) => (
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
                </ReportCollapsibleSection>
              ) : null}
            </>
          )}
        </div>
      ) : null}

      {databaseValidationVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {databaseValidationVm.title}
          </div>
          {databaseValidationVm.empty ? (
            emptyStateText(databaseValidationVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
                {databaseValidationVm.validation.summary}
              </div>
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {databaseValidationVm.validation.checks.map((check) => {
                  const connection = pickConnectionForCheck(dbConnections, check);
                  const approvalStatus = approvalStatusByCheckId[check.check_id] || check.approvalStatus || "PENDING";
                  const executePayload = connection
                    ? buildExecuteValidationPreviewPayload(check, connection, approvalStatus, t)
                    : null;
                  return (
                    <DatabaseValidationCheckCard
                      key={check.check_id}
                      check={check}
                      executePayload={executePayload}
                      executeBusy={executeBusyId === check.check_id}
                      labels={{
                        approvalRequiredLabel: databaseValidationVm.approvalRequiredLabel,
                        readOnlySafetyLabel: databaseValidationVm.readOnlySafetyLabel,
                        databaseTypeLabel: databaseValidationVm.databaseTypeLabel,
                        expectedResultLabel: databaseValidationVm.expectedResultLabel,
                        enabledLabel: databaseValidationVm.enabledLabel,
                        disabledLabel: databaseValidationVm.disabledLabel,
                        previewLabel: databaseValidationVm.previewLabel,
                        executeLabel: databaseValidationVm.executeLabel,
                      }}
                      onSimulateApprove={async () => {
                        setExecuteBusyId(check.check_id);
                        try {
                          const approval = await simulateDatabaseValidationApproval({
                            check_id: check.check_id,
                            status: "APPROVED",
                          });
                          setApprovalStatusByCheckId((prev) => ({
                            ...prev,
                            [check.check_id]: approval?.status || "APPROVED",
                          }));
                        } finally {
                          setExecuteBusyId(null);
                        }
                      }}
                      onExecute={async () => {
                        if (!connection) return null;
                        setExecuteBusyId(check.check_id);
                        try {
                          const result = await executeDatabaseValidation({
                            check,
                            connection_id: connection.connection_id,
                          });
                          return result;
                        } finally {
                          setExecuteBusyId(null);
                        }
                      }}
                    />
                  );
                })}
              </ul>
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {databaseValidationVm.futureFooter}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {apiContractIntelligenceVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {apiContractIntelligenceVm.title}
          </div>
          {apiContractIntelligenceVm.empty ? (
            emptyStateText(apiContractIntelligenceVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
                {apiContractIntelligenceVm.intel.summary}
              </div>
              <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
                <span className="badge badge-orange">
                  {apiContractIntelligenceVm.confidenceLabel}: {apiContractIntelligenceVm.intel.confidenceText}
                </span>
              </div>
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {apiContractIntelligenceVm.intel.contracts.map((contract) => (
                  <ApiContractCard
                    key={contract.contract_id}
                    contract={contract}
                    labels={{
                      contractChangesLabel: apiContractIntelligenceVm.contractChangesLabel,
                      riskAssessmentLabel: apiContractIntelligenceVm.riskAssessmentLabel,
                      previewLabel: apiContractIntelligenceVm.previewLabel,
                      endpointLabel: apiContractIntelligenceVm.endpointLabel,
                      methodLabel: apiContractIntelligenceVm.methodLabel,
                      versionLabel: apiContractIntelligenceVm.versionLabel,
                      riskLevelLabel: apiContractIntelligenceVm.riskLevelLabel,
                      changeCountLabel: apiContractIntelligenceVm.changeCountLabel,
                    }}
                  />
                ))}
              </ul>
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {apiContractIntelligenceVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {contractRiskAssessmentVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {contractRiskAssessmentVm.title}
          </div>
          {contractRiskAssessmentVm.empty ? (
            emptyStateText(contractRiskAssessmentVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
                {contractRiskAssessmentVm.report.summary}
              </div>
              <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
                <span className="badge badge-orange">
                  {contractRiskAssessmentVm.confidenceLabel}: {contractRiskAssessmentVm.report.confidenceText}
                </span>
              </div>
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {contractRiskAssessmentVm.report.assessments.map((assessment) => (
                  <ContractRiskAssessmentCard
                    key={assessment.assessment_id}
                    assessment={assessment}
                    labels={{
                      riskScoreLabel: contractRiskAssessmentVm.riskScoreLabel,
                      riskLevelLabel: contractRiskAssessmentVm.riskLevelLabel,
                      confidenceLabel: contractRiskAssessmentVm.confidenceLabel,
                      affectedJourneysLabel: contractRiskAssessmentVm.affectedJourneysLabel,
                      affectedModulesLabel: contractRiskAssessmentVm.affectedModulesLabel,
                      affectedTestsLabel: contractRiskAssessmentVm.affectedTestsLabel,
                      riskFactorsLabel: contractRiskAssessmentVm.riskFactorsLabel,
                      previewLabel: contractRiskAssessmentVm.previewLabel,
                    }}
                  />
                ))}
              </ul>
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {contractRiskAssessmentVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {dataJourneyValidationVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {dataJourneyValidationVm.title}
          </div>
          {dataJourneyValidationVm.empty ? (
            emptyStateText(dataJourneyValidationVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <div style={{ fontSize: 13, color: "var(--text-2)", lineHeight: 1.55, marginBottom: 12 }}>
                {dataJourneyValidationVm.report.summary}
              </div>
              <div style={{ display: "flex", gap: 8, flexWrap: "wrap", marginBottom: 12 }}>
                <span className="badge badge-orange">
                  {dataJourneyValidationVm.confidenceLabel}: {dataJourneyValidationVm.report.confidenceText}
                </span>
              </div>
              <ul style={{ margin: 0, padding: 0, listStyle: "none" }}>
                {dataJourneyValidationVm.report.journeys.map((journey) => (
                  <DataJourneyCard
                    key={journey.journey_id}
                    journey={journey}
                    labels={{
                      journeyStatusLabel: dataJourneyValidationVm.journeyStatusLabel,
                      completedStagesLabel: dataJourneyValidationVm.completedStagesLabel,
                      missingStagesLabel: dataJourneyValidationVm.missingStagesLabel,
                      inconsistentStagesLabel: dataJourneyValidationVm.inconsistentStagesLabel,
                      confidenceLabel: dataJourneyValidationVm.confidenceLabel,
                      previewLabel: dataJourneyValidationVm.previewLabel,
                      stageStatusLabel: dataJourneyValidationVm.stageStatusLabel,
                    }}
                  />
                ))}
              </ul>
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {dataJourneyValidationVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {enterpriseDependencyMapVm.show ? (
        <div style={{ marginBottom: 16 }}>
          <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
            {enterpriseDependencyMapVm.title}
          </div>
          {enterpriseDependencyMapVm.empty ? (
            emptyStateText(enterpriseDependencyMapVm.emptyMessage)
          ) : (
            <div
              style={{
                padding: "16px 18px",
                background: "var(--bg-2)",
                borderRadius: 8,
                border: "1px solid var(--border, rgba(255,255,255,0.08))",
              }}
            >
              <EnterpriseDependencyMapView vm={enterpriseDependencyMapVm} />
              <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
                {enterpriseDependencyMapVm.readOnlyNote}
              </p>
            </div>
          )}
        </div>
      ) : null}

      {jiraIssueIntelligenceVm.show ? (
        <div style={{ marginBottom: 16 }}>
          {!isCapabilityGated(jiraIssueIntelligenceVm) ? (
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
              {jiraIssueIntelligenceVm.title}
            </div>
          ) : null}
          <JiraIssueIntelligenceView vm={jiraIssueIntelligenceVm} />
        </div>
      ) : null}

      {servicenowIntelligenceVm.show ? (
        <div style={{ marginBottom: 16 }}>
          {!isCapabilityGated(servicenowIntelligenceVm) ? (
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
              {servicenowIntelligenceVm.title}
            </div>
          ) : null}
          <ServiceNowIntelligenceView vm={servicenowIntelligenceVm} />
        </div>
      ) : null}

      {qmetryIntegrationConsolidated ? (
        <div style={{ marginBottom: 16 }}>
          <CapabilityStateCard state={qmetryGroupState} />
          <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
            {coverageIntelligenceVm.readOnlyNote}
          </p>
        </div>
      ) : null}

      {!qmetryIntegrationConsolidated && coverageIntelligenceVm.show ? (
        <div style={{ marginBottom: 16 }}>
          {!isCapabilityGated(coverageIntelligenceVm) ? (
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
              {coverageIntelligenceVm.title}
            </div>
          ) : null}
          <CoverageIntelligenceView vm={coverageIntelligenceVm} />
          {!isCapabilityGated(coverageIntelligenceVm) ? (
            <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
              {coverageIntelligenceVm.readOnlyNote}
            </p>
          ) : null}
        </div>
      ) : null}

      {!qmetryIntegrationConsolidated && recommendationCorrelationVm.show ? (
        <div style={{ marginBottom: 16 }}>
          {!isCapabilityGated(recommendationCorrelationVm) ? (
            <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 8 }}>
              {recommendationCorrelationVm.title}
            </div>
          ) : null}
          <QMetryRecommendationView vm={recommendationCorrelationVm} />
          {!isCapabilityGated(recommendationCorrelationVm) ? (
            <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "12px 0 0", fontStyle: "italic" }}>
              {recommendationCorrelationVm.readOnlyNote}
            </p>
          ) : null}
        </div>
      ) : null}

      {recommendedActionsVm.show && !recommendedActionsVm.empty ? (
        <p style={{ fontSize: 12, color: "var(--text-3)", lineHeight: 1.5, margin: "0 0 16px", fontStyle: "italic" }}>
          {layoutVm.seeRecommendedActionsLabel}
        </p>
      ) : null}

      {approvalWorkflowVm.show ? (
        <ReportCollapsibleSection
          title={approvalWorkflowVm.title}
          defaultOpen={false}
        >
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
        </ReportCollapsibleSection>
      ) : null}
      </ReportCollapsibleSection>
      ) : null}

      {layoutVm.showNoisySections ? (
      <ReportCollapsibleSection
        title={layoutVm.technicalEvidenceTitle}
        defaultOpen={layoutVm.expandTechnicalEvidence}
      >
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

      {!layoutVm.lowConfidence && Array.isArray(report.investigation_plan) ? (
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

      {report.data_gaps?.length > 0 && !layoutVm.lowConfidence ? (
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

      {!testRecommendationsVm.show && vm.recommendedTests.show ? (
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

      </ReportCollapsibleSection>
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
