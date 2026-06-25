// src/pages/DashboardPage.jsx
/**
 * Vanya QA Intelligence — Executive Dashboard
 * Wired to live backend: /dashboard/summary, /dashboard/recent-runs,
 * /dashboard/recent-jobs, /failure-intelligence/summary
 */
import React, { useCallback, useMemo } from "react";
import { Link, useNavigate } from "react-router-dom";
import {
  runTest,
  apiErrorMessage,
} from "../api";
import { useLang } from "../i18n/LangContext";
import { useProject } from "../context/ProjectContext.jsx";
import { useDashboardData } from "../hooks/useDashboardData.js";
import ProjectHealthStrip from "../components/ProjectHealthStrip.jsx";
import OnboardingDashboardSection from "../components/onboarding/OnboardingDashboardSection.jsx";
import { buildOnboardingViewModel, shouldCollapseOnboardingDashboard } from "../utils/onboardingViewUtils.js";
import DashboardCommandHeader from "../components/dashboard/DashboardCommandHeader.jsx";
import CommandCenterView from "../components/dashboard/CommandCenterView.jsx";
import {
  buildCommandCenterHeaderViewModel,
  buildCommandCenterViewModel,
  buildNextRecommendedActionViewModel,
} from "../utils/commandCenterViewUtils.js";
import { buildQualityTrendViewModelFromApi } from "../utils/qualityTrendViewUtils.js";
import { buildScheduledReportViewModel } from "../utils/scheduledReportViewUtils.js";
import ExecutiveReportCenterView from "../components/executive-reports/ExecutiveReportCenterView.jsx";
import { buildEarlyDegradationViewModelFromApi } from "../utils/earlyDegradationViewUtils.js";
import ReportDeliveryCenter from "../components/executive-reports/ReportDeliveryCenter.jsx";
import { buildReleaseReadinessViewModel } from "../utils/releaseReadinessViewUtils.js";
import ValueDashboardView from "../components/value-dashboard/ValueDashboardView.jsx";
import { buildValueDashboardViewModel } from "../utils/valueDashboardViewUtils.js";
import ExecutiveImpactView from "../components/executive-impact/ExecutiveImpactView.jsx";
import { buildExecutiveImpactViewModel } from "../utils/executiveImpactViewUtils.js";
import BusinessRiskView from "../components/business-risk/BusinessRiskView.jsx";
import { buildBusinessRiskViewModel } from "../utils/businessRiskViewUtils.js";
import CoverageIntelligenceView from "../components/coverage-intelligence/CoverageIntelligenceView.jsx";
import QMetryRecommendationView from "../components/qmetry-recommendations/QMetryRecommendationView.jsx";
import { buildCoverageOverviewViewModel } from "../utils/qmetryCoverageViewUtils.js";
import { buildRecommendedTestsOverviewViewModel } from "../utils/qmetryRecommendationViewUtils.js";
import ServiceNowIntelligenceView from "../components/servicenow-intelligence/ServiceNowIntelligenceView.jsx";
import { buildServiceNowIntelligenceOverviewViewModel } from "../utils/servicenowIntelligenceViewUtils.js";
import { buildReportDeliveryViewModel } from "../utils/reportDeliveryViewUtils.js";
import { buildPlatformObservabilityViewModel } from "../utils/platformObservabilityViewUtils.js";
import DashboardSectionState from "../components/dashboard/DashboardSectionState.jsx";
import DashboardIntegrationSlot from "../components/dashboard/DashboardIntegrationSlot.jsx";
import CapabilityStateCard from "../components/capability-state/CapabilityStateCard.jsx";
import {
  buildQMetryIntegrationGroupState,
  shouldConsolidateQMetryIntegration,
} from "../utils/capabilityStateViewUtils.js";
import { formatExecutionStatus } from "../utils/executionStatusDisplayUtils.js";
import ColdProjectGuidance from "../components/dashboard/ColdProjectGuidance.jsx";
import ExecutiveDashboardV2 from "../components/dashboard/ExecutiveDashboardV2.jsx";
import { buildExecutiveDashboardV2ViewModel } from "../utils/executiveDashboardV2ViewUtils.js";
import {
  buildDashboardSectionState,
  buildColdProjectGuidanceViewModel,
  DASHBOARD_SECTION_CTA,
  isColdProject,
} from "../utils/dashboardSectionStateUtils.js";
import { buildExecutiveBriefViewModel } from "../utils/executiveBriefViewUtils.js";
import { buildExecutiveRiskBriefViewModel } from "../utils/executiveRiskBriefViewUtils.js";
import {
  fmtDate,
  fmtMs,
  statusClass,
  isFailStatus,
  filterDashboardRuns,
  findRecentFailedRunForTest,
  deriveSystemRibbonState,
} from "../utils/dashboardHelpers.js";
import Row from "../components/dashboard/Row.jsx";
import KpiCard from "../components/dashboard/KpiCard.jsx";
import SectionCard from "../components/dashboard/SectionCard.jsx";
import WidgetCard from "../components/dashboard/WidgetCard.jsx";
import SystemStatusRibbon from "../components/dashboard/SystemStatusRibbon.jsx";
import TopProblemCard from "../components/dashboard/TopProblemCard.jsx";
import ProjectCapacitySection from "../components/dashboard/ProjectCapacitySection.jsx";
import PassRateTrendChart from "../components/dashboard/PassRateTrendChart.jsx";
import CoverageDonutChart from "../components/dashboard/CoverageDonutChart.jsx";
import FailureDistributionChart from "../components/dashboard/FailureDistributionChart.jsx";
import RiskSummaryCard from "../components/dashboard/RiskSummaryCard.jsx";
import DashboardSparseHints from "../components/dashboard/DashboardSparseHints.jsx";
import { SkeletonTable } from "../components/ui/Skeleton.jsx";
import {
  buildDashboardSparseHintsViewModel,
  formatDashboardCount,
  isDashboardDataSparse,
} from "../utils/dashboardKpiDisplayUtils.js";
import { formatTestDisplayNameWithMeta } from "../utils/humanizeTestNameUtils.js";

// ── Main component ────────────────────────────────────────────────────────────


export default function DashboardPage() {
  const { t } = useLang();
  const navigate = useNavigate();
  const { currentProject, projects } = useProject();
  const projectId = currentProject?.id;

  const {
    summary,
    recentRuns,
    recentJobs,
    fi,
    analytics,
    execStatus,
    loading,
    summaryError,
    runsError,
    jobsError,
    execError,
    fiLoading,
    fiError,
    analyticsLoading,
    analyticsError,
    lastRefresh,
    recentRunsFilter,
    setRecentRunsFilter,
    rerunBusyRunId,
    setRerunBusyRunId,
    runInlineError,
    setRunInlineError,
    topProblemBusy,
    setTopProblemBusy,
    topProblemError,
    setTopProblemError,
    hasKnowledge,
    qualityTrends,
    earlyDegradation,
    valueDashboard,
    executiveImpact,
    businessRisk,
    platformObservability,
    servicenowIntelligence,
    coverageOverview,
    recommendedTests,
    executiveRiskBrief,
    intelErrors,
    intelLoading,
    load,
  } = useDashboardData(projectId, t);

  const s = summary || {};
  const dataSparse = !loading && isDashboardDataSparse(s);
  const sparseHintsVm = useMemo(
    () => buildDashboardSparseHintsViewModel(dataSparse, t),
    [dataSparse, t],
  );
  // QA FINAL — numeric pass_rate only (avoids NaN / string edge cases breaking toFixed / delta)
  const passRateRaw = s.pass_rate;
  const passRateNum =
    passRateRaw == null || passRateRaw === "" ? null : Number(passRateRaw);
  const passRateValid = passRateNum != null && !Number.isNaN(passRateNum);
  const passRate = passRateValid ? `${passRateNum.toFixed(1)}%` : "—";

  const onboardingVm = useMemo(
    () => buildOnboardingViewModel(s.onboarding ?? null, t, { projectId }),
    [s.onboarding, t, projectId],
  );

  const scheduledReportVm = useMemo(
    () => buildScheduledReportViewModel(s.report_center ?? null, t),
    [s.report_center, t],
  );

  const releaseReadinessVm = useMemo(
    () => buildReleaseReadinessViewModel(s, t),
    [s, t],
  );

  const valueDashboardVm = useMemo(
    () => buildValueDashboardViewModel(valueDashboard, t),
    [valueDashboard, t],
  );

  const platformObservabilityVm = useMemo(
    () => buildPlatformObservabilityViewModel(platformObservability, t),
    [platformObservability, t],
  );

  const executiveImpactVm = useMemo(
    () => buildExecutiveImpactViewModel(executiveImpact, t, {
      loadError: intelErrors.executiveImpact || "",
    }),
    [executiveImpact, intelErrors.executiveImpact, t],
  );

  const businessRiskVm = useMemo(
    () => buildBusinessRiskViewModel(businessRisk, t),
    [businessRisk, t],
  );

  const servicenowIntelligenceVm = useMemo(
    () => buildServiceNowIntelligenceOverviewViewModel(servicenowIntelligence, t),
    [servicenowIntelligence, t],
  );

  const coverageOverviewVm = useMemo(
    () => buildCoverageOverviewViewModel(coverageOverview, t),
    [coverageOverview, t],
  );

  const recommendedTestsVm = useMemo(
    () => buildRecommendedTestsOverviewViewModel(recommendedTests, t),
    [recommendedTests, t],
  );

  const reportDeliveryVm = useMemo(
    () => buildReportDeliveryViewModel({ readiness: null, t }),
    [t],
  );

  const qualityTrendVm = useMemo(
    () => buildQualityTrendViewModelFromApi(qualityTrends, t),
    [qualityTrends, t],
  );

  const earlyDegradationVm = useMemo(
    () => buildEarlyDegradationViewModelFromApi(earlyDegradation, t, { quality_trends: qualityTrends }),
    [earlyDegradation, qualityTrends, t],
  );

  const coldProjectGuidanceVm = useMemo(
    () => buildColdProjectGuidanceViewModel(
      projectId && isColdProject({
        releaseReadiness: s,
        valueDashboard,
        businessRisk,
        executiveImpact,
      }),
      t,
    ),
    [projectId, s, valueDashboard, businessRisk, executiveImpact, t],
  );

  const executiveBriefVm = useMemo(
    () => buildExecutiveBriefViewModel({
      releaseReadinessVm,
      businessRiskVm,
      executiveImpactVm,
      valueDashboardVm,
      platformObservabilityVm,
      onboardingVm,
      hasKnowledge,
      totalRuns: s.total_runs ?? 0,
      fi,
      passRateValid,
      passRateNum,
      t,
    }),
    [
      releaseReadinessVm,
      businessRiskVm,
      executiveImpactVm,
      valueDashboardVm,
      platformObservabilityVm,
      onboardingVm,
      hasKnowledge,
      s.total_runs,
      fi,
      passRateValid,
      passRateNum,
      t,
    ],
  );

  const executiveRiskBriefVm = useMemo(
    () => buildExecutiveRiskBriefViewModel(executiveRiskBrief, t),
    [executiveRiskBrief, t],
  );

  const isOperationalDashboard = Boolean(projectId && onboardingVm.isComplete);
  const collapseOnboarding = shouldCollapseOnboardingDashboard(s.onboarding ?? null, onboardingVm);

  const commandCenterHeaderVm = useMemo(
    () => (isOperationalDashboard
      ? buildCommandCenterHeaderViewModel({
          project: currentProject,
          onboardingChecklist: s.onboarding ?? null,
          summary: s,
          fi,
          passRateValid,
          passRateNum,
          lastRefresh,
          t,
          formatTimestamp: fmtDate,
        })
      : null),
    [isOperationalDashboard, currentProject, s, fi, passRateValid, passRateNum, lastRefresh, t],
  );

  const commandCenterVm = useMemo(
    () => (isOperationalDashboard
      ? buildCommandCenterViewModel({
          summary: s,
          fi,
          hasKnowledge,
          passRateValid,
          passRateNum,
          loading,
          t,
        })
      : null),
    [isOperationalDashboard, s, fi, hasKnowledge, passRateValid, passRateNum, loading, t],
  );

  const nextRecommendedActionVm = useMemo(
    () => (isOperationalDashboard
      ? buildNextRecommendedActionViewModel({
          hasKnowledge,
          summary: s,
          fi,
          passRateValid,
          passRateNum,
          businessRiskVm,
          loading,
          t,
        })
      : null),
    [isOperationalDashboard, hasKnowledge, s, fi, passRateValid, passRateNum, businessRiskVm, loading, t],
  );

  const executiveDashboardV2Vm = useMemo(
    () => (projectId
      ? buildExecutiveDashboardV2ViewModel({
          releaseReadinessVm,
          executiveBriefVm,
          earlyDegradationVm,
          qualityTrendVm,
          platformObservabilityVm,
          businessRiskVm,
          executiveRiskBriefVm,
          executiveImpactVm,
          nextRecommendedActionVm,
          totalRuns: s.total_runs ?? 0,
          loading,
          t,
        })
      : null),
    [
      projectId,
      releaseReadinessVm,
      executiveBriefVm,
      earlyDegradationVm,
      qualityTrendVm,
      platformObservabilityVm,
      businessRiskVm,
      executiveRiskBriefVm,
      executiveImpactVm,
      nextRecommendedActionVm,
      s.total_runs,
      loading,
      t,
    ],
  );

  const executiveImpactSection = useMemo(
    () => buildDashboardSectionState({
      data: executiveImpact,
      loadError: intelErrors.executiveImpact,
      loading: intelLoading.executiveImpact,
      empty: executiveImpactVm.empty,
      capabilityState: executiveImpactVm.capabilityState,
      emptyCta: DASHBOARD_SECTION_CTA.incidents,
      hideWhenEmpty: true,
      t,
    }),
    [executiveImpact, intelErrors.executiveImpact, intelLoading.executiveImpact, executiveImpactVm, t],
  );

  const valueDashboardSection = useMemo(
    () => buildDashboardSectionState({
      data: valueDashboard,
      loadError: intelErrors.valueDashboard,
      loading: intelLoading.valueDashboard,
      empty: valueDashboardVm.empty,
      emptyMessage: valueDashboardVm.emptyMessage,
      emptyCta: DASHBOARD_SECTION_CTA.incidents,
      hideWhenEmpty: true,
      t,
    }),
    [valueDashboard, intelErrors.valueDashboard, intelLoading.valueDashboard, valueDashboardVm, t],
  );

  const businessRiskSection = useMemo(
    () => buildDashboardSectionState({
      data: businessRisk,
      loadError: intelErrors.businessRisk,
      loading: intelLoading.businessRisk,
      empty: businessRiskVm.empty,
      capabilityState: businessRiskVm.capabilityState,
      emptyCta: DASHBOARD_SECTION_CTA.incidents,
      hideWhenEmpty: true,
      t,
    }),
    [businessRisk, intelErrors.businessRisk, intelLoading.businessRisk, businessRiskVm, t],
  );

  const platformObservabilitySection = useMemo(
    () => buildDashboardSectionState({
      data: platformObservability,
      loadError: intelErrors.platformObservability,
      loading: intelLoading.platformObservability,
      empty: platformObservabilityVm.empty,
      emptyMessage: platformObservabilityVm.emptyMessage,
      emptyCta: DASHBOARD_SECTION_CTA.settings,
      t,
    }),
    [
      platformObservability,
      intelErrors.platformObservability,
      intelLoading.platformObservability,
      platformObservabilityVm,
      t,
    ],
  );

  const coverageOverviewSection = useMemo(
    () => buildDashboardSectionState({
      data: coverageOverview,
      loadError: intelErrors.coverageOverview,
      loading: intelLoading.coverageOverview,
      empty: coverageOverviewVm.empty,
      capabilityState: coverageOverviewVm.capabilityState,
      emptyCta: DASHBOARD_SECTION_CTA.integrations,
      t,
    }),
    [coverageOverview, intelErrors.coverageOverview, intelLoading.coverageOverview, coverageOverviewVm, t],
  );

  const recommendedTestsSection = useMemo(
    () => buildDashboardSectionState({
      data: recommendedTests,
      loadError: intelErrors.recommendedTests,
      loading: intelLoading.recommendedTests,
      empty: recommendedTestsVm.empty,
      capabilityState: recommendedTestsVm.showContent ? null : recommendedTestsVm.capabilityState,
      emptyCta: DASHBOARD_SECTION_CTA.integrations,
      hideWhenEmpty: true,
      t,
    }),
    [
      recommendedTests,
      intelErrors.recommendedTests,
      intelLoading.recommendedTests,
      recommendedTestsVm,
      t,
    ],
  );

  const servicenowSection = useMemo(
    () => buildDashboardSectionState({
      data: servicenowIntelligence,
      loadError: intelErrors.servicenowIntelligence,
      loading: intelLoading.servicenowIntelligence,
      empty: servicenowIntelligenceVm.empty,
      capabilityState: servicenowIntelligenceVm.capabilityState,
      emptyCta: DASHBOARD_SECTION_CTA.integrations,
      t,
    }),
    [
      servicenowIntelligence,
      intelErrors.servicenowIntelligence,
      intelLoading.servicenowIntelligence,
      servicenowIntelligenceVm,
      t,
    ],
  );

  const filteredRecentRuns = useMemo(
    () => filterDashboardRuns(recentRuns, recentRunsFilter),
    [recentRuns, recentRunsFilter],
  );

  const handleDashboardRerun = useCallback(
    async (r) => {
      const tcId = r.test_id || r.test_case_id;
      if (!tcId) return;
      setRunInlineError("");
      const busyKey = r.run_id || tcId;
      setRerunBusyRunId(busyKey);
      try {
        await runTest(tcId, { headless: true });
        await load();
      } catch (e) {
        setRunInlineError(apiErrorMessage(e) || t("dash.rerun.error"));
      } finally {
        setRerunBusyRunId(null);
      }
    },
    [load, t],
  );

  const openRunEvidence = useCallback(
    (r) => {
      const id = String(r.run_id || "").trim();
      if (!id) return;
      navigate(`/evidence/run/${encodeURIComponent(id)}`);
    },
    [navigate],
  );

  const handleTopProblemRerun = useCallback(
    async (testCaseId) => {
      const tc = String(testCaseId || "").trim();
      if (!tc) return;
      setTopProblemError("");
      setTopProblemBusy(true);
      try {
        await runTest(tc, { headless: true });
        await load();
      } catch (e) {
        setTopProblemError(apiErrorMessage(e) || t("dash.rerun.error"));
      } finally {
        setTopProblemBusy(false);
      }
    },
    [load, t],
  );

  const handleTopProblemView = useCallback(
    (tf) => {
      const id = tf?.test_case_id;
      const run = findRecentFailedRunForTest(recentRuns, id);
      const rid = String(run?.run_id || "").trim();
      if (rid) {
        navigate(`/evidence/run/${encodeURIComponent(rid)}`);
      } else {
        navigate("/runs");
      }
    },
    [navigate, recentRuns],
  );

  // KPI "Total Ejecuciones" — subtexto dinámico con solo los estados > 0
  const runsSubParts = [];
  if ((s.pass_runs  ?? 0) > 0) runsSubParts.push(`${s.pass_runs}  ${t("dash.kpi.pass")}`);
  if ((s.fail_runs  ?? 0) > 0) runsSubParts.push(`${s.fail_runs}  ${t("dash.kpi.fail")}`);
  if ((s.error_runs ?? 0) > 0) runsSubParts.push(`${s.error_runs} ${t("dash.kpi.error")}`);
  const runsSub = runsSubParts.length > 0 ? runsSubParts.join(" · ") : t("dash.kpi.no_runs");

  // KPI "Tasa de Éxito" — subtexto con total_runs + nota discreta si historial < 5
  const totalRunsN  = s.total_runs ?? 0;
  const passRateSub = (
    <>
      {`${t("dash.kpi.based_on")} ${totalRunsN} ${t("dash.kpi.runs_unit")}`}
      {totalRunsN < 5 && (
        <span style={{ fontStyle: "italic", color: "var(--text-3)", marginLeft: 4 }}>
          {`· ${t("dash.risk.limited_history")}`}
        </span>
      )}
    </>
  );

  // MEJORA #1 — benchmark vs 80% target (delta in percentage points)
  const passBench = 80;
  const passRateDeltaPp = passRateValid ? passRateNum - passBench : null;
  const passRateBenchmarkExtra =
    !loading && passRateValid && passRateDeltaPp != null ? (
      // MEJORA #1 — benchmark line (does not replace main pass rate value)
      <div
        style={{
          fontSize: 11,
          fontWeight: 500,
          color: "var(--text-2)",
          lineHeight: 1.35,
          display: "flex",
          flexWrap: "wrap",
          alignItems: "baseline",
          gap: "0 8px",
          marginTop: 4,
        }}
      >
        <span>{t("dash.kpi.meta_target")}</span>
        <span
          style={{
            fontVariantNumeric: "tabular-nums",
            color:
              passRateNum >= passBench
                ? "var(--green)"
                : passRateNum < 60
                  ? "var(--red)"
                  : "var(--orange)",
          }}
        >
          {passRateDeltaPp >= 0 ? "+" : ""}
          {passRateDeltaPp.toFixed(1)} pp
        </span>
      </div>
    ) : null;

  // MEJORA #2 — hide workers + jobs KPIs when both are zero (after load)
  const idleExecKpis =
    !loading && (s.active_workers ?? 0) === 0 && (s.total_jobs ?? 0) === 0;

  const systemRibbon = useMemo(
    () =>
      deriveSystemRibbonState({
        loading,
        s,
        fi,
        passRateValid,
        passRateNum,
        idleExecKpis,
        t,
      }),
    [loading, s, fi, passRateValid, passRateNum, idleExecKpis, t],
  );

  const showTopProblemCard =
    analyticsLoading || !!(analytics && Array.isArray(analytics.top_failures) && analytics.top_failures[0]);

  function renderDashboardMetricsSection() {
    return (
      <>
        {summaryError && !loading ? (
          <div className="alert alert-error" style={{ marginBottom: 20, fontSize: 13 }}>
            {summaryError}{" "}
            <button type="button" className="btn btn-secondary btn-sm" style={{ marginLeft: 8 }} onClick={load}>
              {t("dash.refresh")}
            </button>
          </div>
        ) : null}

        <DashboardSparseHints vm={sparseHintsVm} />

        <div className="kpi-grid" style={{ marginBottom: 28 }}>
          <KpiCard loading={loading} label={t("dash.kpi.total_tests")} value={formatDashboardCount(s.total_test_cases, { loading, sparse: dataSparse })} sub={loading ? "" : `${s.active_test_cases ?? "—"} ${t("dash.kpi.active")}`} icon="☰" />
          <KpiCard loading={loading} label={t("dash.kpi.total_runs")} value={formatDashboardCount(s.total_runs, { loading, sparse: dataSparse })} sub={loading ? "" : runsSub} icon="▶" />
          <KpiCard
            loading={loading}
            label={t("dash.kpi.pass_rate")}
            value={loading ? null : (dataSparse || !passRateValid ? "—" : passRate)}
            valueExtra={passRateBenchmarkExtra}
            sub={loading ? "" : passRateSub}
            accent={
              !loading && passRateValid && !dataSparse
                ? passRateNum >= passBench
                  ? "var(--green)"
                  : "var(--orange)"
                : undefined
            }
            icon="✓"
          />
          {!idleExecKpis && (
            <>
              <KpiCard loading={loading} label={t("dash.kpi.active_workers")} value={formatDashboardCount(s.active_workers, { loading })} sub={loading ? "" : `${s.queue_depth ?? 0} ${t("dash.kpi.queued")}`} icon="⚙" />
              <KpiCard loading={loading} label={t("dash.kpi.total_jobs")} value={formatDashboardCount(s.total_jobs, { loading })} sub={loading ? "" : `${s.running_jobs ?? 0} ${t("dash.kpi.running")} · ${s.queued_jobs ?? 0} ${t("dash.kpi.queued")}`} icon="◈" />
            </>
          )}
          <KpiCard loading={loading} label={t("dash.kpi.ui_tests")} value={formatDashboardCount(s.total_ui_tests, { loading, sparse: dataSparse })} sub={loading ? "" : t("dash.kpi.in_catalog")} icon="◻" />
          <KpiCard loading={loading} label={t("dash.kpi.api_tests")} value={formatDashboardCount(s.total_api_tests, { loading, sparse: dataSparse })} sub={loading ? "" : t("dash.kpi.in_catalog")} icon="⌥" />
          {fi && <KpiCard label={t("dash.kpi.flaky_tests")} value={fi.flaky_tests_count ?? 0} sub={`${fi.total_clusters ?? 0} ${t("dash.kpi.clusters")}`} accent={fi.flaky_tests_count > 0 ? "var(--orange)" : undefined} icon="⚠" />}
          {idleExecKpis && (
            <div
              className="kpi-card dash-kpi-idle-cta"
              style={{
                gridColumn: "1 / -1",
                flexDirection: "row",
                alignItems: "center",
                justifyContent: "space-between",
                flexWrap: "wrap",
                gap: 12,
                minHeight: 72,
              }}
            >
              <span style={{ fontSize: 13, color: "var(--text-2)", fontWeight: 500 }}>{t("dash.kpi.idle_exec_title")}</span>
              <Link to="/batch" style={{ fontSize: 13, color: "var(--accent)", fontWeight: 600, textDecoration: "none", whiteSpace: "nowrap" }}>
                {t("dash.kpi.idle_exec_link")}
              </Link>
            </div>
          )}
        </div>

        {showTopProblemCard ? (
          <TopProblemCard
            analytics={analyticsLoading ? null : analytics}
            loading={analyticsLoading}
            t={t}
            onRerun={handleTopProblemRerun}
            onView={handleTopProblemView}
            busy={topProblemBusy}
            error={topProblemError}
          />
        ) : null}

        {!loading && (
          <ProjectCapacitySection execStatus={execStatus} execError={execError} projects={projects} t={t} />
        )}

        <div style={{ display: "grid", gridTemplateColumns: "minmax(0,2fr) minmax(0,1fr)", gap: 24, marginBottom: 24 }}>
          <WidgetCard title={t("dash.trends.title")} subtitle={`${t("dash.trends.subtitle")} · ${recentRuns.length}`}>
            <PassRateTrendChart runs={recentRuns} loading={loading} t={t} />
          </WidgetCard>
          <WidgetCard title={t("dash.coverage.title")} subtitle={t("dash.coverage.catalog_subtitle")}>
            <CoverageDonutChart summary={summary} loading={loading} t={t} />
          </WidgetCard>
        </div>

        <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 24, marginBottom: 28 }}>
          <WidgetCard title={t("dash.failures.title")} subtitle={t("dash.failures.subtitle")}>
            <FailureDistributionChart fi={fi} loading={fiLoading} t={t} />
          </WidgetCard>
          <WidgetCard title={t("dash.risk.title")} subtitle={t("dash.risk.subtitle")}>
            <RiskSummaryCard
              summary={summary}
              fi={fi}
              loading={loading}
              sectionError={summaryError || fiError}
              t={t}
            />
          </WidgetCard>
        </div>

        <div style={{ display: "flex", flexDirection: "column", gap: 28, alignItems: "stretch" }}>
          <div style={{ display: "flex", flexDirection: "column", gap: 24 }}>
            <SectionCard title={t("dash.recent_runs")} link="/runs" linkLabel={t("dash.recent_runs.link")}>
              {runsError && !recentRuns.length ? (
                <div style={{ padding: "20px", color: "var(--red-text)", fontSize: 13 }}>{runsError}</div>
              ) : loading && !recentRuns.length ? (
                <SkeletonTable rows={5} cols={5} />
              ) : recentRuns.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  {t("dash.no_runs")} <Link to="/catalog" style={{ color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>{t("dash.run_a_test")}</Link>
                </div>
              ) : (
                <>
                  <div
                    className="dash-recent-runs-filters"
                    role="toolbar"
                    aria-label={t("dash.runs.filter.toolbar_aria")}
                    style={{
                      padding: "12px 16px",
                      borderBottom: "1px solid var(--border)",
                      display: "flex",
                      flexWrap: "wrap",
                      gap: 8,
                      alignItems: "center",
                      minHeight: 48,
                      boxSizing: "border-box",
                    }}
                  >
                    {["all", "failed", "passed", "today"].map((key) => (
                      <button
                        key={key}
                        type="button"
                        className={
                          recentRunsFilter === key ? "dash-filter-chip dash-filter-chip-active" : "dash-filter-chip dash-filter-chip-outline"
                        }
                        aria-pressed={recentRunsFilter === key}
                        onClick={() => {
                          setRunInlineError("");
                          setRecentRunsFilter(key);
                        }}
                      >
                        {t(`dash.runs.filter.${key}`)}
                      </button>
                    ))}
                  </div>
                  {runInlineError && (
                    <div style={{ padding: "10px 16px", fontSize: 12, color: "var(--red)", borderBottom: "1px solid var(--border)" }}>
                      {runInlineError}
                    </div>
                  )}
                  {filteredRecentRuns.length === 0 ? (
                    <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>{t("dash.runs.filter.empty")}</div>
                  ) : (
                    <table className="data-table dash-recent-runs-table">
                      <thead><tr>
                        <th>{t("dash.col.test_case")}</th>
                        <th>{t("dash.col.status")}</th>
                        <th>{t("dash.col.duration")}</th>
                        <th>{t("dash.col.executed")}</th>
                        <th className="dash-recent-actions-col">{t("dash.col.actions")}</th>
                      </tr></thead>
                      <tbody>
                        {filteredRecentRuns.slice(0, 8).map((r, i) => {
                          const showFailActions = isFailStatus(r.status);
                          const busyKey = r.run_id || r.test_id || r.test_case_id;
                          const busy = rerunBusyRunId && busyKey && rerunBusyRunId === busyKey;
                          const testMeta = formatTestDisplayNameWithMeta({
                            testId: r.test_id || r.test_case_id,
                            testName: r.test_name,
                          }, t);
                          return (
                            <tr key={r.run_id || i} className="dash-recent-runs-row">
                              <td style={{ fontWeight: 500, fontSize: 12 }} title={testMeta.showTechnicalId ? testMeta.technicalId : undefined}>
                                {testMeta.display || "—"}
                              </td>
                              <td><span className={`badge ${statusClass(r.status)}`}>{formatExecutionStatus(r.status, t)}</span></td>
                              <td style={{ fontSize: 12, color: "var(--text-2)" }}>{fmtMs(r.duration_ms)}</td>
                              <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>
                                {fmtDate(r.started_at || r.executed_at)}
                              </td>
                              <td className="dash-recent-actions-cell" style={{ textAlign: "right", whiteSpace: "nowrap" }}>
                                {showFailActions ? (
                                  <span className="dash-recent-runs-actions">
                                    <button
                                      type="button"
                                      className="btn btn-ghost btn-sm"
                                      style={{
                                        padding: "2px 8px",
                                        fontSize: 11,
                                        minHeight: 28,
                                        opacity: busy ? 0.65 : 1,
                                      }}
                                      aria-busy={busy ? true : undefined}
                                      disabled={busy || !(r.test_id || r.test_case_id)}
                                      onClick={() => handleDashboardRerun(r)}
                                    >
                                      {busy ? t("dash.rerun.running") : t("dash.rerun.cta")}
                                    </button>
                                    <button
                                      type="button"
                                      className="btn btn-ghost btn-sm"
                                      style={{ padding: "2px 8px", fontSize: 11, minHeight: 28, marginLeft: 4 }}
                                      disabled={!r.run_id}
                                      onClick={() => openRunEvidence(r)}
                                    >
                                      {t("dash.runs.view_run")}
                                    </button>
                                  </span>
                                ) : null}
                              </td>
                            </tr>
                          );
                        })}
                      </tbody>
                    </table>
                  )}
                </>
              )}
            </SectionCard>

            <SectionCard title={t("dash.recent_jobs")} link="/batch" linkLabel={t("dash.recent_jobs.link")}>
              {jobsError && !recentJobs.length ? (
                <div style={{ padding: "20px", color: "var(--red-text)", fontSize: 13 }}>{jobsError}</div>
              ) : loading && !recentJobs.length ? (
                <SkeletonTable rows={4} cols={4} />
              ) : recentJobs.length === 0 ? (
                <div style={{ padding: "20px", color: "var(--text-3)", fontSize: 13 }}>
                  {t("dash.no_jobs")} <Link to="/batch" style={{ color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>{t("dash.run_batch")}</Link>
                </div>
              ) : (
                <table className="data-table">
                  <thead><tr>
                    <th>{t("dash.col.job_id")}</th>
                    <th>{t("dash.col.status")}</th>
                    <th style={{ width: 120 }}>{t("dash.col.progress")}</th>
                    <th>{t("dash.col.created")}</th>
                  </tr></thead>
                  <tbody>
                    {recentJobs.slice(0, 6).map((j, i) => (
                      <tr key={j.job_id || i}>
                        <td style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>{(j.job_id || "").slice(0, 12)}…</td>
                        <td><span className={`badge ${statusClass(j.status)}`}>{formatExecutionStatus(j.status, t)}</span></td>
                        <td style={{ fontSize: 12, color: "var(--text-2)" }}>
                          {j.passed_count ?? 0}✓ {j.failed_count ?? 0}✗ / {j.total_count ?? 0}
                        </td>
                        <td style={{ fontSize: 11, color: "var(--text-3)", whiteSpace: "nowrap" }}>{fmtDate(j.created_at)}</td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </SectionCard>
          </div>

          {fi && (
            <div className="card">
              <div className="section-title">{t("dash.insights_summary")}</div>
              <div style={{ display: "flex", flexDirection: "column", gap: 10, marginTop: 6 }}>
                <Row label={t("dash.fi.flaky_tests")} value={fi.flaky_tests_count ?? 0} accent={fi.flaky_tests_count > 0 ? "var(--orange)" : undefined} />
                <Row label={t("dash.fi.clusters")} value={fi.total_clusters ?? 0} />
                <Row label={t("dash.fi.regressions")} value={fi.recurrent_regressions_count ?? 0} accent={fi.recurrent_regressions_count > 0 ? "var(--red)" : undefined} />
                {fi.notes && (
                  <div style={{ fontSize: 12, color: "var(--text-2)", marginTop: 4, lineHeight: 1.5, borderTop: "1px solid var(--border)", paddingTop: 8 }}>
                    {fi.notes}
                  </div>
                )}
              </div>
              <div style={{ marginTop: 12 }}>
                <Link to="/insights" style={{ fontSize: 12, color: "var(--accent)", fontWeight: 500, textDecoration: "none" }}>
                  {t("dash.view_insights")}
                </Link>
              </div>
            </div>
          )}
        </div>

        <div className="card" style={{ padding: "20px 24px", marginTop: 32 }}>
          <div style={{ marginBottom: 18, display: "flex", alignItems: "baseline", justifyContent: "space-between", gap: 16, flexWrap: "wrap" }}>
            <div>
              <div className="section-title" style={{ margin: 0 }}>{t("dash.analytics.title")}</div>
              <div style={{ fontSize: 11, color: "var(--text-3)", marginTop: 2 }}>{t("dash.analytics.subtitle")}</div>
            </div>
          </div>

          {analyticsError && !analytics ? (
            <div style={{ fontSize: 13, color: "var(--red-text)" }}>{analyticsError}</div>
          ) : null}
          {!analytics && !analyticsError ? (
            <div style={{ fontSize: 13, color: "var(--text-3)" }}>
              {analyticsLoading ? t("dash.analytics.loading") : t("dash.analytics.empty")}
            </div>
          ) : null}

          {analytics && (
            <>
              <div style={{ display: "grid", gridTemplateColumns: "repeat(auto-fit, minmax(120px, 1fr))", gap: 14, marginBottom: 24 }}>
                {[
                  { label: t("dash.analytics.runs_7d"), value: analytics.summary.runs_last_7_days },
                  { label: t("dash.analytics.runs_30d"), value: analytics.summary.runs_last_30_days },
                  { label: t("dash.analytics.pass_rate"), value: `${analytics.summary.pass_rate}%`,
                    accent: analytics.summary.pass_rate >= 80 ? "var(--green)" : "var(--orange)" },
                  { label: t("dash.analytics.avg_duration"), value: fmtMs(analytics.summary.avg_duration_ms) },
                ].map(({ label, value, accent }) => (
                  <div key={label} style={{
                    background: "var(--surface-2, var(--bg))",
                    border: "1px solid var(--border)",
                    borderRadius: 6,
                    padding: "8px 12px",
                    textAlign: "center",
                  }}>
                    <div style={{ fontSize: 18, fontWeight: 600, color: accent || "var(--text-1)", lineHeight: 1.1, marginBottom: 4 }}>
                      {value ?? "—"}
                    </div>
                    <div style={{ fontSize: 10, fontWeight: 400, color: "var(--text-4)", lineHeight: 1.2 }}>{label}</div>
                  </div>
                ))}
              </div>

              <div style={{ display: "grid", gridTemplateColumns: "minmax(0,3fr) minmax(0,2fr)", gap: 24 }}>
                <div>
                  <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-4)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 10 }}>
                    {t("dash.analytics.top_failures")}
                  </div>
                  {analytics.top_failures.length === 0 ? (
                    <div style={{ fontSize: 12, color: "var(--text-3)", fontStyle: "italic" }}>
                      {t("dash.analytics.no_failures")}
                    </div>
                  ) : (
                    <table className="data-table" style={{ fontSize: 12 }}>
                      <thead><tr>
                        <th>{t("dash.analytics.col.test")}</th>
                        <th style={{ width: 54, textAlign: "right" }}>{t("dash.analytics.col.runs")}</th>
                        <th style={{ width: 64, textAlign: "right" }}>{t("dash.analytics.col.failures")}</th>
                        <th style={{ width: 76, textAlign: "right" }}>{t("dash.analytics.col.pass_rate")}</th>
                      </tr></thead>
                      <tbody>
                        {analytics.top_failures.slice(0, 7).map((tf) => (
                          <tr key={tf.test_case_id}>
                            <td>
                              <div style={{ fontFamily: "monospace", fontSize: 11, color: "var(--text-2)" }}>{tf.test_case_id}</div>
                              {tf.test_name !== tf.test_case_id && (
                                <div style={{ fontSize: 10, color: "var(--text-3)", marginTop: 1 }}>{tf.test_name}</div>
                              )}
                            </td>
                            <td style={{ textAlign: "right" }}>{tf.total_runs}</td>
                            <td style={{ textAlign: "right" }}>
                              <span style={{ color: "var(--red)", fontWeight: 600 }}>{tf.failed_runs}</span>
                            </td>
                            <td style={{ textAlign: "right" }}>
                              <span style={{ color: tf.pass_rate >= 80 ? "var(--green)" : "var(--orange)", fontWeight: 600 }}>
                                {tf.pass_rate}%
                              </span>
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  )}
                </div>

                <div>
                  <div style={{ fontSize: 11, fontWeight: 500, color: "var(--text-4)", textTransform: "uppercase", letterSpacing: "0.06em", marginBottom: 10 }}>
                    {t("dash.analytics.trend_7d")}
                  </div>
                  <table className="data-table" style={{ fontSize: 12 }}>
                    <thead><tr>
                      <th>{t("dash.analytics.col.date")}</th>
                      <th style={{ width: 48, textAlign: "right" }}>{t("dash.analytics.col.total")}</th>
                      <th style={{ width: 76, textAlign: "right" }}>{t("dash.analytics.col.pass_rate")}</th>
                    </tr></thead>
                    <tbody>
                      {[...analytics.trend].reverse().map((pt) => (
                        <tr key={pt.date}>
                          <td style={{ fontFamily: "monospace", fontSize: 11 }}>{pt.date}</td>
                          <td style={{ textAlign: "right" }}>{pt.total}</td>
                          <td style={{ textAlign: "right" }}>
                            {pt.pass_rate != null ? (
                              <span style={{ color: pt.pass_rate >= 80 ? "var(--green)" : pt.total > 0 ? "var(--orange)" : "var(--text-3)", fontWeight: 600 }}>
                                {pt.total > 0 ? `${pt.pass_rate}%` : "—"}
                              </span>
                            ) : (
                              <span style={{ color: "var(--text-3)" }}>—</span>
                            )}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            </>
          )}
        </div>
      </>
    );
  }

  return (
    <div style={{ height: "100%", overflow: "auto", background: "var(--bg)" }}>

      {/* ── Hero ─────────────────────────────────────────────────────────── */}
      <div className="dash-hero">
        <div style={{ display: "flex", alignItems: "flex-start", justifyContent: "space-between", gap: 24, flexWrap: "wrap" }}>
          <div style={{ flex: "1 1 280px", minWidth: 0 }}>
            <h1 style={{ margin: 0, fontSize: 26, fontWeight: 600, color: "var(--text-1)", letterSpacing: "-0.02em", lineHeight: 1.2 }}>
              {t("dash.title")}
            </h1>
            <p style={{ margin: "10px 0 18px", fontSize: 13, fontWeight: 400, color: "var(--text-3)", lineHeight: 1.55 }}>
              {t("dash.subtitle")}
            </p>
            <div className="dash-hero-pill-row" style={{ display: "flex", gap: 10, flexWrap: "wrap", alignItems: "center" }}>
              {currentProject && (
                <span className="badge badge-gray" style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
                  <span aria-hidden style={{ width: 8, height: 8, borderRadius: "50%", background: currentProject.color || "var(--accent)" }} />
                  {t("dash.active_project", { name: currentProject.name })}
                </span>
              )}
              <span className="badge badge-green">● {t("common.live")}</span>
              <span className="badge badge-accent">{t("common.production")}</span>
              {lastRefresh && (
                <span className="badge badge-gray">
                  {t("dash.refreshed")} {lastRefresh.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}
                </span>
              )}
            </div>
          </div>
          <div style={{ display: "flex", flexDirection: "column", alignItems: "stretch", gap: 10, flexShrink: 0 }}>
            <div className="zu-action-row" style={{ justifyContent: "flex-end" }}>
              <Link to="/generate" className="btn btn-primary btn-lg">
                {t("nav.quick_generate")}
              </Link>
              <Link to="/projects?new=1" className="btn btn-secondary btn-lg">
                {t("projects.create_new")}
              </Link>
            </div>
            <button type="button" className="btn btn-ghost btn-sm" onClick={load} disabled={loading} style={{ alignSelf: "flex-end" }}>
              {loading ? t("dash.refreshing") : t("dash.refresh")}
            </button>
          </div>
        </div>
      </div>

      <SystemStatusRibbon ribbon={systemRibbon} />

      <div style={{ padding: "24px 40px 0" }}>
        {!isOperationalDashboard && projectId ? (
          <>
            <ProjectHealthStrip
              t={t}
              projectId={projectId}
              projectName={currentProject?.name}
              summary={s}
              fi={fi}
              hasKnowledge={hasKnowledge}
              loading={loading}
              passRateValid={passRateValid}
              passRateNum={passRateNum}
              lastRefresh={lastRefresh}
              onInitialized={() => load()}
            />
            {coldProjectGuidanceVm.show ? (
              <div className="card" style={{ padding: "20px 24px", marginBottom: 20 }}>
                <ColdProjectGuidance vm={coldProjectGuidanceVm} />
              </div>
            ) : null}
            <OnboardingDashboardSection
              vm={onboardingVm}
              projectName={currentProject?.name}
              collapsedByDefault={collapseOnboarding}
            />
          </>
        ) : null}
        {isOperationalDashboard && projectId ? (
          <OnboardingDashboardSection
            vm={onboardingVm}
            projectName={currentProject?.name}
            collapsedByDefault={collapseOnboarding}
          />
        ) : null}

        {projectId && executiveDashboardV2Vm ? (
          <ExecutiveDashboardV2
            vm={executiveDashboardV2Vm}
            qualityTrendVm={qualityTrendVm}
            earlyDegradationVm={earlyDegradationVm}
            releaseReadinessVm={releaseReadinessVm}
            platformObservabilityVm={platformObservabilityVm}
            platformObservabilitySection={platformObservabilitySection}
            onRetry={load}
            DashboardSectionState={DashboardSectionState}
            exploreChildren={(
              <>
                {isOperationalDashboard ? (
                  <>
                    <DashboardCommandHeader vm={commandCenterHeaderVm} />
                    <CommandCenterView vm={commandCenterVm} />
                  </>
                ) : null}

                {valueDashboardSection.show ? (
                  <div>
                    <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
                      {valueDashboardVm.title}
                    </div>
                    <DashboardSectionState state={valueDashboardSection} onRetry={load}>
                      <ValueDashboardView vm={valueDashboardVm} />
                    </DashboardSectionState>
                  </div>
                ) : null}

                {executiveImpactSection.show ? (
                  <div>
                    <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
                      {executiveImpactVm.title}
                    </div>
                    <DashboardSectionState state={executiveImpactSection} onRetry={load}>
                      <ExecutiveImpactView vm={executiveImpactVm} />
                    </DashboardSectionState>
                  </div>
                ) : null}

                {businessRiskSection.show ? (
                  <div>
                    <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
                      {businessRiskVm.title}
                    </div>
                    <DashboardSectionState state={businessRiskSection} onRetry={load}>
                      <BusinessRiskView vm={businessRiskVm} />
                    </DashboardSectionState>
                  </div>
                ) : null}

                {shouldConsolidateQMetryIntegration(coverageOverviewVm, recommendedTestsVm) ? (
                  <CapabilityStateCard state={buildQMetryIntegrationGroupState(t)} compact />
                ) : (
                  <>
                    <DashboardIntegrationSlot
                      section={coverageOverviewSection}
                      vm={coverageOverviewVm}
                      contentComponent={CoverageIntelligenceView}
                      onRetry={load}
                    />
                    <DashboardIntegrationSlot
                      section={recommendedTestsSection}
                      vm={recommendedTestsVm}
                      contentComponent={QMetryRecommendationView}
                      onRetry={load}
                    />
                  </>
                )}

                <DashboardIntegrationSlot
                  section={servicenowSection}
                  vm={servicenowIntelligenceVm}
                  contentComponent={ServiceNowIntelligenceView}
                  onRetry={load}
                />

                <div>
                  <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
                    {reportDeliveryVm.title}
                  </div>
                  <ReportDeliveryCenter projectId={projectId} />
                </div>

                {scheduledReportVm.show ? (
                  <div>
                    <div style={{ fontSize: 12, fontWeight: 600, color: "var(--text-3)", marginBottom: 10 }}>
                      {scheduledReportVm.title}
                    </div>
                    <ExecutiveReportCenterView vm={scheduledReportVm} />
                  </div>
                ) : null}

                {renderDashboardMetricsSection()}
              </>
            )}
          />
        ) : null}
      </div>

      {projectId ? <div style={{ paddingBottom: 40 }} /> : null}

      {!projectId ? (
        <div style={{ padding: "32px 40px" }}>
          {renderDashboardMetricsSection()}
        </div>
      ) : null}

    </div>
  );
}
