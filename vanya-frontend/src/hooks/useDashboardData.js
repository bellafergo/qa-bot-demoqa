import { useCallback, useEffect, useState } from "react";
import {
  getDashboardSummary,
  getProjectQualityTrends,
  getProjectEarlyDegradation,
  getProjectValueDashboard,
  getProjectExecutiveImpact,
  getProjectBusinessRisk,
  getPlatformObservability,
  getQMetryCoverage,
  getQMetryRecommendations,
  getServiceNowIntelligence,
  getDashboardRecentRuns,
  getDashboardRecentJobs,
  getFailureIntel,
  getRunsAnalytics,
  getExecStatus,
  getProjectKnowledge,
  apiErrorMessage,
} from "../api";

export function useDashboardData(projectId, t) {
  const [summary, setSummary] = useState(null);
  const [recentRuns, setRecentRuns] = useState([]);
  const [recentJobs, setRecentJobs] = useState([]);
  const [fi, setFi] = useState(null);
  const [analytics, setAnalytics] = useState(null);
  const [execStatus, setExecStatus] = useState(null);
  const [loading, setLoading] = useState(true);
  const [summaryError, setSummaryError] = useState("");
  const [runsError, setRunsError] = useState("");
  const [jobsError, setJobsError] = useState("");
  const [execError, setExecError] = useState("");
  const [fiLoading, setFiLoading] = useState(true);
  const [fiError, setFiError] = useState("");
  const [analyticsLoading, setAnalyticsLoading] = useState(true);
  const [analyticsError, setAnalyticsError] = useState("");
  const [lastRefresh, setLastRefresh] = useState(null);
  // MEJORA #4 — client-side filters for recent runs (no extra API calls)
  const [recentRunsFilter, setRecentRunsFilter] = useState("all");
  // MEJORA #5 — single-flight re-run per row
  const [rerunBusyRunId, setRerunBusyRunId] = useState(null);
  const [runInlineError, setRunInlineError] = useState("");
  const [topProblemBusy, setTopProblemBusy] = useState(false);
  const [topProblemError, setTopProblemError] = useState("");
  const [hasKnowledge, setHasKnowledge] = useState(null);
  const [qualityTrends, setQualityTrends] = useState(null);
  const [earlyDegradation, setEarlyDegradation] = useState(null);
  const [valueDashboard, setValueDashboard] = useState(null);
  const [executiveImpact, setExecutiveImpact] = useState(null);
  const [businessRisk, setBusinessRisk] = useState(null);
  const [platformObservability, setPlatformObservability] = useState(null);
  const [servicenowIntelligence, setServicenowIntelligence] = useState(null);
  const [coverageOverview, setCoverageOverview] = useState(null);
  const [recommendedTests, setRecommendedTests] = useState(null);
  const [intelErrors, setIntelErrors] = useState({});
  const [intelLoading, setIntelLoading] = useState({});

  const trackIntel = useCallback(async (key, fetcher, setter) => {
    setIntelLoading((prev) => ({ ...prev, [key]: true }));
    setIntelErrors((prev) => ({ ...prev, [key]: "" }));
    try {
      const data = await fetcher();
      setter(data);
    } catch (e) {
      setter(null);
      setIntelErrors((prev) => ({
        ...prev,
        [key]: apiErrorMessage(e) || t("dash.load_error"),
      }));
    } finally {
      setIntelLoading((prev) => ({ ...prev, [key]: false }));
    }
  }, [t]);

  const load = useCallback(async () => {
    setLoading(true);
    setSummaryError("");
    setRunsError("");
    setJobsError("");
    setExecError("");
    setFiLoading(true);
    setFiError("");
    setAnalyticsLoading(true);
    setAnalyticsError("");
    setHasKnowledge(null);
    setQualityTrends(null);
    setEarlyDegradation(null);
    setValueDashboard(null);
    setExecutiveImpact(null);
    setBusinessRisk(null);
    setPlatformObservability(null);
    setServicenowIntelligence(null);
    setCoverageOverview(null);
    setRecommendedTests(null);
    setIntelErrors({});
    setIntelLoading({});

    const pid = projectId;
    const summaryParams = pid ? { project_id: pid } : {};

    const [summaryRes, runsRes, jobsRes, execRes] = await Promise.allSettled([
      getDashboardSummary(summaryParams),
      getDashboardRecentRuns(10, pid),
      getDashboardRecentJobs(10, pid),
      getExecStatus(),
    ]);

    if (summaryRes.status === "fulfilled") {
      setSummary(summaryRes.value);
      setSummaryError("");
      setLastRefresh(new Date());
    } else {
      setSummaryError(apiErrorMessage(summaryRes.reason) || t("dash.load_error"));
    }

    if (runsRes.status === "fulfilled") {
      setRecentRuns(Array.isArray(runsRes.value) ? runsRes.value : []);
      setRunsError("");
    } else {
      setRunsError(apiErrorMessage(runsRes.reason) || t("dash.load_error"));
    }

    if (jobsRes.status === "fulfilled") {
      setRecentJobs(Array.isArray(jobsRes.value) ? jobsRes.value : []);
      setJobsError("");
    } else {
      setJobsError(apiErrorMessage(jobsRes.reason) || t("dash.load_error"));
    }

    if (execRes.status === "fulfilled") {
      const ex = execRes.value;
      setExecStatus(ex && typeof ex === "object" ? ex : null);
      setExecError("");
    } else {
      setExecError(apiErrorMessage(execRes.reason) || t("dash.load_error"));
    }

    setLoading(false);

    trackIntel("platformObservability", getPlatformObservability, setPlatformObservability);

    if (pid) {
      getProjectKnowledge(pid)
        .then((k) => setHasKnowledge(!!k))
        .catch((e) => {
          if (e?.status === 404) setHasKnowledge(false);
          else setHasKnowledge(null);
        });
      trackIntel("qualityTrends", () => getProjectQualityTrends(pid), setQualityTrends);
      trackIntel("earlyDegradation", () => getProjectEarlyDegradation(pid), setEarlyDegradation);
      trackIntel("valueDashboard", () => getProjectValueDashboard(pid), setValueDashboard);
      trackIntel("executiveImpact", () => getProjectExecutiveImpact(pid), setExecutiveImpact);
      trackIntel("businessRisk", () => getProjectBusinessRisk(pid), setBusinessRisk);
      trackIntel(
        "servicenowIntelligence",
        () => getServiceNowIntelligence({ project_id: pid }),
        setServicenowIntelligence,
      );
      trackIntel("coverageOverview", () => getQMetryCoverage({ project_id: pid }), setCoverageOverview);
      trackIntel(
        "recommendedTests",
        () => getQMetryRecommendations({ project_id: pid }),
        setRecommendedTests,
      );
    } else {
      setHasKnowledge(null);
      setValueDashboard(null);
      setExecutiveImpact(null);
      setBusinessRisk(null);
      setCoverageOverview(null);
      setRecommendedTests(null);
    }

    getFailureIntel(pid)
      .then((f) => {
        setFi(f);
        setFiError("");
      })
      .catch((e) => {
        setFiError(apiErrorMessage(e) || t("dash.load_error"));
      })
      .finally(() => setFiLoading(false));

    getRunsAnalytics(pid)
      .then((a) => {
        setAnalytics(a);
        setAnalyticsError("");
      })
      .catch((e) => {
        setAnalyticsError(apiErrorMessage(e) || t("dash.load_error"));
      })
      .finally(() => setAnalyticsLoading(false));
  }, [projectId, t, trackIntel]);

  useEffect(() => {
    load();
  }, [load]);

  return {
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
    intelErrors,
    intelLoading,
    load,
  };
}
