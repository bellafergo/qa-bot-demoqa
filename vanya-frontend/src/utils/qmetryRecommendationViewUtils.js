/** View helpers for QMetry Test Recommendation Correlation (QMETRY-01C). */

import {
  attachCapabilityPresentation,
  resolveQMetryCapabilityState,
} from "./capabilityStateViewUtils.js";

export const QMETRY_RECOMMENDATION_I18N_KEYS = {
  title: "qmetry_recommendations.title",
  incidentTitle: "incident.qa.test_recommendation_correlation",
  totalRecommendations: "qmetry_recommendations.total_recommendations",
  executiveSummary: "qmetry_recommendations.executive_summary",
  priorityCritical: "qmetry_recommendations.priority.critical",
  priorityHigh: "qmetry_recommendations.priority.high",
  priorityMedium: "qmetry_recommendations.priority.medium",
  byCapability: "qmetry_recommendations.by_capability",
  reason: "qmetry_recommendations.reason",
  emptyConnection: "qmetry_recommendations.empty_connection",
  emptyCoverage: "qmetry_recommendations.empty_coverage",
  emptyRecommendations: "qmetry_recommendations.empty_recommendations",
  readOnlyNote: "qmetry_recommendations.read_only_note",
};

const PRIORITY_BADGE = {
  CRITICAL: "badge badge-red",
  HIGH: "badge badge-orange",
  MEDIUM: "badge badge-blue",
};

const PRIORITY_LABEL_KEY = {
  CRITICAL: QMETRY_RECOMMENDATION_I18N_KEYS.priorityCritical,
  HIGH: QMETRY_RECOMMENDATION_I18N_KEYS.priorityHigh,
  MEDIUM: QMETRY_RECOMMENDATION_I18N_KEYS.priorityMedium,
};

const PRIORITY_ORDER = { CRITICAL: 0, HIGH: 1, MEDIUM: 2 };

export function formatCount(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "0";
  return n.toLocaleString();
}

export function hasRecommendationSection(report) {
  return report?.qmetry_recommendation_report != null;
}

export function mapRecommendedTest(test, t) {
  if (!test?.test_case_id) return null;
  const priority = String(test.priority || "MEDIUM").toUpperCase();
  return {
    testCaseId: test.test_case_id,
    testCaseName: test.test_case_name || test.test_case_id,
    capability: test.capability || "—",
    reason: test.recommendation_reason || "—",
    priority,
    priorityBadgeClass: PRIORITY_BADGE[priority] || PRIORITY_BADGE.MEDIUM,
    priorityLabel: t(PRIORITY_LABEL_KEY[priority] || PRIORITY_LABEL_KEY.MEDIUM),
    reasonLabel: t(QMETRY_RECOMMENDATION_I18N_KEYS.reason),
  };
}

export function mapRecommendationGroup(group, t) {
  if (!group?.capability) return null;
  const tests = (group.recommended_tests || [])
    .map((test) => mapRecommendedTest(test, t))
    .filter(Boolean);
  if (!tests.length) return null;
  const topPriority = tests.reduce(
    (best, test) => (PRIORITY_ORDER[test.priority] < PRIORITY_ORDER[best] ? test.priority : best),
    tests[0].priority,
  );
  return {
    capability: group.capability,
    tests,
    topPriority,
    topPriorityBadgeClass: PRIORITY_BADGE[topPriority] || PRIORITY_BADGE.MEDIUM,
    topPriorityLabel: t(PRIORITY_LABEL_KEY[topPriority] || PRIORITY_LABEL_KEY.MEDIUM),
  };
}

function buildRecommendationViewModelCore(rec, t, { titleKey }) {
  const connected = Boolean(rec?.connected);
  const groups = (rec?.recommendation_groups || [])
    .map((g) => mapRecommendationGroup(g, t))
    .filter(Boolean);

  const criticalTests = groups.flatMap((g) => g.tests.filter((test) => test.priority === "CRITICAL"));
  const highTests = groups.flatMap((g) => g.tests.filter((test) => test.priority === "HIGH"));
  const mediumTests = groups.flatMap((g) => g.tests.filter((test) => test.priority === "MEDIUM"));

  const capabilityState = resolveQMetryCapabilityState({
    connected,
    totalRecommendations: rec?.total_recommendations ?? 0,
    title: t(titleKey),
    t,
  });

  const baseVm = {
    title: t(titleKey),
    show: true,
    connected,
    totalRecommendations: formatCount(rec?.total_recommendations),
    totalRecommendationsLabel: t(QMETRY_RECOMMENDATION_I18N_KEYS.totalRecommendations),
    executiveSummaryLabel: t(QMETRY_RECOMMENDATION_I18N_KEYS.executiveSummary),
    executiveSummary: rec?.executive_summary || "",
    priorityCriticalLabel: t(QMETRY_RECOMMENDATION_I18N_KEYS.priorityCritical),
    priorityHighLabel: t(QMETRY_RECOMMENDATION_I18N_KEYS.priorityHigh),
    priorityMediumLabel: t(QMETRY_RECOMMENDATION_I18N_KEYS.priorityMedium),
    byCapabilityLabel: t(QMETRY_RECOMMENDATION_I18N_KEYS.byCapability),
    groups,
    criticalTests,
    highTests,
    mediumTests,
    readOnlyNote: t(QMETRY_RECOMMENDATION_I18N_KEYS.readOnlyNote),
  };

  return attachCapabilityPresentation(baseVm, capabilityState);
}

export function buildRecommendationCorrelationViewModel(report, t) {
  if (!hasRecommendationSection(report)) {
    return { show: false };
  }
  return buildRecommendationViewModelCore(report.qmetry_recommendation_report, t, {
    titleKey: QMETRY_RECOMMENDATION_I18N_KEYS.incidentTitle,
  });
}

export function buildRecommendedTestsOverviewViewModel(recommendations, t) {
  if (recommendations == null) {
    return attachCapabilityPresentation(
      { show: true },
      resolveQMetryCapabilityState({
        connected: false,
        title: t(QMETRY_RECOMMENDATION_I18N_KEYS.title),
        t,
      }),
    );
  }
  return {
    ...buildRecommendationViewModelCore(recommendations, t, {
      titleKey: QMETRY_RECOMMENDATION_I18N_KEYS.title,
    }),
    show: true,
  };
}
