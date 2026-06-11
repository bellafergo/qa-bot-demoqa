/** View helpers for QMetry Coverage Intelligence (QMETRY-01B). */

export const QMETRY_COVERAGE_I18N_KEYS = {
  title: "coverage_intelligence.title",
  incidentTitle: "incident.qa.coverage_intelligence",
  summary: "coverage_intelligence.summary",
  totalTests: "coverage_intelligence.total_tests",
  totalMatches: "coverage_intelligence.total_matches",
  byCapability: "coverage_intelligence.by_capability",
  gaps: "coverage_intelligence.gaps",
  criticalGaps: "coverage_intelligence.critical_gaps",
  mediumGaps: "coverage_intelligence.medium_gaps",
  executiveSummary: "coverage_intelligence.executive_summary",
  emptyConnection: "coverage_intelligence.empty_connection",
  emptyTestCases: "coverage_intelligence.empty_test_cases",
  emptyMatches: "coverage_intelligence.empty_matches",
  readOnlyNote: "coverage_intelligence.read_only_note",
  matchedTests: "coverage_intelligence.matched_tests",
  statusStrong: "coverage_intelligence.status.strong",
  statusModerate: "coverage_intelligence.status.moderate",
  statusWeak: "coverage_intelligence.status.weak",
  statusNone: "coverage_intelligence.status.none",
  severityCritical: "coverage_intelligence.severity.critical",
  severityMedium: "coverage_intelligence.severity.medium",
};

const STATUS_BADGE = {
  STRONG: "badge badge-green",
  MODERATE: "badge badge-blue",
  WEAK: "badge badge-orange",
  NONE: "badge badge-red",
};

const STATUS_LABEL_KEY = {
  STRONG: QMETRY_COVERAGE_I18N_KEYS.statusStrong,
  MODERATE: QMETRY_COVERAGE_I18N_KEYS.statusModerate,
  WEAK: QMETRY_COVERAGE_I18N_KEYS.statusWeak,
  NONE: QMETRY_COVERAGE_I18N_KEYS.statusNone,
};

const GAP_BADGE = {
  CRITICAL: "badge badge-red",
  MEDIUM: "badge badge-orange",
};

export function formatCount(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "0";
  return n.toLocaleString();
}

export function hasCoverageIntelligenceSection(report) {
  return report?.coverage_intelligence != null;
}

export function isCoverageIntelligenceEmpty(report) {
  const cov = report?.coverage_intelligence;
  if (!cov) return true;
  if (!cov.connected) return true;
  return cov.total_test_cases === 0;
}

export function hasStandaloneCoverage(report) {
  return report != null;
}

export function isStandaloneCoverageEmpty(report) {
  if (!report) return true;
  if (!report.connected) return true;
  return report.total_test_cases === 0 && !report.total_matches;
}

export function mapCoverageAssessment(assessment, t) {
  if (!assessment?.capability) return null;
  const status = String(assessment.coverage_status || "NONE").toUpperCase();
  return {
    capability: assessment.capability,
    matchedTests: assessment.matched_tests ?? 0,
    totalTests: assessment.total_tests ?? 0,
    coverageStatus: status,
    statusBadgeClass: STATUS_BADGE[status] || STATUS_BADGE.NONE,
    statusLabel: t(STATUS_LABEL_KEY[status] || STATUS_LABEL_KEY.NONE),
    matchedTestsLabel: t(QMETRY_COVERAGE_I18N_KEYS.matchedTests),
  };
}

export function mapCoverageGap(gap, t) {
  if (!gap?.capability) return null;
  const severity = String(gap.severity || "MEDIUM").toUpperCase();
  return {
    capability: gap.capability,
    module: gap.module || "—",
    reason: gap.reason || "—",
    severity,
    severityBadgeClass: GAP_BADGE[severity] || GAP_BADGE.MEDIUM,
    severityLabel: severity === "CRITICAL"
      ? t(QMETRY_COVERAGE_I18N_KEYS.severityCritical)
      : t(QMETRY_COVERAGE_I18N_KEYS.severityMedium),
  };
}

function buildCoverageViewModelCore(cov, t, { titleKey }) {
  const connected = Boolean(cov?.connected);
  const assessments = (cov?.coverage_assessments || [])
    .map((a) => mapCoverageAssessment(a, t))
    .filter(Boolean);
  const gaps = (cov?.coverage_gaps || [])
    .map((g) => mapCoverageGap(g, t))
    .filter(Boolean);
  const criticalGaps = gaps.filter((g) => g.severity === "CRITICAL");
  const mediumGaps = gaps.filter((g) => g.severity === "MEDIUM");

  let emptyMessage = null;
  if (!connected) emptyMessage = t(QMETRY_COVERAGE_I18N_KEYS.emptyConnection);
  else if ((cov?.total_test_cases ?? 0) === 0) emptyMessage = t(QMETRY_COVERAGE_I18N_KEYS.emptyTestCases);
  else if ((cov?.total_matches ?? 0) === 0) emptyMessage = t(QMETRY_COVERAGE_I18N_KEYS.emptyMatches);

  return {
    title: t(titleKey),
    show: true,
    empty: Boolean(emptyMessage),
    emptyMessage,
    connected,
    summaryLabel: t(QMETRY_COVERAGE_I18N_KEYS.summary),
    totalTestsLabel: t(QMETRY_COVERAGE_I18N_KEYS.totalTests),
    totalMatchesLabel: t(QMETRY_COVERAGE_I18N_KEYS.totalMatches),
    totalTests: formatCount(cov?.total_test_cases),
    totalMatches: formatCount(cov?.total_matches),
    byCapabilityLabel: t(QMETRY_COVERAGE_I18N_KEYS.byCapability),
    gapsLabel: t(QMETRY_COVERAGE_I18N_KEYS.gaps),
    criticalGapsLabel: t(QMETRY_COVERAGE_I18N_KEYS.criticalGaps),
    mediumGapsLabel: t(QMETRY_COVERAGE_I18N_KEYS.mediumGaps),
    executiveSummaryLabel: t(QMETRY_COVERAGE_I18N_KEYS.executiveSummary),
    executiveSummary: cov?.executive_summary || "",
    assessments,
    gaps,
    criticalGaps,
    mediumGaps,
    readOnlyNote: t(QMETRY_COVERAGE_I18N_KEYS.readOnlyNote),
  };
}

export function buildCoverageIntelligenceViewModel(report, t) {
  if (!hasCoverageIntelligenceSection(report)) {
    return { show: false };
  }
  const cov = report.coverage_intelligence;
  return buildCoverageViewModelCore(cov, t, { titleKey: QMETRY_COVERAGE_I18N_KEYS.incidentTitle });
}

export function buildCoverageOverviewViewModel(coverage, t) {
  if (!hasStandaloneCoverage(coverage)) {
    return { show: false, empty: true, emptyMessage: t(QMETRY_COVERAGE_I18N_KEYS.emptyConnection) };
  }
  const vm = buildCoverageViewModelCore(coverage, t, { titleKey: QMETRY_COVERAGE_I18N_KEYS.title });
  return {
    ...vm,
    show: true,
    empty: isStandaloneCoverageEmpty(coverage),
  };
}
