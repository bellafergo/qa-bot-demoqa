/** View-model helpers for Incident Investigator report UI (v1.3C). */

export function isLegacyIncidentReport(report) {
  if (!report) return false;
  return (
    report.evidence_strength == null
    && report.impacted_modules_ranked == null
    && report.recommended_tests_v2 == null
  );
}

export function isV13BReport(report) {
  if (!report) return false;
  return report.evidence_strength != null;
}

export function getEngineVersion(report) {
  return report?.meta?.engine_version || "unknown";
}

export function isAnalyzeOnly(report) {
  return report?.meta?.analyze_only === true;
}

export function hasBrowserProbeEvidence(evidenceStrength) {
  return (evidenceStrength?.evidence || []).some(
    (item) => String(item?.label || "").toLowerCase() === "browser probe",
  );
}

export function shouldShowEvidenceEmptyState(evidenceStrength) {
  return !(evidenceStrength?.evidence?.length > 0);
}

export function shouldShowBlastRadiusEmptyState(report) {
  return !(report?.impacted_modules_ranked?.length > 0);
}

export function shouldShowRecommendedTestsEmptyState(report) {
  return !(report?.recommended_tests_v2?.length > 0);
}

export function shouldShowTemporalEmptyState(temporalCorrelation) {
  if (!temporalCorrelation) return true;
  return temporalCorrelation.signal === "none" || !temporalCorrelation.signal;
}

export function isEvidenceCorrelationEmpty(evidenceCorrelation) {
  if (!evidenceCorrelation) return false;
  return (evidenceCorrelation.total_correlations ?? 0) === 0
    || !(evidenceCorrelation.evidence?.length > 0);
}

export function correlationReasonText(item, t) {
  const reason = String(item?.reason || "").trim();
  return reason || t("incident.qa.correlation_reason_unavailable");
}

export function buildIncidentReportViewModel(report, t) {
  const legacy = isLegacyIncidentReport(report);
  const v13b = isV13BReport(report);
  const es = report?.evidence_strength;
  const temporal = report?.temporal_correlation;

  return {
    legacy,
    v13b,
    showLegacyBanner: legacy,
    legacyBannerMessage: t("incident.qa.legacy_banner"),
    engineVersion: getEngineVersion(report),
    analyzeOnly: isAnalyzeOnly(report),
    showEngineMeta: Boolean(report),
    evidenceStrength: {
      show: v13b,
      evidenceEmpty: shouldShowEvidenceEmptyState(es),
      evidenceEmptyMessage: t("incident.qa.evidence_empty"),
      hasBrowserProbe: hasBrowserProbeEvidence(es),
    },
    blastRadius: {
      show: v13b,
      empty: shouldShowBlastRadiusEmptyState(report),
      emptyMessage: t("incident.qa.blast_radius_empty"),
    },
    recommendedTests: {
      show: v13b,
      empty: shouldShowRecommendedTestsEmptyState(report),
      emptyMessage: t("incident.qa.recommended_tests_empty"),
    },
    temporal: {
      showEmpty: v13b && shouldShowTemporalEmptyState(temporal),
      emptyMessage: t("incident.qa.temporal_empty"),
      signal: temporal?.signal,
    },
  };
}
