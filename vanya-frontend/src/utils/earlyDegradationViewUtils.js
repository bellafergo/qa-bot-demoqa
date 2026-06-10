/** View helpers for Early Degradation Detection (OBS-01C). */

export const EARLY_DEGRADATION_I18N_KEYS = {
  title: "incident.qa.early_degradation",
  empty: "incident.qa.early_degradation_empty",
  overallStatus: "incident.qa.early_degradation_overall_status",
  degradingAreas: "incident.qa.early_degradation_degrading_areas",
  criticalAreas: "incident.qa.early_degradation_critical_areas",
  confidence: "incident.qa.early_degradation_confidence",
  riskProjection: "incident.qa.early_degradation_risk_projection",
  recommendedAttention: "incident.qa.early_degradation_recommended_attention",
  readOnlyNote: "incident.qa.early_degradation_read_only_note",
  statusStable: "incident.qa.early_degradation_status_stable",
  statusDegrading: "incident.qa.early_degradation_status_degrading",
  statusRapidDegradation: "incident.qa.early_degradation_status_rapid_degradation",
  statusCriticalDegradation: "incident.qa.early_degradation_status_critical_degradation",
  projectionLowRisk: "incident.qa.early_degradation_projection_low_risk",
  projectionElevatedRisk: "incident.qa.early_degradation_projection_elevated_risk",
  projectionHighRisk: "incident.qa.early_degradation_projection_high_risk",
  projectionIncidentLikely: "incident.qa.early_degradation_projection_incident_likely",
};

const STATUS_LABEL_KEY = {
  STABLE: EARLY_DEGRADATION_I18N_KEYS.statusStable,
  DEGRADING: EARLY_DEGRADATION_I18N_KEYS.statusDegrading,
  RAPID_DEGRADATION: EARLY_DEGRADATION_I18N_KEYS.statusRapidDegradation,
  CRITICAL_DEGRADATION: EARLY_DEGRADATION_I18N_KEYS.statusCriticalDegradation,
};

const STATUS_COLOR = {
  STABLE: "#22c55e",
  DEGRADING: "#eab308",
  RAPID_DEGRADATION: "#f97316",
  CRITICAL_DEGRADATION: "#ef4444",
};

const STATUS_BADGE = {
  STABLE: "badge badge-green",
  DEGRADING: "badge badge-orange",
  RAPID_DEGRADATION: "badge badge-orange",
  CRITICAL_DEGRADATION: "badge badge-red",
};

const PROJECTION_LABEL_KEY = {
  LOW_RISK: EARLY_DEGRADATION_I18N_KEYS.projectionLowRisk,
  ELEVATED_RISK: EARLY_DEGRADATION_I18N_KEYS.projectionElevatedRisk,
  HIGH_RISK: EARLY_DEGRADATION_I18N_KEYS.projectionHighRisk,
  INCIDENT_LIKELY: EARLY_DEGRADATION_I18N_KEYS.projectionIncidentLikely,
};

const PROJECTION_BADGE = {
  LOW_RISK: "badge badge-green",
  ELEVATED_RISK: "badge badge-orange",
  HIGH_RISK: "badge badge-red",
  INCIDENT_LIKELY: "badge badge-red",
};

export function hasEarlyDegradationSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "early_degradation");
}

export function isEarlyDegradationEmpty(report) {
  return report?.early_degradation == null;
}

export function degradationStatusLabelKey(status) {
  return STATUS_LABEL_KEY[String(status || "STABLE").toUpperCase()] || STATUS_LABEL_KEY.STABLE;
}

export function degradationStatusColor(status) {
  return STATUS_COLOR[String(status || "STABLE").toUpperCase()] || STATUS_COLOR.STABLE;
}

export function degradationStatusBadgeClass(status) {
  return STATUS_BADGE[String(status || "STABLE").toUpperCase()] || "badge badge-gray";
}

export function riskProjectionLabelKey(projection) {
  return PROJECTION_LABEL_KEY[String(projection || "LOW_RISK").toUpperCase()] || PROJECTION_LABEL_KEY.LOW_RISK;
}

export function riskProjectionBadgeClass(projection) {
  return PROJECTION_BADGE[String(projection || "LOW_RISK").toUpperCase()] || "badge badge-gray";
}

export function formatDegradationConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function formatScoreRange(signal) {
  if (!signal) return "—";
  return `${signal.previous_score} → ${signal.current_score}`;
}

function _matchesScope(scopeName, candidate) {
  const scope = String(scopeName || "").trim().toLowerCase();
  const token = String(candidate || "").trim().toLowerCase();
  return scope && token && (scope === token || scope.includes(token) || token.includes(scope));
}

export function buildRecommendedAttention(scopeName, report) {
  const items = [];
  const seen = new Set();

  const add = (label, drilldownItem) => {
    const key = String(label || "").trim();
    if (!key || seen.has(key)) return;
    seen.add(key);
    items.push({ label: key, drilldownItem: drilldownItem || null });
  };

  for (const assessment of report?.contract_risk_assessment?.assessments || []) {
    const level = String(assessment.overall_risk_level || "").toUpperCase();
    if (!["CRITICAL", "HIGH"].includes(level)) continue;
    const contractId = assessment.contract_id || "";
    if (!_matchesScope(scopeName, contractId) && !(assessment.affected_modules || []).some((m) => _matchesScope(scopeName, m))) {
      continue;
    }
    const label = `${contractId.replace(/[_-]/g, " ").replace(/\b\w/g, (c) => c.toUpperCase())} Contract Risk`;
    add(label, {
      source: "early_degradation",
      related_entity_type: "api_contract",
      related_entity_id: contractId,
      reason: assessment.summary || label,
      detail: label,
      title: label,
    });
  }

  for (const rec of report?.executive_quality_report?.top_recommendations || []) {
    if (_matchesScope(scopeName, rec)) {
      add(rec, {
        source: "early_degradation",
        related_entity_type: "quality_health",
        related_entity_id: `module:${String(scopeName).toLowerCase()}`,
        reason: rec,
        detail: rec,
        title: rec,
      });
    }
  }

  for (const insight of report?.decision_center?.key_takeaways || []) {
    const title = insight?.title || "";
    if (_matchesScope(scopeName, title)) {
      add(title, {
        source: "early_degradation",
        related_entity_type: "decision_center",
        related_entity_id: `insight:${String(title).toLowerCase().replace(/\s+/g, "_")}`,
        reason: insight.description || title,
        detail: title,
        title,
      });
    }
  }

  for (const result of report?.data_journey_validation?.results || []) {
    const status = String(result.status || "").toUpperCase();
    if (!["BROKEN", "FAILED", "INCONSISTENT"].includes(status)) continue;
    const journey = (report?.data_journey_validation?.journeys || []).find((j) => j.journey_id === result.journey_id);
    const name = journey?.name || result.journey_id;
    if (_matchesScope(scopeName, name)) {
      add(`${name} Journey Validation`, {
        source: "early_degradation",
        related_entity_type: "data_journey",
        related_entity_id: result.journey_id,
        reason: result.summary || name,
        detail: name,
        title: name,
      });
    }
  }

  if (report?.deployment_risk_assessment && _matchesScope(scopeName, "project")) {
    add("Deployment Risk Review", {
      source: "early_degradation",
      related_entity_type: "deployment_risk",
      related_entity_id: "deployment_risk",
      reason: report.deployment_risk_assessment.summary,
      detail: "Deployment risk assessment",
      title: "Deployment Risk",
    });
  }

  const incident = report?.historical_learning?.similar_incidents?.[0];
  if (incident && _matchesScope(scopeName, incident.title)) {
    add(incident.title, {
      source: "early_degradation",
      related_entity_type: "historical_learning",
      related_entity_id: incident.incident_id,
      reason: incident.summary,
      detail: incident.title,
      title: incident.title,
    });
  }

  return items.slice(0, 4);
}

export function buildAssessmentDrilldownItems(assessment, report) {
  const items = [];
  const signal = assessment?.signals?.[0];
  if (signal?.related_entity_type && signal?.related_entity_id) {
    items.push({
      source: "early_degradation",
      related_entity_type: signal.related_entity_type,
      related_entity_id: signal.related_entity_id,
      reason: signal.summary,
      detail: assessment.scope_name,
      title: assessment.scope_name,
    });
  }

  const trend = (report?.quality_trends?.trends || []).find((t) => t.scope_name === assessment.scope_name);
  if (trend?.trend_id) {
    items.push({
      source: "early_degradation",
      related_entity_type: "quality_trend",
      related_entity_id: trend.trend_id,
      reason: trend.trend_direction,
      detail: assessment.scope_name,
      title: "Quality Trend",
    });
  }

  return items;
}

export function buildEarlyDegradationViewModel(report, t) {
  const ed = report?.early_degradation ?? null;

  return {
    show: hasEarlyDegradationSection(report),
    empty: isEarlyDegradationEmpty(report),
    emptyMessage: t(EARLY_DEGRADATION_I18N_KEYS.empty),
    title: t(EARLY_DEGRADATION_I18N_KEYS.title),
    overallStatusLabel: t(EARLY_DEGRADATION_I18N_KEYS.overallStatus),
    degradingAreasLabel: t(EARLY_DEGRADATION_I18N_KEYS.degradingAreas),
    criticalAreasLabel: t(EARLY_DEGRADATION_I18N_KEYS.criticalAreas),
    confidenceLabel: t(EARLY_DEGRADATION_I18N_KEYS.confidence),
    riskProjectionLabel: t(EARLY_DEGRADATION_I18N_KEYS.riskProjection),
    recommendedAttentionLabel: t(EARLY_DEGRADATION_I18N_KEYS.recommendedAttention),
    readOnlyNote: t(EARLY_DEGRADATION_I18N_KEYS.readOnlyNote),
    report: ed
      ? {
          ...ed,
          overallStatusText: t(degradationStatusLabelKey(ed.overall_status)),
          overallStatusColor: degradationStatusColor(ed.overall_status),
          overallStatusBadgeClass: degradationStatusBadgeClass(ed.overall_status),
          confidenceText: formatDegradationConfidence(ed.confidence),
          assessments: (ed.assessments || []).map((assessment) => {
            const signal = assessment.signals?.[0];
            return {
              ...assessment,
              statusLabel: t(degradationStatusLabelKey(assessment.status)),
              statusColor: degradationStatusColor(assessment.status),
              statusBadgeClass: degradationStatusBadgeClass(assessment.status),
              projectionLabel: t(riskProjectionLabelKey(assessment.risk_projection)),
              projectionBadgeClass: riskProjectionBadgeClass(assessment.risk_projection),
              confidenceText: formatDegradationConfidence(assessment.confidence),
              scoreRangeText: formatScoreRange(signal),
              trendIndicator: signal?.score_delta < 0 ? "▼" : signal?.score_delta > 0 ? "▲" : "■",
              recommendedAttention: buildRecommendedAttention(assessment.scope_name, report),
              drilldownItems: buildAssessmentDrilldownItems(assessment, report),
            };
          }),
        }
      : null,
  };
}

/** Dashboard helper when early degradation comes from project API. */
export function buildEarlyDegradationViewModelFromApi(earlyDegradation, t, contextReport = null) {
  const syntheticReport = {
    early_degradation: earlyDegradation,
    ...(contextReport || {}),
  };
  const vm = buildEarlyDegradationViewModel(syntheticReport, t);
  vm.show = Boolean(earlyDegradation);
  vm.empty = !earlyDegradation;
  return vm;
}
