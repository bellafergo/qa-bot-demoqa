/** View helpers for Incident Impact Map (II-03C). */

export const IMPACT_MAP_I18N_KEYS = {
  title: "incident.qa.impact_map",
  empty: "incident.qa.impact_map_empty",
  signals: "incident.qa.impact_map_signals",
  confidence: "incident.qa.impact_map_confidence",
  severity: "incident.qa.impact_map_severity",
  severityHigh: "incident.qa.impact_map_severity_high",
  severityMedium: "incident.qa.impact_map_severity_medium",
  severityLow: "incident.qa.impact_map_severity_low",
  openDetails: "incident.qa.impact_map_open_details",
};

export function hasImpactMapSection(report) {
  return Array.isArray(report?.impact_map);
}

export function isImpactMapEmpty(report) {
  return !report?.impact_map?.length;
}

export function getImpactSeverityLabelKey(severity) {
  const v = String(severity || "low").toLowerCase();
  if (v === "high") return IMPACT_MAP_I18N_KEYS.severityHigh;
  if (v === "medium") return IMPACT_MAP_I18N_KEYS.severityMedium;
  return IMPACT_MAP_I18N_KEYS.severityLow;
}

export function getImpactSeverityBadgeClass(severity) {
  const v = String(severity || "low").toLowerCase();
  if (v === "high") return "badge badge-red";
  if (v === "medium") return "badge badge-orange";
  return "badge badge-gray";
}

export function formatImpactConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function buildImpactMapDrilldownItem(node) {
  if (!node?.related_entity_type || !node?.related_entity_id) return null;
  return {
    source: node.related_entity_type,
    related_entity_type: node.related_entity_type,
    related_entity_id: node.related_entity_id,
    reason: node.description,
    detail: node.title,
    title: node.title,
  };
}

export function buildImpactMapViewModel(report, t) {
  const nodes = Array.isArray(report?.impact_map) ? report.impact_map : [];
  return {
    show: hasImpactMapSection(report),
    empty: nodes.length === 0,
    emptyMessage: t(IMPACT_MAP_I18N_KEYS.empty),
    title: t(IMPACT_MAP_I18N_KEYS.title),
    signalsLabel: t(IMPACT_MAP_I18N_KEYS.signals),
    confidenceLabel: t(IMPACT_MAP_I18N_KEYS.confidence),
    severityLabel: t(IMPACT_MAP_I18N_KEYS.severity),
    openDetailsLabel: t(IMPACT_MAP_I18N_KEYS.openDetails),
    nodes: nodes.map((node) => ({
      ...node,
      severityLabel: t(getImpactSeverityLabelKey(node.severity)),
      severityBadgeClass: getImpactSeverityBadgeClass(node.severity),
      confidenceText: formatImpactConfidence(node.confidence),
      drilldownItem: buildImpactMapDrilldownItem(node),
    })),
  };
}
