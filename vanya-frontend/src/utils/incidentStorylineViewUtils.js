/** View helpers for Incident Storyline (II-03B). */

export const STORYLINE_I18N_KEYS = {
  title: "incident.qa.storyline",
  step: "incident.qa.storyline_step",
  confidence: "incident.qa.storyline_confidence",
  timestamp: "incident.qa.storyline_timestamp",
  empty: "incident.qa.storyline_empty",
};

export function hasStorylineSection(report) {
  return Array.isArray(report?.storyline);
}

export function isStorylineEmpty(report) {
  return !report?.storyline?.length;
}

export function buildStorylineDrilldownItem(step) {
  if (!step?.related_entity_type || !step?.related_entity_id) return null;
  return {
    source: step.related_entity_type,
    related_entity_type: step.related_entity_type,
    related_entity_id: step.related_entity_id,
    reason: step.description,
    detail: step.title,
    title: step.title,
  };
}

export function formatStorylineConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function buildStorylineViewModel(report, t) {
  const steps = Array.isArray(report?.storyline) ? report.storyline : [];
  return {
    show: hasStorylineSection(report),
    empty: steps.length === 0,
    emptyMessage: t(STORYLINE_I18N_KEYS.empty),
    title: t(STORYLINE_I18N_KEYS.title),
    stepLabel: t(STORYLINE_I18N_KEYS.step),
    confidenceLabel: t(STORYLINE_I18N_KEYS.confidence),
    timestampLabel: t(STORYLINE_I18N_KEYS.timestamp),
    steps: steps.map((step, index) => ({
      ...step,
      isLast: index === steps.length - 1,
      drilldownItem: buildStorylineDrilldownItem(step),
      confidenceText: formatStorylineConfidence(step.confidence),
    })),
  };
}
