/** View helpers for Historical Learning & Similar Incidents (II-06A). */

export const HISTORICAL_LEARNING_I18N_KEYS = {
  title: "incident.qa.historical_learning",
  empty: "incident.qa.historical_learning_empty",
  patternSummary: "incident.qa.historical_learning_pattern_summary",
  similarIncidents: "incident.qa.historical_learning_similar_incidents",
  similarity: "incident.qa.historical_learning_similarity",
  confidence: "incident.qa.historical_learning_confidence",
  preview: "incident.qa.historical_learning_preview",
  previewSubtitle: "incident.qa.historical_learning_preview_subtitle",
  previewSummary: "incident.qa.historical_learning_preview_summary",
  previewTimestamp: "incident.qa.historical_learning_preview_timestamp",
  readOnlyNote: "incident.qa.historical_learning_read_only_note",
};

export function hasHistoricalLearningSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "historical_learning");
}

export function isHistoricalLearningEmpty(report) {
  return report?.historical_learning == null;
}

export function formatHistoricalLearningConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function formatSimilarityScore(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function buildSimilarIncidentDrilldownItem(incident) {
  if (!incident?.related_entity_type || !incident?.related_entity_id) return null;
  const et = String(incident.related_entity_type);
  if (et === "hypothesis") return null;
  return {
    source: et,
    related_entity_type: et,
    related_entity_id: incident.related_entity_id,
    reason: incident.summary,
    detail: incident.title,
    title: incident.title,
  };
}

export function buildSimilarIncidentPreviewPayload(incident, t) {
  return {
    title: incident.title,
    fields: [
      {
        label: t(HISTORICAL_LEARNING_I18N_KEYS.similarity),
        value: formatSimilarityScore(incident.similarityScore ?? incident.similarity_score),
      },
      {
        label: t(HISTORICAL_LEARNING_I18N_KEYS.previewSummary),
        value: incident.summary || "—",
      },
      {
        label: t(HISTORICAL_LEARNING_I18N_KEYS.previewTimestamp),
        value: incident.timestampText || incident.occurrence_timestamp || "—",
      },
    ],
    readOnlyNote: t(HISTORICAL_LEARNING_I18N_KEYS.readOnlyNote),
  };
}

export function buildHistoricalLearningViewModel(report, t, formatTimestamp) {
  const learning = report?.historical_learning ?? null;
  const fmtTs = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";

  return {
    show: hasHistoricalLearningSection(report),
    empty: isHistoricalLearningEmpty(report),
    emptyMessage: t(HISTORICAL_LEARNING_I18N_KEYS.empty),
    title: t(HISTORICAL_LEARNING_I18N_KEYS.title),
    patternSummaryLabel: t(HISTORICAL_LEARNING_I18N_KEYS.patternSummary),
    similarIncidentsLabel: t(HISTORICAL_LEARNING_I18N_KEYS.similarIncidents),
    similarityLabel: t(HISTORICAL_LEARNING_I18N_KEYS.similarity),
    confidenceLabel: t(HISTORICAL_LEARNING_I18N_KEYS.confidence),
    previewLabel: t(HISTORICAL_LEARNING_I18N_KEYS.preview),
    readOnlyNote: t(HISTORICAL_LEARNING_I18N_KEYS.readOnlyNote),
    learning: learning
      ? {
          ...learning,
          confidenceText: formatHistoricalLearningConfidence(learning.confidence),
          incidents: (learning.similar_incidents || []).map((incident) => ({
            ...incident,
            similarityText: formatSimilarityScore(incident.similarity_score),
            timestampText: fmtTs(incident.occurrence_timestamp),
            drilldownItem: buildSimilarIncidentDrilldownItem(incident),
            previewPayload: buildSimilarIncidentPreviewPayload(
              {
                ...incident,
                similarityScore: incident.similarity_score,
                timestampText: fmtTs(incident.occurrence_timestamp),
              },
              t,
            ),
          })),
        }
      : null,
  };
}
