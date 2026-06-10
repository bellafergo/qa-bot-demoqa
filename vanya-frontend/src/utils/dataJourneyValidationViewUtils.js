/** View helpers for Data Journey Validation (INT-01B). */

export const DATA_JOURNEY_VALIDATION_I18N_KEYS = {
  title: "incident.qa.data_journey_validation",
  empty: "incident.qa.data_journey_validation_empty",
  journeyStatus: "incident.qa.data_journey_validation_status",
  completedStages: "incident.qa.data_journey_validation_completed_stages",
  missingStages: "incident.qa.data_journey_validation_missing_stages",
  inconsistentStages: "incident.qa.data_journey_validation_inconsistent_stages",
  confidence: "incident.qa.data_journey_validation_confidence",
  preview: "incident.qa.data_journey_validation_preview",
  previewSubtitle: "incident.qa.data_journey_validation_preview_subtitle",
  stages: "incident.qa.data_journey_validation_stages",
  validationResults: "incident.qa.data_journey_validation_validation_results",
  readOnlyNote: "incident.qa.data_journey_validation_read_only_note",
  stageStatus: "incident.qa.data_journey_validation_stage_status",
};

const JOURNEY_STATUS_BADGE = {
  HEALTHY: "badge badge-green",
  DEGRADED: "badge badge-orange",
  BROKEN: "badge badge-red",
};

const STAGE_STATUS_COLOR = {
  PASS: "#22c55e",
  FAIL: "#ef4444",
  UNKNOWN: "#eab308",
};

const STAGE_STATUS_BADGE = {
  PASS: "badge badge-green",
  FAIL: "badge badge-red",
  UNKNOWN: "badge badge-orange",
};

export function hasDataJourneyValidationSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "data_journey_validation");
}

export function isDataJourneyValidationEmpty(report) {
  return report?.data_journey_validation == null;
}

export function formatJourneyConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

export function journeyStatusBadgeClass(status) {
  const key = String(status || "DEGRADED").toUpperCase();
  return JOURNEY_STATUS_BADGE[key] || "badge badge-gray";
}

export function stageStatusBadgeClass(status) {
  const key = String(status || "UNKNOWN").toUpperCase();
  return STAGE_STATUS_BADGE[key] || "badge badge-gray";
}

export function stageStatusColor(status) {
  const key = String(status || "UNKNOWN").toUpperCase();
  return STAGE_STATUS_COLOR[key] || "var(--text-3)";
}

function resultByCheckId(report, checkId) {
  const results = report?.database_validation?.results || [];
  return results.find((r) => r.check_id === checkId) || null;
}

function findCheck(report, checkId) {
  const checks = report?.database_validation?.checks || [];
  return checks.find((c) => c.check_id === checkId) || null;
}

function findContractForStage(report, stage) {
  const contracts = report?.api_contract_intelligence?.contracts || [];
  if (!contracts.length) return null;
  if (stage.stage_type === "api") {
    return contracts.find((c) => /payment/i.test(c.service_name || "")) || contracts[0];
  }
  return null;
}

export function deriveStageStatus(stage, journeyResult, report) {
  const missing = new Set(journeyResult?.missing_stages || []);
  const inconsistent = new Set(journeyResult?.inconsistent_stages || []);

  if (inconsistent.has(stage.name)) return "FAIL";
  if (missing.has(stage.name)) return "FAIL";

  const checkIds = stage.validation_check_ids || [];
  if (checkIds.length) {
    const statuses = checkIds.map((id) => {
      const result = resultByCheckId(report, id);
      if (!result) return "UNKNOWN";
      if (result.status === "SUCCESS") return "PASS";
      if (["BLOCKED", "FAILED", "REJECTED"].includes(String(result.status || "").toUpperCase())) {
        return "FAIL";
      }
      return "UNKNOWN";
    });
    if (statuses.some((s) => s === "FAIL")) return "FAIL";
    if (statuses.every((s) => s === "PASS")) return "PASS";
    return "UNKNOWN";
  }

  if (stage.stage_type === "api") {
    return findContractForStage(report, stage) ? "PASS" : "UNKNOWN";
  }

  if (stage.stage_type === "ui") {
    return journeyResult?.status === "HEALTHY" ? "PASS" : "UNKNOWN";
  }

  return "UNKNOWN";
}

export function buildStageDrilldownItems(stage, report) {
  const items = [];
  for (const checkId of stage.validation_check_ids || []) {
    const check = findCheck(report, checkId);
    if (!check) continue;
    items.push({
      source: "data_journey",
      related_entity_type: "database_validation_check",
      related_entity_id: check.check_id,
      reason: check.name,
      detail: check.query,
      title: check.name,
    });
  }

  if (stage.stage_type === "api") {
    const contract = findContractForStage(report, stage);
    if (contract) {
      items.push({
        source: "data_journey",
        related_entity_type: "api_contract",
        related_entity_id: contract.contract_id,
        reason: contract.service_name,
        detail: `${contract.method} ${contract.endpoint}`,
        title: contract.service_name,
      });
    }
  }

  return items;
}

export function buildJourneyPreviewPayload(journey, result, stages, t) {
  return {
    title: journey.name,
    description: journey.description,
    journeyStatus: result.status,
    completedStages: `${result.completed_stages}/${result.total_stages}`,
    confidence: formatJourneyConfidence(result.confidence),
    summary: result.summary,
    missingStages: result.missing_stages || [],
    inconsistentStages: result.inconsistent_stages || [],
    stages: stages.map((stage) => ({
      name: stage.name,
      stageType: stage.stage_type,
      status: stage.status,
      drilldownItems: stage.drilldownItems,
    })),
    readOnlyNote: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.readOnlyNote),
    labels: {
      journeyStatus: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.journeyStatus),
      completedStages: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.completedStages),
      missingStages: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.missingStages),
      inconsistentStages: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.inconsistentStages),
      confidence: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.confidence),
      stages: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.stages),
      validationResults: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.validationResults),
      stageStatus: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.stageStatus),
    },
  };
}

export function buildDataJourneyValidationViewModel(report, t) {
  const djv = report?.data_journey_validation ?? null;
  const resultsByJourneyId = new Map((djv?.results || []).map((r) => [r.journey_id, r]));

  const journeys = (djv?.journeys || []).map((journey) => {
    const result = resultsByJourneyId.get(journey.journey_id) || null;
    const stages = (journey.stages || []).map((stage) => {
      const status = result ? deriveStageStatus(stage, result, report) : "UNKNOWN";
      const drilldownItems = buildStageDrilldownItems(stage, report);
      return {
        ...stage,
        status,
        statusBadgeClass: stageStatusBadgeClass(status),
        statusColor: stageStatusColor(status),
        drilldownItems,
      };
    });

    return {
      ...journey,
      result,
      status: result?.status || "UNKNOWN",
      statusBadgeClass: journeyStatusBadgeClass(result?.status),
      confidenceText: formatJourneyConfidence(result?.confidence),
      completedText: result ? `${result.completed_stages}/${result.total_stages}` : "—",
      stages,
      previewPayload: result
        ? buildJourneyPreviewPayload(journey, result, stages, t)
        : null,
    };
  });

  return {
    show: hasDataJourneyValidationSection(report),
    empty: isDataJourneyValidationEmpty(report),
    emptyMessage: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.empty),
    title: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.title),
    journeyStatusLabel: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.journeyStatus),
    completedStagesLabel: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.completedStages),
    missingStagesLabel: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.missingStages),
    inconsistentStagesLabel: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.inconsistentStages),
    confidenceLabel: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.confidence),
    previewLabel: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.preview),
    stageStatusLabel: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.stageStatus),
    readOnlyNote: t(DATA_JOURNEY_VALIDATION_I18N_KEYS.readOnlyNote),
    report: djv
      ? {
          ...djv,
          confidenceText: formatJourneyConfidence(djv.confidence),
          journeys,
        }
      : null,
  };
}
