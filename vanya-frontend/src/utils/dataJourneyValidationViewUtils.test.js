import { describe, it, expect } from "vitest";
import {
  DATA_JOURNEY_VALIDATION_I18N_KEYS,
  buildDataJourneyValidationViewModel,
  buildJourneyPreviewPayload,
  buildStageDrilldownItems,
  deriveStageStatus,
  formatJourneyConfidence,
  hasDataJourneyValidationSection,
  isDataJourneyValidationEmpty,
  journeyStatusBadgeClass,
  stageStatusBadgeClass,
  stageStatusColor,
} from "./dataJourneyValidationViewUtils.js";

const t = (key) => key;

const sampleJourney = {
  journey_id: "journey:checkout",
  name: "Checkout Journey",
  description: "Checkout propagation",
  business_area: "checkout",
  stages: [
    {
      stage_id: "journey:checkout:stage:ui",
      name: "Web UI",
      stage_type: "ui",
      validation_check_ids: [],
    },
    {
      stage_id: "journey:checkout:stage:payments_api",
      name: "Payments API",
      stage_type: "api",
      validation_check_ids: [],
    },
    {
      stage_id: "journey:checkout:stage:orders_db",
      name: "Orders DB",
      stage_type: "database",
      validation_check_ids: ["check:checkout:validate_order_record_exists"],
    },
  ],
};

const sampleResult = {
  journey_id: "journey:checkout",
  status: "DEGRADED",
  completed_stages: 2,
  total_stages: 3,
  confidence: 0.72,
  summary: "Checkout Journey degraded with 1 stage issue(s) detected.",
  missing_stages: ["Inventory DB"],
  inconsistent_stages: [],
};

const sampleReport = {
  data_journey_validation: {
    summary: "1 data journey(s) modeled; 1 degraded journey(s) detected.",
    confidence: 0.72,
    journeys: [sampleJourney],
    results: [sampleResult],
  },
  database_validation: {
    checks: [
      {
        check_id: "check:checkout:validate_order_record_exists",
        name: "Validate Checkout order record exists",
        query: "SELECT order_id FROM orders WHERE order_id = :order_id",
        database_type: "postgresql",
      },
    ],
    results: [
      {
        check_id: "check:checkout:validate_order_record_exists",
        status: "SUCCESS",
        summary: "ok",
        confidence: 0.8,
      },
    ],
  },
  api_contract_intelligence: {
    contracts: [
      {
        contract_id: "contract:payments_api_post_payments_vv2",
        service_name: "Payments API",
        endpoint: "/payments",
        method: "POST",
        version: "v2",
      },
    ],
    risk_assessments: [],
  },
};

describe("dataJourneyValidationViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasDataJourneyValidationSection({ data_journey_validation: null })).toBe(true);
    expect(hasDataJourneyValidationSection({})).toBe(false);
    expect(isDataJourneyValidationEmpty({ data_journey_validation: null })).toBe(true);
  });

  it("renders journeys with stages", () => {
    const vm = buildDataJourneyValidationViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.report.journeys[0].name).toBe("Checkout Journey");
    expect(vm.report.journeys[0].stages).toHaveLength(3);
  });

  it("renders empty state via i18n key", () => {
    const vm = buildDataJourneyValidationViewModel({ data_journey_validation: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.data_journey_validation_empty");
  });

  it("maps journey status badges", () => {
    expect(journeyStatusBadgeClass("HEALTHY")).toBe("badge badge-green");
    expect(journeyStatusBadgeClass("BROKEN")).toBe("badge badge-red");
    expect(stageStatusColor("PASS")).toBe("#22c55e");
    expect(stageStatusColor("UNKNOWN")).toBe("#eab308");
    expect(stageStatusBadgeClass("FAIL")).toBe("badge badge-red");
  });

  it("derives stage status from validation results", () => {
    const ordersStage = sampleJourney.stages[2];
    expect(deriveStageStatus(ordersStage, sampleResult, sampleReport)).toBe("PASS");
    expect(deriveStageStatus(ordersStage, { ...sampleResult, missing_stages: ["Orders DB"] }, sampleReport)).toBe(
      "FAIL",
    );
  });

  it("builds preview payload for modal", () => {
    const journey = buildDataJourneyValidationViewModel(sampleReport, t).report.journeys[0];
    const payload = buildJourneyPreviewPayload(journey, journey.result, journey.stages, t);
    expect(payload.title).toBe("Checkout Journey");
    expect(payload.missingStages).toContain("Inventory DB");
    expect(payload.readOnlyNote).toBe("incident.qa.data_journey_validation_read_only_note");
  });

  it("builds drilldown items for database and contract stages", () => {
    const ordersItems = buildStageDrilldownItems(sampleJourney.stages[2], sampleReport);
    expect(ordersItems[0].related_entity_type).toBe("database_validation_check");
    const apiItems = buildStageDrilldownItems(sampleJourney.stages[1], sampleReport);
    expect(apiItems[0].related_entity_type).toBe("api_contract");
  });

  it("exposes translation keys", () => {
    expect(DATA_JOURNEY_VALIDATION_I18N_KEYS.title).toBe("incident.qa.data_journey_validation");
    expect(DATA_JOURNEY_VALIDATION_I18N_KEYS.missingStages).toBe(
      "incident.qa.data_journey_validation_missing_stages",
    );
    expect(formatJourneyConfidence(0.72)).toBe("72%");
  });
});
