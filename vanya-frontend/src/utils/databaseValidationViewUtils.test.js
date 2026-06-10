import { describe, it, expect } from "vitest";
import {
  DATABASE_VALIDATION_I18N_KEYS,
  buildDatabaseValidationPreviewPayload,
  buildDatabaseValidationViewModel,
  formatExpectedResult,
  hasDatabaseValidationSection,
  isDatabaseValidationEmpty,
  isQuerySafe,
} from "./databaseValidationViewUtils.js";

const t = (key) => key;

const sampleCheck = {
  check_id: "dbcheck:checkout:validate_payment_status",
  name: "Validate Checkout payment status",
  description: "Confirm payment status is captured for the checkout order.",
  query: "SELECT payment_status FROM payments WHERE order_id = :order_id",
  database_type: "postgresql",
  expected_result_type: "status_equals",
  expected_value: "captured",
  enabled: true,
  requires_user_approval: true,
};

describe("databaseValidationViewUtils", () => {
  it("detects database validation section and empty state", () => {
    expect(hasDatabaseValidationSection({ database_validation: null })).toBe(true);
    expect(hasDatabaseValidationSection({})).toBe(false);
    expect(isDatabaseValidationEmpty({ database_validation: null })).toBe(true);
  });

  it("renders validation checks", () => {
    const vm = buildDatabaseValidationViewModel(
      {
        database_validation: {
          summary: "2 read-only database validation check(s) prepared.",
          confidence: 0.72,
          checks: [sampleCheck],
          results: [],
        },
      },
      t,
    );
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.validation.checks[0].name).toContain("payment status");
    expect(vm.validation.checks[0].safetySafe).toBe(true);
  });

  it("renders empty state via i18n key", () => {
    const vm = buildDatabaseValidationViewModel({ database_validation: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.database_validation_empty");
  });

  it("shows approval required badge data on checks", () => {
    const vm = buildDatabaseValidationViewModel(
      { database_validation: { checks: [sampleCheck], results: [], summary: "", confidence: 0.7 } },
      t,
    );
    expect(vm.validation.checks[0].requires_user_approval).toBe(true);
    expect(vm.approvalRequiredLabel).toBe(DATABASE_VALIDATION_I18N_KEYS.approvalRequired);
  });

  it("formats expected result and query preview payload", () => {
    expect(formatExpectedResult(sampleCheck)).toBe("status_equals: captured");
    const payload = buildDatabaseValidationPreviewPayload(sampleCheck, t);
    expect(payload.query).toContain("SELECT payment_status");
    expect(payload.safetySafe).toBe(true);
    expect(payload.futureFooter).toBe(DATABASE_VALIDATION_I18N_KEYS.futureFooter);
  });

  it("evaluates query safety status", () => {
    expect(isQuerySafe(sampleCheck.query).safe).toBe(true);
    expect(isQuerySafe("DELETE FROM payments").safe).toBe(false);
    expect(isQuerySafe("SELECT 1; DELETE FROM payments").safe).toBe(false);
  });

  it("builds preview modal fields", () => {
    const payload = buildDatabaseValidationPreviewPayload(sampleCheck, t);
    expect(payload.fields.some((f) => f.label === DATABASE_VALIDATION_I18N_KEYS.validationPurpose)).toBe(true);
    expect(payload.fields.some((f) => f.label === DATABASE_VALIDATION_I18N_KEYS.connectorNote)).toBe(true);
  });

  it("exposes translation keys", () => {
    expect(DATABASE_VALIDATION_I18N_KEYS.title).toBe("incident.qa.database_validation");
    expect(DATABASE_VALIDATION_I18N_KEYS.preview).toBe("incident.qa.database_validation_preview");
    expect(DATABASE_VALIDATION_I18N_KEYS.readOnlySafety).toBe("incident.qa.database_validation_read_only_safety");
  });
});
