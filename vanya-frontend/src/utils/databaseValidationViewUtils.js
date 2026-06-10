/** View helpers for Database Validation Checks (INT-01A). */

export const DATABASE_VALIDATION_I18N_KEYS = {
  title: "incident.qa.database_validation",
  empty: "incident.qa.database_validation_empty",
  preview: "incident.qa.database_validation_preview",
  queryPreview: "incident.qa.database_validation_query_preview",
  readOnlySafety: "incident.qa.database_validation_read_only_safety",
  approvalRequired: "incident.qa.database_validation_approval_required",
  databaseType: "incident.qa.database_validation_database_type",
  expectedResult: "incident.qa.database_validation_expected_result",
  enabled: "incident.qa.database_validation_enabled",
  disabled: "incident.qa.database_validation_disabled",
  previewSubtitle: "incident.qa.database_validation_preview_subtitle",
  validationPurpose: "incident.qa.database_validation_validation_purpose",
  connectorNote: "incident.qa.database_validation_connector_note",
  futureFooter: "incident.qa.database_validation_future_footer",
  safetySafe: "incident.qa.database_validation_safety_safe",
  safetyUnsafe: "incident.qa.database_validation_safety_unsafe",
};

const BLOCKED_KEYWORDS = [
  "INSERT",
  "UPDATE",
  "DELETE",
  "DROP",
  "ALTER",
  "CREATE",
  "TRUNCATE",
  "MERGE",
  "GRANT",
  "REVOKE",
  "CALL",
  "EXEC",
  "EXECUTE",
];

export function hasDatabaseValidationSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "database_validation");
}

export function isDatabaseValidationEmpty(report) {
  return report?.database_validation == null;
}

export function isQuerySafe(query) {
  const q = String(query || "").trim();
  if (!q) return { safe: false, reason: "Empty query" };

  const stripped = q.replace(/;\s*$/, "").trim();
  if (stripped.includes(";")) {
    return { safe: false, reason: "Multiple statements are not allowed" };
  }

  const upper = q.replace(/\s+/g, " ").toUpperCase();
  for (const keyword of BLOCKED_KEYWORDS) {
    const pattern = new RegExp(`\\b${keyword}\\b`);
    if (pattern.test(upper)) {
      return { safe: false, reason: `Blocked keyword: ${keyword}` };
    }
  }

  const compact = upper.trimStart();
  if (compact.startsWith("WITH")) {
    if (!compact.includes("SELECT")) {
      return { safe: false, reason: "WITH queries must contain SELECT" };
    }
  } else if (compact.startsWith("EXPLAIN")) {
    if (!compact.includes("SELECT")) {
      return { safe: false, reason: "EXPLAIN is only allowed for SELECT queries" };
    }
  } else if (!compact.startsWith("SELECT")) {
    return { safe: false, reason: "Only SELECT-style queries are allowed" };
  }

  return { safe: true, reason: "Read-only SELECT query approved" };
}

export function formatExpectedResult(check) {
  const type = check?.expected_result_type || "—";
  const value = check?.expected_value;
  if (value == null || value === "") return type;
  return `${type}: ${value}`;
}

export function buildDatabaseValidationPreviewPayload(check, t) {
  const safety = isQuerySafe(check?.query);
  return {
    title: check?.name || t(DATABASE_VALIDATION_I18N_KEYS.title),
    query: check?.query || "—",
    safetyText: safety.safe
      ? t(DATABASE_VALIDATION_I18N_KEYS.safetySafe)
      : `${t(DATABASE_VALIDATION_I18N_KEYS.safetyUnsafe)} — ${safety.reason}`,
    safetySafe: safety.safe,
    fields: [
      { label: t(DATABASE_VALIDATION_I18N_KEYS.validationPurpose), value: check?.description || "—" },
      { label: t(DATABASE_VALIDATION_I18N_KEYS.databaseType), value: check?.database_type || "—" },
      { label: t(DATABASE_VALIDATION_I18N_KEYS.expectedResult), value: formatExpectedResult(check) },
      { label: t(DATABASE_VALIDATION_I18N_KEYS.connectorNote), value: t(DATABASE_VALIDATION_I18N_KEYS.previewSubtitle) },
    ],
    futureFooter: t(DATABASE_VALIDATION_I18N_KEYS.futureFooter),
  };
}

export function buildDatabaseValidationViewModel(report, t) {
  const validation = report?.database_validation ?? null;
  return {
    show: hasDatabaseValidationSection(report),
    empty: isDatabaseValidationEmpty(report),
    emptyMessage: t(DATABASE_VALIDATION_I18N_KEYS.empty),
    title: t(DATABASE_VALIDATION_I18N_KEYS.title),
    previewLabel: t(DATABASE_VALIDATION_I18N_KEYS.preview),
    queryPreviewLabel: t(DATABASE_VALIDATION_I18N_KEYS.queryPreview),
    readOnlySafetyLabel: t(DATABASE_VALIDATION_I18N_KEYS.readOnlySafety),
    approvalRequiredLabel: t(DATABASE_VALIDATION_I18N_KEYS.approvalRequired),
    databaseTypeLabel: t(DATABASE_VALIDATION_I18N_KEYS.databaseType),
    expectedResultLabel: t(DATABASE_VALIDATION_I18N_KEYS.expectedResult),
    enabledLabel: t(DATABASE_VALIDATION_I18N_KEYS.enabled),
    disabledLabel: t(DATABASE_VALIDATION_I18N_KEYS.disabled),
    futureFooter: t(DATABASE_VALIDATION_I18N_KEYS.futureFooter),
    validation: validation
      ? {
          ...validation,
          checks: (validation.checks || []).map((check) => {
            const safety = isQuerySafe(check.query);
            return {
              ...check,
              expectedResultText: formatExpectedResult(check),
              enabledLabel: check.enabled
                ? t(DATABASE_VALIDATION_I18N_KEYS.enabled)
                : t(DATABASE_VALIDATION_I18N_KEYS.disabled),
              safetyText: safety.safe
                ? t(DATABASE_VALIDATION_I18N_KEYS.safetySafe)
                : t(DATABASE_VALIDATION_I18N_KEYS.safetyUnsafe),
              safetySafe: safety.safe,
              previewPayload: buildDatabaseValidationPreviewPayload(check, t),
            };
          }),
        }
      : null,
  };
}
