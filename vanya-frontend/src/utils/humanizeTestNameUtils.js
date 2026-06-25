/** Human-readable labels for internal test identifiers (presentation only). */

const SKIP_SEGMENTS = new Set([
  "vanya",
  "self",
  "explorer",
  "smoke",
  "spec",
  "test",
  "e2e",
  "ui",
  "case",
  "suite",
]);

const SEGMENT_LABEL_KEYS = {
  exploration: "test_name.segment.exploration",
  landing: "test_name.segment.landing",
  login: "test_name.segment.login",
  logout: "test_name.segment.logout",
  checkout: "test_name.segment.checkout",
  auth: "test_name.segment.auth",
  authentication: "test_name.segment.auth",
  payment: "test_name.segment.payment",
  payments: "test_name.segment.payments",
  register: "test_name.segment.register",
  signup: "test_name.segment.signup",
  catalog: "test_name.segment.catalog",
  search: "test_name.segment.search",
  profile: "test_name.segment.profile",
  dashboard: "test_name.segment.dashboard",
  home: "test_name.segment.home",
  cart: "test_name.segment.cart",
  order: "test_name.segment.order",
  orders: "test_name.segment.orders",
};

function titleCaseToken(token) {
  if (!token) return "";
  return token.charAt(0).toUpperCase() + token.slice(1).toLowerCase();
}

function humanizeSegment(segment, t) {
  const key = String(segment || "").trim().toLowerCase();
  if (!key || SKIP_SEGMENTS.has(key)) return "";
  const labelKey = SEGMENT_LABEL_KEYS[key];
  if (labelKey && t) return t(labelKey);
  if (key === "api" && t) return t("test_name.segment.api");
  return titleCaseToken(key.replace(/-/g, " "));
}

export function humanizeTestName(testId, t) {
  const raw = String(testId || "").trim();
  if (!raw) return "";
  const normalized = raw.replace(/^vanya[_-]*/i, "");
  const parts = normalized
    .split(/__+/)
    .flatMap((chunk) => chunk.split(/_+/))
    .map((p) => p.trim())
    .filter(Boolean);

  const labels = parts
    .map((part) => humanizeSegment(part, t))
    .filter(Boolean);

  const deduped = [...new Set(labels)];
  if (deduped.length > 0) return deduped.join(" ");
  return titleCaseToken(normalized.replace(/[_-]+/g, " "));
}

export function formatTestDisplayName({ testId, testName, testCaseId, name }, t) {
  const id = testId || testCaseId || "";
  const explicit = String(testName || name || "").trim();
  if (explicit && explicit !== id) return explicit;
  return humanizeTestName(id, t) || explicit || id;
}

export function formatTestDisplayNameWithMeta(fields, t) {
  const id = fields?.testId || fields?.testCaseId || fields?.test_case_id || "";
  const display = formatTestDisplayName(
    {
      testId: id,
      testName: fields?.testName || fields?.test_name,
      name: fields?.name,
    },
    t,
  );
  return {
    display,
    technicalId: id || null,
    showTechnicalId: Boolean(id && display && display !== id),
  };
}
