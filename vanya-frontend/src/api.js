// src/api.js
export const API_BASE =
  (import.meta.env.VITE_API_BASE_URL || "").trim() ||
  (import.meta.env.VITE_API_BASE || "").trim() ||
  "https://qa-bot-demoqa.onrender.com"; // fallback seguro

let accessTokenGetter = () => null;
let unauthorizedHandler = null;
let apiErrorNotifier = null;

/** Register how to read the current Supabase access_token (e.g. () => session?.access_token ?? null). */
export function setAccessTokenGetter(fn) {
  accessTokenGetter = typeof fn === "function" ? fn : () => null;
}

/** Called on 401 — e.g. signOut + redirect to login. */
export function setUnauthorizedHandler(fn) {
  unauthorizedHandler = typeof fn === "function" ? fn : null;
}

/** Toast for 403 / 429 / 5xx — set by ToastProvider. Payload: { status, detail } */
export function setApiErrorNotifier(fn) {
  apiErrorNotifier = typeof fn === "function" ? fn : null;
}

function mergeAuthHeaders(headersInit, body) {
  const h = new Headers(headersInit || {});
  const t = accessTokenGetter?.();
  if (t) h.set("Authorization", `Bearer ${t}`);
  if (body instanceof FormData) h.delete("Content-Type");
  return h;
}

/** Strip HTML / proxy noise and shorten noisy transport errors for UI toasts. */
export function normalizeErrorText(text) {
  if (text == null) return null;
  const raw = String(text).trim();
  if (!raw) return null;

  if (/Trailers must have END_STREAM/i.test(raw)) {
    return "Connection interrupted. Please refresh.";
  }
  if (/ConnectionTerminated/i.test(raw)) {
    return "Connection lost. Please try again.";
  }
  if (/^KeyError:\s*\d+$/i.test(raw) || /^KeyError\(\d+\)$/i.test(raw)) {
    return "Database connection interrupted. Please refresh.";
  }
  if (/JSON could not be generated/i.test(raw)) {
    return "Server returned an invalid response. Please try again.";
  }
  if (
    /<title>\s*400 Bad Request\s*<\/title>/i.test(raw) ||
    /<h1>\s*400 Bad Request\s*<\/h1>/i.test(raw)
  ) {
    return "Gateway error (400). Please refresh in a moment.";
  }
  if (/<html[\s>]/i.test(raw)) {
    const titleMatch = raw.match(/<title[^>]*>([^<]+)<\/title>/i);
    if (titleMatch?.[1]) return String(titleMatch[1]).trim().slice(0, 120);
    return "Unexpected HTML response from server.";
  }

  return raw.length > 300 ? `${raw.slice(0, 300)}…` : raw;
}

/** Parse FastAPI-style JSON error body into a single string (or plain text). */
export function parseResponseDetail(text) {
  if (text == null || String(text).trim() === "") return null;
  try {
    const j = JSON.parse(text);
    if (typeof j.detail === "string") return normalizeErrorText(j.detail);
    if (Array.isArray(j.detail)) {
      return normalizeErrorText(
        j.detail
          .map((d) => (typeof d === "string" ? d : d?.msg || JSON.stringify(d)))
          .join(" "),
      );
    }
    if (j.message) return normalizeErrorText(String(j.message));
    if (j.error) return normalizeErrorText(String(j.error));
  } catch {
    /* plain text */
  }
  return normalizeErrorText(String(text).trim());
}

export class ApiHttpError extends Error {
  constructor(status, message, rawBody) {
    super(message || `HTTP ${status}`);
    this.name = "ApiHttpError";
    this.status = status;
    this.rawBody = rawBody;
  }
}

/**
 * Throw ApiHttpError if !res.ok. Invokes unauthorizedHandler on 401.
 * Fires apiErrorNotifier for 403 / 429 / 5xx (not 401).
 */
export function throwIfNotOk(res, text) {
  if (res.ok) return;
  const status = res.status;
  const detail = parseResponseDetail(text);

  if (status === 401 && unauthorizedHandler) {
    try {
      unauthorizedHandler();
    } catch {
      /* ignore */
    }
    throw new ApiHttpError(status, detail || "Session expired. Please sign in again.", text);
  }

  const notify = apiErrorNotifier && (status === 403 || status === 429 || status >= 500);
  if (notify) {
    try {
      apiErrorNotifier({ status, detail });
    } catch {
      /* ignore */
    }
  }

  if (status >= 500) {
    try {
      console.error("[api] HTTP", status, detail || text?.slice?.(0, 500) || "");
    } catch {
      /* ignore */
    }
  }

  const msg =
    detail ||
    (status === 403
      ? "Access denied."
      : status === 429
        ? "Too many requests. Please try again later."
        : status >= 500
          ? "Server error. Please try again later."
          : `Request failed (${status})`);

  throw new ApiHttpError(status, msg, text);
}

function parseJsonBody(res, text) {
  const trimmed = (text || "").trim();
  if (!trimmed) return null;

  const contentType = (res.headers.get("content-type") || "").toLowerCase();
  const looksJson =
    !contentType ||
    contentType.includes("application/json") ||
    contentType.includes("+json");

  if (res.ok && contentType && !looksJson) {
    throw new ApiHttpError(
      res.status,
      `Expected JSON but received ${contentType.split(";")[0].trim()}`,
      text,
    );
  }

  try {
    return JSON.parse(trimmed);
  } catch {
    throw new ApiHttpError(res.status, "Invalid JSON response from server.", text);
  }
}

/**
 * Safe JSON GET — reads text first, validates status/content-type, then parses.
 * @template T
 * @returns {Promise<T|null>}
 */
export async function safeJsonFetch(path, options = {}) {
  const res = await apiFetch(path, {
    ...options,
    method: options.method || "GET",
    headers: {
      Accept: "application/json",
      ...(options.headers || {}),
    },
  });
  const text = await res.text();
  throwIfNotOk(res, text);
  return parseJsonBody(res, text);
}

/** Default client-side timeout for API requests (ms). */
export const DEFAULT_API_TIMEOUT_MS = 25000;

/**
 * Low-level fetch to the API with Bearer token when available.
 * Aborts after timeoutMs to avoid infinite loading states in the UI.
 */
export async function apiFetch(path, options = {}) {
  const { headers: ho, timeoutMs = DEFAULT_API_TIMEOUT_MS, signal: outerSignal, ...rest } = options;
  const headers = mergeAuthHeaders(ho, rest.body);
  const controller = new AbortController();
  let timedOut = false;
  const timer = setTimeout(() => {
    timedOut = true;
    controller.abort();
  }, timeoutMs);
  const onOuterAbort = () => controller.abort();
  if (outerSignal) {
    if (outerSignal.aborted) controller.abort();
    else outerSignal.addEventListener("abort", onOuterAbort, { once: true });
  }
  try {
    return await fetch(`${API_BASE}${path}`, { ...rest, headers, signal: controller.signal });
  } catch (e) {
    if (timedOut || e?.name === "AbortError") {
      throw new ApiHttpError(
        0,
        "Request timed out. The server may be busy — try again in a moment.",
      );
    }
    throw e;
  } finally {
    clearTimeout(timer);
    if (outerSignal) outerSignal.removeEventListener("abort", onOuterAbort);
  }
}

export async function apiGet(path) {
  return safeJsonFetch(path, {
    method: "GET",
    headers: { "Content-Type": "application/json" },
  });
}

export async function apiDelete(path) {
  const res = await apiFetch(path, {
    method: "DELETE",
    headers: { Accept: "application/json", "Content-Type": "application/json" },
  });
  const txt = await res.text();
  throwIfNotOk(res, txt);
  if (!txt || !txt.trim()) return null;
  try {
    return parseJsonBody(res, txt);
  } catch {
    return null;
  }
}

export async function apiPut(path, body) {
  const res = await apiFetch(path, {
    method: "PUT",
    headers: { Accept: "application/json", "Content-Type": "application/json" },
    body: JSON.stringify(body || {}),
  });
  const txt = await res.text();
  throwIfNotOk(res, txt);
  if (!txt || !txt.trim()) return null;
  return parseJsonBody(res, txt);
}

export async function apiPatch(path, body) {
  const res = await apiFetch(path, {
    method: "PATCH",
    headers: { Accept: "application/json", "Content-Type": "application/json" },
    body: JSON.stringify(body ?? {}),
  });
  const txt = await res.text();
  throwIfNotOk(res, txt);
  if (!txt || !txt.trim()) return null;
  return parseJsonBody(res, txt);
}

/** Human-readable message from fetch errors (FastAPI JSON detail or plain text). */
export function apiErrorMessage(err) {
  if (err instanceof ApiHttpError) {
    return normalizeErrorText(err.message) || "Request failed";
  }
  const raw = err?.message ?? String(err);
  if (!raw) return "Request failed";
  const parsed = parseResponseDetail(raw);
  return normalizeErrorText(parsed) || normalizeErrorText(raw) || "Request failed";
}

export async function apiPost(path, body) {
  const res = await apiFetch(path, {
    method: "POST",
    headers: { Accept: "application/json", "Content-Type": "application/json" },
    body: JSON.stringify(body || {}),
  });
  const txt = await res.text();
  throwIfNotOk(res, txt);
  if (!txt || !txt.trim()) return null;
  return parseJsonBody(res, txt);
}

/** JSON request + JSON response with centralized error handling. */
export async function apiFetchJson(path, options = {}) {
  const res = await apiFetch(path, {
    ...options,
    headers: { Accept: "application/json", ...(options.headers || {}) },
  });
  const txt = await res.text();
  throwIfNotOk(res, txt);
  if (!txt || !txt.trim()) return null;
  return parseJsonBody(res, txt);
}

// ========= Convenience =========
export const listThreads = () => apiGet("/threads");
export const createThread = () => apiPost("/threads", {});
export const getThread = (id) => apiGet(`/threads/${id}`);
export const deleteThread = (id) => apiDelete(`/threads/${id}`);

export const chatRun = (prompt, thread_id, extra = {}) =>
  apiPost("/chat_run", { prompt, thread_id, headless: true, ...extra });

// ========= Dashboard =========
export function getDashboardSummary(params = {}) {
  const q = new URLSearchParams();
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/dashboard/summary${qs ? `?${qs}` : ""}`);
}

export function getProjectOnboarding(projectId) {
  const pid = String(projectId || "").trim();
  return apiGet(`/projects/${encodeURIComponent(pid)}/onboarding`);
}

export function getProjectQualityTrends(projectId) {
  const pid = String(projectId || "").trim();
  return apiGet(`/projects/${encodeURIComponent(pid)}/quality-trends`);
}

export function getProjectEarlyDegradation(projectId) {
  const pid = String(projectId || "").trim();
  return apiGet(`/projects/${encodeURIComponent(pid)}/early-degradation`);
}

export function getProjectReleaseReadiness(projectId) {
  const pid = String(projectId || "").trim();
  return apiGet(`/projects/${encodeURIComponent(pid)}/release-readiness`);
}

export function previewReportDelivery(projectId, body) {
  const pid = String(projectId || "").trim();
  return apiPost(`/projects/${encodeURIComponent(pid)}/reports/preview`, body);
}

export function sendReportDelivery(projectId, body) {
  const pid = String(projectId || "").trim();
  return apiPost(`/projects/${encodeURIComponent(pid)}/reports/send`, body);
}

export function getSecurityReadiness() {
  return apiGet("/security/readiness");
}

export function getSecurityProviders(enabledOnly = true) {
  const q = enabledOnly ? "?enabled_only=true" : "?enabled_only=false";
  return apiGet(`/security/providers${q}`);
}

export function getRbacReadiness() {
  return apiGet("/security/rbac");
}

export function getSecurityRoles() {
  return apiGet("/security/roles");
}

export function getSecurityPermissions() {
  return apiGet("/security/permissions");
}

export function getDashboardRecentRuns(limit = 20, project_id) {
  const q = new URLSearchParams({ limit: String(limit) });
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  return apiGet(`/dashboard/recent-runs?${q}`);
}

export function getDashboardRecentJobs(limit = 20, project_id) {
  const q = new URLSearchParams({ limit: String(limit) });
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  return apiGet(`/dashboard/recent-jobs?${q}`);
}

export function getDashboardByModule(project_id) {
  const q = new URLSearchParams();
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/dashboard/by-module${qs ? `?${qs}` : ""}`);
}

// ========= Test Catalog =========
export function listTests(params = {}) {
  const q = new URLSearchParams();
  if (params.module)   q.set("module",   params.module);
  if (params.type)     q.set("type",     params.type);
  if (params.priority) q.set("priority", params.priority);
  if (params.status !== undefined) q.set("status", params.status);
  if (params.test_type) q.set("test_type", params.test_type);
  if (params.search)   q.set("search",   params.search);
  if (params.limit)    q.set("limit",    params.limit);
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/tests${qs ? "?" + qs : ""}`);
}
export const getTest      = (tc_id)            => apiGet(`/tests/${tc_id}`);
/** Persist a catalog test from an executed run (planner/chat/run_store). */
export function createTestFromRun(body) {
  return apiPost("/tests/from-run", {
    run_id: body.run_id,
    name: body.name,
    project_id: body.project_id,
  });
}
export const previewAutoFix = (body)           => apiPost("/tests/auto-fix-preview", body);
/** LLM proposal for catalog steps/assertions (preview only; no persist). */
export const previewCatalogAiEdit = (body)     => apiPost("/tests/ai-edit-preview", body);
export const runTest      = (tc_id, body = {}) => apiPost(`/tests/${tc_id}/run`, body);
export const runSuite     = (body)             => apiPost("/tests/run-suite", body);
export const updateTest   = (tc_id, body = {}) => apiPut(`/tests/${tc_id}`, body);
export const listVersions = (tc_id)            => apiGet(`/tests/${tc_id}/versions`);
export const getVersion   = (tc_id, v)         => apiGet(`/tests/${tc_id}/versions/${v}`);
export const diffVersions = (tc_id, from_, to) => apiGet(`/tests/${tc_id}/versions/${from_}/diff/${to}`);
export const rollbackTest = (tc_id, body)       => apiPost(`/tests/${tc_id}/rollback`, body);

// ========= Test Runs =========
export function listTestRuns(params = {}) {
  const q = new URLSearchParams();
  if (params.test_case_id) q.set("test_case_id", params.test_case_id);
  if (params.limit)        q.set("limit",        params.limit);
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/test-runs${qs ? "?" + qs : ""}`);
}
export const getTestRun = (run_id) => apiGet(`/test-runs/${run_id}`);

// ========= Evidence Library =========
export function listEvidences(params = {}) {
  const q = new URLSearchParams();
  if (params.test_case_id) q.set("test_case_id", params.test_case_id);
  if (params.limit)        q.set("limit",        params.limit);
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/evidences${qs ? "?" + qs : ""}`);
}
export function getEvidenceSummary(project_id) {
  const q = new URLSearchParams();
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/evidences/summary${qs ? `?${qs}` : ""}`);
}

// ========= Orchestrator / Execution =========
export function listJobs(limit = 100, project_id) {
  const q = new URLSearchParams({ limit: String(limit) });
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  return apiGet(`/orchestrator/jobs?${q}`);
}
export const getJob          = (id)          => apiGet(`/orchestrator/jobs/${id}`);
export const enqueueSingle   = (body)        => apiPost("/orchestrator/jobs/single", body);
export const enqueueSuite    = (body)        => apiPost("/orchestrator/jobs/suite", body);
export const runBatch        = (body)        => apiPost("/execution/run-batch", body);
export const retryFailed     = (body)        => apiPost("/execution/retry-failed", body);
export const getExecStatus   = ()            => apiGet("/execution/status");

// ========= RCA / Business Risk =========
export const analyzeRCA      = (body) => apiPost("/rca/analyze",           body);
export const analyzeRisk     = (body) => apiPost("/business-risk/analyze", body);

// ========= Test Generation =========
export const generateTests   = (body) => apiPost("/test-generation/generate", body);
export const approveTests    = (body) => apiPost("/test-generation/approve",  body);

// ========= Explorer Drafts (legacy) =========
export const generateDrafts          = (url)    => apiPost("/drafts/generate",            { url });
export const generateDraftsFromPages = (pages)  => apiPost("/drafts/generate-from-pages", { pages });
export const approveDrafts = (drafts, { activate = true, projectId = null } = {}) => {
  const body = { drafts, activate };
  if (projectId != null && String(projectId).trim()) {
    body.project_id = String(projectId).trim();
  }
  return apiPost("/drafts/approve", body);
};

// ========= Persistent Drafts =========
export const listSavedDrafts     = (status)         => apiGet(`/drafts${status ? `?status=${status}` : ""}`);
export const batchSaveDrafts     = (drafts)         => apiPost("/drafts/batch", { drafts });
export const createSavedDraft    = (body)            => apiPost("/drafts", body);
export const updateSavedDraft    = (id, body)        => apiPut(`/drafts/${id}`, body);
export const deleteSavedDraft    = (id)              => apiDelete(`/drafts/${id}`);
export const approveSavedDraft   = (id)              => apiPost(`/drafts/${id}/approve`, {});
export const aiSuggestDraft              = (id) => apiPost(`/drafts/${id}/ai-suggest`,                    {});
export const suggestDraftAssertions      = (id) => apiPost(`/drafts/${id}/suggest-assertions`,            {});
export const suggestDraftAssertionsFromDom = (id) => apiPost(`/drafts/${id}/suggest-assertions-from-dom`, {});

// ========= App Map =========
export const exploreApp = (url, maxPages = 5, projectId = null) => {
  const body = { url, max_pages: maxPages };
  if (projectId != null && String(projectId).trim()) {
    body.project_id = String(projectId).trim();
  }
  return apiPost("/app-explorer/explore-app", body);
};

// ========= PR Analysis =========
export const analyzePR           = (body) => apiPost("/pr-analysis/analyze",             body);
export const analyzePRAndEnqueue = (body) => apiPost("/pr-analysis/analyze-and-enqueue", body);

// ========= GitHub =========
export const fetchGithubPR = (url, projectId = null) => {
  const body = { url };
  if (projectId != null && String(projectId).trim()) {
    body.project_id = String(projectId).trim();
  }
  return apiPost("/github/pr/fetch", body);
};

// ========= API Testing =========
export const parseSpec        = (body) => apiPost("/api-testing/parse-spec",     body);
export const generateApiTests = (body) => apiPost("/api-testing/generate-tests", body);
export const approveApiTests  = (body) => apiPost("/api-testing/approve",        body);
export const runApiTests      = (body) => apiPost("/api-testing/run",            body);

// ========= Run Analytics =========
export function getRunsAnalytics(project_id) {
  const q = new URLSearchParams();
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/analytics/runs/dashboard${qs ? `?${qs}` : ""}`);
}

// ========= Coverage =========
export const getCoverageSummary    = () => apiGet("/coverage/summary");
export const generateCoverageTests = (module) => apiPost("/coverage/generate-tests", { module });
export const saveCoverageDrafts    = (module, suggestions) => apiPost("/coverage/save-drafts", { module, suggestions });

// ========= Failure Intelligence =========
export function getFailureIntel(project_id) {
  const q = new URLSearchParams();
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/failure-intelligence/summary${qs ? `?${qs}` : ""}`);
}

export function getFlakyTests(project_id) {
  const q = new URLSearchParams();
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/failure-intelligence/flaky-tests${qs ? `?${qs}` : ""}`);
}

export function getRegressions(project_id) {
  const q = new URLSearchParams();
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/failure-intelligence/regressions${qs ? `?${qs}` : ""}`);
}

export function getClusters(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.module) q.set("module", params.module);
  if (params.root_cause_category) q.set("root_cause_category", params.root_cause_category);
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/failure-intelligence/clusters${qs ? `?${qs}` : ""}`);
}
export const getRunClusters  = (limit = 50) => apiGet(`/failure-intelligence/run-clusters?limit=${limit}`);

export function getDeepInsights(project_id) {
  const q = new URLSearchParams();
  if (project_id != null && String(project_id).trim()) {
    q.set("project_id", String(project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/failure-intelligence/deep-insights${qs ? `?${qs}` : ""}`);
}

// ========= Risk-Based Test Selection =========
export const selectTests    = (body) => apiPost("/risk-selection/select-tests",   body);
export const selectAndRun   = (body) => apiPost("/risk-selection/select-and-run", body);
export const suggestModules = (body) => apiPost("/risk-selection/suggest-modules", body);

// ========= Integrations =========
export const listIntegrations         = ()          => apiGet(`/integrations`);
export const getIntegration           = (id)        => apiGet(`/integrations/${id}`);
export const runHealthCheck           = (id)        => apiPost(`/integrations/${id}/health-check`, {});
export const enableIntegration        = (id)        => apiPost(`/integrations/${id}/enable`,       {});
export const disableIntegration       = (id)        => apiPost(`/integrations/${id}/disable`,      {});
export const updateIntegrationConfig  = (id, body)  => apiPost(`/integrations/${id}/config`,       body);
export const getIntegrationActions    = (id)        => apiGet(`/integrations/${id}/actions`);
export const alertingReady            = ()          => apiGet(`/integrations/alerting/ready`);
export const sendAlert                = (body)      => apiPost("/integrations/send-alert", body);
export const integrationsReadiness    = ()          => apiGet(`/integrations/readiness`);
export const createItsmTicket         = (body)      => apiPost("/integrations/create-ticket", body);

// ========= Jira (read-only discovery, JIRA-01A) =========
export const getJiraStatus            = ()          => apiGet("/integrations/jira/status");
export const listJiraProjects         = ()          => apiGet("/integrations/jira/projects");
export const listJiraIssues           = (params = {}) => {
  const qs = new URLSearchParams();
  if (params.project_key) qs.set("project_key", params.project_key);
  if (params.max_results != null) qs.set("max_results", String(params.max_results));
  const q = qs.toString();
  return apiGet(`/integrations/jira/issues${q ? `?${q}` : ""}`);
};
export const listJiraEpics            = (params = {}) => {
  const qs = new URLSearchParams();
  if (params.project_key) qs.set("project_key", params.project_key);
  if (params.max_results != null) qs.set("max_results", String(params.max_results));
  const q = qs.toString();
  return apiGet(`/integrations/jira/epics${q ? `?${q}` : ""}`);
};
export const listJiraReleases         = (params = {}) => {
  const qs = new URLSearchParams();
  if (params.project_key) qs.set("project_key", params.project_key);
  const q = qs.toString();
  return apiGet(`/integrations/jira/releases${q ? `?${q}` : ""}`);
};
export const listJiraFixVersions      = (params = {}) => {
  const qs = new URLSearchParams();
  if (params.project_key) qs.set("project_key", params.project_key);
  const q = qs.toString();
  return apiGet(`/integrations/jira/fix-versions${q ? `?${q}` : ""}`);
};

// ========= Projects (multi-project catalog scope) =========
/** GET /projects — 200 + [] or empty body must be success (never throw from JSON parse). */
export async function listProjects() {
  const res = await apiFetch("/projects", { method: "GET", headers: { "Content-Type": "application/json" } });
  const text = await res.text();
  throwIfNotOk(res, text);
  if (text == null || text.trim() === "") return [];
  try {
    const data = JSON.parse(text);
    return Array.isArray(data) ? data : [];
  } catch {
    throw new ApiHttpError(res.status, text.slice(0, 200) || "Invalid JSON response", text);
  }
}
export const getProject     = (projectId)           => apiGet(`/projects/${encodeURIComponent(projectId)}`);
export const createProject  = (payload)           => apiPost("/projects", payload);
export const updateProject  = (projectId, payload) => apiPatch(`/projects/${encodeURIComponent(projectId)}`, payload);
export const deleteProject  = (projectId)           => apiDelete(`/projects/${encodeURIComponent(projectId)}`);
/** POST /projects/{id}/initialize — rebuild memory, inventory catalog, optional smoke queue. */
export function initializeProject(projectId, body = {}) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/initialize`, {
    run_smoke: body.run_smoke !== false,
    refresh_knowledge: body.refresh_knowledge !== false,
  });
}

// ========= Browser inspection watches (Phase 3G–4E) =========
/** POST /browser-inspections/watch — create watch (execution_mode: cloud | local_agent). */
export function createBrowserInspectionWatch(body) {
  return apiPost("/browser-inspections/watch", body ?? {});
}

/** GET /browser-inspections/watch — list watches (optional project scope). */
export function listBrowserInspectionWatches(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/browser-inspections/watch${qs ? `?${qs}` : ""}`);
}

export const getBrowserInspectionWatch = (watchId) =>
  apiGet(`/browser-inspections/watch/${encodeURIComponent(watchId)}`);

export function patchBrowserInspectionWatch(watchId, body) {
  return apiPatch(`/browser-inspections/watch/${encodeURIComponent(watchId)}`, body ?? {});
}

/** POST /browser-inspections/watch/{id}/run-now */
export function postBrowserWatchRunNow(watchId, { force = false } = {}) {
  const q = force ? "?force=true" : "";
  return apiPost(`/browser-inspections/watch/${encodeURIComponent(watchId)}/run-now${q}`, {});
}

/** Pin baseline to latest inspection (use_latest). */
export function postBrowserWatchBaselineUseLatest(watchId) {
  return apiPost(`/browser-inspections/watch/${encodeURIComponent(watchId)}/baseline`, { use_latest: true });
}

export const getBrowserWatchMetrics = (watchId) =>
  apiGet(`/browser-inspections/watch/${encodeURIComponent(watchId)}/metrics`);

/**
 * GET /browser-inspections/watch/{id}/events with cursor pagination.
 * Returns { items, next_cursor } when paged (default).
 */
export function getBrowserWatchEventsPage(watchId, params = {}) {
  const q = new URLSearchParams({ paged: "true" });
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.cursor) q.set("cursor", String(params.cursor));
  return apiGet(`/browser-inspections/watch/${encodeURIComponent(watchId)}/events?${q}`);
}

// ========= Local agents admin (Phase 4E) — optional admin headers for GET /local-agents (configure per deployment). =========
function localAgentAdminHeaders() {
  const h = {};
  const reg = (import.meta.env.VITE_LOCAL_AGENT_REGISTER_KEY || "").trim();
  if (reg) h["X-Vanya-Local-Agent-Register-Key"] = reg;
  const st = (import.meta.env.VITE_VANYA_SERVICE_TOKEN || "").trim();
  if (st) h["X-Service-Token"] = st;
  return h;
}

/** GET /local-agents — requires admin key when LOCAL_AGENT_REGISTER_SECRET is set on server. */
export function listLocalAgents(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/local-agents${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

export function getLocalAgent(agentId) {
  return apiFetchJson(`/local-agents/${encodeURIComponent(agentId)}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

export function disableLocalAgent(agentId) {
  return apiFetchJson(`/local-agents/${encodeURIComponent(agentId)}/disable`, {
    method: "POST",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
    body: "{}",
  });
}

/** GET /local-agents/database-connections — INT-03B secure connectors. */
export function listDatabaseConnections(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.agent_id != null && String(params.agent_id).trim()) {
    q.set("agent_id", String(params.agent_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/local-agents/database-connections${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

/** GET /local-agents/internal-apis — INT-03C internal API connectors. */
export function listInternalApiConnectors(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.agent_id != null && String(params.agent_id).trim()) {
    q.set("agent_id", String(params.agent_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/local-agents/internal-apis${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

/** GET /local-agents/enterprise-systems — INT-03D enterprise systems. */
export function listEnterpriseSystems(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.agent_id != null && String(params.agent_id).trim()) {
    q.set("agent_id", String(params.agent_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/local-agents/enterprise-systems${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

/** GET /enterprise-system-validations */
export function listEnterpriseSystemValidations(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.connector_id != null && String(params.connector_id).trim()) {
    q.set("connector_id", String(params.connector_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/enterprise-system-validations${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

/** GET /internal-api-validations */
export function listInternalApiValidations(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.connector_id != null && String(params.connector_id).trim()) {
    q.set("connector_id", String(params.connector_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/internal-api-validations${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

/** GET /database-validation/executions */
export function listDatabaseValidationExecutions(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.connection_id != null && String(params.connection_id).trim()) {
    q.set("connection_id", String(params.connection_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/database-validation/executions${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

/** POST /database-validation/approvals/simulate */
export function simulateDatabaseValidationApproval(body) {
  return apiFetchJson("/database-validation/approvals/simulate", {
    method: "POST",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
}

/** POST /database-validation/executions */
export function executeDatabaseValidation(body) {
  return apiFetchJson("/database-validation/executions", {
    method: "POST",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
}

/** GET /local-agents/foundation/report — INT-03A foundation inventory report. */
export function getLocalAgentFoundationReport(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiFetchJson(`/local-agents/foundation/report${qs ? `?${qs}` : ""}`, {
    method: "GET",
    headers: { ...localAgentAdminHeaders(), "Content-Type": "application/json" },
  });
}

// ========= Incident Investigator (Autonomous) =========

/** POST /incidents/investigate */
export function investigateIncident(body) {
  return apiPost("/incidents/investigate", body);
}

/** GET /incidents/runs */
export function listIncidentRuns(params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  if (params.project_id) q.set("project_id", String(params.project_id));
  const qs = q.toString();
  return apiGet(`/incidents/runs${qs ? `?${qs}` : ""}`);
}

/** GET /incidents/runs/{id} */
export function getIncidentRun(runId) {
  return apiGet(`/incidents/runs/${encodeURIComponent(runId)}`);
}

/** POST /projects/{id}/incidents/investigate — QA Intelligence correlation */
export function investigateProjectIncident(projectId, body) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/incidents/investigate`, body);
}

/** GET /projects/{id}/incidents/history */
export function listProjectIncidentHistory(projectId, params = {}) {
  const q = new URLSearchParams();
  if (params.limit != null) q.set("limit", String(params.limit));
  const qs = q.toString();
  return apiGet(`/projects/${encodeURIComponent(projectId)}/incidents/history${qs ? `?${qs}` : ""}`);
}

/** GET /projects/{id}/incidents/{incidentId} */
export function getProjectIncidentReport(projectId, incidentId) {
  return apiGet(`/projects/${encodeURIComponent(projectId)}/incidents/${encodeURIComponent(incidentId)}`);
}

// ========= Project Knowledge / System Memory (Phase 1) =========

/** GET /projects/{id}/knowledge */
export function getProjectKnowledge(projectId) {
  return apiGet(`/projects/${encodeURIComponent(projectId)}/knowledge`);
}

/** PR Analysis v1 — System Memory + Risk Engine (project-scoped). */
export function analyzeProjectPR(projectId, body) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/pr-analysis`, body);
}

// ========= GitHub Integration — SaaS GitHub App (project-scoped) =========

export function getProjectGitHubInstallUrl(projectId) {
  return apiGet(`/projects/${encodeURIComponent(projectId)}/github/install-url`);
}

export function connectProjectGitHubApp(projectId, body) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/github/connect-app`, body);
}

export function listProjectGitHubRepositories(projectId) {
  return apiGet(`/projects/${encodeURIComponent(projectId)}/github/repositories`);
}

export function selectProjectGitHubRepository(projectId, body) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/github/select-repository`, body);
}

export function getProjectGitHubStatus(projectId, validate = true) {
  const q = new URLSearchParams({ validate: validate ? "true" : "false" });
  return apiGet(`/projects/${encodeURIComponent(projectId)}/github/status?${q}`);
}

export function disconnectProjectGitHub(projectId) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/github/disconnect`, {});
}

export function listProjectGitHubPRs(projectId, limit = 20) {
  const q = new URLSearchParams({ limit: String(limit) });
  return apiGet(`/projects/${encodeURIComponent(projectId)}/github/pull-requests?${q}`);
}

export function analyzeProjectGitHubPR(projectId, prNumber) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/github/pull-requests/${prNumber}/analyze`, {});
}

// ========= Azure DevOps Integration — OAuth (project-scoped) =========

export function getProjectAzureDevOpsAuthorizeUrl(projectId) {
  return apiGet(`/projects/${encodeURIComponent(projectId)}/azure-devops/authorize-url`);
}

export function getProjectAzureDevOpsStatus(projectId, validate = true) {
  const q = new URLSearchParams({ validate: validate ? "true" : "false" });
  return apiGet(`/projects/${encodeURIComponent(projectId)}/azure-devops/status?${q}`);
}

export function disconnectProjectAzureDevOps(projectId) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/azure-devops/disconnect`, {});
}

export function listProjectAzureDevOpsOrganizations(projectId) {
  return apiGet(`/projects/${encodeURIComponent(projectId)}/azure-devops/organizations`);
}

export function listProjectAzureDevOpsProjects(projectId, organization) {
  const q = new URLSearchParams({ organization });
  return apiGet(`/projects/${encodeURIComponent(projectId)}/azure-devops/projects?${q}`);
}

export function listProjectAzureDevOpsRepositories(projectId, organization, azureProject) {
  const q = new URLSearchParams({ organization, azure_project: azureProject });
  return apiGet(`/projects/${encodeURIComponent(projectId)}/azure-devops/repositories?${q}`);
}

export function selectProjectAzureDevOpsTarget(projectId, body) {
  return apiPost(`/projects/${encodeURIComponent(projectId)}/azure-devops/select-target`, body);
}

export function listProjectAzureDevOpsPRs(projectId, limit = 20) {
  const q = new URLSearchParams({ limit: String(limit) });
  return apiGet(`/projects/${encodeURIComponent(projectId)}/azure-devops/pull-requests?${q}`);
}

export function analyzeProjectAzureDevOpsPR(projectId, pullRequestId) {
  return apiPost(
    `/projects/${encodeURIComponent(projectId)}/azure-devops/pull-requests/${pullRequestId}/analyze`,
    {},
  );
}

/** POST /projects/{id}/knowledge/refresh — mode: "replace" (default) | "merge" */
export function refreshProjectKnowledge(projectId, mode = "replace") {
  const q = new URLSearchParams({ mode: String(mode || "replace") });
  return apiPost(`/projects/${encodeURIComponent(projectId)}/knowledge/refresh?${q}`, {});
}
