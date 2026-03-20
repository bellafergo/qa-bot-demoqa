// src/api.js
const API_BASE =
  (import.meta.env.VITE_API_BASE_URL || "").trim() ||
  (import.meta.env.VITE_API_BASE || "").trim() ||
  "https://qa-bot-demoqa.onrender.com"; // fallback seguro

async function safeReadBody(res) {
  const text = await res.text();
  if (!text) return { text: "", json: null };
  try {
    return { text, json: JSON.parse(text) };
  } catch {
    return { text, json: null };
  }
}

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

// Retry solo para errores típicos de cold start / gateway
async function fetchWithRetry(url, options, retries = 2) {
  let lastErr;
  for (let i = 0; i <= retries; i++) {
    try {
      const res = await fetch(url, options);
      if ([502, 503, 504].includes(res.status) && i < retries) {
        await sleep(600 * (i + 1));
        continue;
      }
      return res;
    } catch (e) {
      lastErr = e;
      if (i < retries) {
        await sleep(600 * (i + 1));
        continue;
      }
      throw lastErr;
    }
  }
  throw lastErr || new Error("fetch failed");
}

async function handleJsonOrThrow(res) {
  const { text, json } = await safeReadBody(res);
  if (!res.ok) {
    const detail =
      (json && (json.detail || json.message)) ||
      text ||
      `${res.status} ${res.statusText}`;
    throw new Error(detail);
  }
  return json;
}

export async function apiGet(path) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "GET",
    headers: { "Content-Type": "application/json" },
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

export async function apiDelete(path) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "DELETE",
    headers: { "Content-Type": "application/json" },
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

export async function apiPut(path, body) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "PUT",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body || {}),
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

export async function apiPost(path, body) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body || {}),
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

// ========= Convenience =========
export const listThreads = () => apiGet("/threads");
export const createThread = () => apiPost("/threads", {});
export const getThread = (id) => apiGet(`/threads/${id}`);
export const deleteThread = (id) => apiDelete(`/threads/${id}`);

export const chatRun = (prompt, thread_id, extra = {}) =>
  apiPost("/chat_run", { prompt, thread_id, headless: true, ...extra });

// ========= Dashboard =========
export const getDashboardSummary   = ()            => apiGet("/dashboard/summary");
export const getDashboardRecentRuns = (limit = 20) => apiGet(`/dashboard/recent-runs?limit=${limit}`);
export const getDashboardRecentJobs = (limit = 20) => apiGet(`/dashboard/recent-jobs?limit=${limit}`);
export const getDashboardByModule  = ()            => apiGet("/dashboard/by-module");

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
  const qs = q.toString();
  return apiGet(`/tests${qs ? "?" + qs : ""}`);
}
export const getTest      = (tc_id)            => apiGet(`/tests/${tc_id}`);
export const previewAutoFix = (body)           => apiPost("/tests/auto-fix-preview", body);
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
  const qs = q.toString();
  return apiGet(`/test-runs${qs ? "?" + qs : ""}`);
}
export const getTestRun = (run_id) => apiGet(`/test-runs/${run_id}`);

// ========= Evidence Library =========
export function listEvidences(params = {}) {
  const q = new URLSearchParams();
  if (params.test_case_id) q.set("test_case_id", params.test_case_id);
  if (params.limit)        q.set("limit",        params.limit);
  const qs = q.toString();
  return apiGet(`/evidences${qs ? "?" + qs : ""}`);
}

// ========= Orchestrator / Execution =========
export const listJobs        = (limit = 100) => apiGet(`/orchestrator/jobs?limit=${limit}`);
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
export const approveDrafts           = (drafts) => apiPost("/drafts/approve",             { drafts });

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
export const exploreApp = (url, maxPages = 5) =>
  apiPost("/app-explorer/explore-app", { url, max_pages: maxPages });

// ========= PR Analysis =========
export const analyzePR           = (body) => apiPost("/pr-analysis/analyze",             body);
export const analyzePRAndEnqueue = (body) => apiPost("/pr-analysis/analyze-and-enqueue", body);

// ========= GitHub =========
export const fetchGithubPR = (url) => apiPost("/github/pr/fetch", { url });

// ========= API Testing =========
export const parseSpec        = (body) => apiPost("/api-testing/parse-spec",     body);
export const generateApiTests = (body) => apiPost("/api-testing/generate-tests", body);
export const approveApiTests  = (body) => apiPost("/api-testing/approve",        body);
export const runApiTests      = (body) => apiPost("/api-testing/run",            body);

// ========= Run Analytics =========
export const getRunsAnalytics = () => apiGet("/analytics/runs/dashboard");

// ========= Coverage =========
export const getCoverageSummary    = () => apiGet("/coverage/summary");
export const generateCoverageTests = (module) => apiPost("/coverage/generate-tests", { module });
export const saveCoverageDrafts    = (module, suggestions) => apiPost("/coverage/save-drafts", { module, suggestions });

// ========= Failure Intelligence =========
export const getFailureIntel = () => apiGet("/failure-intelligence/summary");
export const getFlakyTests   = () => apiGet("/failure-intelligence/flaky-tests");
export const getRegressions  = () => apiGet("/failure-intelligence/regressions");
export const getClusters     = () => apiGet("/failure-intelligence/clusters");
export const getRunClusters  = (limit = 50) => apiGet(`/failure-intelligence/run-clusters?limit=${limit}`);

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
export const sendAlert                = (body)      => apiPost(`/integrations/send-alert`, body);
export const integrationsReadiness    = ()          => apiGet(`/integrations/readiness`);
export const createItsmTicket         = (body)      => apiPost(`/integrations/create-ticket`, body);