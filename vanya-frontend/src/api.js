// src/api.js
const API_BASE =
  (import.meta.env.VITE_API_BASE_URL || "").trim() ||
  (import.meta.env.VITE_API_BASE || "").trim() ||
  "https://qa-bot-demoqa.onrender.com"; // fallback seguro

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
  const txt = await res.text();
  if (!txt) return null;
  try {
    return JSON.parse(txt);
  } catch {
    return null;
  }
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

export async function apiPatch(path, body) {
  const res = await fetch(`${API_BASE}${path}`, {
    method: "PATCH",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body ?? {}),
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  return res.json();
}

/** Human-readable message from fetch errors (FastAPI JSON detail or plain text). */
export function apiErrorMessage(err) {
  const raw = err?.message ?? String(err);
  if (!raw) return "Request failed";
  try {
    const j = JSON.parse(raw);
    if (typeof j.detail === "string") return j.detail;
    if (Array.isArray(j.detail)) {
      return j.detail
        .map((d) => (typeof d === "string" ? d : d.msg || JSON.stringify(d)))
        .join(" ");
    }
    if (j.message) return String(j.message);
  } catch {
    /* plain text */
  }
  return raw;
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
export function getDashboardSummary(params = {}) {
  const q = new URLSearchParams();
  if (params.project_id != null && String(params.project_id).trim()) {
    q.set("project_id", String(params.project_id).trim());
  }
  const qs = q.toString();
  return apiGet(`/dashboard/summary${qs ? `?${qs}` : ""}`);
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
  const qs = q.toString();
  return apiGet(`/evidences${qs ? "?" + qs : ""}`);
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
export const approveDrafts = (drafts, { activate = true } = {}) =>
  apiPost("/drafts/approve", { drafts, activate });

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

// ========= Projects (multi-project catalog scope) =========
/** GET /projects — 200 + [] or empty body must be success (never throw from JSON parse). */
export async function listProjects() {
  const res = await fetch(`${API_BASE}/projects`, {
    method: "GET",
    headers: { "Content-Type": "application/json" },
  });
  if (!res.ok) {
    const txt = await res.text();
    throw new Error(txt || res.statusText);
  }
  const text = await res.text();
  if (text == null || text.trim() === "") return [];
  try {
    const data = JSON.parse(text);
    return Array.isArray(data) ? data : [];
  } catch {
    throw new Error(text.slice(0, 200) || "Invalid JSON response");
  }
}
export const getProject     = (projectId)           => apiGet(`/projects/${encodeURIComponent(projectId)}`);
export const createProject  = (payload)           => apiPost("/projects", payload);
export const updateProject  = (projectId, payload) => apiPatch(`/projects/${encodeURIComponent(projectId)}`, payload);
export const deleteProject  = (projectId)           => apiDelete(`/projects/${encodeURIComponent(projectId)}`);