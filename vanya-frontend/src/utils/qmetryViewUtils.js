/** View helpers for read-only QMetry integration (QMETRY-01A). */

export const QMETRY_I18N_KEYS = {
  title: "integrations.qmetry.panel_title",
  connectionStatus: "integrations.qmetry.connection_status",
  connected: "integrations.qmetry.connected",
  disconnected: "integrations.qmetry.disconnected",
  serverUrl: "integrations.qmetry.server_url",
  lastSync: "integrations.qmetry.last_sync",
  projects: "integrations.qmetry.projects",
  testCases: "integrations.qmetry.test_cases",
  testCycles: "integrations.qmetry.test_cycles",
  testSuites: "integrations.qmetry.test_suites",
  testRuns: "integrations.qmetry.test_runs",
  readOnlyNote: "integrations.qmetry.read_only_note",
  emptyConnection: "integrations.qmetry.empty_connection",
  emptyProjects: "integrations.qmetry.empty_projects",
  emptyTestCases: "integrations.qmetry.empty_test_cases",
  emptyTestCycles: "integrations.qmetry.empty_test_cycles",
  emptyTestSuites: "integrations.qmetry.empty_test_suites",
  emptyTestRuns: "integrations.qmetry.empty_test_runs",
  refresh: "integrations.qmetry.refresh",
  loading: "integrations.qmetry.loading",
  priority: "integrations.qmetry.priority",
  status: "integrations.qmetry.status",
  executionDate: "integrations.qmetry.execution_date",
};

export function formatCount(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "0";
  return n.toLocaleString();
}

export function connectionBadgeClass(connected) {
  return connected ? "badge badge-green" : "badge badge-gray";
}

export function buildConnectionCardViewModel(status, t) {
  const connected = Boolean(status?.connected);
  return {
    title: t(QMETRY_I18N_KEYS.connectionStatus),
    connectedLabel: connected ? t(QMETRY_I18N_KEYS.connected) : t(QMETRY_I18N_KEYS.disconnected),
    connectedBadgeClass: connectionBadgeClass(connected),
    serverUrlLabel: t(QMETRY_I18N_KEYS.serverUrl),
    serverUrl: status?.base_url || "—",
    lastSyncLabel: t(QMETRY_I18N_KEYS.lastSync),
    lastSync: status?.last_sync || null,
    counts: [
      { label: t(QMETRY_I18N_KEYS.projects), value: formatCount(status?.project_count) },
      { label: t(QMETRY_I18N_KEYS.testCases), value: formatCount(status?.test_case_count) },
      { label: t(QMETRY_I18N_KEYS.testRuns), value: formatCount(status?.run_count) },
    ],
    readOnlyNote: t(QMETRY_I18N_KEYS.readOnlyNote),
    showEmptyConnection: !connected,
    emptyConnectionText: t(QMETRY_I18N_KEYS.emptyConnection),
  };
}

export function buildProjectCardViewModel(project) {
  if (!project?.project_id) return null;
  return {
    projectId: project.project_id,
    projectName: project.project_name || project.project_id,
  };
}

export function buildTestCaseCardViewModel(testCase) {
  if (!testCase?.test_case_id) return null;
  return {
    testCaseId: testCase.test_case_id,
    name: testCase.name || "—",
    priority: testCase.priority || "—",
    status: testCase.status || "—",
  };
}

function buildDiscoverySection({ label, items, emptyText, connected }) {
  return {
    label,
    items,
    showEmpty: connected && items.length === 0,
    emptyText,
  };
}

function buildListItem(id, name, meta) {
  if (!id) return null;
  return { key: id, label: name || id, meta: meta || null };
}

export function buildQMetryIntegrationViewModel({
  status,
  projects,
  testCases,
  testCycles,
  testSuites,
  testRuns,
  t,
}) {
  const connection = buildConnectionCardViewModel(status, t);
  const connected = Boolean(status?.connected);
  const projectItems = (projects || []).map(buildProjectCardViewModel).filter(Boolean);
  const testCaseItems = (testCases || []).map(buildTestCaseCardViewModel).filter(Boolean);
  const cycleItems = (testCycles || [])
    .map((c) => buildListItem(c.cycle_id, c.cycle_name, c.status))
    .filter(Boolean);
  const suiteItems = (testSuites || [])
    .map((s) => buildListItem(s.suite_id, s.suite_name))
    .filter(Boolean);
  const runItems = (testRuns || [])
    .map((r) => buildListItem(r.run_id, r.run_name, r.execution_date || r.status))
    .filter(Boolean);

  return {
    connection,
    projects: projectItems,
    testCases: testCaseItems,
    testCycles: buildDiscoverySection({
      label: t(QMETRY_I18N_KEYS.testCycles),
      items: cycleItems,
      emptyText: t(QMETRY_I18N_KEYS.emptyTestCycles),
      connected,
    }),
    testSuites: buildDiscoverySection({
      label: t(QMETRY_I18N_KEYS.testSuites),
      items: suiteItems,
      emptyText: t(QMETRY_I18N_KEYS.emptyTestSuites),
      connected,
    }),
    testRuns: buildDiscoverySection({
      label: t(QMETRY_I18N_KEYS.testRuns),
      items: runItems,
      emptyText: t(QMETRY_I18N_KEYS.emptyTestRuns),
      connected,
    }),
    showEmptyProjects: connected && projectItems.length === 0,
    emptyProjectsText: t(QMETRY_I18N_KEYS.emptyProjects),
    showEmptyTestCases: connected && testCaseItems.length === 0,
    emptyTestCasesText: t(QMETRY_I18N_KEYS.emptyTestCases),
    loadingLabel: t(QMETRY_I18N_KEYS.loading),
    refreshLabel: t(QMETRY_I18N_KEYS.refresh),
    projectsLabel: t(QMETRY_I18N_KEYS.projects),
    testCasesLabel: t(QMETRY_I18N_KEYS.testCases),
  };
}

export function deriveQMetryHeaderState(status) {
  if (!status?.connected) {
    return { enabled: false, health: "unconfigured", labelKey: "integrations.health.unconfigured" };
  }
  return { enabled: true, health: "ok", labelKey: "integrations.qmetry.header_connected" };
}
