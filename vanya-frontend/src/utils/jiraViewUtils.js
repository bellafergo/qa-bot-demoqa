/** View helpers for read-only Jira integration (JIRA-01A). */

export const JIRA_I18N_KEYS = {
  title: "integrations.jira.panel_title",
  connectionStatus: "integrations.jira.connection_status",
  connected: "integrations.jira.connected",
  disconnected: "integrations.jira.disconnected",
  serverUrl: "integrations.jira.server_url",
  lastSync: "integrations.jira.last_sync",
  projects: "integrations.jira.projects",
  issues: "integrations.jira.issues",
  epics: "integrations.jira.epics",
  releases: "integrations.jira.releases",
  fixVersions: "integrations.jira.fix_versions",
  readOnlyNote: "integrations.jira.read_only_note",
  emptyConnection: "integrations.jira.empty_connection",
  emptyProjects: "integrations.jira.empty_projects",
  emptyIssues: "integrations.jira.empty_issues",
  refresh: "integrations.jira.refresh",
  loading: "integrations.jira.loading",
  projectKey: "integrations.jira.project_key",
  issueType: "integrations.jira.issue_type",
  status: "integrations.jira.status",
  assignee: "integrations.jira.assignee",
  priority: "integrations.jira.priority",
  summary: "integrations.jira.summary",
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
    title: t(JIRA_I18N_KEYS.connectionStatus),
    connectedLabel: connected ? t(JIRA_I18N_KEYS.connected) : t(JIRA_I18N_KEYS.disconnected),
    connectedBadgeClass: connectionBadgeClass(connected),
    serverUrlLabel: t(JIRA_I18N_KEYS.serverUrl),
    serverUrl: status?.server_url || "—",
    lastSyncLabel: t(JIRA_I18N_KEYS.lastSync),
    lastSync: status?.last_sync || null,
    counts: [
      { label: t(JIRA_I18N_KEYS.projects), value: formatCount(status?.project_count) },
      { label: t(JIRA_I18N_KEYS.issues), value: formatCount(status?.issue_count) },
      { label: t(JIRA_I18N_KEYS.epics), value: formatCount(status?.epic_count) },
      { label: t(JIRA_I18N_KEYS.releases), value: formatCount(status?.release_count) },
      { label: t(JIRA_I18N_KEYS.fixVersions), value: formatCount(status?.fix_version_count) },
    ],
    readOnlyNote: t(JIRA_I18N_KEYS.readOnlyNote),
    showEmptyConnection: !connected,
    emptyConnectionText: t(JIRA_I18N_KEYS.emptyConnection),
  };
}

export function buildProjectCardViewModel(project) {
  if (!project?.project_key) return null;
  return {
    projectId: project.project_id,
    projectKey: project.project_key,
    projectName: project.project_name || project.project_key,
    subtitle: project.project_key,
  };
}

export function buildIssueCardViewModel(issue) {
  if (!issue?.issue_key) return null;
  return {
    issueId: issue.issue_id,
    issueKey: issue.issue_key,
    summary: issue.summary || "—",
    issueType: issue.issue_type || "—",
    status: issue.status || "—",
    assignee: issue.assignee || "—",
    priority: issue.priority || "—",
  };
}

export function buildJiraIntegrationViewModel({ status, projects, issues, t }) {
  const connection = buildConnectionCardViewModel(status, t);
  const connected = Boolean(status?.connected);
  const projectItems = (projects || [])
    .map(buildProjectCardViewModel)
    .filter(Boolean);
  const issueItems = (issues || [])
    .map(buildIssueCardViewModel)
    .filter(Boolean);

  return {
    connection,
    projects: projectItems,
    issues: issueItems,
    showEmptyProjects: connected && projectItems.length === 0,
    emptyProjectsText: t(JIRA_I18N_KEYS.emptyProjects),
    showEmptyIssues: connected && issueItems.length === 0,
    emptyIssuesText: t(JIRA_I18N_KEYS.emptyIssues),
    loadingLabel: t(JIRA_I18N_KEYS.loading),
    refreshLabel: t(JIRA_I18N_KEYS.refresh),
    projectsLabel: t(JIRA_I18N_KEYS.projects),
    issuesLabel: t(JIRA_I18N_KEYS.issues),
  };
}

export function deriveJiraHeaderState(status) {
  if (!status?.connected) {
    return { enabled: false, health: "unconfigured", labelKey: "integrations.health.unconfigured" };
  }
  return { enabled: true, health: "ok", labelKey: "integrations.jira.header_connected" };
}
