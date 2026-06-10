import { describe, it, expect } from "vitest";
import {
  JIRA_I18N_KEYS,
  buildConnectionCardViewModel,
  buildIssueCardViewModel,
  buildJiraIntegrationViewModel,
  buildProjectCardViewModel,
  connectionBadgeClass,
  deriveJiraHeaderState,
  formatCount,
} from "./jiraViewUtils.js";

const t = (key) => key;

const sampleStatus = {
  connected: true,
  server_url: "https://acme.atlassian.net",
  project_count: 12,
  issue_count: 1248,
  epic_count: 67,
  release_count: 14,
  fix_version_count: 31,
  last_sync: "2026-06-10T12:00:00Z",
};

describe("jiraViewUtils", () => {
  it("formats counts for display", () => {
    expect(formatCount(1248)).toBe("1,248");
    expect(formatCount(undefined)).toBe("0");
  });

  it("builds connection card view model", () => {
    const vm = buildConnectionCardViewModel(sampleStatus, t);
    expect(vm.connectedBadgeClass).toBe(connectionBadgeClass(true));
    expect(vm.counts).toHaveLength(5);
    expect(vm.counts[1].value).toBe("1,248");
    expect(vm.showEmptyConnection).toBe(false);
  });

  it("shows empty connection state", () => {
    const vm = buildConnectionCardViewModel({ connected: false }, t);
    expect(vm.showEmptyConnection).toBe(true);
    expect(vm.emptyConnectionText).toBe(JIRA_I18N_KEYS.emptyConnection);
  });

  it("renders project cards", () => {
    const vm = buildProjectCardViewModel({
      project_id: "1",
      project_key: "QA",
      project_name: "QA Platform",
    });
    expect(vm.projectKey).toBe("QA");
    expect(vm.projectName).toBe("QA Platform");
  });

  it("renders issue cards", () => {
    const vm = buildIssueCardViewModel({
      issue_id: "1",
      issue_key: "QA-42",
      summary: "Login fails",
      issue_type: "Bug",
      status: "Open",
      assignee: "Alex",
      priority: "High",
    });
    expect(vm.issueKey).toBe("QA-42");
    expect(vm.summary).toBe("Login fails");
  });

  it("builds integration view model with empty project and issue states", () => {
    const vm = buildJiraIntegrationViewModel({
      status: sampleStatus,
      projects: [],
      issues: [],
      epics: [],
      releases: [],
      fixVersions: [],
      t,
    });
    expect(vm.showEmptyProjects).toBe(true);
    expect(vm.showEmptyIssues).toBe(true);
    expect(vm.epics.showEmpty).toBe(true);
    expect(vm.releases.showEmpty).toBe(true);
    expect(vm.fixVersions.showEmpty).toBe(true);
  });

  it("renders epic and release list items", () => {
    const vm = buildJiraIntegrationViewModel({
      status: sampleStatus,
      projects: [],
      issues: [],
      epics: [{ epic_key: "QA-10", epic_name: "Checkout Epic" }],
      releases: [{ release_id: "1", release_name: "2026.1", release_date: "2026-03-01" }],
      fixVersions: [{ version_id: "2", version_name: "2026.2" }],
      t,
    });
    expect(vm.epics.items[0].label).toBe("Checkout Epic");
    expect(vm.releases.items[0].label).toBe("2026.1");
    expect(vm.fixVersions.items[0].label).toBe("2026.2");
  });

  it("derives header state from connection status", () => {
    expect(deriveJiraHeaderState({ connected: true }).health).toBe("ok");
    expect(deriveJiraHeaderState({ connected: false }).health).toBe("unconfigured");
  });
});
