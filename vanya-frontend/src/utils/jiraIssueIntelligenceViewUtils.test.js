import { describe, it, expect } from "vitest";
import {
  JIRA_ISSUE_INTELLIGENCE_I18N_KEYS,
  buildCorrelationCardViewModel,
  buildJiraIssueIntelligenceViewModel,
  hasJiraIssueIntelligenceSection,
  isJiraIssueIntelligenceEmpty,
} from "./jiraIssueIntelligenceViewUtils.js";

const t = (key) => key;

const sampleIntel = {
  connected: true,
  total_issues: 4,
  correlated_issues: 2,
  blocker_count: 1,
  high_priority_count: 2,
  summary: "2 of 4 issues correlate",
  top_blockers: [
    {
      issue_key: "QA-9",
      issue_type: "Bug",
      status: "Open",
      priority: "Blocker",
      summary: "Payments outage",
      correlation_score: 70,
      correlation_reason: "Priority weight (+30); Impacted module match: Payments (+25)",
      related_module: "Payments",
      is_blocker: true,
    },
  ],
  issue_correlations: [
    {
      issue_key: "QA-9",
      issue_type: "Bug",
      status: "Open",
      priority: "Blocker",
      summary: "Payments outage",
      correlation_score: 70,
      correlation_reason: "Priority weight (+30); Impacted module match: Payments (+25)",
      related_module: "Payments",
      is_blocker: true,
    },
    {
      issue_key: "QA-3",
      issue_type: "Task",
      status: "In Progress",
      priority: "High",
      summary: "Staging auth fix",
      correlation_score: 30,
      correlation_reason: "Priority weight (+10); Degraded environment match: Staging (+20)",
      related_environment: "Staging",
      is_blocker: false,
    },
  ],
};

describe("jiraIssueIntelligenceViewUtils", () => {
  it("detects section presence", () => {
    expect(hasJiraIssueIntelligenceSection({ jira_issue_intelligence: sampleIntel })).toBe(true);
    expect(hasJiraIssueIntelligenceSection({})).toBe(false);
  });

  it("renders blocker and correlation cards", () => {
    const card = buildCorrelationCardViewModel(sampleIntel.top_blockers[0], t);
    expect(card.isBlocker).toBe(true);
    expect(card.relatedModule).toBe("Payments");
  });

  it("builds intelligence view model", () => {
    const vm = buildJiraIssueIntelligenceViewModel({ jira_issue_intelligence: sampleIntel }, t);
    expect(vm.show).toBe(true);
    expect(vm.blockerCount).toBe(1);
    expect(vm.correlations).toHaveLength(2);
    expect(vm.showTopBlockers).toBe(true);
  });

  it("shows empty connection state", () => {
    expect(isJiraIssueIntelligenceEmpty({ jira_issue_intelligence: { connected: false } })).toBe(true);
    const vm = buildJiraIssueIntelligenceViewModel(
      { jira_issue_intelligence: { connected: false, total_issues: 0, correlated_issues: 0 } },
      t,
    );
    expect(vm.capabilityState.state).toBe("INTEGRATION_REQUIRED");
    expect(vm.showContent).toBe(false);
  });

  it("shows insufficient history when connected without correlations", () => {
    const vm = buildJiraIssueIntelligenceViewModel(
      {
        jira_issue_intelligence: {
          connected: true,
          total_issues: 2,
          correlated_issues: 0,
          blocker_count: 0,
          high_priority_count: 0,
          issue_correlations: [],
          top_blockers: [],
        },
      },
      t,
    );
    expect(vm.capabilityState.state).toBe("INSUFFICIENT_HISTORY");
    expect(vm.capabilityState.cta.path).toBe("/runs");
  });
});
