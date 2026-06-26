import { describe, expect, it } from "vitest";
import { deriveGitHubPanelPhase, GITHUB_PANEL_PHASE } from "./githubIntegrationViewUtils.js";

describe("githubIntegrationViewUtils", () => {
  it("returns NOT_CONNECTED without installation_id", () => {
    expect(deriveGitHubPanelPhase({ status: { installation_id: "" } }))
      .toBe(GITHUB_PANEL_PHASE.NOT_CONNECTED);
  });

  it("returns REPO_SELECTED when full_name is set", () => {
    expect(deriveGitHubPanelPhase({
      status: { installation_id: "1", full_name: "acme/app" },
      reposCount: 3,
    })).toBe(GITHUB_PANEL_PHASE.REPO_SELECTED);
  });

  it("returns CONNECTED_NO_REPOS when repo list is empty", () => {
    expect(deriveGitHubPanelPhase({
      status: { installation_id: "1", full_name: "" },
      reposCount: 0,
      reposLoading: false,
    })).toBe(GITHUB_PANEL_PHASE.CONNECTED_NO_REPOS);
  });

  it("returns CONNECTED_SELECT_REPO when repos exist but none selected", () => {
    expect(deriveGitHubPanelPhase({
      status: { installation_id: "1", full_name: "" },
      reposCount: 2,
    })).toBe(GITHUB_PANEL_PHASE.CONNECTED_SELECT_REPO);
  });
});
