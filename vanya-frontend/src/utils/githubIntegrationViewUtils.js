/** GitHub integration panel — presentation-only state machine. */

export const GITHUB_PANEL_PHASE = {
  NOT_CONNECTED: "NOT_CONNECTED",
  CONNECTED_SELECT_REPO: "CONNECTED_SELECT_REPO",
  CONNECTED_NO_REPOS: "CONNECTED_NO_REPOS",
  REPO_SELECTED: "REPO_SELECTED",
};

export function deriveGitHubPanelPhase({
  status,
  changingRepo = false,
  reposCount = 0,
  reposLoading = false,
}) {
  if (!status?.installation_id) return GITHUB_PANEL_PHASE.NOT_CONNECTED;
  if (status.full_name && !changingRepo) return GITHUB_PANEL_PHASE.REPO_SELECTED;
  if (!reposLoading && reposCount === 0) return GITHUB_PANEL_PHASE.CONNECTED_NO_REPOS;
  return GITHUB_PANEL_PHASE.CONNECTED_SELECT_REPO;
}
