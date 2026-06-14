/** Parse GitHub App post-install redirect query params. */

export function parseGitHubInstallCallback(search) {
  const raw = typeof search === "string" ? search : "";
  const params = new URLSearchParams(raw.startsWith("?") ? raw.slice(1) : raw);
  const installationId = (
    params.get("installation_id")
    || params.get("github_installation_id")
    || ""
  ).trim();
  if (!installationId) return null;
  const stateProject = (params.get("state") || "").trim().toLowerCase();
  const setupAction = (params.get("setup_action") || "").trim();
  return { installationId, stateProject, setupAction };
}

export function shouldRunGitHubInstallCallback({ installationId, stateProject, projectId }) {
  const pid = (projectId || "").trim().toLowerCase();
  if (!pid || !installationId) {
    return { run: false, reason: "missing_project_or_installation_id" };
  }
  if (stateProject && stateProject !== pid) {
    return { run: false, reason: "state_project_mismatch", expected: stateProject, actual: pid };
  }
  return { run: true, reason: "ok" };
}

export function clearGitHubInstallCallbackParams() {
  if (typeof window === "undefined") return;
  window.history.replaceState({}, "", window.location.pathname);
}
