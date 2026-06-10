/** Read-only PR Analysis drilldown helpers (II-02D mini-fix). */

export function parsePrAnalysisDrilldown(searchParams) {
  const provider = String(searchParams?.get?.("provider") || "").trim().toLowerCase();
  const prNumber = String(searchParams?.get?.("pr") || "").trim();
  if (!provider || !prNumber) return null;
  return { provider, prNumber };
}

export function isGithubDrilldownProvider(provider) {
  const p = String(provider || "").trim().toLowerCase();
  return p === "github" || p === "github_app";
}

export function isAzureDrilldownProvider(provider) {
  const p = String(provider || "").trim().toLowerCase();
  return p === "azure_devops" || p === "azure";
}

export function findGithubDrilldownPr(ghPRs, prNumber) {
  return (ghPRs || []).find((pr) => String(pr.number) === String(prNumber)) || null;
}

export function findAzureDrilldownPr(azPRs, prNumber) {
  return (azPRs || []).find((pr) => String(pr.pull_request_id) === String(prNumber)) || null;
}

export function resolvePrAnalysisDrilldown({ provider, prNumber, ghPRs, azPRs }) {
  if (isGithubDrilldownProvider(provider)) {
    const match = findGithubDrilldownPr(ghPRs, prNumber);
    return match
      ? { found: true, provider: "github", match, formPatch: {
          pr_id: String(prNumber),
          title: match.title || "",
          branch: match.branch || "",
        } }
      : { found: false, provider: "github", match: null, formPatch: null };
  }
  if (isAzureDrilldownProvider(provider)) {
    const match = findAzureDrilldownPr(azPRs, prNumber);
    return match
      ? { found: true, provider: "azure_devops", match, formPatch: {
          pr_id: String(prNumber),
          title: match.title || "",
          branch: match.branch || "",
        } }
      : { found: false, provider: "azure_devops", match: null, formPatch: null };
  }
  return { found: false, provider, match: null, formPatch: null };
}
