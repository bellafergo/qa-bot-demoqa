/** Pure helpers for API step evidence visibility in Runs UI. */

export function runSteps(run) {
  if (!run) return [];
  return run.steps || run.steps_result || [];
}

export function hasApiStepEvidence(run) {
  return runSteps(run).some(
    (s) => s?.evidence && typeof s.evidence === "object" && Object.keys(s.evidence).length > 0
  );
}

/** True when run is API-typed and at least one step carries structured HTTP evidence. */
export function shouldShowApiHttpEvidence(run, runType) {
  if (!hasApiStepEvidence(run)) return false;
  return runType === "api";
}
