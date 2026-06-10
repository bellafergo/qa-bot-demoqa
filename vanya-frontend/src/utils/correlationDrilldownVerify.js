/** Read-only entity existence checks for correlation drilldown (GET only). */

export async function verifyDrilldownEntity(entityType, entityId, projectId, api) {
  const { getTestRun, getBrowserInspectionWatch, getClusters } = api;

  switch (entityType) {
    case "run":
      try {
        await getTestRun(entityId);
        return true;
      } catch {
        return false;
      }
    case "browser_watch":
      try {
        await getBrowserInspectionWatch(entityId);
        return true;
      } catch {
        return false;
      }
    case "failure_cluster": {
      try {
        const data = await getClusters(projectId ? { project_id: projectId } : {});
        const rows = Array.isArray(data) ? data : [];
        return rows.some((row) => String(row.cluster_id || "") === String(entityId));
      } catch {
        return false;
      }
    }
    case "pr_analysis":
      return true;
    default:
      return false;
  }
}
