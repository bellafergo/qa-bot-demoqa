/** Read-only drilldown helpers for Evidence Correlation (II-02D). */

export const DRILLDOWN_NAVIGABLE_TYPES = new Set([
  "run",
  "pr_analysis",
  "browser_watch",
  "failure_cluster",
]);

export const DRILLDOWN_NON_NAVIGABLE_TYPES = new Set([
  "memory_hint",
  "browser_probe",
]);

export function parsePrAnalysisEntityId(entityId) {
  const raw = String(entityId || "").trim();
  if (!raw) return null;
  const idx = raw.indexOf(":");
  if (idx <= 0) return null;
  return {
    provider: raw.slice(0, idx),
    prNumber: raw.slice(idx + 1),
  };
}

export function hasDrilldownMetadata(item) {
  return Boolean(String(item?.related_entity_type || "").trim() && String(item?.related_entity_id || "").trim());
}

export function isNonNavigableDrilldownType(entityType) {
  return DRILLDOWN_NON_NAVIGABLE_TYPES.has(String(entityType || ""));
}

export function isNavigableDrilldownType(entityType) {
  return DRILLDOWN_NAVIGABLE_TYPES.has(String(entityType || ""));
}

export function shouldShowDrilldownUnavailable(item) {
  const entityType = String(item?.related_entity_type || "").trim();
  if (!entityType) return true;
  if (isNonNavigableDrilldownType(entityType)) return false;
  return !hasDrilldownMetadata(item);
}

export function getDrilldownActionLabelKey(entityType) {
  const map = {
    run: "incident.qa.drilldown.view_run",
    pr_analysis: "incident.qa.drilldown.view_pr_analysis",
    browser_watch: "incident.qa.drilldown.view_browser_alert",
    failure_cluster: "incident.qa.drilldown.view_cluster",
  };
  return map[String(entityType || "")] || null;
}

export function getNonNavigableDrilldownLabelKey(entityType) {
  if (entityType === "memory_hint") return "incident.qa.drilldown.memory_hint";
  if (entityType === "browser_probe") return "incident.qa.drilldown.browser_probe";
  return "incident.qa.drilldown.not_navigable";
}

export function buildDrilldownNavigation(item) {
  const entityType = String(item?.related_entity_type || "").trim();
  const entityId = String(item?.related_entity_id || "").trim();
  if (!entityType || !entityId || !isNavigableDrilldownType(entityType)) return null;

  switch (entityType) {
    case "run":
      return {
        kind: "navigate",
        path: "/runs",
        state: { tab: 0, run_id: entityId },
      };
    case "pr_analysis": {
      const parsed = parsePrAnalysisEntityId(entityId);
      if (!parsed) return null;
      const q = new URLSearchParams({
        provider: parsed.provider,
        pr: parsed.prNumber,
      });
      return {
        kind: "navigate",
        path: `/pr-analysis?${q.toString()}`,
      };
    }
    case "browser_watch":
      return {
        kind: "navigate",
        path: `/browser-watch?watch=${encodeURIComponent(entityId)}`,
      };
    case "failure_cluster": {
      const q = new URLSearchParams({
        tab: "clusters",
        cluster: entityId,
      });
      return {
        kind: "navigate",
        path: `/failure-intel?${q.toString()}`,
      };
    }
    default:
      return null;
  }
}

export function buildDrilldownModalPayload(item) {
  const entityType = String(item?.related_entity_type || "").trim();
  const entityId = String(item?.related_entity_id || "").trim();
  const reason = String(item?.reason || "").trim();
  const source = String(item?.source || "").trim();

  if (entityType === "browser_watch") {
    return {
      titleKey: "incident.qa.drilldown.modal.browser_watch_title",
      fields: [
        { labelKey: "incident.qa.drilldown.modal.watch_id", value: entityId || "—" },
        { labelKey: "incident.qa.drilldown.modal.source", value: source || "—" },
        { labelKey: "incident.qa.drilldown.modal.reason", value: reason || "—" },
      ],
    };
  }

  if (entityType === "failure_cluster") {
    const moduleMatch = String(item?.detail || "").match(/\(([^,)]+),/);
    const moduleName = moduleMatch ? moduleMatch[1].trim() : "—";
    return {
      titleKey: "incident.qa.drilldown.modal.cluster_title",
      fields: [
        { labelKey: "incident.qa.drilldown.modal.cluster_id", value: entityId || "—" },
        { labelKey: "incident.qa.drilldown.modal.module", value: moduleName },
        { labelKey: "incident.qa.drilldown.modal.reason", value: reason || "—" },
      ],
    };
  }

  return {
    titleKey: "incident.qa.drilldown.modal.generic_title",
    fields: [
      { labelKey: "incident.qa.drilldown.modal.source", value: source || "—" },
      { labelKey: "incident.qa.drilldown.modal.reason", value: reason || "—" },
    ],
  };
}

export function shouldUseDrilldownModalFallback(item) {
  const entityType = String(item?.related_entity_type || "").trim();
  return entityType === "browser_watch" || entityType === "failure_cluster";
}
