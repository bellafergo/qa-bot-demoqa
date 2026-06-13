/** View helpers for Local Agents Foundation (INT-03A). */

/** Default Foundation Agent registration payload for self-service onboarding. */
export const FOUNDATION_AGENT_DEFAULTS = {
  name: "Foundation Agent",
  environment: "production",
  version: "1.0.0",
  capabilities: ["database_validation", "contract_validation"],
};

export const LOCAL_AGENTS_I18N_KEYS = {
  createFoundationAgent: "localAgents.create_foundation_agent",
  toastFoundationCreated: "localAgents.toast.foundation_created",
  toastFoundationExists: "localAgents.toast.foundation_exists",
  toastNoProject: "localAgents.toast.no_project",
  title: "localAgents.title",
  capabilities: "localAgents.foundation.capabilities",
  inventory: "localAgents.foundation.inventory",
  lastHeartbeat: "localAgents.foundation.last_heartbeat",
  online: "localAgents.foundation.online",
  offline: "localAgents.foundation.offline",
  unknown: "localAgents.foundation.unknown",
  environment: "localAgents.foundation.environment",
  version: "localAgents.col.version",
  empty: "localAgents.empty_title",
  readOnlyNote: "localAgents.foundation.read_only_note",
  databases: "localAgents.foundation.databases",
  repositories: "localAgents.foundation.repositories",
  services: "localAgents.foundation.services",
  summary: "localAgents.foundation.summary",
};

const STATUS_BADGE = {
  ONLINE: "badge badge-green",
  OFFLINE: "badge badge-gray",
  UNKNOWN: "badge badge-orange",
};

const CAPABILITY_LABELS = {
  database_validation: "Database Validation",
  contract_validation: "Contract Validation",
  browser_probe: "Browser Probe",
  filesystem_inventory: "Filesystem Inventory",
  repo_inventory: "Repository Inventory",
  network_inventory: "Network Inventory",
  browser_inspection: "Browser Inspection",
  playwright: "Playwright",
  localhost_access: "Localhost Access",
  intranet_access: "Intranet Access",
};

export function foundationStatusBadgeClass(status) {
  const key = String(status || "UNKNOWN").toUpperCase();
  return STATUS_BADGE[key] || "badge badge-gray";
}

export function formatCapabilityLabel(capabilityId, t) {
  const key = String(capabilityId || "").trim();
  if (!key) return "—";
  const i18nKey = `localAgents.foundation.capability.${key}`;
  const translated = typeof t === "function" ? t(i18nKey) : i18nKey;
  if (translated !== i18nKey) return translated;
  return CAPABILITY_LABELS[key] || key;
}

export function formatCapabilityList(capabilities, t) {
  return (capabilities || []).map((cap) => formatCapabilityLabel(cap, t));
}

export function inventoryLines(inventory) {
  if (!inventory) {
    return { databases: [], repositories: [], services: [] };
  }
  return {
    databases: inventory.databases_detected || [],
    repositories: inventory.repositories_detected || [],
    services: inventory.services_detected || [],
  };
}

export function hasInventory(inventory) {
  const lines = inventoryLines(inventory);
  return Boolean(lines.databases.length || lines.repositories.length || lines.services.length);
}

export function buildLocalAgentRow(agent, inventory, t, formatTimestamp) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  return {
    ...agent,
    statusLabel:
      agent.status === "ONLINE"
        ? t(LOCAL_AGENTS_I18N_KEYS.online)
        : agent.status === "OFFLINE"
          ? t(LOCAL_AGENTS_I18N_KEYS.offline)
          : t(LOCAL_AGENTS_I18N_KEYS.unknown),
    statusBadgeClass: foundationStatusBadgeClass(agent.status),
    capabilityLabels: formatCapabilityList(agent.capabilities, t),
    inventory,
    inventoryLines: inventoryLines(inventory),
    hasInventory: hasInventory(inventory),
    lastHeartbeatText: fmt(agent.last_heartbeat_at),
    registeredAtText: fmt(agent.registered_at),
    environment: agent.environment || "—",
    version: agent.version || "—",
  };
}

export function buildLocalAgentsViewModel(report, t, formatTimestamp) {
  const agents = report?.agents || [];
  const inventoryByAgent = new Map((report?.inventory || []).map((inv) => [inv.agent_id, inv]));

  return {
    title: t(LOCAL_AGENTS_I18N_KEYS.title),
    empty: agents.length === 0,
    emptyMessage: t(LOCAL_AGENTS_I18N_KEYS.empty),
    summary: report?.summary || "",
    summaryLabel: t(LOCAL_AGENTS_I18N_KEYS.summary),
    capabilitiesLabel: t(LOCAL_AGENTS_I18N_KEYS.capabilities),
    inventoryLabel: t(LOCAL_AGENTS_I18N_KEYS.inventory),
    lastHeartbeatLabel: t(LOCAL_AGENTS_I18N_KEYS.lastHeartbeat),
    environmentLabel: t(LOCAL_AGENTS_I18N_KEYS.environment),
    versionLabel: t(LOCAL_AGENTS_I18N_KEYS.version),
    readOnlyNote: t(LOCAL_AGENTS_I18N_KEYS.readOnlyNote),
    databasesLabel: t(LOCAL_AGENTS_I18N_KEYS.databases),
    repositoriesLabel: t(LOCAL_AGENTS_I18N_KEYS.repositories),
    servicesLabel: t(LOCAL_AGENTS_I18N_KEYS.services),
    agents: agents.map((agent) =>
      buildLocalAgentRow(agent, inventoryByAgent.get(agent.agent_id) || null, t, formatTimestamp),
    ),
  };
}
