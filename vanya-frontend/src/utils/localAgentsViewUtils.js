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

export const LOCAL_AGENTS_ENTERPRISE_I18N_KEYS = {
  commandCenterTitle: "localAgents.enterprise.command_center",
  summaryTotalAgents: "localAgents.enterprise.summary.total_agents",
  summaryOnline: "localAgents.enterprise.summary.online",
  summaryOffline: "localAgents.enterprise.summary.offline",
  summaryDbConnections: "localAgents.enterprise.summary.db_connections",
  summaryCapabilities: "localAgents.enterprise.summary.capabilities",
  agentsListTitle: "localAgents.enterprise.agents_list",
  created: "localAgents.enterprise.created",
  lastSeen: "localAgents.enterprise.last_seen",
  lastSeenNever: "localAgents.enterprise.last_seen_never",
  detailSelect: "localAgents.enterprise.detail.select",
  detailInventorySummary: "localAgents.enterprise.detail.inventory_summary",
  detailDbConnections: "localAgents.enterprise.detail.db_connections",
  detailRecentValidations: "localAgents.enterprise.detail.recent_validations",
  detailSystemsRegistered: "localAgents.enterprise.detail.systems_registered",
  detailMetadata: "localAgents.enterprise.detail.metadata",
  detailFoundationAgent: "localAgents.enterprise.detail.foundation_agent",
  detailInventoryItems: "localAgents.enterprise.detail.inventory_items",
  detailYes: "localAgents.enterprise.detail.yes",
  detailNo: "localAgents.enterprise.detail.no",
  dbSectionTitle: "localAgents.enterprise.db_section_title",
  dbEmptyDesc: "localAgents.enterprise.db_empty_desc",
  dbStatusRegistered: "localAgents.enterprise.db.status_registered",
  dbAgentLabel: "localAgents.enterprise.db.agent_label",
  dbCreated: "localAgents.enterprise.db.created",
  capabilitiesOverview: "localAgents.enterprise.capabilities_overview",
  capabilitiesAgents: "localAgents.enterprise.capabilities_agents",
  statusOnline: "localAgents.enterprise.status.online",
  statusOffline: "localAgents.enterprise.status.offline",
  statusWarning: "localAgents.enterprise.status.warning",
  statusError: "localAgents.enterprise.status.error",
};

const STATUS_BADGE = {
  ONLINE: "badge badge-green",
  OFFLINE: "badge badge-gray",
  UNKNOWN: "badge badge-orange",
  WARNING: "badge badge-orange",
  ERROR: "badge badge-red",
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

/** Heuristic agent runtime status (online <2m, stale <10m). */
export function getAgentStatus(agent, nowMs = Date.now()) {
  if (!agent?.enabled || String(agent.status || "").toLowerCase() === "disabled") {
    return {
      key: "ERROR",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusError,
      badgeClass: STATUS_BADGE.ERROR,
      needsAttention: true,
    };
  }
  const iso = agent?.last_seen_at || agent?.last_heartbeat_at;
  if (!iso) {
    return {
      key: "OFFLINE",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusOffline,
      badgeClass: STATUS_BADGE.OFFLINE,
      needsAttention: true,
    };
  }
  const ms = nowMs - new Date(iso).getTime();
  if (!Number.isFinite(ms) || ms < 0) {
    return {
      key: "OFFLINE",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusOffline,
      badgeClass: STATUS_BADGE.OFFLINE,
      needsAttention: true,
    };
  }
  const two = 2 * 60 * 1000;
  const ten = 10 * 60 * 1000;
  if (ms < two) {
    return {
      key: "ONLINE",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusOnline,
      badgeClass: STATUS_BADGE.ONLINE,
      needsAttention: false,
    };
  }
  if (ms < ten) {
    return {
      key: "WARNING",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusWarning,
      badgeClass: STATUS_BADGE.WARNING,
      needsAttention: true,
    };
  }
  return {
    key: "OFFLINE",
    labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusOffline,
    badgeClass: STATUS_BADGE.OFFLINE,
    needsAttention: true,
  };
}

export function isFoundationAgent(agent) {
  const name = String(agent?.name || "").trim().toLowerCase();
  return name === "foundation agent" || String(agent?.agent_id || "").toLowerCase().includes("foundation");
}

export function getSummaryMetrics(agents, dbConnections) {
  const list = Array.isArray(agents) ? agents : [];
  let online = 0;
  let offline = 0;
  for (const agent of list) {
    const status = getAgentStatus(agent).key;
    if (status === "ONLINE") online += 1;
    else offline += 1;
  }
  const capabilityCount = list.reduce((sum, a) => sum + (a.capabilities?.length || 0), 0);
  return {
    totalAgents: list.length,
    online,
    offline,
    dbConnections: (dbConnections || []).length,
    capabilities: capabilityCount,
  };
}

export function getCapabilitySummary(agents, t) {
  const counts = new Map();
  for (const agent of agents || []) {
    for (const cap of agent.capabilities || []) {
      const key = String(cap);
      counts.set(key, (counts.get(key) || 0) + 1);
    }
  }
  return [...counts.entries()]
    .map(([capabilityId, agentCount]) => ({
      capabilityId,
      label: formatCapabilityLabel(capabilityId, t),
      agentCount,
      agentsLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.capabilitiesAgents, { count: agentCount }),
    }))
    .sort((a, b) => b.agentCount - a.agentCount || a.label.localeCompare(b.label));
}

export function sortAgents(agents, nowMs = Date.now()) {
  const order = { ONLINE: 0, WARNING: 1, OFFLINE: 2, ERROR: 3 };
  return [...(agents || [])].sort((a, b) => {
    const aFoundation = isFoundationAgent(a) ? 0 : 1;
    const bFoundation = isFoundationAgent(b) ? 0 : 1;
    if (aFoundation !== bFoundation) return aFoundation - bFoundation;
    const sa = getAgentStatus(a, nowMs).key;
    const sb = getAgentStatus(b, nowMs).key;
    return (order[sa] ?? 9) - (order[sb] ?? 9);
  });
}

function formatShortDate(iso, t) {
  if (!iso) return t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever);
  try {
    return new Date(iso).toLocaleDateString(undefined, { month: "short", day: "numeric" });
  } catch {
    return "—";
  }
}

export function buildMetadataTags({ detail, foundationRow, t }) {
  const metadata = detail?.metadata && typeof detail.metadata === "object" ? detail.metadata : {};
  const inventoryCount =
    (foundationRow?.inventoryLines?.databases?.length || 0)
    + (foundationRow?.inventoryLines?.repositories?.length || 0)
    + (foundationRow?.inventoryLines?.services?.length || 0);

  return [
    {
      label: t(LOCAL_AGENTS_I18N_KEYS.environment),
      value: foundationRow?.environment || metadata.environment || detail?.metadata?.environment || "—",
    },
    {
      label: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailFoundationAgent),
      value: isFoundationAgent(detail) ? t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailYes) : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailNo),
    },
    {
      label: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailInventoryItems),
      value: String(inventoryCount),
    },
  ];
}

export function buildAgentListItemViewModel(agent, foundationRow, t, nowMs = Date.now()) {
  const status = getAgentStatus(agent, nowMs);
  return {
    agentId: agent.agent_id,
    name: agent.name || agent.agent_id,
    status,
    statusLabel: t(status.labelKey),
    environment: foundationRow?.environment || agent.metadata?.environment || agent.environment || "—",
    capabilityLabels: foundationRow?.capabilityLabels || formatCapabilityList(agent.capabilities, t),
    createdText: formatShortDate(agent.created_at, t),
    lastSeenText: agent.last_seen_at ? formatShortDate(agent.last_seen_at, t) : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever),
    isFoundation: isFoundationAgent(agent),
    needsAttention: status.needsAttention,
  };
}

export function buildAgentDetailViewModel({
  detail,
  foundationRow,
  dbConnectionCount,
  validationCount,
  systemsCount,
  t,
  formatTimestamp,
}) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  const status = getAgentStatus(detail || {});
  return {
    name: detail?.name || "—",
    agentId: detail?.agent_id || "",
    status,
    statusLabel: t(status.labelKey),
    environment: foundationRow?.environment || detail?.metadata?.environment || "—",
    version: foundationRow?.version || detail?.version || "—",
    lastHeartbeat: foundationRow?.lastHeartbeatText || fmt(detail?.last_seen_at) || t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever),
    capabilities: foundationRow?.capabilityLabels || formatCapabilityList(detail?.capabilities, t),
    inventorySummary: {
      dbConnections: dbConnectionCount,
      recentValidations: validationCount,
      systemsRegistered: systemsCount,
      dbConnectionsLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailDbConnections),
      recentValidationsLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailRecentValidations),
      systemsRegisteredLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailSystemsRegistered),
    },
    metadataTags: buildMetadataTags({ detail, foundationRow, t }),
    metadataTitle: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailMetadata),
    recentJobs: (detail?.recent_jobs || []).slice(0, 5),
    environmentLabel: t(LOCAL_AGENTS_I18N_KEYS.environment),
    versionLabel: t(LOCAL_AGENTS_I18N_KEYS.version),
    lastHeartbeatLabel: t(LOCAL_AGENTS_I18N_KEYS.lastHeartbeat),
    capabilitiesLabel: t(LOCAL_AGENTS_I18N_KEYS.capabilities),
    inventorySummaryLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailInventorySummary),
  };
}

export function buildLocalAgentsConsoleViewModel({
  agents,
  dbConnections,
  foundationReport,
  t,
  formatTimestamp,
  nowMs,
}) {
  const sorted = sortAgents(agents, nowMs);
  const foundationVm = buildLocalAgentsViewModel(
    foundationReport || { agents: [], inventory: [], summary: "" },
    t,
    formatTimestamp,
  );
  const foundationById = new Map(foundationVm.agents.map((a) => [a.agent_id, a]));

  return {
    summary: getSummaryMetrics(agents, dbConnections),
    summaryLabels: {
      totalAgents: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.summaryTotalAgents),
      online: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.summaryOnline),
      offline: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.summaryOffline),
      dbConnections: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.summaryDbConnections),
      capabilities: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.summaryCapabilities),
    },
    agentsListTitle: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.agentsListTitle),
    createdLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.created),
    lastSeenLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeen),
    agents: sorted.map((agent) =>
      buildAgentListItemViewModel(agent, foundationById.get(agent.agent_id) || null, t, nowMs),
    ),
    capabilitySummary: getCapabilitySummary(agents, t),
    capabilitiesOverviewTitle: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.capabilitiesOverview),
    foundationById,
  };
}

export function buildDatabaseConnectionCardViewModel(connection, agentNameById, t, formatTimestamp) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  const agentName = agentNameById.get(connection.agent_id) || connection.agent_id;
  const status = String(connection.status || "UNKNOWN").toUpperCase();
  const statusBadgeClass =
    status === "CONNECTED"
      ? "badge badge-green"
      : status === "DEGRADED"
        ? "badge badge-orange"
        : status === "ERROR"
          ? "badge badge-red"
          : status === "PENDING_VALIDATION"
            ? "badge badge-gray"
            : "badge badge-orange";
  const statusLabel =
    status === "CONNECTED"
      ? t("localAgents.database.connected")
      : status === "DEGRADED"
        ? t("localAgents.database.degraded")
        : status === "ERROR"
          ? t("localAgents.database.error")
          : status === "PENDING_VALIDATION"
            ? t("localAgents.database.pending_validation")
            : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.dbStatusRegistered);
  const scope = String(connection.asset_scope || "customer_external");
  const mode = String(connection.execution_mode || "local_agent");
  return {
    connectionId: connection.connection_id,
    name: connection.name,
    databaseType: connection.database_type,
    agentLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.dbAgentLabel, { name: agentName }),
    statusLabel,
    statusBadgeClass,
    assetScopeLabel:
      scope === "platform_internal"
        ? t("localAgents.database.asset_scope_platform")
        : t("localAgents.database.asset_scope_customer"),
    executionModeLabel:
      mode === "platform_backend"
        ? t("localAgents.database.execution_mode_platform")
        : t("localAgents.database.execution_mode_local_agent"),
    lastProbeText: fmt(connection.last_probe_at),
    createdText: fmt(connection.created_at),
    createdLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.dbCreated),
  };
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
