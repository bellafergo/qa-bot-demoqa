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
  statusPlatformManaged: "localAgents.enterprise.status.platform_managed",
  statusDegraded: "localAgents.enterprise.status.degraded",
  statusUnavailable: "localAgents.enterprise.status.unavailable",
  lastProbe: "localAgents.enterprise.last_probe",
  detailLastProbe: "localAgents.enterprise.detail.last_probe",
  detailLastValidation: "localAgents.enterprise.detail.last_validation",
  detailPlatformAssetHealth: "localAgents.enterprise.detail.platform_asset_health",
  detailPlatformManaged: "localAgents.enterprise.detail.platform_managed",
  platformAssetHealthSummary: "localAgents.enterprise.platform_asset_health_summary",
};

const STATUS_BADGE = {
  ONLINE: "badge badge-green",
  OFFLINE: "badge badge-gray",
  UNKNOWN: "badge badge-orange",
  WARNING: "badge badge-orange",
  ERROR: "badge badge-red",
  PLATFORM_MANAGED: "badge badge-green",
  DEGRADED: "badge badge-orange",
  UNAVAILABLE: "badge badge-red",
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

export function isPlatformManagedAgent(agent) {
  const metadata = agent?.metadata && typeof agent.metadata === "object" ? agent.metadata : {};
  if (metadata.platform_managed === true) return true;
  return String(metadata.agent_type || "").trim().toLowerCase() === "platform";
}

export function filterPlatformConnections(agentId, connections = []) {
  const aid = String(agentId || "").trim();
  if (!aid) return [];
  return (connections || []).filter(
    (connection) =>
      String(connection?.agent_id || "").trim() === aid
      && String(connection?.asset_scope || "").trim().toLowerCase() === "platform_internal",
  );
}

function latestIsoTimestamp(values = []) {
  let latest = null;
  let latestMs = Number.NEGATIVE_INFINITY;
  for (const raw of values) {
    if (!raw) continue;
    const ms = new Date(raw).getTime();
    if (!Number.isFinite(ms) || ms <= latestMs) continue;
    latestMs = ms;
    latest = raw;
  }
  return latest;
}

function agentScopedExecutions(agentId, executions = []) {
  const aid = String(agentId || "").trim();
  if (!aid) return [];
  return (executions || []).filter((execution) => String(execution?.agent_id || "").trim() === aid);
}

/** Platform-managed agents derive health from probes and validation evidence, not heartbeat. */
export function resolvePlatformAgentStatus(agent, options = {}) {
  if (!agent?.enabled || String(agent.status || "").toLowerCase() === "disabled") {
    return {
      key: "ERROR",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusError,
      badgeClass: STATUS_BADGE.ERROR,
      needsAttention: true,
    };
  }

  const platformConnections = filterPlatformConnections(agent.agent_id, options.connections);
  const scopedExecutions = agentScopedExecutions(agent.agent_id, options.executions);

  if (platformConnections.length === 0) {
    return {
      key: "UNAVAILABLE",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusUnavailable,
      badgeClass: STATUS_BADGE.UNAVAILABLE,
      needsAttention: true,
    };
  }

  const connectionStatuses = platformConnections.map((c) => String(c.status || "UNKNOWN").toUpperCase());
  const probeStatuses = platformConnections.map((c) => String(c.last_probe_status || "").toUpperCase());
  const hasSuccessfulProbe = probeStatuses.some((status) => status === "SUCCESS");
  const hasSuccessfulExecution = scopedExecutions.some(
    (execution) => String(execution.status || "").toUpperCase() === "SUCCESS",
  );
  const hasValidationEvidence = hasSuccessfulProbe || hasSuccessfulExecution;

  if (connectionStatuses.every((status) => status === "ERROR")) {
    return {
      key: "UNAVAILABLE",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusUnavailable,
      badgeClass: STATUS_BADGE.UNAVAILABLE,
      needsAttention: true,
    };
  }

  const hasDegradedSignal =
    connectionStatuses.some((status) => status === "DEGRADED" || status === "PENDING_VALIDATION")
    || probeStatuses.some((status) => status === "FAILED");

  const allConnected = connectionStatuses.every((status) => status === "CONNECTED");

  if (allConnected && hasValidationEvidence && !hasDegradedSignal) {
    return {
      key: "PLATFORM_MANAGED",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusPlatformManaged,
      badgeClass: STATUS_BADGE.PLATFORM_MANAGED,
      needsAttention: false,
    };
  }

  if (hasDegradedSignal || connectionStatuses.some((status) => status === "CONNECTED")) {
    return {
      key: "DEGRADED",
      labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusDegraded,
      badgeClass: STATUS_BADGE.DEGRADED,
      needsAttention: true,
    };
  }

  return {
    key: "UNAVAILABLE",
    labelKey: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusUnavailable,
    badgeClass: STATUS_BADGE.UNAVAILABLE,
    needsAttention: true,
  };
}

export function buildPlatformHealthSummary(agent, connections = [], executions = [], t, formatTimestamp) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  const platformConnections = filterPlatformConnections(agent?.agent_id, connections);
  const scopedExecutions = agentScopedExecutions(agent?.agent_id, executions);
  const lastProbeAt = latestIsoTimestamp(platformConnections.map((c) => c.last_probe_at));
  const lastValidationAt = latestIsoTimestamp(
    scopedExecutions
      .filter((execution) => String(execution.status || "").toUpperCase() === "SUCCESS")
      .map((execution) => execution.executed_at),
  );
  const connectedCount = platformConnections.filter(
    (connection) => String(connection.status || "").toUpperCase() === "CONNECTED",
  ).length;

  return {
    lastProbe: lastProbeAt ? fmt(lastProbeAt) : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever),
    lastValidation: lastValidationAt ? fmt(lastValidationAt) : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever),
    platformAssetHealth:
      platformConnections.length === 0
        ? t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever)
        : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.platformAssetHealthSummary, {
            connected: String(connectedCount),
            total: String(platformConnections.length),
          }),
    lastProbeLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailLastProbe),
    lastValidationLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailLastValidation),
    platformAssetHealthLabel: t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailPlatformAssetHealth),
  };
}

/** Heuristic agent runtime status (online <2m, stale <10m). Platform agents use probe-derived status. */
export function getAgentStatus(agent, nowMs = Date.now(), options = {}) {
  if (isPlatformManagedAgent(agent)) {
    return resolvePlatformAgentStatus(agent, options);
  }

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

function statusOptionsForAgent(agent, connections = [], executions = []) {
  return {
    connections: filterPlatformConnections(agent?.agent_id, connections),
    executions: agentScopedExecutions(agent?.agent_id, executions),
  };
}

export function getSummaryMetrics(agents, dbConnections, dbExecutions = []) {
  const list = Array.isArray(agents) ? agents : [];
  let online = 0;
  let offline = 0;
  for (const agent of list) {
    const status = getAgentStatus(agent, Date.now(), statusOptionsForAgent(agent, dbConnections, dbExecutions)).key;
    if (status === "ONLINE" || status === "PLATFORM_MANAGED") online += 1;
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

export function sortAgents(agents, nowMs = Date.now(), options = {}) {
  const { connections = [], executions = [] } = options;
  const order = {
    PLATFORM_MANAGED: 0,
    ONLINE: 0,
    WARNING: 1,
    DEGRADED: 1,
    OFFLINE: 2,
    UNAVAILABLE: 2,
    ERROR: 3,
  };
  return [...(agents || [])].sort((a, b) => {
    const aFoundation = isFoundationAgent(a) ? 0 : 1;
    const bFoundation = isFoundationAgent(b) ? 0 : 1;
    if (aFoundation !== bFoundation) return aFoundation - bFoundation;
    const sa = getAgentStatus(a, nowMs, statusOptionsForAgent(a, connections, executions)).key;
    const sb = getAgentStatus(b, nowMs, statusOptionsForAgent(b, connections, executions)).key;
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

export function buildAgentListItemViewModel(agent, foundationRow, t, nowMs = Date.now(), options = {}) {
  const { connections = [], executions = [] } = options;
  const isPlatformManaged = isPlatformManagedAgent(agent);
  const status = getAgentStatus(agent, nowMs, statusOptionsForAgent(agent, connections, executions));
  const platformConnections = filterPlatformConnections(agent.agent_id, connections);
  const lastProbeAt = latestIsoTimestamp(platformConnections.map((c) => c.last_probe_at));
  return {
    agentId: agent.agent_id,
    name: agent.name || agent.agent_id,
    status,
    statusLabel: t(status.labelKey),
    environment: foundationRow?.environment || agent.metadata?.environment || agent.environment || "—",
    capabilityLabels: foundationRow?.capabilityLabels || formatCapabilityList(agent.capabilities, t),
    createdText: formatShortDate(agent.created_at, t),
    lastSeenText: agent.last_seen_at ? formatShortDate(agent.last_seen_at, t) : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever),
    secondaryLabel: isPlatformManaged
      ? t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastProbe)
      : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeen),
    secondaryText: isPlatformManaged
      ? (lastProbeAt ? formatShortDate(lastProbeAt, t) : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever))
      : (agent.last_seen_at ? formatShortDate(agent.last_seen_at, t) : t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever)),
    isFoundation: isFoundationAgent(agent),
    isPlatformManaged,
    platformManagedLabel: isPlatformManaged
      ? t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailPlatformManaged)
      : null,
    needsAttention: status.needsAttention,
  };
}

export function buildAgentDetailViewModel({
  detail,
  foundationRow,
  dbConnectionCount,
  validationCount,
  systemsCount,
  dbConnections = [],
  dbExecutions = [],
  t,
  formatTimestamp,
}) {
  const fmt = typeof formatTimestamp === "function" ? formatTimestamp : (v) => v || "—";
  const isPlatformManaged = isPlatformManagedAgent(detail || {});
  const status = getAgentStatus(detail || {}, Date.now(), statusOptionsForAgent(detail, dbConnections, dbExecutions));
  const platformHealth = isPlatformManaged
    ? buildPlatformHealthSummary(detail, dbConnections, dbExecutions, t, formatTimestamp)
    : null;
  return {
    name: detail?.name || "—",
    agentId: detail?.agent_id || "",
    status,
    statusLabel: t(status.labelKey),
    isPlatformManaged,
    environment: foundationRow?.environment || detail?.metadata?.environment || "—",
    version: foundationRow?.version || detail?.version || "—",
    lastHeartbeat: foundationRow?.lastHeartbeatText || fmt(detail?.last_seen_at) || t(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever),
    platformHealth,
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
  dbExecutions = [],
  foundationReport,
  t,
  formatTimestamp,
  nowMs,
}) {
  const sorted = sortAgents(agents, nowMs, { connections: dbConnections, executions: dbExecutions });
  const foundationVm = buildLocalAgentsViewModel(
    foundationReport || { agents: [], inventory: [], summary: "" },
    t,
    formatTimestamp,
  );
  const foundationById = new Map(foundationVm.agents.map((a) => [a.agent_id, a]));

  return {
    summary: getSummaryMetrics(agents, dbConnections, dbExecutions),
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
      buildAgentListItemViewModel(
        agent,
        foundationById.get(agent.agent_id) || null,
        t,
        nowMs,
        { connections: dbConnections, executions: dbExecutions },
      ),
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
