import { describe, it, expect } from "vitest";
import {
  LOCAL_AGENTS_I18N_KEYS,
  LOCAL_AGENTS_ENTERPRISE_I18N_KEYS,
  FOUNDATION_AGENT_DEFAULTS,
  buildLocalAgentsViewModel,
  buildLocalAgentsConsoleViewModel,
  buildAgentDetailViewModel,
  buildAgentListItemViewModel,
  buildMetadataTags,
  buildDatabaseConnectionCardViewModel,
  formatCapabilityLabel,
  formatCapabilityList,
  foundationStatusBadgeClass,
  getAgentStatus,
  getSummaryMetrics,
  getCapabilitySummary,
  sortAgents,
  isFoundationAgent,
  hasInventory,
  inventoryLines,
} from "./localAgentsViewUtils.js";

const t = (key, vars) => {
  if (vars && key === LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.dbAgentLabel) {
    return `Agent: ${vars.name}`;
  }
  if (vars && key === LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.capabilitiesAgents) {
    return `${vars.count} agents`;
  }
  return key;
};

const NOW = new Date("2026-06-12T12:00:00Z").getTime();

const foundationAgent = {
  agent_id: "agent:foundation:001",
  name: "Foundation Agent",
  enabled: true,
  environment: "production",
  version: "1.0.0",
  capabilities: ["database_validation", "contract_validation"],
  created_at: "2026-06-12T08:00:00Z",
  last_seen_at: null,
};

const talentAgent = {
  agent_id: "agent:zuperio:001",
  name: "Zuperio Talent Agent",
  enabled: true,
  environment: "production",
  version: "1.0.0",
  capabilities: ["database_validation", "contract_validation"],
  created_at: "2026-06-12T08:00:00Z",
  last_seen_at: null,
};

const onlineAgent = {
  agent_id: "agent:online:001",
  name: "Online Agent",
  enabled: true,
  capabilities: ["browser_probe"],
  created_at: "2026-06-12T08:00:00Z",
  last_seen_at: "2026-06-12T11:59:30Z",
};

const sampleReport = {
  summary: "1 local agent(s) registered; 1 online.",
  agents: [
    {
      agent_id: "agent:store_001:store_001",
      name: "Store-001",
      version: "1.0.0",
      status: "ONLINE",
      registered_at: "2026-06-10T10:00:00Z",
      last_heartbeat_at: "2026-06-10T10:05:00Z",
      environment: "production",
      capabilities: ["database_validation", "contract_validation"],
      metadata: {},
    },
  ],
  inventory: [
    {
      agent_id: "agent:store_001:store_001",
      databases_detected: ["Oracle", "SQL Server"],
      repositories_detected: [],
      services_detected: [],
    },
  ],
};

describe("localAgentsViewUtils", () => {
  it("renders agents from foundation report", () => {
    const vm = buildLocalAgentsViewModel(sampleReport, t, (v) => v);
    expect(vm.agents).toHaveLength(1);
    expect(vm.agents[0].name).toBe("Store-001");
    expect(vm.summary).toContain("1 local agent");
  });

  it("maps status badges", () => {
    expect(foundationStatusBadgeClass("ONLINE")).toBe("badge badge-green");
    expect(foundationStatusBadgeClass("OFFLINE")).toBe("badge badge-gray");
    expect(foundationStatusBadgeClass("UNKNOWN")).toBe("badge badge-orange");
    const vm = buildLocalAgentsViewModel(sampleReport, t, (v) => v);
    expect(vm.agents[0].statusBadgeClass).toBe("badge badge-green");
  });

  it("displays capabilities", () => {
    const labels = formatCapabilityList(["database_validation", "contract_validation"], t);
    expect(labels[0]).toBe("Database Validation");
    expect(formatCapabilityLabel("contract_validation", t)).toBe("Contract Validation");
  });

  it("displays inventory", () => {
    const inv = sampleReport.inventory[0];
    expect(hasInventory(inv)).toBe(true);
    expect(inventoryLines(inv).databases).toEqual(["Oracle", "SQL Server"]);
    const vm = buildLocalAgentsViewModel(sampleReport, t, (v) => v);
    expect(vm.agents[0].hasInventory).toBe(true);
    expect(vm.agents[0].inventoryLines.databases).toContain("Oracle");
  });

  it("renders empty state", () => {
    const vm = buildLocalAgentsViewModel({ agents: [], inventory: [], summary: "No local agents registered." }, t);
    expect(vm.empty).toBe(true);
    expect(vm.emptyMessage).toBe("localAgents.empty_title");
  });

  it("exposes translation keys", () => {
    expect(LOCAL_AGENTS_I18N_KEYS.title).toBe("localAgents.title");
    expect(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.commandCenterTitle).toBe("localAgents.enterprise.command_center");
  });

  it("defines foundation agent self-service defaults", () => {
    expect(FOUNDATION_AGENT_DEFAULTS.name).toBe("Foundation Agent");
    expect(FOUNDATION_AGENT_DEFAULTS.capabilities).toEqual([
      "database_validation",
      "contract_validation",
    ]);
  });
});

describe("localAgents enterprise view utils", () => {
  it("sorts online agents before offline when neither is foundation", () => {
    const sorted = sortAgents([talentAgent, onlineAgent], NOW);
    expect(sorted[0].agent_id).toBe(onlineAgent.agent_id);
    expect(sorted[1].agent_id).toBe(talentAgent.agent_id);
  });

  it("places foundation agent first among same status", () => {
    const sorted = sortAgents([talentAgent, foundationAgent], NOW);
    expect(sorted[0].name).toBe("Foundation Agent");
    expect(sorted[1].name).toBe("Zuperio Talent Agent");
  });

  it("aggregates capability summary across agents", () => {
    const summary = getCapabilitySummary([foundationAgent, talentAgent], t);
    expect(summary).toHaveLength(2);
    const db = summary.find((item) => item.capabilityId === "database_validation");
    expect(db.agentCount).toBe(2);
    expect(db.label).toBe("Database Validation");
  });

  it("computes summary metrics for offline agents", () => {
    const metrics = getSummaryMetrics([foundationAgent, talentAgent], [{ connection_id: "c1" }]);
    expect(metrics.totalAgents).toBe(2);
    expect(metrics.online).toBe(0);
    expect(metrics.offline).toBe(2);
    expect(metrics.dbConnections).toBe(1);
    expect(metrics.capabilities).toBe(4);
  });

  it("marks agents without heartbeat as offline and needing attention", () => {
    const status = getAgentStatus(foundationAgent, NOW);
    expect(status.key).toBe("OFFLINE");
    expect(status.needsAttention).toBe(true);
    expect(status.badgeClass).toBe("badge badge-gray");
  });

  it("marks recent heartbeat as online", () => {
    const status = getAgentStatus(onlineAgent, NOW);
    expect(status.key).toBe("ONLINE");
    expect(status.needsAttention).toBe(false);
    expect(status.badgeClass).toBe("badge badge-green");
  });

  it("detects foundation agent by name", () => {
    expect(isFoundationAgent(foundationAgent)).toBe(true);
    expect(isFoundationAgent(talentAgent)).toBe(false);
  });

  it("builds detail view model without raw metadata JSON fields", () => {
    const detail = {
      ...foundationAgent,
      metadata: { environment: "production", custom_flag: true },
      recent_jobs: [],
    };
    const vm = buildAgentDetailViewModel({
      detail,
      foundationRow: null,
      dbConnectionCount: 1,
      validationCount: 3,
      systemsCount: 0,
      t,
      formatTimestamp: (v) => v,
    });
    expect(vm.name).toBe("Foundation Agent");
    expect(vm.inventorySummary.dbConnections).toBe(1);
    expect(vm.inventorySummary.recentValidations).toBe(3);
    expect(vm.metadataTags).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ label: LOCAL_AGENTS_I18N_KEYS.environment }),
        expect.objectContaining({ label: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailFoundationAgent, value: LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailYes }),
      ]),
    );
    expect(vm.metadataTags.every((tag) => typeof tag.label === "string" && typeof tag.value === "string")).toBe(true);
  });

  it("builds metadata tags with inventory count", () => {
    const tags = buildMetadataTags({
      detail: foundationAgent,
      foundationRow: {
        environment: "production",
        inventoryLines: { databases: ["PostgreSQL"], repositories: [], services: [] },
      },
      t,
    });
    const inventoryTag = tags.find((tag) => tag.label === LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.detailInventoryItems);
    expect(inventoryTag.value).toBe("1");
  });

  it("builds console view model with sorted agents and summary", () => {
    const consoleVm = buildLocalAgentsConsoleViewModel({
      agents: [talentAgent, foundationAgent, onlineAgent],
      dbConnections: [],
      foundationReport: { agents: [], inventory: [], summary: "" },
      t,
      formatTimestamp: (v) => v,
      nowMs: NOW,
    });
    expect(consoleVm.summary.totalAgents).toBe(3);
    expect(consoleVm.agents[0].name).toBe("Foundation Agent");
    expect(consoleVm.agents[1].name).toBe("Online Agent");
    expect(consoleVm.agents[2].name).toBe("Zuperio Talent Agent");
  });

  it("builds list item with never last seen for offline agent", () => {
    const item = buildAgentListItemViewModel(foundationAgent, null, t, NOW);
    expect(item.statusLabel).toBe(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.statusOffline);
    expect(item.lastSeenText).toBe(LOCAL_AGENTS_ENTERPRISE_I18N_KEYS.lastSeenNever);
    expect(item.needsAttention).toBe(true);
  });

  it("builds database connection card view model with platform asset metadata", () => {
    const agentNameById = new Map([[foundationAgent.agent_id, foundationAgent.name]]);
    const card = buildDatabaseConnectionCardViewModel(
      {
        connection_id: "conn-1",
        agent_id: foundationAgent.agent_id,
        name: "Platform Operational Store (SQLite)",
        database_type: "sqlite",
        asset_scope: "platform_internal",
        execution_mode: "platform_backend",
        status: "CONNECTED",
        last_probe_at: "2026-06-12T11:00:00Z",
        created_at: "2026-06-12T10:00:00Z",
      },
      agentNameById,
      t,
      (v) => v,
    );
    expect(card.name).toBe("Platform Operational Store (SQLite)");
    expect(card.agentLabel).toBe("Agent: Foundation Agent");
    expect(card.assetScopeLabel).toBe("localAgents.database.asset_scope_platform");
    expect(card.executionModeLabel).toBe("localAgents.database.execution_mode_platform");
    expect(card.lastProbeText).toBe("2026-06-12T11:00:00Z");
  });

  it("handles empty agents and connections in summary metrics", () => {
    const metrics = getSummaryMetrics([], []);
    expect(metrics).toEqual({
      totalAgents: 0,
      online: 0,
      offline: 0,
      dbConnections: 0,
      capabilities: 0,
    });
  });
});
