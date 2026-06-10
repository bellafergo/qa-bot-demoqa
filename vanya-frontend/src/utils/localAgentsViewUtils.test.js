import { describe, it, expect } from "vitest";
import {
  LOCAL_AGENTS_I18N_KEYS,
  buildLocalAgentsViewModel,
  formatCapabilityLabel,
  formatCapabilityList,
  foundationStatusBadgeClass,
  hasInventory,
  inventoryLines,
} from "./localAgentsViewUtils.js";

const t = (key) => key;

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
    expect(LOCAL_AGENTS_I18N_KEYS.capabilities).toBe("localAgents.foundation.capabilities");
    expect(LOCAL_AGENTS_I18N_KEYS.inventory).toBe("localAgents.foundation.inventory");
    expect(LOCAL_AGENTS_I18N_KEYS.lastHeartbeat).toBe("localAgents.foundation.last_heartbeat");
    expect(LOCAL_AGENTS_I18N_KEYS.online).toBe("localAgents.foundation.online");
  });
});
