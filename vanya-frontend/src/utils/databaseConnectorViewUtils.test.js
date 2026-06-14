import { describe, expect, it } from "vitest";
import {
  DATABASE_CONNECTOR_I18N_KEYS,
  assetScopeLabel,
  buildDatabaseConnectionsViewModel,
  connectionStatusLabel,
  executionModeLabel,
} from "./databaseConnectorViewUtils.js";

const t = (key, vars) => {
  if (vars && typeof vars === "object") {
    let s = key;
    for (const [k, v] of Object.entries(vars)) {
      s = s.split(`{{${k}}}`).join(String(v));
    }
    return s;
  }
  return key;
};

describe("databaseConnectorViewUtils", () => {
  it("does not expose create sample connection key", () => {
    expect(DATABASE_CONNECTOR_I18N_KEYS.registerPlatformAssets).toBe(
      "localAgents.database.register_platform_assets",
    );
    expect(DATABASE_CONNECTOR_I18N_KEYS.createSampleConnection).toBeUndefined();
  });

  it("exposes empty state with register platform assets action", () => {
    const vm = buildDatabaseConnectionsViewModel([], t);
    expect(vm.empty).toBe(true);
    expect(vm.registerPlatformAssetsLabel).toBe("localAgents.database.register_platform_assets");
  });

  it("renders platform asset scope and execution mode labels", () => {
    expect(assetScopeLabel("platform_internal", t)).toBe("localAgents.database.asset_scope_platform");
    expect(assetScopeLabel("customer_external", t)).toBe("localAgents.database.asset_scope_customer");
    expect(executionModeLabel("platform_backend", t)).toBe("localAgents.database.execution_mode_platform");
    expect(executionModeLabel("local_agent", t)).toBe("localAgents.database.execution_mode_local_agent");
  });

  it("maps connection health statuses", () => {
    expect(connectionStatusLabel("CONNECTED", t)).toBe("localAgents.database.connected");
    expect(connectionStatusLabel("DEGRADED", t)).toBe("localAgents.database.degraded");
    expect(connectionStatusLabel("PENDING_VALIDATION", t)).toBe("localAgents.database.pending_validation");
  });

  it("includes probe timestamp in connection view model", () => {
    const vm = buildDatabaseConnectionsViewModel(
      [
        {
          connection_id: "c1",
          status: "CONNECTED",
          asset_scope: "platform_internal",
          execution_mode: "platform_backend",
          last_probe_at: "2026-06-12T10:00:00Z",
          created_at: "2026-06-12T09:00:00Z",
        },
      ],
      t,
      (v) => v,
    );
    expect(vm.connections[0].lastProbeText).toBe("2026-06-12T10:00:00Z");
    expect(vm.connections[0].assetScopeLabel).toBe("localAgents.database.asset_scope_platform");
  });
});
