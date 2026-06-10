import { describe, it, expect } from "vitest";
import {
  DATABASE_CONNECTOR_I18N_KEYS,
  buildDatabaseConnectionsViewModel,
  buildDatabaseExecutionsViewModel,
  buildExecuteValidationPreviewPayload,
  connectionStatusBadgeClass,
  executionStatusBadgeClass,
  pickConnectionForCheck,
} from "./databaseConnectorViewUtils.js";

const t = (key) => key;

describe("databaseConnectorViewUtils", () => {
  it("renders database connections", () => {
    const vm = buildDatabaseConnectionsViewModel(
      [
        {
          connection_id: "dbconn:agent:payments",
          agent_id: "agent:demo:store",
          name: "Payments DB",
          database_type: "postgresql",
          host_label: "payments.internal",
          database_name: "payments",
          status: "CONNECTED",
          created_at: "2026-06-10T10:00:00Z",
        },
      ],
      t,
      (v) => v,
    );
    expect(vm.connections).toHaveLength(1);
    expect(vm.connections[0].name).toBe("Payments DB");
  });

  it("maps connection status badges", () => {
    expect(connectionStatusBadgeClass("CONNECTED")).toBe("badge badge-green");
    expect(connectionStatusBadgeClass("DISCONNECTED")).toBe("badge badge-gray");
  });

  it("renders execution results", () => {
    const vm = buildDatabaseExecutionsViewModel(
      [
        {
          execution_id: "dbexec:abc",
          check_id: "dbcheck:payments:status",
          connection_id: "dbconn:agent:payments",
          executed_at: "2026-06-10T10:05:00Z",
          status: "SUCCESS",
          row_count: 2,
          summary: "Read-only validation completed.",
        },
      ],
      t,
      (v) => v,
    );
    expect(vm.executions[0].statusBadgeClass).toBe("badge badge-green");
    expect(vm.executions[0].summary).toContain("Read-only");
  });

  it("shows approval required state in execute preview", () => {
    const payload = buildExecuteValidationPreviewPayload(
      { name: "Validate payment status", database_type: "postgresql", check_id: "dbcheck:1" },
      { name: "Payments DB", host_label: "payments.internal" },
      "PENDING",
      t,
    );
    expect(payload.approvalRequired).toBe(true);
    expect(payload.canExecute).toBe(false);
  });

  it("picks matching connection for check", () => {
    const conn = pickConnectionForCheck(
      [
        { database_type: "mysql", name: "Orders" },
        { database_type: "postgresql", name: "Payments" },
      ],
      { database_type: "postgresql" },
    );
    expect(conn.name).toBe("Payments");
  });

  it("exposes translation keys", () => {
    expect(DATABASE_CONNECTOR_I18N_KEYS.connectionsTitle).toBe("localAgents.database.connections_title");
    expect(DATABASE_CONNECTOR_I18N_KEYS.executeValidation).toBe("incident.qa.database_validation_execute");
    expect(DATABASE_CONNECTOR_I18N_KEYS.blocked).toBe("localAgents.database.blocked");
  });
});
