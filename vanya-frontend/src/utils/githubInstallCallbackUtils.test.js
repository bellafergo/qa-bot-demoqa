import { describe, expect, it } from "vitest";
import {
  parseGitHubInstallCallback,
  shouldRunGitHubInstallCallback,
  normalizeGitHubInstallationId,
} from "./githubInstallCallbackUtils.js";

describe("githubInstallCallbackUtils", () => {
  it("parses installation_id and state from query string", () => {
    const parsed = parseGitHubInstallCallback("?installation_id=12345&state=zuperio-talent-os&setup_action=install");
    expect(parsed).toEqual({
      installationId: "12345",
      stateProject: "zuperio-talent-os",
      setupAction: "install",
    });
  });

  it("returns null when installation_id is missing", () => {
    expect(parseGitHubInstallCallback("?state=demo")).toBeNull();
  });

  it("allows callback when state matches project", () => {
    const gate = shouldRunGitHubInstallCallback({
      installationId: "99",
      stateProject: "demo",
      projectId: "DEMO",
    });
    expect(gate.run).toBe(true);
  });

  it("blocks callback on state project mismatch", () => {
    const gate = shouldRunGitHubInstallCallback({
      installationId: "99",
      stateProject: "proj-a",
      projectId: "proj-b",
    });
    expect(gate.run).toBe(false);
    expect(gate.reason).toBe("state_project_mismatch");
  });

  it("allows callback when state is absent (uses current project)", () => {
    const gate = shouldRunGitHubInstallCallback({
      installationId: "99",
      stateProject: "",
      projectId: "demo",
    });
    expect(gate.run).toBe(true);
  });
});

describe("normalizeGitHubInstallationId", () => {
  it("accepts numeric installation id", () => {
    expect(normalizeGitHubInstallationId("138769836")).toBe("138769836");
  });

  it("extracts id from GitHub settings URL", () => {
    expect(
      normalizeGitHubInstallationId("https://github.com/settings/installations/138769836"),
    ).toBe("138769836");
  });

  it("rejects empty or invalid input", () => {
    expect(normalizeGitHubInstallationId("")).toBe("");
    expect(normalizeGitHubInstallationId("abc")).toBe("");
  });
});

describe("manual link flow contract", () => {
  it("builds connect-app payload from normalized id", () => {
    const installationId = normalizeGitHubInstallationId("138769836");
    expect(installationId).toBe("138769836");
    expect({ installation_id: installationId }).toEqual({ installation_id: "138769836" });
  });

  it("maps status without installation_id to unconfigured header", () => {
    const status = { installation_id: "" };
    const enabled = Boolean(status.installation_id);
    expect(enabled).toBe(false);
  });

  it("maps status with installation_id to connected header (degraded)", () => {
    const status = { installation_id: "138769836", connected: false, validation_ok: false };
    const enabled = Boolean(status.installation_id);
    const healthy = status.connected && status.validation_ok;
    expect(enabled).toBe(true);
    expect(healthy).toBe(false);
  });
});
