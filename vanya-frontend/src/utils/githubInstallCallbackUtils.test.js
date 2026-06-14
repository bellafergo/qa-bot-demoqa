import { describe, expect, it } from "vitest";
import {
  parseGitHubInstallCallback,
  shouldRunGitHubInstallCallback,
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
