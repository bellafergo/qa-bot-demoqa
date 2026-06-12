import { describe, it, expect } from "vitest";
import {
  ENVIRONMENT_TYPES,
  buildEnvironmentsPayload,
  buildProjectSettingsPayload,
  newEnvironmentRow,
  readEnvironmentsFromProject,
  suggestDefaultEnvironments,
  validateEnvironmentRows,
} from "../lib/projectEnvironmentUtils.js";

describe("ProjectModal environments", () => {
  it("renders environment types used by the modal", () => {
    expect(ENVIRONMENT_TYPES).toEqual(["QA", "STAGING", "PRODUCTION", "OTHER"]);
  });

  it("reads environments from project settings", () => {
    const rows = readEnvironmentsFromProject({
      settings: {
        environments: [
          { name: "QA", type: "QA", url: "https://qa.example.com" },
          { name: "Prod", type: "PRODUCTION" },
        ],
      },
    });
    expect(rows).toHaveLength(2);
    expect(rows[0].name).toBe("QA");
    expect(rows[1].type).toBe("PRODUCTION");
  });

  it("suggests one production environment from base_url without auto-saving", () => {
    const suggested = suggestDefaultEnvironments("https://zuperio-talent-os.vercel.app", []);
    expect(suggested).toHaveLength(1);
    expect(suggested[0].type).toBe("PRODUCTION");
    expect(suggested[0].url).toBe("https://zuperio-talent-os.vercel.app");
    expect(buildEnvironmentsPayload([])).toEqual([]);
  });

  it("adds environment rows via helper factory", () => {
    const row = newEnvironmentRow("STAGING");
    expect(row.type).toBe("STAGING");
    expect(row.name).toBe("");
    expect(row.id).toBeTruthy();
  });

  it("removes invalid rows from payload while preserving valid ones", () => {
    const payload = buildEnvironmentsPayload([
      { name: "QA", type: "QA", url: "https://qa.example.com" },
      { name: "", type: "STAGING", url: "" },
    ]);
    expect(payload).toEqual([{ name: "QA", type: "QA", url: "https://qa.example.com" }]);
  });

  it("validates required name and type fields", () => {
    const errors = validateEnvironmentRows([
      { name: "QA", type: "QA" },
      { name: "", type: "STAGING" },
      { name: "Prod", type: "" },
    ]);
    expect(errors).toHaveLength(2);
    expect(errors[0]).toEqual({ index: 1, field: "name" });
    expect(errors[1]).toEqual({ index: 2, field: "type" });
  });

  it("allows empty url values", () => {
    expect(validateEnvironmentRows([{ name: "QA", type: "QA", url: "" }])).toEqual([]);
    expect(buildEnvironmentsPayload([{ name: "QA", type: "QA", url: "" }])).toEqual([
      { name: "QA", type: "QA" },
    ]);
  });

  it("preserves existing project settings keys in payload builder", () => {
    const settings = buildProjectSettingsPayload({
      loginProfile: { login_url: "/login" },
      variables: { EMAIL: "qa@example.com" },
      environments: [
        { name: "QA", type: "QA" },
        { name: "Staging", type: "STAGING", url: "https://staging.example.com" },
      ],
      includeEnvironments: true,
    });
    expect(settings.login_profile.login_url).toBe("/login");
    expect(settings.variables.EMAIL).toBe("qa@example.com");
    expect(settings.environments).toEqual([
      { name: "QA", type: "QA" },
      { name: "Staging", type: "STAGING", url: "https://staging.example.com" },
    ]);
  });

  it("saves settings.environments payload for onboarding completion", () => {
    const settings = buildProjectSettingsPayload({
      loginProfile: null,
      variables: null,
      environments: [
        { name: "QA", type: "QA" },
        { name: "Production", type: "PRODUCTION", url: "https://zuperio-talent-os.vercel.app" },
      ],
      includeEnvironments: true,
    });
    expect(settings.environments).toHaveLength(2);
    expect(settings.environments[1].url).toBe("https://zuperio-talent-os.vercel.app");
  });
});
