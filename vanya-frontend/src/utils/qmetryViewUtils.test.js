import { describe, it, expect } from "vitest";
import {
  QMETRY_I18N_KEYS,
  buildConnectionCardViewModel,
  buildProjectCardViewModel,
  buildQMetryIntegrationViewModel,
  buildTestCaseCardViewModel,
  connectionBadgeClass,
  deriveQMetryHeaderState,
  formatCount,
} from "./qmetryViewUtils.js";

const t = (key) => key;

const sampleStatus = {
  connected: true,
  base_url: "https://jira.example.com",
  project_count: 3,
  test_case_count: 320,
  run_count: 48,
  last_sync: "2026-06-10T12:00:00Z",
};

describe("qmetryViewUtils", () => {
  it("formats counts for display", () => {
    expect(formatCount(320)).toBe("320");
    expect(formatCount(undefined)).toBe("0");
  });

  it("builds connection card view model", () => {
    const vm = buildConnectionCardViewModel(sampleStatus, t);
    expect(vm.connectedBadgeClass).toBe(connectionBadgeClass(true));
    expect(vm.counts).toHaveLength(3);
    expect(vm.counts[1].value).toBe("320");
    expect(vm.showEmptyConnection).toBe(false);
    expect(vm.serverUrl).toBe("https://jira.example.com");
  });

  it("shows empty connection state", () => {
    const vm = buildConnectionCardViewModel({ connected: false }, t);
    expect(vm.showEmptyConnection).toBe(true);
    expect(vm.emptyConnectionText).toBe(QMETRY_I18N_KEYS.emptyConnection);
  });

  it("renders project cards", () => {
    const vm = buildProjectCardViewModel({
      project_id: "1",
      project_name: "QA Platform",
    });
    expect(vm.projectId).toBe("1");
    expect(vm.projectName).toBe("QA Platform");
  });

  it("renders test case cards", () => {
    const vm = buildTestCaseCardViewModel({
      test_case_id: "QA-T1",
      name: "Login test",
      priority: "High",
      status: "Approved",
    });
    expect(vm.testCaseId).toBe("QA-T1");
    expect(vm.name).toBe("Login test");
  });

  it("builds integration view model with empty states", () => {
    const vm = buildQMetryIntegrationViewModel({
      status: sampleStatus,
      projects: [],
      testCases: [],
      testCycles: [],
      testSuites: [],
      testRuns: [],
      t,
    });
    expect(vm.showEmptyProjects).toBe(true);
    expect(vm.showEmptyTestCases).toBe(true);
    expect(vm.testCycles.showEmpty).toBe(true);
    expect(vm.testSuites.showEmpty).toBe(true);
    expect(vm.testRuns.showEmpty).toBe(true);
  });

  it("derives header state from connection status", () => {
    expect(deriveQMetryHeaderState({ connected: true }).health).toBe("ok");
    expect(deriveQMetryHeaderState({ connected: false }).health).toBe("unconfigured");
  });
});
