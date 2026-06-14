import { describe, expect, it } from "vitest";
import { buildBusinessWorkflowsViewModel, BUSINESS_WORKFLOW_I18N_KEYS } from "./businessWorkflowViewUtils.js";

const t = (key) => {
  const map = {
    [BUSINESS_WORKFLOW_I18N_KEYS.title]: "Business Workflows",
    [BUSINESS_WORKFLOW_I18N_KEYS.empty]: "No business workflows detected with sufficient evidence.",
    [BUSINESS_WORKFLOW_I18N_KEYS.confidenceHigh]: "High",
    [BUSINESS_WORKFLOW_I18N_KEYS.confidenceMedium]: "Medium",
    [BUSINESS_WORKFLOW_I18N_KEYS.confidenceLow]: "Low",
    [BUSINESS_WORKFLOW_I18N_KEYS.modules]: "Modules",
    [BUSINESS_WORKFLOW_I18N_KEYS.routes]: "Routes",
    [BUSINESS_WORKFLOW_I18N_KEYS.apis]: "APIs",
    [BUSINESS_WORKFLOW_I18N_KEYS.tests]: "Tests",
    [BUSINESS_WORKFLOW_I18N_KEYS.clusters]: "Related failures",
    [BUSINESS_WORKFLOW_I18N_KEYS.summary]: "Summary",
    [BUSINESS_WORKFLOW_I18N_KEYS.confidence]: "Confidence",
    [BUSINESS_WORKFLOW_I18N_KEYS.noModules]: "No modules",
    [BUSINESS_WORKFLOW_I18N_KEYS.noRoutes]: "No routes",
    [BUSINESS_WORKFLOW_I18N_KEYS.noApis]: "No APIs",
    [BUSINESS_WORKFLOW_I18N_KEYS.noTests]: "No tests",
    [BUSINESS_WORKFLOW_I18N_KEYS.noClusters]: "No failures",
  };
  return map[key] || key;
};

describe("businessWorkflowViewUtils", () => {
  it("maps workflow counts from coverage", () => {
    const vm = buildBusinessWorkflowsViewModel(
      {
        workflows: [
          {
            name: "Authentication Flow",
            type: "authentication",
            confidence: "high",
            modules: ["AUTH"],
            routes: ["/login"],
            apis: ["GET /api/auth/session"],
            tests: ["TC-TOS-009"],
            failure_clusters: ["CL-auth01"],
            coverage: { routes: 1, apis: 1, tests: 1, clusters: 1 },
            summary: "Auth flow",
          },
        ],
      },
      t,
    );
    expect(vm.hasWorkflows).toBe(true);
    expect(vm.workflows[0].counts.routes).toBe(1);
    expect(vm.workflows[0].counts.apis).toBe(1);
    expect(vm.workflows[0].counts.tests).toBe(1);
    expect(vm.workflows[0].counts.clusters).toBe(1);
  });

  it("shows empty state when no workflows", () => {
    const vm = buildBusinessWorkflowsViewModel({ workflows: [] }, t);
    expect(vm.hasWorkflows).toBe(false);
    expect(vm.emptyMessage).toBe("No business workflows detected with sufficient evidence.");
  });

  it("maps confidence labels EN", () => {
    const vm = buildBusinessWorkflowsViewModel(
      {
        workflows: [
          { name: "A", type: "authentication", confidence: "high" },
          { name: "B", type: "candidate_lifecycle", confidence: "medium" },
          { name: "C", type: "matching", confidence: "low" },
        ],
      },
      t,
    );
    expect(vm.workflows[0].confidenceLabel).toBe("High");
    expect(vm.workflows[1].confidenceLabel).toBe("Medium");
    expect(vm.workflows[2].confidenceLabel).toBe("Low");
  });

  it("maps confidence labels ES via t()", () => {
    const tEs = (key) => {
      const map = {
        [BUSINESS_WORKFLOW_I18N_KEYS.confidenceHigh]: "Alta",
        [BUSINESS_WORKFLOW_I18N_KEYS.confidenceMedium]: "Media",
        [BUSINESS_WORKFLOW_I18N_KEYS.confidenceLow]: "Baja",
        [BUSINESS_WORKFLOW_I18N_KEYS.title]: "Workflows de Negocio",
        [BUSINESS_WORKFLOW_I18N_KEYS.empty]: "No se detectaron workflows de negocio con suficiente evidencia.",
        [BUSINESS_WORKFLOW_I18N_KEYS.modules]: "Módulos",
        [BUSINESS_WORKFLOW_I18N_KEYS.routes]: "Rutas",
        [BUSINESS_WORKFLOW_I18N_KEYS.apis]: "APIs",
        [BUSINESS_WORKFLOW_I18N_KEYS.tests]: "Tests",
        [BUSINESS_WORKFLOW_I18N_KEYS.clusters]: "Fallos relacionados",
        [BUSINESS_WORKFLOW_I18N_KEYS.summary]: "Resumen",
        [BUSINESS_WORKFLOW_I18N_KEYS.confidence]: "Confianza",
        [BUSINESS_WORKFLOW_I18N_KEYS.noModules]: "Sin módulos",
        [BUSINESS_WORKFLOW_I18N_KEYS.noRoutes]: "Sin rutas",
        [BUSINESS_WORKFLOW_I18N_KEYS.noApis]: "Sin APIs",
        [BUSINESS_WORKFLOW_I18N_KEYS.noTests]: "Sin tests",
        [BUSINESS_WORKFLOW_I18N_KEYS.noClusters]: "Sin fallos",
      };
      return map[key] || key;
    };
    const vm = buildBusinessWorkflowsViewModel(
      { workflows: [{ name: "Auth", type: "authentication", confidence: "high" }] },
      tEs,
    );
    expect(vm.workflows[0].confidenceLabel).toBe("Alta");
  });
});
