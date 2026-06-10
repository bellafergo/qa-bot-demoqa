import { describe, it, expect } from "vitest";
import {
  ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS,
  buildEnterpriseDependencyMapViewModel,
  buildNodeDrilldownItem,
  buildRiskDistribution,
  formatMapConfidence,
  hasEnterpriseDependencyMapSection,
  isEnterpriseDependencyMapEmpty,
  riskLevelBadgeClass,
  riskLevelColor,
} from "./enterpriseDependencyMapViewUtils.js";

const t = (key) => key;

const sampleReport = {
  enterprise_dependency_map: {
    summary: "Enterprise dependency map with 8 nodes and 7 relationships.",
    confidence: 0.82,
    nodes: [
      {
        node_id: "node:journey:journey_checkout",
        node_type: "journey",
        name: "Checkout Journey",
        description: "Checkout flow",
        risk_level: "HIGH",
        confidence: 0.82,
      },
      {
        node_id: "node:api:contract_payments",
        node_type: "api",
        name: "Payments API",
        description: "POST /payments",
        risk_level: "CRITICAL",
        confidence: 0.85,
      },
      {
        node_id: "node:contract:contract_payments",
        node_type: "contract",
        name: "Payments API Contract",
        description: "v2",
        risk_level: "CRITICAL",
        confidence: 0.85,
      },
    ],
    edges: [
      {
        edge_id: "edge:node:journey:journey_checkout:depends_on:node:api:x",
        source_node_id: "node:journey:journey_checkout",
        target_node_id: "node:api:contract_payments",
        relationship_type: "depends_on",
        confidence: 0.85,
      },
      {
        edge_id: "edge:node:api:contract_payments:uses:node:contract:contract_payments",
        source_node_id: "node:api:contract_payments",
        target_node_id: "node:contract:contract_payments",
        relationship_type: "uses",
        confidence: 0.88,
      },
    ],
  },
  api_contract_intelligence: {
    contracts: [{ contract_id: "contract_payments", service_name: "Payments API" }],
    risk_assessments: [],
  },
};

describe("enterpriseDependencyMapViewUtils", () => {
  it("detects section and empty state", () => {
    expect(hasEnterpriseDependencyMapSection({ enterprise_dependency_map: null })).toBe(true);
    expect(hasEnterpriseDependencyMapSection({})).toBe(false);
    expect(isEnterpriseDependencyMapEmpty({ enterprise_dependency_map: null })).toBe(true);
  });

  it("renders dependency map", () => {
    const vm = buildEnterpriseDependencyMapViewModel(sampleReport, t);
    expect(vm.show).toBe(true);
    expect(vm.empty).toBe(false);
    expect(vm.map.nodeCount).toBe(3);
    expect(vm.map.relationshipCount).toBe(2);
  });

  it("renders empty state via i18n key", () => {
    const vm = buildEnterpriseDependencyMapViewModel({ enterprise_dependency_map: null }, t);
    expect(vm.emptyMessage).toBe("incident.qa.enterprise_dependency_map_empty");
  });

  it("maps risk colors and badges", () => {
    expect(riskLevelColor("LOW")).toBe("#22c55e");
    expect(riskLevelColor("CRITICAL")).toBe("#ef4444");
    expect(riskLevelBadgeClass("HIGH")).toBe("badge badge-orange");
  });

  it("builds risk distribution and affected areas", () => {
    const dist = buildRiskDistribution(sampleReport.enterprise_dependency_map.nodes);
    expect(dist.CRITICAL).toBe(2);
    const vm = buildEnterpriseDependencyMapViewModel(sampleReport, t);
    expect(vm.map.affectedAreas).toContain("Checkout Journey");
  });

  it("builds preview payloads and graph chains", () => {
    const vm = buildEnterpriseDependencyMapViewModel(sampleReport, t);
    expect(vm.map.nodes[0].previewPayload.title).toBe("Checkout Journey");
    expect(vm.map.graphChains.length).toBeGreaterThan(0);
  });

  it("builds drilldown items", () => {
    const item = buildNodeDrilldownItem(sampleReport.enterprise_dependency_map.nodes[1], sampleReport);
    expect(item.related_entity_type).toBe("api");
  });

  it("exposes translation keys", () => {
    expect(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.title).toBe("incident.qa.enterprise_dependency_map");
    expect(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.dependencies).toBe(
      "incident.qa.enterprise_dependency_map_dependencies",
    );
    expect(formatMapConfidence(0.82)).toBe("82%");
  });
});
