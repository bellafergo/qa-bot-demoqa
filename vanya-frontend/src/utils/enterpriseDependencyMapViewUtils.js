/** View helpers for Enterprise Dependency Map (INT-04A). */

export const ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS = {
  title: "incident.qa.enterprise_dependency_map",
  empty: "incident.qa.enterprise_dependency_map_empty",
  dependencies: "incident.qa.enterprise_dependency_map_dependencies",
  dependents: "incident.qa.enterprise_dependency_map_dependents",
  affectedAreas: "incident.qa.enterprise_dependency_map_affected_areas",
  riskDistribution: "incident.qa.enterprise_dependency_map_risk_distribution",
  nodeCount: "incident.qa.enterprise_dependency_map_node_count",
  relationshipCount: "incident.qa.enterprise_dependency_map_relationship_count",
  preview: "incident.qa.enterprise_dependency_map_preview",
  previewSubtitle: "incident.qa.enterprise_dependency_map_preview_subtitle",
  readOnlyNote: "incident.qa.enterprise_dependency_map_read_only_note",
  riskLevel: "incident.qa.enterprise_dependency_map_risk_level",
  relatedJourneys: "incident.qa.enterprise_dependency_map_related_journeys",
  relatedContracts: "incident.qa.enterprise_dependency_map_related_contracts",
  relatedValidations: "incident.qa.enterprise_dependency_map_related_validations",
};

const RISK_COLOR = {
  LOW: "#22c55e",
  MEDIUM: "#eab308",
  HIGH: "#f97316",
  CRITICAL: "#ef4444",
};

const RISK_BADGE = {
  LOW: "badge badge-green",
  MEDIUM: "badge badge-orange",
  HIGH: "badge badge-orange",
  CRITICAL: "badge badge-red",
};

export function hasEnterpriseDependencyMapSection(report) {
  return Object.prototype.hasOwnProperty.call(report ?? {}, "enterprise_dependency_map");
}

export function isEnterpriseDependencyMapEmpty(report) {
  return report?.enterprise_dependency_map == null;
}

export function riskLevelColor(riskLevel) {
  return RISK_COLOR[String(riskLevel || "LOW").toUpperCase()] || "var(--text-3)";
}

export function riskLevelBadgeClass(riskLevel) {
  return RISK_BADGE[String(riskLevel || "LOW").toUpperCase()] || "badge badge-gray";
}

export function formatMapConfidence(value) {
  const n = Number(value);
  if (!Number.isFinite(n)) return "—";
  return `${Math.round(n * 100)}%`;
}

function edgesBySource(edges) {
  const map = new Map();
  for (const edge of edges || []) {
    const list = map.get(edge.source_node_id) || [];
    list.push(edge);
    map.set(edge.source_node_id, list);
  }
  return map;
}

function edgesByTarget(edges) {
  const map = new Map();
  for (const edge of edges || []) {
    const list = map.get(edge.target_node_id) || [];
    list.push(edge);
    map.set(edge.target_node_id, list);
  }
  return map;
}

export function buildNodeDrilldownItem(node, report) {
  if (!node?.node_id) return null;
  const typeMap = {
    contract: "api_contract",
    journey: "data_journey",
    validation: "database_validation_check",
    module: "impact_map",
    business_flow: "recommended_test",
  };
  const entityType = typeMap[node.node_type] || node.node_type;
  let entityId = node.node_id.replace(/^node:[^:]+:/, "");
  if (node.node_type === "contract") {
    entityId = node.node_id.replace("node:contract:", "");
  }
  if (node.node_type === "validation") {
    const check = (report?.database_validation?.checks || []).find((c) => c.name === node.name);
    if (check) entityId = check.check_id;
  }
  if (node.node_type === "business_flow") {
    const rec = (report?.test_recommendations?.recommendations || []).find((r) => r.test_name === node.name);
    if (rec) entityId = rec.recommendation_id;
  }
  return {
    source: "enterprise_dependency_map",
    related_entity_type: entityType,
    related_entity_id: entityId,
    reason: node.name,
    detail: node.description,
    title: node.name,
  };
}

export function buildRiskDistribution(nodes) {
  const dist = { LOW: 0, MEDIUM: 0, HIGH: 0, CRITICAL: 0 };
  for (const node of nodes || []) {
    const key = String(node.risk_level || "LOW").toUpperCase();
    if (dist[key] != null) dist[key] += 1;
  }
  return dist;
}

export function buildAffectedAreas(nodes) {
  return [...new Set(
    (nodes || [])
      .filter((n) => ["journey", "module", "business_flow"].includes(n.node_type))
      .filter((n) => ["HIGH", "CRITICAL"].includes(String(n.risk_level || "").toUpperCase()))
      .map((n) => n.name),
  )].sort();
}

export function buildGraphChains(nodes, edges) {
  const bySource = edgesBySource(edges);
  const journeys = (nodes || []).filter((n) => n.node_type === "journey");
  const chains = [];

  for (const journey of journeys) {
    const chain = [journey];
    let currentId = journey.node_id;
    const seen = new Set([currentId]);
    for (let i = 0; i < 12; i += 1) {
      const outs = (bySource.get(currentId) || []).filter((e) => e.relationship_type === "depends_on");
      if (!outs.length) break;
      const next = (nodes || []).find((n) => n.node_id === outs[0].target_node_id);
      if (!next || seen.has(next.node_id)) break;
      chain.push(next);
      seen.add(next.node_id);
      currentId = next.node_id;
    }
    chains.push(chain);
  }
  return chains;
}

export function buildNodePreviewPayload(node, nodes, edges, report, t) {
  const bySource = edgesBySource(edges);
  const byTarget = edgesByTarget(edges);
  const dependencies = (bySource.get(node.node_id) || [])
    .map((edge) => (nodes || []).find((n) => n.node_id === edge.target_node_id))
    .filter(Boolean);
  const dependents = (byTarget.get(node.node_id) || [])
    .map((edge) => (nodes || []).find((n) => n.node_id === edge.source_node_id))
    .filter(Boolean);

  const relatedJourneys = (nodes || []).filter((n) => n.node_type === "journey").map((n) => n.name);
  const relatedContracts = (nodes || []).filter((n) => n.node_type === "contract").map((n) => n.name);
  const relatedValidations = (nodes || []).filter((n) => n.node_type === "validation").map((n) => n.name);

  return {
    title: node.name,
    description: node.description,
    nodeType: node.node_type,
    riskLevel: node.risk_level,
    confidence: formatMapConfidence(node.confidence),
    dependencies,
    dependents,
    relatedJourneys,
    relatedContracts,
    relatedValidations,
    drilldownItem: buildNodeDrilldownItem(node, report),
    readOnlyNote: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.readOnlyNote),
    labels: {
      dependencies: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.dependencies),
      dependents: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.dependents),
      riskLevel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.riskLevel),
      relatedJourneys: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.relatedJourneys),
      relatedContracts: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.relatedContracts),
      relatedValidations: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.relatedValidations),
    },
  };
}

export function buildEnterpriseDependencyMapViewModel(report, t) {
  const map = report?.enterprise_dependency_map ?? null;
  const nodes = map?.nodes || [];
  const edges = map?.edges || [];
  const riskDistribution = buildRiskDistribution(nodes);
  const affectedAreas = buildAffectedAreas(nodes);

  const enrichedNodes = nodes.map((node) => ({
    ...node,
    riskColor: riskLevelColor(node.risk_level),
    riskBadgeClass: riskLevelBadgeClass(node.risk_level),
    drilldownItem: buildNodeDrilldownItem(node, report),
    previewPayload: buildNodePreviewPayload(node, nodes, edges, report, t),
  }));

  const graphChains = buildGraphChains(enrichedNodes, edges);

  const enrichedEdges = edges.map((edge) => {
    const source = nodes.find((n) => n.node_id === edge.source_node_id);
    const target = nodes.find((n) => n.node_id === edge.target_node_id);
    return {
      ...edge,
      sourceName: source?.name || edge.source_node_id,
      targetName: target?.name || edge.target_node_id,
    };
  });

  return {
    show: hasEnterpriseDependencyMapSection(report),
    empty: isEnterpriseDependencyMapEmpty(report),
    emptyMessage: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.empty),
    title: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.title),
    dependenciesLabel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.dependencies),
    dependentsLabel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.dependents),
    affectedAreasLabel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.affectedAreas),
    riskDistributionLabel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.riskDistribution),
    nodeCountLabel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.nodeCount),
    relationshipCountLabel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.relationshipCount),
    previewLabel: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.preview),
    readOnlyNote: t(ENTERPRISE_DEPENDENCY_MAP_I18N_KEYS.readOnlyNote),
    map: map
      ? {
          ...map,
          confidenceText: formatMapConfidence(map.confidence),
          nodes: enrichedNodes,
          edges: enrichedEdges,
          nodeCount: nodes.length,
          relationshipCount: edges.length,
          riskDistribution,
          affectedAreas,
          graphChains,
        }
      : null,
  };
}
