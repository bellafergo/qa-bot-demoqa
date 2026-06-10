# services/enterprise_dependency_map_service.py
"""
Integration Intelligence INT-04A — Enterprise Dependency Map (read-only).

Builds a deterministic dependency graph from existing Vanya intelligence only.
No infrastructure scanning, network calls, or mutations.
"""
from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass
from typing import Dict, List, Optional, Set, Tuple

from models.incident_models import (
    ApiContractReport,
    ContractRiskReport,
    DatabaseValidationReport,
    DataJourneyReport,
    DecisionCenterSummary,
    DependencyEdge,
    DependencyNode,
    DeploymentRiskAssessment,
    EnterpriseDependencyMap,
    HistoricalLearningReport,
    IncidentImpactNode,
    TestRecommendationReport,
)

_RISK_RANK = {"LOW": 0, "MEDIUM": 1, "HIGH": 2, "CRITICAL": 3}
_RISK_LEVELS = ("LOW", "MEDIUM", "HIGH", "CRITICAL")
_CONFIDENCE_DECAY = 0.85
_AREA_SUFFIXES = (" flow", " smoke", " suite", " regression", " ui", " module")


@dataclass
class _Graph:
    nodes: Dict[str, DependencyNode]
    edges: Dict[str, DependencyEdge]

    def add_node(self, node: DependencyNode) -> None:
        existing = self.nodes.get(node.node_id)
        if existing is None or _RISK_RANK.get(node.risk_level, 0) > _RISK_RANK.get(existing.risk_level, 0):
            self.nodes[node.node_id] = node
        elif existing and node.confidence > existing.confidence:
            self.nodes[node.node_id] = DependencyNode(
                node_id=existing.node_id,
                node_type=existing.node_type,
                name=existing.name,
                description=existing.description or node.description,
                risk_level=existing.risk_level,
                confidence=node.confidence,
            )

    def add_edge(self, edge: DependencyEdge) -> None:
        self.edges[edge.edge_id] = edge

    def set_risk(self, node_id: str, risk_level: str, confidence: Optional[float] = None) -> None:
        node = self.nodes.get(node_id)
        if not node:
            return
        current = _RISK_RANK.get(node.risk_level, 0)
        incoming = _RISK_RANK.get(risk_level, 0)
        if incoming > current:
            self.nodes[node_id] = DependencyNode(
                node_id=node.node_id,
                node_type=node.node_type,
                name=node.name,
                description=node.description,
                risk_level=risk_level,
                confidence=confidence if confidence is not None else node.confidence,
            )


def build_node_id(node_type: str, slug: str) -> str:
    type_slug = re.sub(r"[^a-z0-9]+", "_", (node_type or "node").strip().lower()).strip("_")
    name_slug = re.sub(r"[^a-z0-9]+", "_", (slug or "item").strip().lower()).strip("_")
    return f"node:{type_slug}:{name_slug}"


def build_edge_id(source_node_id: str, relationship_type: str, target_node_id: str) -> str:
    rel = re.sub(r"[^a-z0-9]+", "_", (relationship_type or "rel").strip().lower()).strip("_")
    return f"edge:{source_node_id}:{rel}:{target_node_id}"


def _normalize_area(name: str) -> str:
    s = (name or "").strip().lower()
    for suffix in _AREA_SUFFIXES:
        if s.endswith(suffix):
            s = s[: -len(suffix)].strip()
    return re.sub(r"\s+", " ", s) or ""


def _area_category(area: str) -> str:
    key = _normalize_area(area)
    if any(k in key for k in ("checkout", "cart", "order")):
        return "checkout"
    if any(k in key for k in ("payment", "payments", "billing")):
        return "payments"
    if any(k in key for k in ("auth", "authentication", "login", "session")):
        return "authentication"
    if any(k in key for k in ("inventory", "stock")):
        return "inventory"
    if any(k in key for k in ("erp", "fulfillment")):
        return "erp"
    return key or "general"


def _base_risk_from_deployment(deployment: Optional[DeploymentRiskAssessment]) -> str:
    if not deployment:
        return "LOW"
    level = (deployment.risk_level or "low").lower()
    if level == "critical":
        return "CRITICAL"
    if level == "high":
        return "HIGH"
    if level == "medium":
        return "MEDIUM"
    return "LOW"


def _base_risk_from_decision_center(decision_center: Optional[DecisionCenterSummary]) -> str:
    if not decision_center:
        return "LOW"
    status = decision_center.overall_status
    if status == "RED":
        return "CRITICAL"
    if status == "ORANGE":
        return "HIGH"
    if status == "YELLOW":
        return "MEDIUM"
    return "LOW"


def _merge_risk(*levels: str) -> str:
    best = 0
    for level in levels:
        best = max(best, _RISK_RANK.get((level or "LOW").upper(), 0))
    return _RISK_LEVELS[min(best, 3)]


def _propagate_risk_down(graph: _Graph) -> None:
    """Propagate elevated risk one hop downstream with decreasing severity and confidence."""
    for _ in range(3):
        for edge in list(graph.edges.values()):
            source = graph.nodes.get(edge.source_node_id)
            target = graph.nodes.get(edge.target_node_id)
            if not source or not target:
                continue
            src_rank = _RISK_RANK.get(source.risk_level, 0)
            tgt_rank = _RISK_RANK.get(target.risk_level, 0)
            if src_rank <= tgt_rank:
                continue
            propagated = _RISK_LEVELS[max(0, src_rank - 1)]
            new_conf = round(min(source.confidence, target.confidence) * _CONFIDENCE_DECAY, 2)
            graph.set_risk(target.node_id, propagated, new_conf)


def _blast_radius_from_critical(graph: _Graph) -> Set[str]:
    """Elevate all nodes reachable from CRITICAL sources."""
    affected: Set[str] = set()
    critical_ids = [n.node_id for n in graph.nodes.values() if n.risk_level == "CRITICAL"]
    for critical_id in critical_ids:
        queue = [critical_id]
        visited = {critical_id}
        while queue:
            current = queue.pop(0)
            affected.add(current)
            for edge in graph.edges.values():
                if edge.source_node_id != current:
                    continue
                downstream = edge.target_node_id
                if downstream in visited:
                    continue
                visited.add(downstream)
                queue.append(downstream)
                node = graph.nodes.get(downstream)
                if not node:
                    continue
                rank = _RISK_RANK.get(node.risk_level, 0)
                source_rank = _RISK_RANK.get(graph.nodes[current].risk_level, 0)
                if source_rank > rank:
                    graph.set_risk(
                        downstream,
                        _RISK_LEVELS[min(3, source_rank)],
                        round(node.confidence * _CONFIDENCE_DECAY, 2),
                    )
    return affected


def _stage_node_type(stage_type: str) -> str:
    if stage_type == "api":
        return "api"
    if stage_type == "database":
        return "database"
    if stage_type in ("erp", "audit", "queue"):
        return "external_system"
    if stage_type == "ui":
        return "module"
    return "module"


def _add_journey_graph(
    graph: _Graph,
    data_journey_validation: DataJourneyReport,
    base_risk: str,
) -> None:
    for journey in data_journey_validation.journeys:
        journey_node_id = build_node_id("journey", journey.journey_id)
        graph.add_node(
            DependencyNode(
                node_id=journey_node_id,
                node_type="journey",
                name=journey.name,
                description=journey.description,
                risk_level=base_risk,
                confidence=0.82,
            )
        )
        prev_id = journey_node_id
        for stage in journey.stages:
            stage_slug = stage.stage_id.split(":stage:")[-1] if ":stage:" in stage.stage_id else stage.name
            stage_type = _stage_node_type(stage.stage_type)
            stage_node_id = build_node_id(stage_type, f"{journey.journey_id}:{stage_slug}")
            graph.add_node(
                DependencyNode(
                    node_id=stage_node_id,
                    node_type=stage_type,
                    name=stage.name,
                    description=f"Stage in {journey.name}",
                    risk_level=base_risk,
                    confidence=0.8,
                )
            )
            graph.add_edge(
                DependencyEdge(
                    edge_id=build_edge_id(prev_id, "depends_on", stage_node_id),
                    source_node_id=prev_id,
                    target_node_id=stage_node_id,
                    relationship_type="depends_on",
                    confidence=0.85,
                )
            )
            prev_id = stage_node_id


def _add_contract_graph(
    graph: _Graph,
    api_contract_intelligence: ApiContractReport,
    contract_risk_assessment: Optional[ContractRiskReport],
) -> None:
    risk_by_contract: Dict[str, str] = {}
    if contract_risk_assessment:
        for assessment in contract_risk_assessment.assessments:
            risk_by_contract[assessment.contract_id] = assessment.overall_risk_level

    for contract in api_contract_intelligence.contracts:
        api_node_id = build_node_id("api", contract.contract_id)
        contract_node_id = build_node_id("contract", contract.contract_id)
        contract_risk = risk_by_contract.get(contract.contract_id, "MEDIUM")
        graph.add_node(
            DependencyNode(
                node_id=api_node_id,
                node_type="api",
                name=contract.service_name,
                description=f"{contract.method} {contract.endpoint}",
                risk_level=_merge_risk(contract_risk, "MEDIUM") if contract_risk != "LOW" else "MEDIUM",
                confidence=contract.confidence,
            )
        )
        graph.add_node(
            DependencyNode(
                node_id=contract_node_id,
                node_type="contract",
                name=f"{contract.service_name} Contract",
                description=f"Version {contract.version}",
                risk_level=contract_risk,
                confidence=contract.confidence,
            )
        )
        graph.add_edge(
            DependencyEdge(
                edge_id=build_edge_id(api_node_id, "uses", contract_node_id),
                source_node_id=api_node_id,
                target_node_id=contract_node_id,
                relationship_type="uses",
                confidence=0.88,
            )
        )

        service_key = contract.service_name.lower()
        for node in graph.nodes.values():
            if node.node_type != "api" or node.node_id == api_node_id:
                continue
            node_key = node.name.lower()
            if service_key in node_key or node_key in service_key or ("payment" in node_key and "payment" in service_key):
                graph.add_edge(
                    DependencyEdge(
                        edge_id=build_edge_id(node.node_id, "calls", api_node_id),
                        source_node_id=node.node_id,
                        target_node_id=api_node_id,
                        relationship_type="calls",
                        confidence=0.8,
                    )
                )


def _add_database_validation_graph(
    graph: _Graph,
    database_validation: DatabaseValidationReport,
) -> None:
    for check in database_validation.checks:
        validation_id = build_node_id("validation", check.check_id)
        db_slug = _normalize_area(check.name) or check.check_id
        db_node_id = build_node_id("database", db_slug)
        graph.add_node(
            DependencyNode(
                node_id=validation_id,
                node_type="validation",
                name=check.name,
                description=check.description or "Read-only validation check",
                risk_level="MEDIUM",
                confidence=0.78,
            )
        )
        graph.add_node(
            DependencyNode(
                node_id=db_node_id,
                node_type="database",
                name=check.database_type or "Database",
                description=check.query[:120] if check.query else "Database dependency",
                risk_level="LOW",
                confidence=0.75,
            )
        )
        graph.add_edge(
            DependencyEdge(
                edge_id=build_edge_id(validation_id, "reads_from", db_node_id),
                source_node_id=validation_id,
                target_node_id=db_node_id,
                relationship_type="reads_from",
                confidence=0.84,
            )
        )

        for journey_node in [n for n in graph.nodes.values() if n.node_type == "journey"]:
            area = _area_category(journey_node.name)
            if area in check.check_id.lower() or area in check.name.lower():
                graph.add_edge(
                    DependencyEdge(
                        edge_id=build_edge_id(journey_node.node_id, "validates", validation_id),
                        source_node_id=journey_node.node_id,
                        target_node_id=validation_id,
                        relationship_type="validates",
                        confidence=0.8,
                    )
                )

        for db_stage in [n for n in graph.nodes.values() if n.node_type == "database" and "db" in n.name.lower()]:
            if any(k in check.name.lower() for k in ("order", "inventory", "session", "audit", "payment")):
                if any(k in db_stage.name.lower() for k in ("order", "inventory", "session", "audit", "payment")):
                    graph.add_edge(
                        DependencyEdge(
                            edge_id=build_edge_id(db_stage.node_id, "validates", validation_id),
                            source_node_id=db_stage.node_id,
                            target_node_id=validation_id,
                            relationship_type="validates",
                            confidence=0.78,
                        )
                    )


def _add_impact_map_graph(graph: _Graph, impact_map: List[IncidentImpactNode]) -> None:
    for node in impact_map:
        module_id = build_node_id("module", node.title)
        severity_risk = "HIGH" if node.severity == "high" else "MEDIUM" if node.severity == "medium" else "LOW"
        graph.add_node(
            DependencyNode(
                node_id=module_id,
                node_type="module",
                name=node.title,
                description=node.description,
                risk_level=severity_risk,
                confidence=node.confidence,
            )
        )
        for journey_node in [n for n in graph.nodes.values() if n.node_type == "journey"]:
            if _area_category(journey_node.name) == _area_category(node.title):
                graph.add_edge(
                    DependencyEdge(
                        edge_id=build_edge_id(module_id, "depends_on", journey_node.node_id),
                        source_node_id=module_id,
                        target_node_id=journey_node.node_id,
                        relationship_type="depends_on",
                        confidence=0.76,
                    )
                )


def _add_test_graph(
    graph: _Graph,
    test_recommendations: Optional[TestRecommendationReport],
    recommended_tests: List[str],
    contract_risk_assessment: Optional[ContractRiskReport],
) -> None:
    tests: List[Tuple[str, str, float]] = []
    if test_recommendations:
        for rec in test_recommendations.recommendations:
            tests.append((rec.test_name, rec.recommendation_id, rec.confidence))
    for name in recommended_tests:
        slug = re.sub(r"[^a-z0-9]+", "_", name.lower()).strip("_")
        tests.append((name, slug, 0.7))

    affected_tests: Set[str] = set()
    if contract_risk_assessment:
        for assessment in contract_risk_assessment.assessments:
            affected_tests.update(assessment.affected_tests)

    for test_name, test_id, confidence in tests:
        test_node_id = build_node_id("module", f"test_{test_id}")
        risk = "HIGH" if test_name in affected_tests else "MEDIUM"
        graph.add_node(
            DependencyNode(
                node_id=test_node_id,
                node_type="business_flow",
                name=test_name,
                description="Recommended regression coverage",
                risk_level=risk,
                confidence=confidence,
            )
        )
        for contract_node in [n for n in graph.nodes.values() if n.node_type == "contract"]:
            if any(k in test_name.lower() for k in ("payment", "checkout", "auth")):
                if any(k in contract_node.name.lower() for k in ("payment", "checkout", "auth")):
                    graph.add_edge(
                        DependencyEdge(
                            edge_id=build_edge_id(contract_node.node_id, "validates", test_node_id),
                            source_node_id=contract_node.node_id,
                            target_node_id=test_node_id,
                            relationship_type="validates",
                            confidence=0.8,
                        )
                    )


def _apply_contract_blast_radius(
    graph: _Graph,
    contract_risk_assessment: Optional[ContractRiskReport],
) -> None:
    if not contract_risk_assessment:
        return
    for assessment in contract_risk_assessment.assessments:
        contract_node_id = build_node_id("contract", assessment.contract_id)
        if contract_node_id in graph.nodes:
            graph.set_risk(contract_node_id, assessment.overall_risk_level, assessment.confidence)
        for journey_name in assessment.affected_journeys:
            journey_key = journey_name.lower()
            for node in graph.nodes.values():
                if node.node_type != "journey":
                    continue
                node_key = node.name.lower()
                if node_key == journey_key or journey_key in node_key or node_key in journey_key:
                    graph.set_risk(
                        node.node_id,
                        _merge_risk(node.risk_level, assessment.overall_risk_level),
                        round(min(node.confidence, assessment.confidence), 2),
                    )
        for test_name in assessment.affected_tests:
            for node in graph.nodes.values():
                if node.name == test_name:
                    graph.set_risk(
                        node.node_id,
                        _merge_risk(node.risk_level, assessment.overall_risk_level),
                        round(min(node.confidence, assessment.confidence), 2),
                    )
        for check_name_fragment in ("order", "inventory", "payment", "session", "audit"):
            if assessment.overall_risk_level not in ("HIGH", "CRITICAL"):
                continue
            for node in graph.nodes.values():
                if node.node_type == "validation" and check_name_fragment in node.name.lower():
                    graph.set_risk(
                        node.node_id,
                        _merge_risk(node.risk_level, "HIGH"),
                        round(node.confidence * _CONFIDENCE_DECAY, 2),
                    )


def _has_source_intelligence(
    *,
    impact_map: List[IncidentImpactNode],
    data_journey_validation: Optional[DataJourneyReport],
    api_contract_intelligence: Optional[ApiContractReport],
    database_validation: Optional[DatabaseValidationReport],
) -> bool:
    if impact_map:
        return True
    if data_journey_validation and data_journey_validation.journeys:
        return True
    if api_contract_intelligence and api_contract_intelligence.contracts:
        return True
    if database_validation and database_validation.checks:
        return True
    return False


def build_enterprise_dependency_map(
    *,
    impact_map: List[IncidentImpactNode],
    data_journey_validation: Optional[DataJourneyReport],
    api_contract_intelligence: Optional[ApiContractReport],
    contract_risk_assessment: Optional[ContractRiskReport],
    database_validation: Optional[DatabaseValidationReport],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    decision_center: Optional[DecisionCenterSummary],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_tests: Optional[List[str]] = None,
) -> Optional[EnterpriseDependencyMap]:
    if not _has_source_intelligence(
        impact_map=impact_map,
        data_journey_validation=data_journey_validation,
        api_contract_intelligence=api_contract_intelligence,
        database_validation=database_validation,
    ):
        return None

    base_risk = _merge_risk(
        _base_risk_from_deployment(deployment_risk_assessment),
        _base_risk_from_decision_center(decision_center),
    )
    if historical_learning and historical_learning.similar_incidents:
        base_risk = _merge_risk(base_risk, "MEDIUM")

    graph = _Graph(nodes={}, edges={})

    if data_journey_validation:
        _add_journey_graph(graph, data_journey_validation, base_risk)

    if impact_map:
        _add_impact_map_graph(graph, impact_map)

    if api_contract_intelligence:
        _add_contract_graph(graph, api_contract_intelligence, contract_risk_assessment)

    if database_validation:
        _add_database_validation_graph(graph, database_validation)

    _add_test_graph(
        graph,
        test_recommendations,
        recommended_tests or [],
        contract_risk_assessment,
    )

    _apply_contract_blast_radius(graph, contract_risk_assessment)
    _propagate_risk_down(graph)
    blast_nodes = _blast_radius_from_critical(graph)

    if not graph.nodes:
        return None

    nodes = sorted(graph.nodes.values(), key=lambda n: n.node_id)
    edges = sorted(graph.edges.values(), key=lambda e: e.edge_id)

    risk_counts = {level: 0 for level in _RISK_LEVELS}
    for node in nodes:
        risk_counts[node.risk_level] = risk_counts.get(node.risk_level, 0) + 1

    affected_areas = sorted(
        {
            n.name
            for n in nodes
            if n.risk_level in ("HIGH", "CRITICAL") and n.node_type in ("journey", "module", "business_flow")
        }
    )
    critical_count = risk_counts.get("CRITICAL", 0)
    if critical_count:
        summary = (
            f"Enterprise dependency map with {len(nodes)} nodes and {len(edges)} relationships; "
            f"{critical_count} CRITICAL node(s); blast radius spans {len(blast_nodes)} node(s)."
        )
    else:
        summary = (
            f"Enterprise dependency map with {len(nodes)} nodes and {len(edges)} relationships "
            f"across {len(affected_areas) or len(nodes)} area(s)."
        )

    digest = hashlib.sha256("|".join(n.node_id for n in nodes).encode("utf-8")).hexdigest()[:8]
    confidence = round(min(0.95, sum(n.confidence for n in nodes) / max(len(nodes), 1)), 2)

    return EnterpriseDependencyMap(
        nodes=nodes,
        edges=edges,
        summary=f"{summary} Affected: {', '.join(affected_areas) if affected_areas else 'none elevated'}. Ref: {digest}.",
        confidence=confidence,
    )
