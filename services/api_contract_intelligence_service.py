# services/api_contract_intelligence_service.py
"""
Integration Intelligence INT-02A — API Contract Intelligence (read-only).

Models, compares, and validates API contracts from existing incident intelligence.
No live API execution, endpoint scanning, network calls, or persistence changes.
"""
from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Set, Tuple

from models.api_testing_models import OpenAPISpecRequest
from models.incident_models import (
    ApiContract,
    ApiContractReport,
    ContractChange,
    ApiContractChangeAssessment,
    DeploymentRiskAssessment,
    HistoricalLearningReport,
    IncidentHypothesis,
    IncidentImpactNode,
    RecommendedAction,
    RelatedPRAnalysisSummary,
    TestRecommendationReport,
)

_SEVERITY_RANK = {"LOW": 0, "MEDIUM": 1, "HIGH": 2, "CRITICAL": 3}
_RISK_LEVELS = ("LOW", "MEDIUM", "HIGH", "CRITICAL")
_AREA_SUFFIXES = (" flow", " smoke", " suite", " regression", " ui", " module")


@dataclass
class _ContractTemplate:
    service_name: str
    endpoint: str
    method: str
    version: str
    request_properties: Dict[str, str]
    response_properties: Dict[str, str]


def build_contract_id(service_name: str, method: str, endpoint: str, version: str) -> str:
    raw = f"{service_name}:{method}:{endpoint}:v{version}".lower()
    slug = re.sub(r"[^a-z0-9]+", "_", raw).strip("_")
    return f"contract:{slug}"


def build_change_id(contract_id: str, change_type: str, field_name: str) -> str:
    field_key = re.sub(r"[^a-z0-9]+", "_", (field_name or "field").lower()).strip("_")
    return f"change:{contract_id}:{change_type}:{field_key}"


def build_assessment_id(contract_id: str) -> str:
    return f"assessment:{contract_id}"


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
    return key or "general"


def _object_schema(properties: Dict[str, str]) -> Dict[str, Any]:
    return {
        "type": "object",
        "properties": {name: {"type": ptype} for name, ptype in properties.items()},
    }


def _schema_properties(schema: Optional[Dict[str, Any]]) -> Dict[str, str]:
    if not schema:
        return {}
    props = schema.get("properties") or {}
    out: Dict[str, str] = {}
    for name, definition in props.items():
        if isinstance(definition, dict):
            out[name] = str(definition.get("type", "unknown"))
        else:
            out[name] = str(definition)
    return out


def _example_snippet(properties: Dict[str, str]) -> str:
    example: Dict[str, Any] = {}
    for name, ptype in sorted(properties.items()):
        if ptype in ("integer", "int"):
            example[name] = 123
        elif ptype in ("number", "float"):
            example[name] = 100
        elif ptype == "boolean":
            example[name] = True
        else:
            example[name] = "Bella"
    return json.dumps(example, indent=2, sort_keys=True)


def _contract_template(category: str) -> Optional[_ContractTemplate]:
    if category == "payments":
        return _ContractTemplate(
            service_name="Payments API",
            endpoint="/payments",
            method="POST",
            version="v2",
            request_properties={"customer_id": "integer", "amount": "number"},
            response_properties={"payment_id": "string", "status": "string"},
        )
    if category == "checkout":
        return _ContractTemplate(
            service_name="Checkout API",
            endpoint="/checkout",
            method="POST",
            version="v2",
            request_properties={"cart_id": "string", "customer_id": "integer"},
            response_properties={"order_id": "string", "status": "string"},
        )
    if category == "authentication":
        return _ContractTemplate(
            service_name="Authentication API",
            endpoint="/auth/login",
            method="POST",
            version="v2",
            request_properties={"username": "string", "password": "string"},
            response_properties={"token": "string", "expires_in": "integer"},
        )
    return None


def _is_rename_candidate(old_name: str, new_name: str) -> bool:
    old_l = old_name.lower()
    new_l = new_name.lower()
    if old_l == new_l:
        return False
    if old_l in new_l or new_l in old_l:
        return True
    old_parts = {p for p in re.split(r"[_-]", old_l) if p}
    new_parts = {p for p in re.split(r"[_-]", new_l) if p}
    return bool(old_parts & new_parts)


def detect_schema_changes(
    old_properties: Dict[str, str],
    new_properties: Dict[str, str],
    contract_id: str,
) -> List[ContractChange]:
    """Deterministically detect added, removed, type, and rename contract changes."""
    changes: List[ContractChange] = []
    old_keys = set(old_properties)
    new_keys = set(new_properties)

    removed = sorted(old_keys - new_keys)
    added = sorted(new_keys - old_keys)
    rename_pairs: List[Tuple[str, str]] = []
    used_added: Set[str] = set()
    used_removed: Set[str] = set()

    for old_field in removed:
        for new_field in added:
            if new_field in used_added:
                continue
            if old_properties.get(old_field) != new_properties.get(new_field):
                continue
            if _is_rename_candidate(old_field, new_field):
                rename_pairs.append((old_field, new_field))
                used_added.add(new_field)
                used_removed.add(old_field)
                break

    for old_field, new_field in rename_pairs:
        changes.append(
            ContractChange(
                change_id=build_change_id(contract_id, "rename", new_field),
                severity="HIGH",
                change_type="rename",
                field_name=new_field,
                old_value=old_field,
                new_value=new_field,
                description=f"Renamed field '{old_field}' to '{new_field}'.",
            )
        )

    for field in sorted(old_keys & new_keys):
        old_type = old_properties[field]
        new_type = new_properties[field]
        if old_type != new_type:
            changes.append(
                ContractChange(
                    change_id=build_change_id(contract_id, "type_change", field),
                    severity="CRITICAL",
                    change_type="type_change",
                    field_name=field,
                    old_value=old_type,
                    new_value=new_type,
                    description=f"Type changed for '{field}' from {old_type} to {new_type}.",
                )
            )

    for field in removed:
        if field in used_removed:
            continue
        changes.append(
            ContractChange(
                change_id=build_change_id(contract_id, "removed_field", field),
                severity="HIGH",
                change_type="removed_field",
                field_name=field,
                old_value=field,
                new_value=None,
                description=f"Removed field '{field}'.",
            )
        )

    for field in added:
        if field in used_added:
            continue
        changes.append(
            ContractChange(
                change_id=build_change_id(contract_id, "added_field", field),
                severity="LOW",
                change_type="added_field",
                field_name=field,
                old_value=None,
                new_value=field,
                description=f"Added field '{field}'.",
            )
        )

    changes.sort(key=lambda c: (c.change_type, c.field_name))
    return changes


def calculate_contract_risk_level(
    changes: List[ContractChange],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    area_label: str,
) -> str:
    if not changes:
        return "LOW"

    rank = max(_SEVERITY_RANK.get(c.severity, 0) for c in changes)

    deploy_level = (deployment_risk_assessment.risk_level or "low").lower() if deployment_risk_assessment else "low"
    if deploy_level == "critical":
        rank = min(3, rank + 1)
    elif deploy_level == "high" and rank < 3:
        rank = min(3, rank + 1)

    if _historical_area_boost(historical_learning, area_label) and rank < 3:
        rank += 1

    return _RISK_LEVELS[min(rank, 3)]


def _historical_area_boost(
    historical_learning: Optional[HistoricalLearningReport],
    area_label: str,
) -> bool:
    if not historical_learning:
        return False
    haystack = " ".join(
        [
            historical_learning.pattern_summary or "",
            *[inc.title for inc in historical_learning.similar_incidents],
            *[inc.summary for inc in historical_learning.similar_incidents],
        ]
    ).lower()
    area_key = _normalize_area(area_label)
    if not area_key:
        return False
    tokens = {area_key, *area_key.split()}
    if area_key.endswith("s") and len(area_key) > 3:
        tokens.add(area_key[:-1])
    return any(tok in haystack for tok in tokens if len(tok) > 3)


def _risk_summary(
    service_name: str,
    changes: List[ContractChange],
    risk_level: str,
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    area_label: str,
) -> str:
    parts: List[str] = []
    removed = [c for c in changes if c.change_type == "removed_field"]
    type_changes = [c for c in changes if c.change_type == "type_change"]
    renames = [c for c in changes if c.change_type == "rename"]

    if removed:
        fields = ", ".join(f"'{c.field_name}'" for c in removed)
        parts.append(f"{service_name} contract removed required field(s) {fields}")
    elif type_changes:
        field = type_changes[0].field_name
        parts.append(f"{service_name} contract changed type for field '{field}'")
    elif renames:
        pair = renames[0]
        parts.append(
            f"{service_name} contract renamed field '{pair.old_value}' to '{pair.new_value}'"
        )
    elif changes:
        parts.append(f"{service_name} contract has {len(changes)} low-severity addition(s)")

    if deployment_risk_assessment and deployment_risk_assessment.risk_level in ("high", "critical"):
        parts.append("deployment risk assessment indicates elevated rollout risk")

    if _historical_area_boost(historical_learning, area_label):
        parts.append("historical incidents indicate elevated deployment risk")

    if not parts:
        return f"{service_name} contract changes assessed at {risk_level} risk."

    summary = ". ".join(parts)
    if not summary.endswith("."):
        summary += "."
    return summary


def _confidence_from_signals(
    changes: List[ContractChange],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    pr_signals: bool,
) -> float:
    base = 0.55
    if changes:
        base += 0.15
    if deployment_risk_assessment:
        base += 0.1
    if pr_signals:
        base += 0.1
    if any(c.severity in ("HIGH", "CRITICAL") for c in changes):
        base += 0.05
    return round(min(base, 0.92), 2)


def _collect_categories(
    impact_map: List[IncidentImpactNode],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    hypotheses: List[IncidentHypothesis],
) -> List[str]:
    categories: List[str] = []
    seen: Set[str] = set()

    def add_area(name: str) -> None:
        cat = _area_category(name)
        if cat in ("checkout", "payments", "authentication") and cat not in seen:
            seen.add(cat)
            categories.append(cat)

    for node in impact_map:
        add_area(node.title)
    for pr in related_pr_analysis:
        for module in pr.impacted_modules:
            add_area(module)
    if test_recommendations:
        for rec in test_recommendations.recommendations:
            add_area(rec.test_name)
            add_area(rec.reason)
    for action in recommended_actions:
        add_area(action.title)
        add_area(action.description)
    for hyp in hypotheses:
        add_area(hyp.statement)

    return categories


def _pr_api_signals(related_pr_analysis: List[RelatedPRAnalysisSummary]) -> bool:
    keywords = ("openapi", "swagger", "api", "contract", "endpoint", "breaking")
    for pr in related_pr_analysis:
        blob = " ".join([*(pr.risk_signals or []), pr.reason or ""]).lower()
        if any(k in blob for k in keywords):
            return True
    return False


def _proposed_request_properties(
    category: str,
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    pr_api_signals: bool,
    hypotheses: List[IncidentHypothesis],
) -> Dict[str, str]:
    template = _contract_template(category)
    if not template:
        return {}
    proposed = dict(template.request_properties)

    deploy_level = (deployment_risk_assessment.risk_level or "low").lower() if deployment_risk_assessment else "low"
    hyp_text = " ".join(h.statement for h in hypotheses).lower()
    historical_boost = _historical_area_boost(historical_learning, category)

    if category == "payments":
        if deploy_level == "critical" or "type change" in hyp_text or "string amount" in hyp_text:
            proposed["amount"] = "string"
        elif deploy_level in ("high", "critical") or historical_boost:
            proposed.pop("amount", None)
            proposed["total_amount"] = "number"
        elif pr_api_signals:
            proposed["customer_name"] = "string"
        else:
            proposed["customer_name"] = "string"
    elif category == "checkout":
        if deploy_level in ("high", "critical"):
            proposed.pop("cart_id", None)
        elif pr_api_signals:
            proposed["promo_code"] = "string"
    elif category == "authentication":
        if deploy_level == "critical":
            proposed["username"] = "integer"
        elif deploy_level == "high":
            proposed.pop("password", None)
        elif pr_api_signals:
            proposed["mfa_token"] = "string"

    return proposed


def _contracts_from_stored_specs(
    stored_specs: List[Dict[str, str]],
) -> Tuple[List[ApiContract], List[ApiContractChangeAssessment]]:
    if not stored_specs:
        return [], []

    from services.openapi_parser_service import openapi_parser_service

    parsed_groups: List[Tuple[str, str, List[Any]]] = []
    for item in stored_specs:
        spec_text = (item.get("spec_text") or "").strip()
        source = item.get("source") or "stored_spec"
        version = item.get("version") or "v1"
        if not spec_text:
            continue
        endpoints = openapi_parser_service.parse(OpenAPISpecRequest(spec_text=spec_text))
        parsed_groups.append((source, version, endpoints))

    if not parsed_groups:
        return [], []

    contracts: List[ApiContract] = []
    assessments: List[ApiContractChangeAssessment] = []

    if len(parsed_groups) >= 2:
        (_, old_version, old_endpoints), (_, new_version, new_endpoints) = parsed_groups[0], parsed_groups[1]
        new_index = {(ep.path, ep.method.upper()): ep for ep in new_endpoints}
        for ep in old_endpoints:
            key = (ep.path, ep.method.upper())
            new_ep = new_index.get(key)
            if not new_ep:
                continue
            service_name = (ep.tags[0] if ep.tags else "API").strip() or "API"
            old_req = _schema_properties(ep.request_body_schema)
            new_req = _schema_properties(new_ep.request_body_schema)
            contract_id = build_contract_id(service_name, ep.method.upper(), ep.path, new_version)
            contracts.append(
                ApiContract(
                    contract_id=contract_id,
                    service_name=service_name,
                    endpoint=ep.path,
                    method=ep.method.upper(),
                    version=new_version,
                    request_schema=_object_schema(new_req),
                    response_schema=new_ep.response_schema or {},
                    source=f"{parsed_groups[1][0]}",
                    confidence=0.85,
                )
            )
            changes = detect_schema_changes(old_req, new_req, contract_id)
            if not changes:
                continue
            risk_level = calculate_contract_risk_level(changes, None, None, service_name)
            assessments.append(
                ApiContractChangeAssessment(
                    assessment_id=build_assessment_id(contract_id),
                    risk_level=risk_level,
                    confidence=0.85,
                    summary=_risk_summary(service_name, changes, risk_level, None, None, service_name),
                    changes=changes,
                )
            )
        return contracts, assessments

    source, version, endpoints = parsed_groups[0]
    for ep in endpoints[:5]:
        service_name = (ep.tags[0] if ep.tags else "API").strip() or "API"
        req_props = _schema_properties(ep.request_body_schema)
        contract_id = build_contract_id(service_name, ep.method.upper(), ep.path, version)
        contracts.append(
            ApiContract(
                contract_id=contract_id,
                service_name=service_name,
                endpoint=ep.path,
                method=ep.method.upper(),
                version=version,
                request_schema=_object_schema(req_props),
                response_schema=ep.response_schema or {},
                source=source,
                confidence=0.8,
            )
        )
    return contracts, assessments


def build_api_contract_intelligence(
    *,
    impact_map: List[IncidentImpactNode],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    deployment_risk_assessment: Optional[DeploymentRiskAssessment],
    historical_learning: Optional[HistoricalLearningReport],
    test_recommendations: Optional[TestRecommendationReport],
    recommended_actions: List[RecommendedAction],
    hypotheses: List[IncidentHypothesis],
    stored_specs: Optional[List[Dict[str, str]]] = None,
    explicit_contracts: Optional[List[Dict[str, Any]]] = None,
) -> Optional[ApiContractReport]:
    contracts: List[ApiContract] = []
    assessments: List[ApiContractChangeAssessment] = []
    pr_api_signals = _pr_api_signals(related_pr_analysis)

    if stored_specs:
        spec_contracts, spec_assessments = _contracts_from_stored_specs(stored_specs)
        contracts.extend(spec_contracts)
        assessments.extend(spec_assessments)

    if explicit_contracts:
        for item in explicit_contracts:
            service = item.get("service_name", "API")
            endpoint = item.get("endpoint", "/")
            method = item.get("method", "GET").upper()
            version = item.get("version", "v1")
            contract_id = build_contract_id(service, method, endpoint, version)
            old_props = item.get("old_properties") or _schema_properties(item.get("old_schema"))
            new_props = item.get("new_properties") or _schema_properties(item.get("new_schema"))
            contracts.append(
                ApiContract(
                    contract_id=contract_id,
                    service_name=service,
                    endpoint=endpoint,
                    method=method,
                    version=version,
                    request_schema=_object_schema(new_props),
                    response_schema=item.get("response_schema") or {},
                    source=item.get("source", "explicit_definition"),
                    confidence=float(item.get("confidence", 0.9)),
                )
            )
            changes = detect_schema_changes(old_props, new_props, contract_id)
            if changes:
                risk_level = calculate_contract_risk_level(
                    changes,
                    deployment_risk_assessment,
                    historical_learning,
                    service,
                )
                assessments.append(
                    ApiContractChangeAssessment(
                        assessment_id=build_assessment_id(contract_id),
                        risk_level=risk_level,
                        confidence=_confidence_from_signals(changes, deployment_risk_assessment, pr_api_signals),
                        summary=_risk_summary(
                            service,
                            changes,
                            risk_level,
                            deployment_risk_assessment,
                            historical_learning,
                            service,
                        ),
                        changes=changes,
                    )
                )

    categories = _collect_categories(
        impact_map=impact_map,
        related_pr_analysis=related_pr_analysis,
        test_recommendations=test_recommendations,
        recommended_actions=recommended_actions,
        hypotheses=hypotheses,
    )

    for category in categories:
        template = _contract_template(category)
        if not template:
            continue
        old_props = dict(template.request_properties)
        new_props = _proposed_request_properties(
            category=category,
            deployment_risk_assessment=deployment_risk_assessment,
            historical_learning=historical_learning,
            pr_api_signals=pr_api_signals,
            hypotheses=hypotheses,
        )
        contract_id = build_contract_id(
            template.service_name,
            template.method,
            template.endpoint,
            template.version,
        )
        if any(c.contract_id == contract_id for c in contracts):
            continue
        changes = detect_schema_changes(old_props, new_props, contract_id)
        if not changes:
            continue

        source_parts = ["impact_map"]
        if pr_api_signals:
            source_parts.append("pr_metadata")
        if deployment_risk_assessment:
            source_parts.append("deployment_risk")
        if historical_learning:
            source_parts.append("historical_learning")

        contracts.append(
            ApiContract(
                contract_id=contract_id,
                service_name=template.service_name,
                endpoint=template.endpoint,
                method=template.method,
                version=template.version,
                request_schema=_object_schema(new_props),
                response_schema=_object_schema(template.response_properties),
                source="+".join(source_parts),
                confidence=_confidence_from_signals(changes, deployment_risk_assessment, pr_api_signals),
            )
        )
        risk_level = calculate_contract_risk_level(
            changes,
            deployment_risk_assessment,
            historical_learning,
            template.service_name,
        )
        assessments.append(
            ApiContractChangeAssessment(
                assessment_id=build_assessment_id(contract_id),
                risk_level=risk_level,
                confidence=_confidence_from_signals(changes, deployment_risk_assessment, pr_api_signals),
                summary=_risk_summary(
                    template.service_name,
                    changes,
                    risk_level,
                    deployment_risk_assessment,
                    historical_learning,
                    category,
                ),
                changes=changes,
            )
        )

    if not contracts:
        return None

    contracts.sort(key=lambda c: (c.service_name, c.endpoint, c.method))
    assessments.sort(key=lambda a: a.assessment_id)

    high_risk = sum(1 for a in assessments if a.risk_level in ("HIGH", "CRITICAL"))
    if high_risk:
        summary = (
            f"{len(contracts)} API contract(s) modeled with {len(assessments)} change assessment(s); "
            f"{high_risk} elevated-risk contract change(s) detected."
        )
    else:
        summary = (
            f"{len(contracts)} API contract(s) modeled with {len(assessments)} change assessment(s)."
        )

    digest = hashlib.sha256(
        "|".join(c.contract_id for c in contracts).encode("utf-8")
    ).hexdigest()[:8]
    confidence = round(
        min(
            0.95,
            sum(c.confidence for c in contracts) / max(len(contracts), 1),
        ),
        2,
    )
    summary = f"{summary} Ref: {digest}."

    return ApiContractReport(
        contracts=contracts,
        risk_assessments=assessments,
        summary=summary,
        confidence=confidence,
    )


def contract_change_preview(
    old_properties: Dict[str, str],
    new_properties: Dict[str, str],
) -> Tuple[str, str]:
    """Return JSON snippets for read-only UI previews."""
    return _example_snippet(old_properties), _example_snippet(new_properties)
