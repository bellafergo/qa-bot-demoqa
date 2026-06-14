# models/project_knowledge_models.py
"""System Memory / App Knowledge Graph — contracts (Phase 1 MVP)."""
from __future__ import annotations

from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

from models.risk_engine_models import ModuleRisk, RecommendedTest, RiskLevel


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


KnowledgeSource = Literal[
    "explorer",
    "app_map",
    "catalog",
    "runs",
    "failure_intelligence",
    "incidents",
    "refresh",
    "manual",
    "repository",
]


class KnowledgeRoute(BaseModel):
    url: str
    title: str = ""
    page_types: List[str] = Field(default_factory=list)
    source: str = "unknown"
    last_seen_at: str = ""
    module: str = ""
    file_path: str = ""


class KnowledgeApi(BaseModel):
    url: str
    method: str = "GET"
    source: str = "unknown"
    last_seen_at: str = ""
    module: str = ""
    file_path: str = ""


class KnowledgeForm(BaseModel):
    name: str = ""
    url: str = ""
    fields: List[str] = Field(default_factory=list)
    source: str = "unknown"
    last_seen_at: str = ""


class KnowledgeTable(BaseModel):
    name: str = ""
    url: str = ""
    columns: List[str] = Field(default_factory=list)
    source: str = "unknown"
    last_seen_at: str = ""


class KnowledgeWorkflow(BaseModel):
    name: str
    steps: List[str] = Field(default_factory=list)
    source: str = "unknown"
    last_seen_at: str = ""


class KnowledgeModule(BaseModel):
    name: str
    test_count: int = 0
    routes: List[str] = Field(default_factory=list)
    apis: List[str] = Field(default_factory=list)
    source: str = "catalog"
    last_seen_at: str = ""
    entity_type: str = ""
    fields: List[str] = Field(default_factory=list)
    relations: List[str] = Field(default_factory=list)
    file_path: str = ""


class KnowledgeRelatedTest(BaseModel):
    test_case_id: str
    name: str = ""
    module: str = ""
    test_type: str = "ui"
    priority: str = ""
    last_run_status: str = ""
    last_run_at: str = ""


class KnowledgeFailureEntry(BaseModel):
    test_case_id: str = ""
    test_name: str = ""
    module: str = ""
    failure_type: str = ""
    last_failed_at: str = ""
    count: int = 1


class KnowledgeIncidentEntry(BaseModel):
    id: str
    description: str = ""
    severity: str = "info"
    suspected_area: str = "unknown"
    target_url: str = ""
    created_at: str = ""


class ProjectKnowledge(BaseModel):
    project_id: str
    project_name: str = ""
    modules: List[KnowledgeModule] = Field(default_factory=list)
    routes: List[KnowledgeRoute] = Field(default_factory=list)
    apis: List[KnowledgeApi] = Field(default_factory=list)
    components: List[Dict[str, Any]] = Field(default_factory=list)
    forms: List[KnowledgeForm] = Field(default_factory=list)
    tables: List[KnowledgeTable] = Field(default_factory=list)
    workflows: List[KnowledgeWorkflow] = Field(default_factory=list)
    related_tests: List[KnowledgeRelatedTest] = Field(default_factory=list)
    failure_history: List[KnowledgeFailureEntry] = Field(default_factory=list)
    incident_history: List[KnowledgeIncidentEntry] = Field(default_factory=list)
    risk_score: float = Field(default=0.0, ge=0.0, le=100.0)
    risk_level: RiskLevel = "LOW"
    risk_explanation: List[str] = Field(default_factory=list)
    module_risks: List[ModuleRisk] = Field(default_factory=list)
    recommended_tests: List[RecommendedTest] = Field(default_factory=list)
    metadata: Dict[str, Any] = Field(default_factory=dict)
    updated_at: str = Field(default_factory=_utc_now_iso)

    model_config = {"extra": "ignore"}


KnowledgeRefreshMode = Literal["replace", "merge"]


class ProjectKnowledgeRefreshRequest(BaseModel):
    include_catalog: bool = True
    include_runs: bool = True
    include_failures: bool = True
    include_incidents: bool = True
    include_discovery: bool = True
    include_repository: bool = False
    mode: KnowledgeRefreshMode = "replace"


class ProjectKnowledgeContext(BaseModel):
    """Lightweight slice for Incident Investigator."""
    project_id: str
    modules: List[str] = Field(default_factory=list)
    known_routes: List[str] = Field(default_factory=list)
    related_tests: List[str] = Field(default_factory=list)
    recent_failures: List[str] = Field(default_factory=list)
    recent_incidents: List[str] = Field(default_factory=list)
    risk_score: float = 0.0
    risk_level: RiskLevel = "LOW"
    module_risk_hints: List[str] = Field(default_factory=list)
    hints: List[str] = Field(default_factory=list)
