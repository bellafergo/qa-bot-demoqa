# models/incident_models.py
"""Autonomous Incident Investigator — request/response contracts (MVP)."""
from __future__ import annotations

from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

IncidentStatus = Literal["running", "completed", "failed"]
IncidentSeverity = Literal["critical", "high", "medium", "low", "info"]
IncidentReproduced = Literal["true", "false", "unknown"]
SuspectedArea = Literal["frontend", "backend", "network", "auth", "data", "unknown"]


class InvestigateIncidentRequest(BaseModel):
    incident_description: str = Field(..., min_length=3, max_length=4000)
    target_url: Optional[str] = Field(default=None, max_length=2048)
    project_id: Optional[str] = Field(default=None, max_length=128)
    module: Optional[str] = Field(default=None, max_length=256)
    max_steps: int = Field(default=5, ge=1, le=20)
    credentials_mode: str = Field(default="none", max_length=32)
    allow_destructive_actions: bool = False
    timeout_ms: int = Field(default=30_000, ge=5_000, le=120_000)

    model_config = {"extra": "ignore"}


class IncidentInvestigationRun(BaseModel):
    id: str
    created_at: str
    updated_at: str
    status: IncidentStatus
    incident_description: str
    target_url: Optional[str] = None
    project_id: Optional[str] = None
    module: Optional[str] = None
    severity: IncidentSeverity = "info"
    reproduced: IncidentReproduced = "unknown"
    suspected_area: SuspectedArea = "unknown"
    suspected_endpoint: Optional[str] = None
    symptom_observed: str = ""
    probable_cause: str = ""
    console_errors: List[Dict[str, Any]] = Field(default_factory=list)
    network_errors: List[Dict[str, Any]] = Field(default_factory=list)
    http_errors: List[Dict[str, Any]] = Field(default_factory=list)
    screenshot_url: Optional[str] = None
    screenshot_b64: Optional[str] = None
    steps_executed: List[str] = Field(default_factory=list)
    diagnosis_summary: str = ""
    recommendations: List[str] = Field(default_factory=list)
    reproduction_steps: List[str] = Field(default_factory=list)
    raw_evidence: Dict[str, Any] = Field(default_factory=dict)
    meta: Dict[str, Any] = Field(default_factory=dict)
    error_message: Optional[str] = None

    model_config = {"extra": "ignore"}


class IncidentInvestigationListResponse(BaseModel):
    items: List[IncidentInvestigationRun] = Field(default_factory=list)
    total: int = 0


# ── Project-scoped QA Intelligence investigation (deterministic, no LLM) ───────

IncidentReportSeverity = Literal["low", "medium", "high", "critical"]
EvidenceKind = Literal["run", "evidence", "failure_cluster", "knowledge", "pr", "browser"]


class ProjectInvestigateIncidentRequest(BaseModel):
    description: str = Field(..., min_length=3, max_length=4000)
    severity: IncidentReportSeverity = "medium"
    time_window_hours: int = Field(default=72, ge=1, le=720)
    target_url: Optional[str] = Field(default=None, max_length=2048)
    module: Optional[str] = Field(default=None, max_length=256)
    include_browser_probe: bool = False

    model_config = {"extra": "ignore"}


class IncidentHypothesis(BaseModel):
    id: str = ""
    rank: int = 0
    statement: str
    confidence: float = Field(ge=0.0, le=1.0)
    basis: Literal["evidence", "inference", "assumption"] = "inference"
    supporting_refs: List[str] = Field(default_factory=list)


class IncidentEvidenceItem(BaseModel):
    kind: Literal["evidence", "inference", "assumption"]
    label: str
    detail: str
    ref: str = ""


class IncidentEvidenceStrength(BaseModel):
    evidence: List[IncidentEvidenceItem] = Field(default_factory=list)
    inference: List[IncidentEvidenceItem] = Field(default_factory=list)
    assumptions: List[IncidentEvidenceItem] = Field(default_factory=list)


class BlastRadiusModule(BaseModel):
    module: str
    score: float = Field(ge=0.0, le=100.0)
    reason: str = ""


RecommendationStrength = Literal["high", "medium", "low"]
TemporalCorrelationLevel = Literal["strong", "medium", "weak", "none"]


class RecommendedTestRecommendation(BaseModel):
    test_case_id: str
    name: str = ""
    module: str = ""
    reason: str = ""
    recommendation_strength: RecommendationStrength = "medium"


class TemporalCorrelationSummary(BaseModel):
    signal: TemporalCorrelationLevel = "none"
    reason: str = ""
    event_chain: List[str] = Field(default_factory=list)


class CorrelatedEvidence(BaseModel):
    source: str
    confidence: float = Field(ge=0.0, le=1.0)
    title: str
    detail: str
    timestamp: Optional[str] = None
    related_run_id: Optional[str] = None
    reason: str = ""
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class EvidenceCorrelationSummary(BaseModel):
    total_correlations: int = 0
    strongest_source: Optional[str] = None
    evidence: List[CorrelatedEvidence] = Field(default_factory=list)


class InvestigationPlanItem(BaseModel):
    title: str
    reason: str
    priority: int = Field(default=0, ge=0, le=100)
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class IncidentStorylineStep(BaseModel):
    step_number: int = Field(ge=1)
    title: str
    description: str
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None
    timestamp: Optional[str] = None


class IncidentImpactNode(BaseModel):
    title: str
    description: str
    severity: Literal["high", "medium", "low"] = "low"
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    related_entity_count: int = Field(default=0, ge=0)
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class RiskFactor(BaseModel):
    title: str
    description: str
    weight: float = Field(default=0.0, ge=0.0, le=1.0)
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class DeploymentRiskAssessment(BaseModel):
    risk_score: int = Field(default=0, ge=0, le=100)
    risk_level: Literal["low", "medium", "high", "critical"] = "low"
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    summary: str = ""
    contributing_factors: List[RiskFactor] = Field(default_factory=list)


class RecommendedTest(BaseModel):
    recommendation_id: str
    test_name: str
    test_type: str = "smoke"
    priority: int = Field(default=99, ge=1)
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    reason: str = ""
    estimated_risk_reduction: float = Field(default=0.0, ge=0.0, le=1.0)
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None
    requires_user_approval: bool = True


class TestRecommendationReport(BaseModel):
    recommendations: List[RecommendedTest] = Field(default_factory=list)
    summary: str = ""
    recommendation_confidence: float = Field(default=0.0, ge=0.0, le=1.0)


class DecisionCenterInsight(BaseModel):
    title: str
    description: str
    priority: int = Field(default=99, ge=1)
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class DecisionCenterSummary(BaseModel):
    overall_status: Literal["GREEN", "YELLOW", "ORANGE", "RED"] = "GREEN"
    executive_summary: str = ""
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    top_risk_level: str = "LOW"
    top_risk_score: int = Field(default=0, ge=0, le=100)
    top_hypothesis: Optional[str] = None
    top_impacted_area: Optional[str] = None
    recommended_test_count: int = Field(default=0, ge=0)
    recommended_action_count: int = Field(default=0, ge=0)
    key_takeaways: List[DecisionCenterInsight] = Field(default_factory=list)


class SimilarIncident(BaseModel):
    incident_id: str
    title: str
    similarity_score: float = Field(ge=0.0, le=1.0)
    summary: str = ""
    occurrence_timestamp: Optional[str] = None
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class HistoricalLearningReport(BaseModel):
    similar_incidents: List[SimilarIncident] = Field(default_factory=list)
    pattern_summary: str = ""
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)


ApprovalStatus = Literal["PENDING", "APPROVED", "REJECTED"]


class ApprovalRequest(BaseModel):
    approval_id: str
    approval_type: str
    title: str
    description: str = ""
    status: ApprovalStatus = "PENDING"
    created_at: str = ""
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class ApprovalWorkflowSummary(BaseModel):
    pending_count: int = Field(default=0, ge=0)
    approved_count: int = Field(default=0, ge=0)
    rejected_count: int = Field(default=0, ge=0)
    requests: List[ApprovalRequest] = Field(default_factory=list)


class DatabaseValidationCheck(BaseModel):
    check_id: str
    name: str
    description: str = ""
    query: str
    database_type: str = "postgresql"
    expected_result_type: str = "row_exists"
    expected_value: Optional[str] = None
    enabled: bool = True
    requires_user_approval: bool = True


class DatabaseValidationResult(BaseModel):
    check_id: str
    status: str = "PLANNED"
    summary: str = ""
    observed_value: Optional[str] = None
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    executed_at: Optional[str] = None
    read_only: bool = True


class DatabaseValidationReport(BaseModel):
    checks: List[DatabaseValidationCheck] = Field(default_factory=list)
    results: List[DatabaseValidationResult] = Field(default_factory=list)
    summary: str = ""
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)


class RecommendedAction(BaseModel):
    action_id: str
    title: str
    description: str
    reason: str
    priority: int = Field(default=50, ge=1, le=99)
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    action_type: str
    requires_user_approval: bool = True
    related_entity_type: Optional[str] = None
    related_entity_id: Optional[str] = None


class IncidentActionAvailable(BaseModel):
    action: str
    label: str
    requires_user_approval: bool = True
    reason: str = ""


class RelatedRunSummary(BaseModel):
    run_id: str
    test_id: str = ""
    test_name: str = ""
    status: str = ""
    started_at: Optional[str] = None
    error_summary: Optional[str] = None
    rca_summary: Optional[str] = None
    module: str = ""
    evidence_url: Optional[str] = None


class RelatedEvidenceSummary(BaseModel):
    run_id: str
    test_name: str = ""
    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    screenshot_url: Optional[str] = None
    status: str = ""
    started_at: Optional[str] = None


class RelatedPRSummary(BaseModel):
    provider: Literal["github", "azure_devops"] = "github"
    pr_id: str
    title: str = ""
    branch: str = ""
    author: str = ""
    html_url: str = ""
    updated_at: str = ""
    match_reason: str = ""


class ProjectIncidentInvestigationReport(BaseModel):
    id: Optional[str] = None
    created_at: Optional[str] = None
    project_id: str
    description: str
    severity: IncidentReportSeverity = "medium"
    time_window_hours: int = 72
    summary: str = ""
    hypotheses: List[IncidentHypothesis] = Field(default_factory=list)
    primary_hypothesis_id: Optional[str] = None
    related_runs: List[RelatedRunSummary] = Field(default_factory=list)
    related_evidence: List[RelatedEvidenceSummary] = Field(default_factory=list)
    related_prs: List[RelatedPRSummary] = Field(default_factory=list)
    related_pr_analysis: List["RelatedPRAnalysisSummary"] = Field(default_factory=list)
    timeline: List["IncidentTimelineEvent"] = Field(default_factory=list)
    temporal_correlation: Optional[TemporalCorrelationSummary] = None
    impacted_modules: List[str] = Field(default_factory=list)
    impacted_modules_ranked: List[BlastRadiusModule] = Field(default_factory=list)
    recommended_tests: List[str] = Field(default_factory=list)
    recommended_tests_v2: List[RecommendedTestRecommendation] = Field(default_factory=list)
    evidence_strength: Optional[IncidentEvidenceStrength] = None
    evidence_correlation: Optional[EvidenceCorrelationSummary] = None
    investigation_plan: List[InvestigationPlanItem] = Field(default_factory=list)
    storyline: List[IncidentStorylineStep] = Field(default_factory=list)
    impact_map: List[IncidentImpactNode] = Field(default_factory=list)
    recommended_actions: List[RecommendedAction] = Field(default_factory=list)
    deployment_risk_assessment: Optional[DeploymentRiskAssessment] = None
    test_recommendations: Optional[TestRecommendationReport] = None
    decision_center: Optional[DecisionCenterSummary] = None
    historical_learning: Optional[HistoricalLearningReport] = None
    approval_workflow: Optional[ApprovalWorkflowSummary] = None
    database_validation: Optional[DatabaseValidationReport] = None
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    confidence_breakdown: List["ConfidenceFactor"] = Field(default_factory=list)
    next_steps: List[str] = Field(default_factory=list)
    evidence_found: List[str] = Field(default_factory=list)
    data_gaps: List[str] = Field(default_factory=list)
    browser_investigation: Optional[IncidentInvestigationRun] = None
    actions_available: List[IncidentActionAvailable] = Field(default_factory=list)
    meta: Dict[str, Any] = Field(default_factory=dict)

    model_config = {"extra": "ignore"}


class RelatedPRAnalysisSummary(BaseModel):
    pr_number: str
    provider: str = "manual"
    pr_risk_score: float = Field(default=0.0, ge=0.0, le=100.0)
    risk_level: str = "LOW"
    impacted_modules: List[str] = Field(default_factory=list)
    recommended_tests: List[str] = Field(default_factory=list)
    risk_signals: List[str] = Field(default_factory=list)
    analyzed_at: Optional[str] = None
    reason: str = ""


class IncidentTimelineEvent(BaseModel):
    timestamp: str
    event_type: Literal[
        "run_failed",
        "pr_analyzed",
        "browser_watch_alert",
        "incident_reported",
        "failure_cluster",
    ]
    title: str
    details: str = ""
    source: str = ""
    time_distance_minutes: Optional[int] = None
    relative_to_previous: Optional[str] = None


class ConfidenceFactor(BaseModel):
    label: str
    delta: float
    reason: str


class IncidentInvestigationReportRecord(BaseModel):
    id: str
    project_id: str
    description: str
    severity: IncidentReportSeverity = "medium"
    summary: str = ""
    confidence: float = Field(default=0.0, ge=0.0, le=1.0)
    created_at: str


class ProjectIncidentInvestigationListResponse(BaseModel):
    items: List[IncidentInvestigationReportRecord] = Field(default_factory=list)
    total: int = 0
