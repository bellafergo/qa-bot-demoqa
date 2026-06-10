# models/release_readiness_models.py
"""
REL-01A — Release Readiness intelligence compositor envelope.

Holds references to existing intelligence and connection models only.
No integration domain types or scoring logic live here.
"""
from __future__ import annotations

from typing import TYPE_CHECKING, Any, Dict, List, Optional

from pydantic import BaseModel, Field

from models.azure_devops_integration_models import AzureDevOpsConnectionStatus
from models.github_integration_models import GitHubConnectionStatus

if TYPE_CHECKING:
    from models.incident_models import (
        ContractRiskReport,
        DataJourneyReport,
        DecisionCenterSummary,
        DeploymentRiskAssessment,
        EarlyDegradationReport,
        EnterpriseDependencyMap,
        ExecutiveQualityReport,
        MultiEnvironmentReport,
        QualityHealthReport,
        QualityTrendReport,
    )


class ReleaseReadinessView(BaseModel):
    """Read-only compositor view over incident intelligence and connection state."""

    project_id: str
    generated_at: str
    source_incident_report_id: Optional[str] = None

    deployment_risk_assessment: Optional["DeploymentRiskAssessment"] = None
    contract_risk_assessment: Optional["ContractRiskReport"] = None
    data_journey_validation: Optional["DataJourneyReport"] = None
    enterprise_dependency_map: Optional["EnterpriseDependencyMap"] = None
    multi_environment: Optional["MultiEnvironmentReport"] = None
    quality_health: Optional["QualityHealthReport"] = None
    quality_trends: Optional["QualityTrendReport"] = None
    early_degradation: Optional["EarlyDegradationReport"] = None
    executive_quality_report: Optional["ExecutiveQualityReport"] = None
    decision_center: Optional["DecisionCenterSummary"] = None

    github: Optional[GitHubConnectionStatus] = None
    azure_devops: Optional[AzureDevOpsConnectionStatus] = None

    integration_readiness: Dict[str, Any] = Field(default_factory=dict)

    overall_status: str = "UNKNOWN"
    data_gaps: List[str] = Field(default_factory=list)
    summary: str = ""

    model_config = {"extra": "ignore"}


def _resolve_forward_refs() -> None:
    from models.incident_models import (
        ContractRiskReport,
        DataJourneyReport,
        DecisionCenterSummary,
        DeploymentRiskAssessment,
        EarlyDegradationReport,
        EnterpriseDependencyMap,
        ExecutiveQualityReport,
        MultiEnvironmentReport,
        QualityHealthReport,
        QualityTrendReport,
    )

    incident_types = {
        "DeploymentRiskAssessment": DeploymentRiskAssessment,
        "ContractRiskReport": ContractRiskReport,
        "DataJourneyReport": DataJourneyReport,
        "DecisionCenterSummary": DecisionCenterSummary,
        "EarlyDegradationReport": EarlyDegradationReport,
        "EnterpriseDependencyMap": EnterpriseDependencyMap,
        "ExecutiveQualityReport": ExecutiveQualityReport,
        "MultiEnvironmentReport": MultiEnvironmentReport,
        "QualityHealthReport": QualityHealthReport,
        "QualityTrendReport": QualityTrendReport,
    }
    ReleaseReadinessView.model_rebuild(_types_namespace=incident_types)


_resolve_forward_refs()
