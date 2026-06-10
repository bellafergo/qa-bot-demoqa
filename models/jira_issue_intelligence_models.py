# models/jira_issue_intelligence_models.py
"""Read-only Jira issue intelligence models (JIRA-01B)."""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel, Field


class JiraIssueCorrelation(BaseModel):
    issue_key: str
    issue_type: str
    status: str
    priority: Optional[str] = None
    summary: str
    correlation_score: int = Field(default=0, ge=0, le=100)
    correlation_reason: str = ""
    related_module: Optional[str] = None
    related_environment: Optional[str] = None
    is_blocker: bool = False


class JiraIssueIntelligenceReport(BaseModel):
    connected: bool = False
    total_issues: int = 0
    correlated_issues: int = 0
    blocker_count: int = 0
    high_priority_count: int = 0
    top_blockers: List[JiraIssueCorrelation] = Field(default_factory=list)
    issue_correlations: List[JiraIssueCorrelation] = Field(default_factory=list)
    summary: str = ""
    data_gaps: List[str] = Field(default_factory=list)
