# models/project_initialize_models.py
"""Project initialization orchestration contracts."""
from __future__ import annotations

from typing import Any, Dict, List, Literal, Optional

from pydantic import BaseModel, Field

InitStepStatus = Literal["ok", "skipped", "failed", "partial"]


class ProjectInitStep(BaseModel):
    step: str
    status: InitStepStatus
    message: str = ""
    details: Dict[str, Any] = Field(default_factory=dict)


class ProjectInitializeRequest(BaseModel):
    run_smoke: bool = True
    refresh_knowledge: bool = True


class ProjectInitializeResponse(BaseModel):
    ok: bool
    project_id: str
    steps: List[ProjectInitStep] = Field(default_factory=list)
    catalog_tests: int = 0
    smoke_tests: int = 0
    critical_tests: int = 0
    job_id: Optional[str] = None
    knowledge_updated: bool = False
    total_runs: int = 0
    message: str = ""
