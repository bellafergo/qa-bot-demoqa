# models/onboarding_models.py
"""ENT-03A — Guided Onboarding (read-only guidance)."""
from __future__ import annotations

from typing import List

from pydantic import BaseModel, Field


class OnboardingStep(BaseModel):
    step_id: str
    title: str
    description: str = ""
    category: str
    status: str = "NOT_STARTED"
    priority: int = Field(default=50, ge=1, le=99)
    completion_percentage: int = Field(default=0, ge=0, le=100)
    recommended_next_action: str = ""


class OnboardingChecklist(BaseModel):
    project_id: str
    overall_completion: int = Field(default=0, ge=0, le=100)
    readiness_level: str = "NOT_READY"
    completed_steps: int = Field(default=0, ge=0)
    total_steps: int = Field(default=0, ge=0)
    next_recommended_step: str = ""
    steps: List[OnboardingStep] = Field(default_factory=list)
