# models/platform_asset_models.py
"""Platform-owned database asset bootstrap (read-only, no credentials)."""
from __future__ import annotations

from typing import List, Optional

from pydantic import BaseModel, Field

from models.database_connector_models import DatabaseConnection


class PlatformAssetBootstrapRequest(BaseModel):
    project_id: str = Field(..., min_length=1, max_length=256)


class PlatformAssetBootstrapResponse(BaseModel):
    project_id: str
    agent_id: Optional[str] = None
    agent_already_exists: bool = False
    connections: List[DatabaseConnection] = Field(default_factory=list)
    probes_run: int = 0
    probes_succeeded: int = 0
    already_bootstrapped: bool = False
