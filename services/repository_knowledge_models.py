# services/repository_knowledge_models.py
"""Internal dataclasses for repository knowledge indexing (Phase B)."""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, List


@dataclass
class ParsedPrismaModel:
    name: str
    fields: List[str] = field(default_factory=list)
    relations: List[str] = field(default_factory=list)


@dataclass
class ParsedApiEndpoint:
    url: str
    method: str
    file_path: str = ""


@dataclass
class ParsedUiRoute:
    url: str
    file_path: str = ""
    title: str = ""


@dataclass
class RepositoryIndexResult:
    routes: List[Any] = field(default_factory=list)
    apis: List[Any] = field(default_factory=list)
    modules: List[Any] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    skipped: bool = False
    warnings: List[str] = field(default_factory=list)
