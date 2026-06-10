# api/routes/project_routes.py
"""REST API for catalog projects (multi-project scoping)."""
from __future__ import annotations

import logging
from typing import List

from fastapi import APIRouter, HTTPException, Response
from sqlalchemy.exc import IntegrityError

from core.json_api import json_error_from_exception
from models.project import ProjectCreate, ProjectPublic, ProjectUpdate
from models.onboarding_models import OnboardingChecklist
from models.project_initialize_models import ProjectInitializeRequest, ProjectInitializeResponse
from services.db.catalog_repository import catalog_repo
from services.db.project_errors import ProjectDuplicateIdError
from services.db.project_repository import project_repo
from services.project_settings_service import mask_settings_for_api, merge_settings

logger = logging.getLogger("vanya.project_routes")

router = APIRouter(prefix="/projects", tags=["projects"])


def _norm_pid(project_id: str) -> str:
    return (project_id or "").strip().lower()


def _to_public(p) -> ProjectPublic:
    return ProjectPublic(
        id=p.id,
        name=p.name,
        description=p.description or "",
        color=p.color or "#6366f1",
        base_url=p.base_url,
        settings=mask_settings_for_api(getattr(p, "settings", None) or {}),
        created_at=p.created_at,
        updated_at=p.updated_at,
    )


@router.get("", response_model=List[ProjectPublic])
def list_projects():
    return [_to_public(p) for p in project_repo.list_projects()]


@router.post("", response_model=ProjectPublic, status_code=201)
def create_project(body: ProjectCreate):
    try:
        p = project_repo.create_project(body)
        return _to_public(p)
    except (IntegrityError, ProjectDuplicateIdError):
        logger.info("project create conflict: %s", body.id)
        raise HTTPException(
            status_code=409,
            detail=f"Project '{body.id}' already exists",
        ) from None


@router.get("/{project_id}", response_model=ProjectPublic)
def get_project(project_id: str):
    p = project_repo.get_project(_norm_pid(project_id))
    if p is None:
        raise HTTPException(status_code=404, detail="Project not found")
    return _to_public(p)


@router.patch("/{project_id}", response_model=ProjectPublic)
def patch_project(project_id: str, body: ProjectUpdate):
    pid = _norm_pid(project_id)
    data = body.model_dump(exclude_unset=True)
    if not data:
        p = project_repo.get_project(pid)
        if p is None:
            raise HTTPException(status_code=404, detail="Project not found")
        return _to_public(p)
    if "settings" in data:
        existing = project_repo.get_project(pid)
        if existing is None:
            raise HTTPException(status_code=404, detail="Project not found")
        data["settings"] = merge_settings(existing.settings, data["settings"])
        merged = data["settings"] if isinstance(data["settings"], dict) else {}
        vars_m = merged.get("variables") if isinstance(merged.get("variables"), dict) else {}
        logger.info(
            "project PATCH id=%s settings_keys=%s variables_keys=%s has_EMAIL=%s has_PASSWORD=%s",
            pid,
            sorted(merged.keys()),
            sorted(vars_m.keys()),
            "EMAIL" in vars_m,
            "PASSWORD" in vars_m,
        )
    updated = project_repo.update_project(pid, data)
    if updated is None:
        raise HTTPException(status_code=404, detail="Project not found")
    return _to_public(updated)


@router.get("/{project_id}/onboarding", response_model=OnboardingChecklist)
def get_project_onboarding(project_id: str):
    """Read-only guided onboarding checklist for a project (ENT-03A)."""
    from services.onboarding_service import build_onboarding_checklist

    pid = _norm_pid(project_id)
    try:
        return build_onboarding_checklist(pid)
    except LookupError as e:
        raise HTTPException(status_code=404, detail=str(e)) from None
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from None
    except Exception as exc:
        return json_error_from_exception(
            500,
            "Project onboarding checklist failed",
            exc,
            logger=logger,
            context={"endpoint": f"/projects/{pid}/onboarding"},
        )


@router.post("/{project_id}/initialize", response_model=ProjectInitializeResponse)
def initialize_project(project_id: str, body: ProjectInitializeRequest | None = None):
    """
    Prepare a project for enterprise use: rebuild system memory, inventory catalog,
    optionally queue smoke/critical runs. Returns per-step status (partial OK).
    """
    from services.project_initialize_service import initialize_project as run_init

    pid = _norm_pid(project_id)
    try:
        return run_init(pid, body)
    except ValueError as e:
        raise HTTPException(status_code=404 if "not found" in str(e).lower() else 400, detail=str(e)) from None
    except Exception as exc:
        return json_error_from_exception(
            500,
            "Project initialize failed",
            exc,
            logger=logger,
            context={"endpoint": f"/projects/{pid}/initialize"},
        )


@router.delete("/{project_id}", status_code=204)
def delete_project(project_id: str):
    pid = _norm_pid(project_id)
    if project_repo.get_project(pid) is None:
        raise HTTPException(status_code=404, detail="Project not found")
    n = catalog_repo.count_tests_for_project(pid)
    if n > 0:
        raise HTTPException(
            status_code=409,
            detail=f"Cannot delete project '{pid}': {n} test case(s) still reference it",
        )
    project_repo.delete_project(pid)
    return Response(status_code=204)
