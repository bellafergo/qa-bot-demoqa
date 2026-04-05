# api/routes/project_routes.py
"""REST API for catalog projects (multi-project scoping)."""
from __future__ import annotations

import logging
from typing import List

from fastapi import APIRouter, HTTPException, Response
from sqlalchemy.exc import IntegrityError

from models.project import Project, ProjectCreate, ProjectUpdate
from services.db.catalog_repository import catalog_repo
from services.db.project_repository import project_repo

logger = logging.getLogger("vanya.project_routes")

router = APIRouter(prefix="/projects", tags=["projects"])


def _norm_pid(project_id: str) -> str:
    return (project_id or "").strip().lower()


@router.get("", response_model=List[Project])
def list_projects():
    return project_repo.list_projects()


@router.post("", response_model=Project, status_code=201)
def create_project(body: ProjectCreate):
    try:
        return project_repo.create_project(body)
    except IntegrityError:
        logger.info("project create conflict: %s", body.id)
        raise HTTPException(
            status_code=409,
            detail=f"Project '{body.id}' already exists",
        ) from None


@router.get("/{project_id}", response_model=Project)
def get_project(project_id: str):
    p = project_repo.get_project(_norm_pid(project_id))
    if p is None:
        raise HTTPException(status_code=404, detail="Project not found")
    return p


@router.patch("/{project_id}", response_model=Project)
def patch_project(project_id: str, body: ProjectUpdate):
    pid = _norm_pid(project_id)
    data = body.model_dump(exclude_unset=True)
    if not data:
        p = project_repo.get_project(pid)
        if p is None:
            raise HTTPException(status_code=404, detail="Project not found")
        return p
    updated = project_repo.update_project(pid, data)
    if updated is None:
        raise HTTPException(status_code=404, detail="Project not found")
    return updated


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
