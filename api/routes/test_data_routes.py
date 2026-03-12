# api/routes/test_data_routes.py
"""
Synthetic Test Data REST API
=============================

POST /test-data/generate          — generate synthetic entity data
GET  /test-data/entity-types      — list supported entity types
GET  /test-data/health            — liveness check
"""
from __future__ import annotations

import logging

from fastapi import APIRouter, HTTPException

from models.test_data_models import TestDataRequest, TestDataResponse
from services.test_data_service import test_data_service, SUPPORTED_ENTITY_TYPES

logger = logging.getLogger("vanya.test_data_routes")

router = APIRouter(prefix="/test-data", tags=["test-data"])


@router.get("/health")
def health():
    """Confirm the Test Data service is reachable."""
    return {"status": "ok", "service": "test-data"}


@router.get("/entity-types")
def entity_types():
    """Return the list of supported entity types."""
    return {"entity_types": SUPPORTED_ENTITY_TYPES}


@router.post("/generate", response_model=TestDataResponse)
def generate(req: TestDataRequest):
    """
    Generate synthetic test data for a given entity type.

    Supply a ``seed`` for deterministic / reproducible output.
    Use ``constraints`` to narrow generated values (e.g. fix email domain).

    Example request::

        {"entity_type": "user", "count": 3, "seed": 42,
         "constraints": {"email_domain": "example.com"}}
    """
    try:
        return test_data_service.generate(req)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        logger.exception("test_data: generate failed")
        raise HTTPException(status_code=500, detail=str(e))
