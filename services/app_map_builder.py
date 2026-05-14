# services/app_map_builder.py
"""
Single-page semantic app map (Phase 2) on top of browser inspection.

Pipeline: ``validate_target_url`` (via ``inspect_url_collect``) → Playwright inspection
→ ``dom_semantic_classifier.classify_semantic_map`` → ``AppMapResponse``.

TODO(convergence): Share more surface with ``/app-explorer/explore`` once both read the
same normalized inventory contract; keep HTML-parser explorer for backward compatibility.
"""
from __future__ import annotations

from models.app_map_models import AppMapResponse, InspectUrlMapRequest
from services.browser_inspection_persistence import (
    merge_persist_fields_into_app_map,
    persist_light_browser_inspection,
)
from services.browser_inspector_service import inspect_url_collect
from services.dom_semantic_classifier import classify_semantic_map


def build_app_map(req: InspectUrlMapRequest) -> AppMapResponse:
    """
    Build semantic app map for one URL (cloud execution only today).

    ``execution_mode`` must remain ``cloud``; other modes are rejected at validation.
    """
    from models.browser_inspection_models import InspectUrlRequest

    inner = InspectUrlRequest(
        url=req.url,
        project_id=req.project_id,
        timeout_ms=req.timeout_ms,
    )
    inspection, inv, extras = inspect_url_collect(inner)
    sem = classify_semantic_map(inspection, inv, extras)

    warnings = list(inspection.warnings)
    if req.local_agent_id or req.retention_policy:
        warnings.append("persist_options_ignored_until_storage_schema")

    persist_hints = {
        "source": "browser_inspection",
        "inspection_id": inspection.inspection_id,
        "project_id": req.project_id,
        "execution_mode": req.execution_mode,
        "local_agent_id": req.local_agent_id,
        "retention_policy": req.retention_policy,
        "sqlite_test_case_id": "_browser_inspection",
        "note": "Light rows via persist_run_payload (Phase 3A); GET /browser-inspections for history.",
    }

    resp = AppMapResponse(
        inspection_id=inspection.inspection_id,
        url=inspection.url,
        final_url=inspection.final_url,
        page_type=sem["page_type"],
        confidence=sem["confidence"],
        detected_patterns=sem["detected_patterns"],
        main_navigation=sem["main_navigation"],
        primary_actions=sem["primary_actions"],
        forms=sem["forms"],
        tables=sem["tables"],
        search_elements=sem["search_elements"],
        risk_notes=sem["risk_notes"],
        suggested_test_flows=sem["suggested_test_flows"],
        selector_candidates=inspection.selector_candidates,
        warnings=warnings,
        persist_hints=persist_hints,
    )

    rid, ok, pwarn = persist_light_browser_inspection(
        inspection,
        inv=inv,
        extras=extras,
        project_id=req.project_id,
        app_map=resp,
    )
    resp = merge_persist_fields_into_app_map(
        resp,
        persisted_run_id=rid if ok else None,
        persisted=ok,
        persistence_warning=pwarn,
    )
    if ok and rid:
        ph = dict(resp.persist_hints)
        ph["persisted_run_id"] = rid
        resp = resp.model_copy(update={"persist_hints": ph})
    return resp
