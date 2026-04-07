# services/draft_approval_service.py
"""
Draft Approval Service for Vanya.

Closes the loop between the Draft Generator and the Test Catalog:
    suggest_tests → generate_test_drafts → approve_drafts → test catalog

Public API
----------
    draft_to_catalog_test(draft)          -> dict | None   [pure]
        Transform one draft dict into a catalog-ready dict.
        Returns None when required fields are missing.

    approve_drafts(drafts)                -> list[dict]    [pure]
        Process a batch of drafts, skip invalids, deduplicate by test_id.

    persist_approved_drafts(catalog_tests) -> dict          [I/O]
        Persist a list of catalog-ready dicts into the test catalog via
        TestCatalogService.  Returns {"saved": [...], "skipped": [...]}.
        Kept separate so the pure transformation layer stays testable without
        a running database.

Catalog-ready dict schema
-------------------------
    {
        "test_id":  str,          # == draft test_name
        "name":     str,          # == draft test_name
        "status":   "active",
        "priority": str,          # "low" | "medium" | "high" (passthrough)
        "type":     str,          # inferred from reason (see _map_type)
        "module":   "discovered",
        "source":   "draft_generator",
        "reason":   str,          # original draft reason
        "steps":    list[dict],   # Playwright-compatible steps from draft
        "meta": {
            "generated_from": "application_explorer",
            "draft": True,
        },
    }

Pure module — draft_to_catalog_test and approve_drafts have no I/O.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Set, Tuple

# ── Reason → catalog type mapping ─────────────────────────────────────────────
# TestCase.type must be one of: smoke | regression | functional | negative | e2e

_REASON_TO_TYPE: Dict[str, str] = {
    "form_detected":           "functional",
    "required_field_detected": "negative",
    "search_button_detected":  "functional",
    "links_detected":          "smoke",
}
_DEFAULT_TYPE = "smoke"

# Valid priority values accepted by TestCase.priority
_VALID_PRIORITIES = frozenset({"low", "medium", "high", "critical"})
_DEFAULT_PRIORITY = "medium"

_MAX_TEST_ID_LEN = 120


# ── Public API — pure ──────────────────────────────────────────────────────────

def draft_to_catalog_test(
    draft: Any,
    *,
    project_id: Optional[str] = None,
) -> Optional[Dict[str, Any]]:
    """
    Transform one draft dict into a catalog-ready dict.

    Returns None if the draft is missing 'test_name' or has empty 'steps'.
    Never raises.
    """
    if not isinstance(draft, dict):
        return None

    test_name = _s(draft.get("test_name"))
    if not test_name:
        return None

    steps = draft.get("steps")
    if not isinstance(steps, list) or not steps:
        return None

    reason   = _s(draft.get("reason"))
    priority = _s(draft.get("priority")).lower()
    if priority not in _VALID_PRIORITIES:
        priority = _DEFAULT_PRIORITY

    module = _s(draft.get("module")) or "discovered"
    pid = _s(project_id) or _s(draft.get("project_id")) or ""

    # Use an explicit ID if provided (backward/forward compatibility), otherwise
    # derive a scoped ID so drafts from different apps/projects don't collide.
    explicit_id = _s(draft.get("test_case_id")) or _s(draft.get("test_id"))
    test_id = explicit_id or _make_scoped_test_id(module, test_name, pid)

    return {
        "test_id":  test_id,
        "name":     test_name,
        "status":   "active",
        "priority": priority,
        "type":     _map_type(reason),
        "module":   module,
        "project_id": pid or "default",
        "source":   "draft_generator",
        "reason":   reason,
        "steps":    list(steps),
        "meta": {
            "generated_from": "application_explorer",
            "draft": True,
        },
    }


def approve_drafts(
    drafts: Any,
    *,
    project_id: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Convert a list of drafts into approved, catalog-ready dicts.

    - Invalid drafts are reported as skipped with code INVALID_DRAFT.
    - Duplicates within the batch are reported as skipped with code DUPLICATE_IN_BATCH.
    - Returns {"approved": [...], "skipped": [...]} for any input.
    """
    if not isinstance(drafts, list):
        return {"approved": [], "skipped": [{"code": "INVALID_REQUEST", "reason": "drafts must be a list"}]}

    seen:   Set[str]             = set()
    approved: List[Dict[str, Any]] = []
    skipped:  List[Dict[str, Any]] = []

    for draft in drafts:
        ct = draft_to_catalog_test(draft, project_id=project_id)
        if ct is None:
            skipped.append({
                "code": "INVALID_DRAFT",
                "test_name": _s(getattr(draft, "get", lambda *_: "")("test_name")) if isinstance(draft, dict) else "",
                "reason": "Missing test_name or steps",
            })
            continue
        tid = _s(ct.get("test_id"))
        if tid in seen:
            skipped.append({"code": "DUPLICATE_IN_BATCH", "test_id": tid, "reason": "Duplicate test_id in request batch"})
            continue
        seen.add(tid)
        approved.append(ct)

    return {"approved": approved, "skipped": skipped}


# ── Persistence wrapper — I/O ──────────────────────────────────────────────────

def persist_approved_drafts(
    catalog_tests: List[Dict[str, Any]],
) -> Dict[str, Any]:
    """
    Persist a list of catalog-ready dicts into the test catalog.

    Uses TestCatalogService.create_test_case() under the hood.
    Tests with a test_id that already exists in the catalog are silently
    skipped (no overwrite).

    Returns:
        {
            "saved":   [test_id, ...],   # successfully persisted
            "skipped": [{"test_id": ..., "code": ..., "reason": ...}, ...],
        }
    """
    from services.test_catalog_service import TestCatalogService
    from models.test_case import TestCaseCreate

    service = TestCatalogService()
    saved:   List[str]            = []
    skipped: List[Dict[str, Any]] = []

    for ct in (catalog_tests or []):
        if not isinstance(ct, dict):
            continue
        tid = _s(ct.get("test_id"))
        if not tid:
            continue
        try:
            payload = TestCaseCreate(
                test_case_id = tid,
                name         = _s(ct.get("name")) or tid,
                module       = _s(ct.get("module")) or "discovered",
                type         = ct.get("type") or _DEFAULT_TYPE,
                priority     = ct.get("priority") or _DEFAULT_PRIORITY,
                status       = "active",
                test_type    = "ui",
                project_id   = _s(ct.get("project_id")) or "default",
                steps        = list(ct.get("steps") or []),
                assertions   = [],
            )
            service.create_test_case(payload)
            saved.append(tid)
        except Exception as exc:
            msg = str(exc)
            code = "PERSIST_ERROR"
            if "already exists" in msg or "already exists." in msg:
                code = "DUPLICATE_IN_CATALOG"
            skipped.append({"test_id": tid, "code": code, "reason": msg})

    return {"saved": saved, "skipped": skipped}


# ── Helpers ────────────────────────────────────────────────────────────────────

def _map_type(reason: str) -> str:
    return _REASON_TO_TYPE.get(reason, _DEFAULT_TYPE)


def _s(v: Any) -> str:
    try:
        return str(v).strip() if v is not None else ""
    except Exception:
        return ""


def _slug(v: str) -> str:
    s = re.sub(r"[^a-z0-9]+", "_", (v or "").lower().strip())
    return s.strip("_")


def _make_scoped_test_id(module: str, test_name: str, project_id: str) -> str:
    """
    Avoid global collisions: drafts often have generic names like 'navigation_smoke'.
    Scope them by module (+ project_id if provided) to be persistable by default.
    """
    m = _slug(module) or "discovered"
    n = _slug(test_name) or "test"
    p = _slug(project_id) if project_id and project_id != "default" else ""
    base = "__".join([x for x in (p, m, n) if x])
    return base[:_MAX_TEST_ID_LEN] or n[:_MAX_TEST_ID_LEN]
