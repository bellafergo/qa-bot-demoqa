"""Pack lightweight browser_inspection result for cloud ``result_ref`` (Phase 4C)."""
from __future__ import annotations

import hashlib
import json
from typing import Any, Dict, List, Optional

from models.browser_inspection_models import BrowserInspectionResult, SelectorCandidate

from models.local_agent_models import LOCAL_AGENT_MAX_RESULT_REF_LEN


def _trim_dict_list(items: List[Dict[str, Any]], *, max_items: int, max_keys: int = 12) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for it in (items or [])[:max_items]:
        if not isinstance(it, dict):
            continue
        keys = list(it.keys())[:max_keys]
        out.append({k: it[k] for k in keys})
    return out


def _trim_console_net(rows: List[Dict[str, Any]], *, max_items: int, text_max: int) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for r in (rows or [])[:max_items]:
        if not isinstance(r, dict):
            continue
        text = str(r.get("text") or r.get("failure") or "")[:text_max]
        item: Dict[str, Any] = {}
        if "text" in r:
            item["text"] = text
        if "location" in r and r.get("location"):
            item["location"] = str(r.get("location"))[:200]
        if "url" in r and r.get("url"):
            item["url"] = str(r.get("url"))[:400]
        if "failure" in r and r.get("failure"):
            item["failure"] = str(r.get("failure"))[:text_max]
        if item:
            out.append(item)
    return out


def _selectors_light(cands: List[SelectorCandidate]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for c in (cands or [])[:12]:
        out.append(
            {
                "kind": c.kind,
                "selector": (c.selector or "")[:200],
                "priority": c.priority,
            }
        )
    return out


def pack_browser_inspection_result_ref(
    result: BrowserInspectionResult,
    *,
    raw_runner: Optional[Dict[str, Any]] = None,
    max_len: int = LOCAL_AGENT_MAX_RESULT_REF_LEN,
) -> str:
    """
    JSON suitable for ``result_ref`` — no ``screenshot_b64``; bounded lists.
    """
    b64 = (raw_runner or {}).get("screenshot_b64") if raw_runner else None
    shot_present = bool(b64 and str(b64).strip())
    shot_hash: Optional[str] = None
    if shot_present:
        shot_hash = hashlib.sha256(str(b64).encode("utf-8")).hexdigest()[:24]

    payload: Dict[str, Any] = {
        "kind": "vanya_local_agent_browser_inspection",
        "v": 1,
        "inspection_id": result.inspection_id,
        "inspection_succeeded": result.inspection_succeeded,
        "url": (result.url or "")[:300],
        "final_url": (result.final_url or "")[:300],
        "title": (result.title or "")[:200],
        "status_code": result.status_code,
        "screenshot_present": shot_present,
        "screenshot_sha256_24": shot_hash,
        "inventory_counts": dict(result.inventory_counts or {}),
        "headings": _trim_dict_list([h for h in (result.headings or []) if isinstance(h, dict)], max_items=8),
        "links": _trim_dict_list([x for x in (result.links or []) if isinstance(x, dict)], max_items=8),
        "buttons": _trim_dict_list([x for x in (result.buttons or []) if isinstance(x, dict)], max_items=8),
        "inputs": _trim_dict_list([x for x in (result.inputs or []) if isinstance(x, dict)], max_items=8),
        "forms": _trim_dict_list([x for x in (result.forms or []) if isinstance(x, dict)], max_items=6),
        "images_without_alt": _trim_dict_list(
            [x for x in (result.images_without_alt or []) if isinstance(x, dict)],
            max_items=6,
        ),
        "selector_candidates": _selectors_light(list(result.selector_candidates or [])),
        "performance": {k: v for k, v in list((result.performance or {}).items())[:8]},
        "console_errors": _trim_console_net(
            [x for x in (result.console_errors or []) if isinstance(x, dict)],
            max_items=10,
            text_max=120,
        ),
        "network_errors": _trim_console_net(
            [x for x in (result.network_errors or []) if isinstance(x, dict)],
            max_items=10,
            text_max=120,
        ),
        "warnings": list((result.warnings or [])[:6]),
    }

    raw = json.dumps(payload, separators=(",", ":"), default=str)
    if len(raw) <= max_len:
        return raw
    payload.pop("selector_candidates", None)
    payload.pop("images_without_alt", None)
    payload["warnings"] = list((result.warnings or [])[:2])
    raw = json.dumps(payload, separators=(",", ":"), default=str)
    if len(raw) <= max_len:
        return raw
    return raw[:max_len]
