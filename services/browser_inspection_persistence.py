# services/browser_inspection_persistence.py
"""
Lightweight persistence for browser inspection + app map (Phase 3A).

Uses ``run_access.persist_run_payload`` → ``run_store.save_run`` (SQLite bridge + optional
Supabase). Payloads are **strictly bounded** — no HTML/DOM, no screenshot_b64, no full
inventory arrays in the persisted row.
"""
from __future__ import annotations

import json
import logging
import os
from typing import Any, Dict, List, Optional, Tuple

from models.app_map_models import AppMapResponse
from models.browser_inspection_models import BrowserInspectionResult

logger = logging.getLogger("vanya.browser_inspection_persist")

_MAX_CONSOLE = 15
_MAX_NET = 15
_MAX_TEXT = 400
_MAX_WARNINGS_STORE = 30
_MAX_PATTERNS = 40
_MAX_FLOWS = 20
_MAX_RISKS = 20
_MAX_META_BYTES = 48_000


def browser_inspection_persist_enabled() -> bool:
    raw = (os.getenv("VANYA_PERSIST_BROWSER_INSPECTION") or "1").strip().lower()
    return raw not in ("0", "false", "no", "off")


def _clip(s: str, n: int) -> str:
    t = (s or "").strip()
    return t if len(t) <= n else t[: n - 1] + "…"


def _summarize_console(rows: List[Any]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for r in rows or []:
        if not isinstance(r, dict):
            continue
        item = {
            "text": _clip(str(r.get("text") or ""), _MAX_TEXT) or None,
            "location": _clip(str(r.get("location") or ""), _MAX_TEXT) or None,
        }
        out.append({k: v for k, v in item.items() if v})
        if len(out) >= _MAX_CONSOLE:
            break
    return out


def _summarize_network(rows: List[Any]) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for r in rows or []:
        if not isinstance(r, dict):
            continue
        item = {
            "url": _clip(str(r.get("url") or ""), _MAX_TEXT) or None,
            "failure": _clip(str(r.get("failure") or r.get("error") or ""), _MAX_TEXT) or None,
        }
        out.append({k: v for k, v in item.items() if v})
        if len(out) >= _MAX_NET:
            break
    return out


def _nav_summary_item(item: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "text": _clip(str(item.get("text") or ""), 200) or None,
        "href": _clip(str(item.get("href") or ""), 400) or None,
    }


def _action_summary_item(item: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "text": _clip(str(item.get("text") or item.get("label") or ""), 200) or None,
        "kind": _clip(str(item.get("kind") or ""), 80) or None,
    }


def build_app_map_summary(m: AppMapResponse) -> Dict[str, Any]:
    nav = m.main_navigation[:5] if isinstance(m.main_navigation, list) else []
    act = m.primary_actions[:8] if isinstance(m.primary_actions, list) else []
    return {
        "page_type": list(m.page_type)[:20],
        "confidence": m.confidence,
        "detected_patterns": [_clip(str(x), 200) for x in (m.detected_patterns or [])][:_MAX_PATTERNS],
        "main_navigation_count": len(m.main_navigation or []),
        "main_navigation_summary": [_nav_summary_item(x) for x in nav if isinstance(x, dict)],
        "primary_actions_summary": [_action_summary_item(x) for x in act if isinstance(x, dict)],
        "suggested_test_flows": [_clip(str(x), 300) for x in (m.suggested_test_flows or [])][:_MAX_FLOWS],
        "risk_notes": [_clip(str(x), 400) for x in (m.risk_notes or [])][:_MAX_RISKS],
    }


def build_browser_inspection_summary(
    result: BrowserInspectionResult,
    *,
    inv: Optional[Dict[str, Any]] = None,
    extras: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    inv = inv if isinstance(inv, dict) else {}
    extras = extras if isinstance(extras, dict) else {}
    counts = dict(result.inventory_counts or {})
    if not counts:
        counts = {
            "headings_count": len(inv.get("headings") or []),
            "links_count": len(inv.get("links") or []),
            "buttons_count": len(inv.get("buttons") or []),
            "inputs_count": len(inv.get("inputs") or []),
            "forms_count": len(extras.get("forms") or []),
            "images_without_alt_count": len(extras.get("images_without_alt") or []),
            "selector_candidates_count": len(result.selector_candidates or []),
        }
    return {
        "inspection_id": result.inspection_id,
        "url": _clip(result.url, 2048),
        "final_url": _clip(result.final_url, 2048),
        "title": _clip(result.title, 500),
        "status_code": result.status_code,
        "screenshot_url": result.screenshot_url,
        "console_errors": _summarize_console(list(result.console_errors or [])),
        "network_errors": _summarize_network(list(result.network_errors or [])),
        "counts": counts,
        "warnings": [_clip(str(w), 500) for w in (result.warnings or [])][:_MAX_WARNINGS_STORE],
        "inspection_succeeded": bool(result.inspection_succeeded),
    }


def _meta_json_size(meta: Dict[str, Any]) -> int:
    try:
        return len(json.dumps(meta, ensure_ascii=False, default=str))
    except Exception:
        return len(str(meta))


def _shrink_meta(meta: Dict[str, Any]) -> Dict[str, Any]:
    """Ensure serialized meta stays under ``_MAX_META_BYTES`` (best-effort)."""
    if _meta_json_size(meta) <= _MAX_META_BYTES:
        return meta
    m = dict(meta)
    bis = m.get("browser_inspection_summary")
    if isinstance(bis, dict):
        ce = bis.get("console_errors")
        if isinstance(ce, list) and len(ce) > 5:
            bis = dict(bis)
            bis["console_errors"] = ce[:5]
        ne = bis.get("network_errors")
        if isinstance(ne, list) and len(ne) > 5:
            bis = dict(bis)
            bis["network_errors"] = ne[:5]
        m["browser_inspection_summary"] = bis
    ams = m.get("app_map_summary")
    if isinstance(ams, dict):
        ams = dict(ams)
        for k in ("detected_patterns", "suggested_test_flows", "risk_notes"):
            v = ams.get(k)
            if isinstance(v, list) and len(v) > 8:
                ams[k] = v[:8]
        m["app_map_summary"] = ams
    if _meta_json_size(m) > _MAX_META_BYTES:
        bis = m.get("browser_inspection_summary")
        bid = bis.get("inspection_id") if isinstance(bis, dict) else None
        m["browser_inspection_summary"] = {"inspection_id": bid, "truncated": True}
        m["app_map_summary"] = None
    return m


def build_lightweight_run_payload(
    result: BrowserInspectionResult,
    *,
    inv: Optional[Dict[str, Any]] = None,
    extras: Optional[Dict[str, Any]] = None,
    project_id: Optional[str],
    app_map: Optional[AppMapResponse] = None,
    meta_source: str = "browser_inspection",
    execution_mode: str = "cloud",
    local_agent_id: Optional[str] = None,
    watch_id: Optional[str] = None,
    job_id: Optional[str] = None,
    artifact_sha256: Optional[str] = None,
) -> Dict[str, Any]:
    """Minimal dict for ``save_run`` / Supabase ``result`` — safe to store as JSON."""
    summary = build_browser_inspection_summary(result, inv=inv, extras=extras)
    app_summary: Optional[Dict[str, Any]] = None
    if app_map is not None:
        app_summary = build_app_map_summary(app_map)

    perf = result.performance if isinstance(result.performance, dict) else {}
    duration_ms = 0
    for k in ("timing_nav_ms", "domContentLoaded_ms", "load_ms"):
        v = perf.get(k)
        if isinstance(v, (int, float)) and v > 0:
            duration_ms = int(v)
            break

    status = "passed" if result.inspection_succeeded else "failed"
    title = (result.title or "").strip() or result.final_url or result.url
    test_name = _clip(f"[Browser inspection] {title}", 200)

    meta: Dict[str, Any] = {
        "source": (meta_source or "browser_inspection").strip() or "browser_inspection",
        "test_case_id": "_browser_inspection",
        "project_id": (project_id or "").strip() or None,
        "execution_mode": (execution_mode or "cloud").strip() or "cloud",
        "inspection_id": result.inspection_id,
        "run_type": "browser_inspection",
        "browser_inspection_summary": summary,
        "app_map_summary": app_summary,
    }
    la = (local_agent_id or "").strip()
    if la:
        meta["local_agent_id"] = la
    w = (watch_id or "").strip()
    if w:
        meta["watch_id"] = w
    j = (job_id or "").strip()
    if j:
        meta["job_id"] = j
    ah = (artifact_sha256 or "").strip()
    if ah:
        meta["artifact_sha256"] = ah[:64]
    meta = {k: v for k, v in meta.items() if v is not None}
    meta = _shrink_meta(meta)

    rid = result.inspection_id
    payload: Dict[str, Any] = {
        "run_id": rid,
        "evidence_id": rid,
        "status": status,
        "test_name": test_name,
        "duration_ms": duration_ms,
        "started_at": result.created_at,
        "evidence_url": result.screenshot_url,
        "steps": [],
        "logs": [],
        "meta": meta,
    }
    return payload


def persist_light_browser_inspection(
    result: BrowserInspectionResult,
    *,
    inv: Optional[Dict[str, Any]] = None,
    extras: Optional[Dict[str, Any]] = None,
    project_id: Optional[str],
    app_map: Optional[AppMapResponse] = None,
    meta_source: str = "browser_inspection",
    execution_mode: str = "cloud",
    local_agent_id: Optional[str] = None,
    watch_id: Optional[str] = None,
    job_id: Optional[str] = None,
    artifact_sha256: Optional[str] = None,
) -> Tuple[Optional[str], bool, Optional[str]]:
    """
    Persist a bounded run payload. Returns ``(persisted_run_id, persisted, warning)``.

    On failure, returns ``(None, False, short_message)`` — callers should still return HTTP 200
    with the live inspection body + ``persistence_warning``.
    """
    if not browser_inspection_persist_enabled():
        return None, False, None

    try:
        from services.run_access import persist_run_payload

        payload = build_lightweight_run_payload(
            result,
            inv=inv,
            extras=extras,
            project_id=project_id,
            app_map=app_map,
            meta_source=meta_source,
            execution_mode=execution_mode,
            local_agent_id=local_agent_id,
            watch_id=watch_id,
            job_id=job_id,
            artifact_sha256=artifact_sha256,
        )
        # Hard guard: never ship known heavy keys.
        payload.pop("screenshot_b64", None)
        if isinstance(payload.get("meta"), dict):
            payload["meta"].pop("screenshot_b64", None)

        key = persist_run_payload(payload)
        if not key:
            return None, False, "persistence_skipped: save_run returned no key"
        return str(result.inspection_id), True, None
    except Exception as exc:
        logger.warning("browser_inspection_persist: failed inspection_id=%s — %s", result.inspection_id, exc)
        return None, False, f"persistence_failed: {type(exc).__name__}"


def merge_persist_fields_into_inspection(
    result: BrowserInspectionResult,
    *,
    persisted_run_id: Optional[str],
    persisted: bool,
    persistence_warning: Optional[str],
) -> BrowserInspectionResult:
    return result.model_copy(
        update={
            "persisted_run_id": persisted_run_id,
            "persisted": persisted,
            "persistence_warning": persistence_warning,
        }
    )


def merge_persist_fields_into_app_map(
    resp: AppMapResponse,
    *,
    persisted_run_id: Optional[str],
    persisted: bool,
    persistence_warning: Optional[str],
) -> AppMapResponse:
    return resp.model_copy(
        update={
            "persisted_run_id": persisted_run_id,
            "persisted": persisted,
            "persistence_warning": persistence_warning,
        }
    )
