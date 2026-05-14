# services/browser_inspection_diff_service.py
"""
Deterministic diff between two persisted browser inspections (Phase 3B).

Reads ``test_run_repo`` summaries only — no Playwright, no DOM/HTML in responses.
"""
from __future__ import annotations

import json
from typing import Any, Dict, List, Optional, Set

from fastapi import HTTPException

from models.browser_inspection_diff_models import (
    BrowserInspectionChanges,
    BrowserInspectionDiffRequest,
    BrowserInspectionDiffResponse,
    ChangeLevel,
    CountsDelta,
)
from services.browser_visual_diff_service import compare_browser_visual_pair


_KNOWN_PAGE_TYPES = frozenset(
    {
        "login_page",
        "dashboard",
        "landing_page",
        "form_page",
        "crud_table",
        "search_interface",
        "ecommerce_page",
        "checkout_page",
        "profile_settings",
        "error_page",
    }
)


def _meta(run: Any) -> Dict[str, Any]:
    m = getattr(run, "meta", None)
    return m if isinstance(m, dict) else {}


def _bis(meta: Dict[str, Any]) -> Dict[str, Any]:
    x = meta.get("browser_inspection_summary")
    return x if isinstance(x, dict) else {}


def _ams(meta: Dict[str, Any]) -> Dict[str, Any]:
    x = meta.get("app_map_summary")
    return x if isinstance(x, dict) else {}


def _counts(bis: Dict[str, Any]) -> Dict[str, int]:
    c = bis.get("counts")
    if not isinstance(c, dict):
        return {}
    keys = (
        "links_count",
        "buttons_count",
        "inputs_count",
        "forms_count",
        "images_without_alt_count",
        "selector_candidates_count",
    )
    out: Dict[str, int] = {}
    for k in keys:
        try:
            out[k] = int(c.get(k) or 0)
        except Exception:
            out[k] = 0
    return out


def _len_list(x: Any) -> int:
    return len(x) if isinstance(x, list) else 0


def _action_key(item: Dict[str, Any]) -> str:
    return json.dumps(
        {"t": (item.get("text") or ""), "k": (item.get("kind") or "")},
        sort_keys=True,
        ensure_ascii=True,
    )


def _set_from_actions_summary(ams: Dict[str, Any]) -> Set[str]:
    raw = ams.get("primary_actions_summary")
    if not isinstance(raw, list):
        return set()
    out: Set[str] = set()
    for it in raw:
        if isinstance(it, dict):
            out.add(_action_key(it))
    return out


def _set_from_str_list(ams: Dict[str, Any], key: str) -> Set[str]:
    raw = ams.get(key)
    if not isinstance(raw, list):
        return set()
    return {str(x) for x in raw if x is not None}


def _page_types(ams: Dict[str, Any]) -> List[str]:
    pt = ams.get("page_type")
    if isinstance(pt, list):
        return [str(x) for x in pt]
    return []


def _known_overlap(types: List[str]) -> bool:
    return any(t in _KNOWN_PAGE_TYPES for t in types)


def _load_inspection_row(run_id: str) -> Any:
    from services.db.test_run_repository import test_run_repo

    rid = (run_id or "").strip()
    if not rid:
        raise HTTPException(status_code=404, detail="inspection not found")
    run = test_run_repo.get_run(rid)
    if run is None:
        raise HTTPException(status_code=404, detail="inspection not found")
    meta = _meta(run)
    if getattr(run, "test_case_id", None) != "_browser_inspection" or meta.get("source") != "browser_inspection":
        raise HTTPException(status_code=404, detail="inspection not found")
    return run


def _project_id(meta: Dict[str, Any]) -> str:
    return str(meta.get("project_id") or "").strip()


def compare_browser_inspections(req: BrowserInspectionDiffRequest) -> BrowserInspectionDiffResponse:
    base_run = _load_inspection_row(req.base_inspection_id)
    tgt_run = _load_inspection_row(req.target_inspection_id)

    bm, tm = _meta(base_run), _meta(tgt_run)
    bp, tp = _project_id(bm), _project_id(tm)

    warnings: List[str] = []
    if req.project_id and str(req.project_id).strip():
        q = str(req.project_id).strip()
        if bp and bp != q:
            raise HTTPException(status_code=400, detail="base_inspection project_id does not match request")
        if tp and tp != q:
            raise HTTPException(status_code=400, detail="target_inspection project_id does not match request")
    if bp and tp and bp != tp:
        raise HTTPException(
            status_code=400,
            detail="base and target inspections belong to different projects",
        )

    bb, tb = _bis(bm), _bis(tm)
    ba, ta = _ams(bm), _ams(tm)

    title_b = str(bb.get("title") or "")
    title_t = str(tb.get("title") or "")
    url_b = str(bb.get("final_url") or bb.get("url") or "")
    url_t = str(tb.get("final_url") or tb.get("url") or "")

    sc_b = bb.get("status_code")
    sc_t = tb.get("status_code")
    try:
        sci_b = int(sc_b) if sc_b is not None else None
    except Exception:
        sci_b = None
    try:
        sci_t = int(sc_t) if sc_t is not None else None
    except Exception:
        sci_t = None

    cb = _counts(bb)
    ct = _counts(tb)
    delta = CountsDelta(
        links_count=ct.get("links_count", 0) - cb.get("links_count", 0),
        buttons_count=ct.get("buttons_count", 0) - cb.get("buttons_count", 0),
        inputs_count=ct.get("inputs_count", 0) - cb.get("inputs_count", 0),
        forms_count=ct.get("forms_count", 0) - cb.get("forms_count", 0),
        images_without_alt_count=ct.get("images_without_alt_count", 0) - cb.get("images_without_alt_count", 0),
        selector_candidates_count=ct.get("selector_candidates_count", 0) - cb.get("selector_candidates_count", 0),
    )

    ce_b = _len_list(bb.get("console_errors"))
    ce_t = _len_list(tb.get("console_errors"))
    ne_b = _len_list(bb.get("network_errors"))
    ne_t = _len_list(tb.get("network_errors"))
    w_b = _len_list(bb.get("warnings"))
    w_t = _len_list(tb.get("warnings"))
    rn_b = _len_list(ba.get("risk_notes"))
    rn_t = _len_list(ta.get("risk_notes"))

    pt_b, pt_t = _page_types(ba), _page_types(ta)
    page_type_changed = pt_b != pt_t

    sa_b, sa_t = _set_from_actions_summary(ba), _set_from_actions_summary(ta)
    removed_actions = sa_b - sa_t
    added_actions = sa_t - sa_b
    primary_changed: List[str] = []
    for x in sorted(removed_actions):
        primary_changed.append(f"removed:{x}")
    for x in sorted(added_actions):
        primary_changed.append(f"added:{x}")

    flows_b = _set_from_str_list(ba, "suggested_test_flows")
    flows_t = _set_from_str_list(ta, "suggested_test_flows")
    flows_delta: List[str] = []
    for x in sorted(flows_b - flows_t):
        flows_delta.append(f"removed:{x}")
    for x in sorted(flows_t - flows_b):
        flows_delta.append(f"added:{x}")

    title_changed = title_b != title_t
    final_url_changed = url_b != url_t
    status_code_changed = sci_b != sci_t

    changes = BrowserInspectionChanges(
        title_changed=title_changed,
        final_url_changed=final_url_changed,
        status_code_changed=status_code_changed,
        page_type_changed=page_type_changed,
        counts_delta=delta,
        console_errors_delta=ce_t - ce_b,
        network_errors_delta=ne_t - ne_b,
        warnings_delta=w_t - w_b,
        risk_notes_delta=rn_t - rn_b,
        primary_actions_changed=primary_changed[:40],
        suggested_test_flows_changed=flows_delta[:40],
    )

    regression: List[str] = []
    improvement: List[str] = []

    if sci_b is not None and sci_t is not None and sci_b < 400 <= sci_t:
        regression.append("http_status_worsened")
    if sci_b is not None and sci_t is not None and sci_b >= 400 > sci_t:
        improvement.append("http_status_improved")

    if changes.network_errors_delta > 0:
        regression.append("network_errors_increased")
    if changes.network_errors_delta < 0:
        improvement.append("network_errors_decreased")

    if changes.console_errors_delta > 0:
        regression.append("console_errors_increased")
    if changes.console_errors_delta < 0:
        improvement.append("console_errors_decreased")

    if delta.buttons_count < 0 or delta.inputs_count < 0:
        regression.append("interactive_elements_decreased")
    if delta.buttons_count > 0 or delta.inputs_count > 0:
        improvement.append("interactive_elements_increased")

    if delta.selector_candidates_count <= -3:
        regression.append("selector_candidates_dropped_significantly")
    if delta.selector_candidates_count >= 3:
        improvement.append("selector_candidates_increased")

    if delta.images_without_alt_count > 0:
        regression.append("images_without_alt_increased")
    if delta.images_without_alt_count < 0:
        improvement.append("images_without_alt_decreased")

    if _known_overlap(pt_b) and not _known_overlap(pt_t) and ("unknown" in pt_t or not pt_t):
        regression.append("page_type_degraded_to_unknown")

    if (not _known_overlap(pt_b)) and _known_overlap(pt_t):
        improvement.append("page_type_became_known_category")

    if changes.risk_notes_delta > 0:
        regression.append("risk_notes_increased")
    if changes.risk_notes_delta < 0:
        improvement.append("risk_notes_decreased")

    if removed_actions and not added_actions:
        regression.append("primary_actions_removed")
    if added_actions and len(removed_actions) <= len(added_actions):
        improvement.append("primary_actions_enriched")

    score = 0
    if title_changed or final_url_changed:
        score += 1
    if status_code_changed:
        score += 2
    if page_type_changed:
        score += 2
    if abs(delta.links_count) + abs(delta.buttons_count) + abs(delta.inputs_count) >= 6:
        score += 2
    if abs(delta.selector_candidates_count) >= 3:
        score += 2
    if changes.console_errors_delta != 0 or changes.network_errors_delta != 0:
        score += min(3, abs(changes.console_errors_delta) + abs(changes.network_errors_delta))
    score += min(4, len(regression) * 2)
    score += min(2, len(improvement))

    if (
        not title_changed
        and not final_url_changed
        and not status_code_changed
        and not page_type_changed
        and all(
            v == 0
            for v in (
                delta.links_count,
                delta.buttons_count,
                delta.inputs_count,
                delta.forms_count,
                delta.images_without_alt_count,
                delta.selector_candidates_count,
            )
        )
        and changes.console_errors_delta == 0
        and changes.network_errors_delta == 0
        and changes.warnings_delta == 0
        and changes.risk_notes_delta == 0
        and not primary_changed
        and not flows_delta
    ):
        level: ChangeLevel = "none"
    elif score <= 2:
        level = "low"
    elif score <= 6:
        level = "medium"
    else:
        level = "high"

    summary_parts: List[str] = []
    if level == "none":
        summary_parts.append("No material UI metadata changes detected between snapshots.")
    else:
        if title_changed:
            summary_parts.append("Title changed.")
        if final_url_changed:
            summary_parts.append("Final URL changed.")
        if status_code_changed:
            summary_parts.append("HTTP status changed.")
        if page_type_changed:
            summary_parts.append("Semantic page type changed.")
        if regression:
            summary_parts.append(f"Regression signals: {len(regression)}.")
        if improvement:
            summary_parts.append(f"Improvement signals: {len(improvement)}.")
    summary = " ".join(summary_parts) if summary_parts else f"Change level {level}."

    shot_b = str(bb.get("screenshot_url") or "").strip() or None
    shot_t = str(tb.get("screenshot_url") or "").strip() or None
    vres = compare_browser_visual_pair(shot_b, shot_t)
    warnings.extend(vres.warnings)
    if vres.visual_change_level in ("medium", "high"):
        regression.append("visual_change_suspected")
    if vres.visual_change_detected:
        tail = (
            f" Visual change: {vres.visual_change_level} "
            f"(similarity {vres.visual_similarity_score})."
        )
        summary = (summary + tail)[:800]

    return BrowserInspectionDiffResponse(
        base_inspection_id=req.base_inspection_id.strip(),
        target_inspection_id=req.target_inspection_id.strip(),
        change_level=level,
        summary=summary[:800],
        changes=changes,
        regression_signals=regression,
        improvement_signals=improvement,
        warnings=warnings,
        visual_change_detected=vres.visual_change_detected,
        visual_change_level=vres.visual_change_level,
        visual_hash_changed=vres.visual_hash_changed,
        visual_similarity_score=vres.visual_similarity_score,
    )
