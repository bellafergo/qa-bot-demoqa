# services/browser_inspection_watch_service.py
"""
Scheduled browser inspection watches — diff, baselines, metrics (Phase 3C–3F).
"""
from __future__ import annotations

import logging
import uuid
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeout
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Union

from fastapi import HTTPException

from core.target_url_validation import TargetURLNotAllowed, validate_target_url
from models.browser_inspection_diff_models import BrowserInspectionDiffRequest
from models.browser_inspection_models import InspectUrlRequest
from models.browser_inspection_watch_metrics_models import (
    WatchEventItem,
    WatchEventsPageResponse,
    WatchMetricsResponse,
)
from models.browser_inspection_watch_models import (
    BrowserInspectionWatchCreate,
    BrowserInspectionWatchPatch,
    BrowserInspectionWatchResponse,
    BrowserInspectionWatchRunNowResponse,
    WatchBaselineSetRequest,
)
from services.alerting import schedule_browser_inspection_watch_alert
from services.browser_inspection_diff_service import compare_browser_inspections
from services.browser_inspection_persistence import (
    merge_persist_fields_into_inspection,
    persist_light_browser_inspection,
)
from services.browser_watch_alert_dedupe import watch_alert_try_reserve_slot
from services.browser_inspector_service import inspect_url_collect
from services.db.browser_inspection_watch_repository import browser_inspection_watch_repo

logger = logging.getLogger("vanya.browser_inspection_watch")

_EXECUTOR = ThreadPoolExecutor(max_workers=2, thread_name_prefix="bio-watch")


def _utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _parse_iso(ts: Optional[str]) -> Optional[datetime]:
    if not ts:
        return None
    try:
        return datetime.fromisoformat(str(ts).replace("Z", "+00:00"))
    except Exception:
        return None


def _url_norm(u: str) -> str:
    return (u or "").strip().lower().rstrip("/")


def _meta(run: Any) -> Dict[str, Any]:
    m = getattr(run, "meta", None)
    return m if isinstance(m, dict) else {}


def _bis(meta: Dict[str, Any]) -> Dict[str, Any]:
    x = meta.get("browser_inspection_summary")
    return x if isinstance(x, dict) else {}


def _project_id(meta: Dict[str, Any]) -> str:
    return str(meta.get("project_id") or "").strip()


def watch_is_due(watch: Dict[str, Any]) -> bool:
    if not watch.get("enabled"):
        return False
    last = _parse_iso(watch.get("last_run_at"))
    if last is None:
        return True
    now = datetime.now(timezone.utc)
    delta_min = (now - last).total_seconds() / 60.0
    return delta_min >= float(watch.get("interval_minutes") or 60)


def max_change_level(a: str, b: str) -> str:
    order = {"none": 0, "low": 1, "medium": 2, "high": 3}
    mx = max(order.get(str(a or "none").lower(), 0), order.get(str(b or "none").lower(), 0))
    for lab in ("high", "medium", "low", "none"):
        if order[lab] == mx:
            return lab
    return "none"


def threshold_allows_alert(threshold: str, change_level: str) -> bool:
    order = {"none": 0, "low": 1, "medium": 2, "high": 3}
    cl = order.get(str(change_level or "none").lower(), 0)
    th = str(threshold or "medium").lower()
    if th == "high":
        return cl >= 3
    if th == "medium":
        return cl >= 2
    if th == "low":
        return cl >= 1
    return False


def compute_last_status(
    *,
    enabled: bool,
    last_run_at: Optional[str],
    last_run_error: Optional[str],
    effective_change_level: Optional[str],
) -> str:
    if not enabled:
        return "disabled"
    if not last_run_at:
        return "never_run"
    if last_run_error:
        return "failed"
    lev = str(effective_change_level or "none").lower()
    if lev == "none":
        return "healthy"
    return "changed"


def validate_inspection_for_watch(inspection_id: str, watch: Dict[str, Any]) -> None:
    from services.db.test_run_repository import test_run_repo

    rid = (inspection_id or "").strip()
    if not rid:
        raise HTTPException(status_code=400, detail="inspection_id required")
    run = test_run_repo.get_run(rid)
    if run is None:
        raise HTTPException(status_code=404, detail="inspection not found")
    meta = _meta(run)
    if getattr(run, "test_case_id", None) != "_browser_inspection" or meta.get("source") != "browser_inspection":
        raise HTTPException(status_code=400, detail="not a browser inspection run")
    bis = _bis(meta)
    bu = _url_norm(str(bis.get("final_url") or bis.get("url") or ""))
    wu = _url_norm(str(watch.get("url") or ""))
    if bu and wu and bu != wu:
        raise HTTPException(status_code=400, detail="inspection URL does not match watch URL")
    mp = _project_id(meta)
    wp = str(watch.get("project_id") or "").strip()
    if wp and mp and mp != wp:
        raise HTTPException(status_code=400, detail="inspection project_id does not match watch")


def resolve_diff_base_id(watch: Dict[str, Any], new_inspection_id: str) -> Optional[str]:
    mode = (watch.get("compare_mode") or "last").strip().lower()
    if mode == "baseline":
        bid = (watch.get("baseline_inspection_id") or "").strip()
        if bid and bid != new_inspection_id:
            return bid
        return None
    prev = (watch.get("last_inspection_id") or "").strip()
    if prev and prev != new_inspection_id:
        return prev
    return None


def _record_watch_error(watch_id: str, message: str) -> None:
    msg = (message or "error")[:500]
    try:
        eid = str(uuid.uuid4())
        browser_inspection_watch_repo.insert_event(
            event_id=eid,
            watch_id=watch_id,
            base_inspection_id=None,
            target_inspection_id="_",
            change_level="none",
            summary=msg,
            regression_signals=[],
            improvement_signals=[],
            alert_triggered=False,
            alert_kind=None,
            visual_meta=None,
            event_type="error",
        )
        browser_inspection_watch_repo.update_watch(
            watch_id,
            last_status="failed",
            last_run_error=msg,
            last_run_at=_utc_now_iso(),
        )
    except Exception:
        logger.exception("browser_inspection_watch: record_watch_error failed")


def _dict_to_watch_response(d: Dict[str, Any]) -> BrowserInspectionWatchResponse:
    return BrowserInspectionWatchResponse(
        watch_id=d["watch_id"],
        url=d["url"],
        project_id=d.get("project_id"),
        interval_minutes=int(d.get("interval_minutes") or 60),
        change_threshold=d.get("change_threshold") or "medium",  # type: ignore[arg-type]
        enabled=bool(d.get("enabled")),
        execution_mode=d.get("execution_mode") or "cloud",  # type: ignore[arg-type]
        compare_mode=d.get("compare_mode") or "last",  # type: ignore[arg-type]
        baseline_inspection_id=d.get("baseline_inspection_id"),
        baseline_set_at=d.get("baseline_set_at"),
        baseline_updated_by=d.get("baseline_updated_by"),
        last_run_at=d.get("last_run_at"),
        last_inspection_id=d.get("last_inspection_id"),
        last_diff_id=d.get("last_diff_id"),
        last_status=d.get("last_status") or "never_run",  # type: ignore[arg-type]
        current_status=d.get("current_status") or d.get("last_status") or "never_run",  # type: ignore[arg-type]
        last_effective_change_level=d.get("last_effective_change_level"),
        last_change_level=d.get("last_change_level") or d.get("last_effective_change_level"),
        last_visual_change_level=d.get("last_visual_change_level"),
        last_alert_at=d.get("last_alert_at"),
        last_run_error=d.get("last_run_error"),
        created_at=d.get("created_at") or "",
        updated_at=d.get("updated_at") or "",
    )


def create_watch(body: BrowserInspectionWatchCreate) -> BrowserInspectionWatchResponse:
    if str(body.execution_mode) != "cloud":
        raise HTTPException(status_code=422, detail="execution_mode must be cloud")
    try:
        validated = validate_target_url(body.url.strip())
    except TargetURLNotAllowed as e:
        raise HTTPException(status_code=400, detail=str(e) or "Target URL not allowed") from None

    wid = browser_inspection_watch_repo.create_watch(
        url=validated,
        project_id=body.project_id,
        interval_minutes=body.interval_minutes,
        change_threshold=body.change_threshold,
        enabled=body.enabled,
        execution_mode=body.execution_mode,
        compare_mode=str(body.compare_mode or "last"),
    )
    row = browser_inspection_watch_repo.get_watch(wid)
    if not row:
        raise HTTPException(status_code=500, detail="watch create failed")
    return _dict_to_watch_response(row)


def list_watches(*, project_id: Optional[str], limit: int) -> list[BrowserInspectionWatchResponse]:
    rows = browser_inspection_watch_repo.list_watches(project_id=project_id, limit=limit)
    return [_dict_to_watch_response(r) for r in rows]


def get_watch(watch_id: str) -> BrowserInspectionWatchResponse:
    row = browser_inspection_watch_repo.get_watch(watch_id)
    if not row:
        raise HTTPException(status_code=404, detail="watch not found")
    return _dict_to_watch_response(row)


def _validate_url_for_local_agent_watch(url: str) -> str:
    u = (url or "").strip()
    if len(u) > 2048:
        raise HTTPException(status_code=400, detail="url too long")
    low = u.lower()
    if not low.startswith(("http://", "https://")):
        raise HTTPException(status_code=400, detail="url must be http or https")
    return u


def patch_watch(watch_id: str, body: BrowserInspectionWatchPatch) -> BrowserInspectionWatchResponse:
    row = browser_inspection_watch_repo.get_watch(watch_id)
    if not row:
        raise HTTPException(status_code=404, detail="watch not found")
    updates: Dict[str, Any] = {}
    next_em = (
        str(body.execution_mode).strip().lower()
        if body.execution_mode is not None
        else str(row.get("execution_mode") or "cloud").strip().lower()
    )
    if body.url is not None:
        if next_em == "local_agent":
            updates["url"] = _validate_url_for_local_agent_watch(body.url)
        else:
            try:
                updates["url"] = validate_target_url(body.url.strip())
            except TargetURLNotAllowed as e:
                raise HTTPException(status_code=400, detail=str(e) or "Target URL not allowed") from None
    if body.project_id is not None:
        updates["project_id"] = body.project_id
    if body.interval_minutes is not None:
        updates["interval_minutes"] = body.interval_minutes
    if body.change_threshold is not None:
        updates["change_threshold"] = body.change_threshold
    if body.enabled is not None:
        updates["enabled"] = body.enabled
    if body.compare_mode is not None:
        updates["compare_mode"] = str(body.compare_mode)
    if body.execution_mode is not None:
        em = str(body.execution_mode).strip().lower()
        if em not in ("cloud", "local_agent"):
            raise HTTPException(status_code=400, detail="invalid execution_mode")
        updates["execution_mode"] = em
    if not updates:
        return _dict_to_watch_response(row)
    ok = browser_inspection_watch_repo.update_watch(watch_id, **updates)
    if not ok:
        raise HTTPException(status_code=404, detail="watch not found")
    row2 = browser_inspection_watch_repo.get_watch(watch_id)
    return _dict_to_watch_response(row2 or row)


def set_watch_baseline(watch_id: str, body: WatchBaselineSetRequest) -> BrowserInspectionWatchResponse:
    row = browser_inspection_watch_repo.get_watch(watch_id)
    if not row:
        raise HTTPException(status_code=404, detail="watch not found")
    if body.use_latest:
        bid = (row.get("last_inspection_id") or "").strip()
        if not bid:
            raise HTTPException(status_code=400, detail="no last_inspection_id to pin as baseline")
    else:
        bid = (body.inspection_id or "").strip()
        if not bid:
            raise HTTPException(status_code=400, detail="inspection_id or use_latest required")
    validate_inspection_for_watch(bid, row)
    now = _utc_now_iso()
    browser_inspection_watch_repo.update_watch(
        watch_id,
        baseline_inspection_id=bid,
        baseline_set_at=now,
        baseline_updated_by=(body.baseline_updated_by or "").strip() or None,
    )
    browser_inspection_watch_repo.insert_event(
        event_id=str(uuid.uuid4()),
        watch_id=watch_id,
        base_inspection_id=None,
        target_inspection_id=bid,
        change_level="none",
        summary="baseline_set",
        regression_signals=[],
        improvement_signals=[],
        alert_triggered=False,
        alert_kind=None,
        visual_meta=None,
        event_type="baseline_set",
    )
    row2 = browser_inspection_watch_repo.get_watch(watch_id)
    return _dict_to_watch_response(row2 or row)


def list_watch_events(
    watch_id: str,
    *,
    limit: int = 100,
    cursor: Optional[str] = None,
    paged: bool = False,
) -> Union[List[WatchEventItem], WatchEventsPageResponse]:
    if not browser_inspection_watch_repo.get_watch(watch_id):
        raise HTTPException(status_code=404, detail="watch not found")
    try:
        raw, next_c = browser_inspection_watch_repo.list_events_page(watch_id, limit=limit, cursor=cursor)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail="invalid cursor") from exc
    items = [WatchEventItem(**r) for r in raw]
    if paged or (cursor is not None and cursor != ""):
        return WatchEventsPageResponse(items=items, next_cursor=next_c)
    return items


def get_watch_metrics(watch_id: str) -> WatchMetricsResponse:
    w = browser_inspection_watch_repo.get_watch(watch_id)
    if not w:
        raise HTTPException(status_code=404, detail="watch not found")
    agg = browser_inspection_watch_repo.aggregate_metrics(watch_id)
    c = agg.get("counts") or {}
    total_runs = int(c.get("run_completed", 0))
    total_diffs = int(c.get("diff_generated", 0))
    alerts_deduped = int(c.get("alert_deduped", 0))
    failed_runs = int(c.get("error", 0))
    visual_fetch_failures = int(c.get("visual_diff_skipped", 0))
    alerts_triggered = int(agg.get("alerts_triggered") or 0)
    last_alert_at = w.get("last_alert_at") or agg.get("last_alert_from_diff_at")
    status = str(w.get("last_status") or "never_run")
    last_change = (
        w.get("last_change_level") or w.get("last_effective_change_level") or agg.get("last_diff_change_level")
    )
    last_vis = w.get("last_visual_change_level") or agg.get("last_visual_change_level")
    return WatchMetricsResponse(
        watch_id=watch_id,
        total_runs=total_runs,
        total_diffs=total_diffs,
        alerts_triggered=alerts_triggered,
        alerts_deduped=alerts_deduped,
        last_change_level=last_change,
        last_visual_change_level=last_vis,
        last_run_at=w.get("last_run_at"),
        last_alert_at=last_alert_at,
        failed_runs=failed_runs,
        visual_fetch_failures=visual_fetch_failures,
        current_status=status,
    )


def execute_watch_tick(watch_id: str, *, force: bool = False, timeout_s: int = 90) -> BrowserInspectionWatchRunNowResponse:
    watch = browser_inspection_watch_repo.get_watch(watch_id)
    if not watch:
        raise HTTPException(status_code=404, detail="watch not found")
    if not watch.get("enabled") and not force:
        raise HTTPException(status_code=400, detail="watch is disabled")

    mode = str(watch.get("execution_mode") or "cloud").strip().lower()
    if mode == "local_agent" and not force:
        raise HTTPException(
            status_code=400,
            detail="execution_mode is local_agent; cloud execution is disabled. Use Vanya Local Agent (Phase 4B+).",
        )

    tick_warnings: List[str] = []
    compare_mode = str(watch.get("compare_mode") or "last")

    browser_inspection_watch_repo.insert_event(
        event_id=str(uuid.uuid4()),
        watch_id=watch_id,
        base_inspection_id=None,
        target_inspection_id="_",
        change_level="none",
        summary="run_started",
        regression_signals=[],
        improvement_signals=[],
        alert_triggered=False,
        alert_kind=None,
        visual_meta=None,
        event_type="run_started",
    )

    req = InspectUrlRequest(
        url=watch["url"],
        project_id=watch.get("project_id"),
        timeout_ms=15_000,
    )

    def _do_collect():
        return inspect_url_collect(req)

    try:
        fut = _EXECUTOR.submit(_do_collect)
        inspection, inv, extras = fut.result(timeout=timeout_s)
    except FuturesTimeout:
        logger.error("browser_inspection_watch: collect timeout watch_id=%s", watch_id)
        _record_watch_error(watch_id, "inspection timed out")
        raise HTTPException(status_code=504, detail="inspection timed out") from None
    except TargetURLNotAllowed as e:
        raise HTTPException(status_code=400, detail=str(e) or "Target URL not allowed") from None
    except Exception as exc:
        logger.exception("browser_inspection_watch: inspection failed watch_id=%s", watch_id)
        _record_watch_error(watch_id, f"Inspection failed: {exc}")
        raise HTTPException(status_code=500, detail=f"Inspection failed: {exc}") from exc

    rid, okp, pwarn = persist_light_browser_inspection(
        inspection,
        inv=inv,
        extras=extras,
        project_id=watch.get("project_id"),
        app_map=None,
    )
    if not okp:
        msg = pwarn or "persistence failed"
        _record_watch_error(watch_id, msg)
        raise HTTPException(status_code=500, detail=msg) from None

    inspection = merge_persist_fields_into_inspection(
        inspection,
        persisted_run_id=rid if okp else None,
        persisted=okp,
        persistence_warning=pwarn,
    )
    new_id = inspection.inspection_id
    now = _utc_now_iso()

    browser_inspection_watch_repo.insert_event(
        event_id=str(uuid.uuid4()),
        watch_id=watch_id,
        base_inspection_id=None,
        target_inspection_id=new_id,
        change_level="none",
        summary="run_completed",
        regression_signals=[],
        improvement_signals=[],
        alert_triggered=False,
        alert_kind=None,
        visual_meta=None,
        event_type="run_completed",
    )

    wstate = dict(watch)
    if compare_mode == "baseline" and not (wstate.get("baseline_inspection_id") or "").strip():
        browser_inspection_watch_repo.update_watch(
            watch_id,
            baseline_inspection_id=new_id,
            baseline_set_at=now,
        )
        wstate["baseline_inspection_id"] = new_id
        tick_warnings.append("watch_baseline_auto_pinned: first inspection stored as baseline")

    diff_base = resolve_diff_base_id(wstate, new_id)

    diff_id: Optional[str] = None
    change_level = "none"
    summary: Optional[str] = None
    regression: list[str] = []
    improvement: list[str] = []
    alert_triggered = False
    alert_kind: Optional[str] = None
    effective_out = "none"
    alert_dedupe_suppressed_out = False
    vis_det: Optional[bool] = None
    vis_lvl: Optional[str] = None
    vis_sim: Optional[float] = None
    last_alert_at: Optional[str] = watch.get("last_alert_at")

    if diff_base:
        diff = None
        try:
            diff = compare_browser_inspections(
                BrowserInspectionDiffRequest(
                    base_inspection_id=diff_base,
                    target_inspection_id=new_id,
                    project_id=watch.get("project_id"),
                )
            )
        except HTTPException as exc:
            if exc.status_code != 404:
                raise
            logger.warning(
                "browser_inspection_watch: diff baseline missing watch_id=%s base=%s",
                watch_id,
                diff_base,
            )
            tick_warnings.append("watch_diff_skipped: baseline inspection not found in store")
        if diff is not None:
            diff_id = str(uuid.uuid4())
            change_level = diff.change_level
            summary = diff.summary
            regression = list(diff.regression_signals or [])
            improvement = list(diff.improvement_signals or [])
            effective_level = max_change_level(change_level, diff.visual_change_level or "none")
            effective_out = effective_level
            vis_det = diff.visual_change_detected
            vis_lvl = diff.visual_change_level
            vis_sim = diff.visual_similarity_score
            visual_meta: Dict[str, Any] = {"visual_change_detected": diff.visual_change_detected}
            if diff.visual_change_level is not None:
                visual_meta["visual_change_level"] = diff.visual_change_level
            if diff.visual_similarity_score is not None:
                visual_meta["visual_similarity_score"] = diff.visual_similarity_score
            if diff.visual_hash_changed is not None:
                visual_meta["visual_hash_changed"] = diff.visual_hash_changed
            alert_dedupe_suppressed = False
            if threshold_allows_alert(watch.get("change_threshold") or "medium", effective_level):
                if regression:
                    alert_kind = "browser_inspection_regression_detected"
                else:
                    alert_kind = "browser_inspection_change_detected"
                allow_send, dedupe_reason = watch_alert_try_reserve_slot(
                    watch_id=watch_id,
                    change_level=effective_level,
                    regression_signals=regression,
                    summary=summary or "",
                    visual_change_level=str(diff.visual_change_level or "none"),
                    visual_similarity_score=diff.visual_similarity_score,
                )
                if not allow_send:
                    logger.warning(
                        "browser_inspection_watch: alert deduped watch_id=%s reason=%s",
                        watch_id,
                        dedupe_reason,
                    )
                    alert_dedupe_suppressed = True
                    alert_triggered = False
                    alert_kind = None
                    alert_dedupe_suppressed_out = True
                    browser_inspection_watch_repo.insert_event(
                        event_id=str(uuid.uuid4()),
                        watch_id=watch_id,
                        base_inspection_id=diff_base,
                        target_inspection_id=new_id,
                        change_level=effective_level,
                        summary=(dedupe_reason or "alert_deduped")[:800],
                        regression_signals=[],
                        improvement_signals=[],
                        alert_triggered=False,
                        alert_kind=None,
                        visual_meta={"dedupe": True},
                        event_type="alert_deduped",
                    )
                else:
                    alert_triggered = True
                    last_alert_at = now
                    try:
                        schedule_browser_inspection_watch_alert(
                            alert_kind=alert_kind or "",
                            watch_id=watch_id,
                            url=watch["url"],
                            change_level=effective_level,
                            summary=summary or "",
                            regression_signals=regression[:20],
                            base_inspection_id=diff_base,
                            target_inspection_id=new_id,
                            visual_change_detected=diff.visual_change_detected,
                            visual_change_level=diff.visual_change_level,
                            visual_similarity_score=diff.visual_similarity_score,
                        )
                    except Exception:
                        logger.exception("browser_inspection_watch: alert scheduling failed")

            if any(str(x).startswith("visual_diff_skipped") for x in (diff.warnings or [])):
                browser_inspection_watch_repo.insert_event(
                    event_id=str(uuid.uuid4()),
                    watch_id=watch_id,
                    base_inspection_id=diff_base,
                    target_inspection_id=new_id,
                    change_level=effective_level,
                    summary="visual_diff_skipped",
                    regression_signals=[],
                    improvement_signals=[],
                    alert_triggered=False,
                    alert_kind=None,
                    visual_meta=None,
                    event_type="visual_diff_skipped",
                )

            browser_inspection_watch_repo.insert_event(
                event_id=diff_id,
                watch_id=watch_id,
                base_inspection_id=diff_base,
                target_inspection_id=new_id,
                change_level=effective_level,
                summary=summary or "",
                regression_signals=regression,
                improvement_signals=improvement,
                alert_triggered=alert_triggered,
                alert_kind=alert_kind,
                visual_meta=visual_meta or None,
                event_type="diff_generated",
            )

    lec_out = effective_out if diff_id else str(watch.get("last_effective_change_level") or "none")
    if diff_id:
        lvc_out = str(vis_lvl) if vis_lvl is not None else watch.get("last_visual_change_level")
    else:
        lvc_out = watch.get("last_visual_change_level")

    last_status_final = compute_last_status(
        enabled=bool(watch.get("enabled")),
        last_run_at=now,
        last_run_error=None,
        effective_change_level=lec_out,
    )

    browser_inspection_watch_repo.update_watch(
        watch_id,
        last_run_at=now,
        last_inspection_id=new_id,
        last_diff_id=diff_id if diff_id else watch.get("last_diff_id"),
        last_status=last_status_final,
        last_effective_change_level=lec_out,
        last_visual_change_level=lvc_out,
        last_alert_at=last_alert_at,
        last_run_error=None,
    )

    return BrowserInspectionWatchRunNowResponse(
        watch_id=watch_id,
        inspection_id=new_id,
        diff_id=diff_id,
        base_inspection_id=diff_base,
        compare_mode=compare_mode,  # type: ignore[arg-type]
        warnings=tick_warnings,
        change_level=effective_out,
        alert_triggered=alert_triggered,
        alert_kind=alert_kind,
        summary=summary,
        visual_change_detected=vis_det,
        visual_change_level=vis_lvl,
        visual_similarity_score=vis_sim,
        alert_dedupe_suppressed=alert_dedupe_suppressed_out,
    )


def tick_due_watches(*, max_per_tick: int = 10) -> int:
    """Run up to ``max_per_tick`` due enabled watches. Returns number of ticks attempted."""
    n = 0
    for w in browser_inspection_watch_repo.list_watches(project_id=None, limit=500):
        if not w.get("enabled"):
            continue
        if str(w.get("execution_mode") or "cloud").strip().lower() == "local_agent":
            continue
        if not watch_is_due(w):
            continue
        if n >= max_per_tick:
            break
        try:
            execute_watch_tick(w["watch_id"], force=False)
            n += 1
        except HTTPException:
            logger.warning("browser_inspection_watch: tick skipped watch_id=%s (HTTP)", w.get("watch_id"))
        except Exception:
            logger.exception("browser_inspection_watch: tick failed watch_id=%s", w.get("watch_id"))
    return n
