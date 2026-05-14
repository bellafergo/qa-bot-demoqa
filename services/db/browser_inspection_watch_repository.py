# services/db/browser_inspection_watch_repository.py
"""SQLite persistence for scheduled browser inspection watches (Phase 3C–3F)."""
from __future__ import annotations

import base64
import json
import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from sqlalchemy import Column, Integer, String, Text, and_, desc, or_, text

from services.db.sqlite_db import Base, get_session

logger = logging.getLogger("vanya.db.browser_inspection_watch")

_SENTINEL_NO_INSPECTION = "_"


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _encode_event_cursor(created_at: str, event_id: str) -> str:
    raw = json.dumps({"ca": created_at, "eid": event_id}, separators=(",", ":")).encode("utf-8")
    return base64.urlsafe_b64encode(raw).decode("ascii").rstrip("=")


def _decode_event_cursor(cur: str) -> Tuple[str, str]:
    s = (cur or "").strip()
    if not s:
        raise ValueError("empty cursor")
    pad = "=" * (-len(s) % 4)
    raw = base64.urlsafe_b64decode(s + pad)
    d = json.loads(raw.decode("utf-8"))
    return str(d["ca"]), str(d["eid"])


class BrowserInspectionWatchRow(Base):
    __tablename__ = "browser_inspection_watches"

    watch_id = Column(String, primary_key=True)
    url = Column(String, nullable=False)
    project_id = Column(String, nullable=True)
    interval_minutes = Column(Integer, nullable=False, default=60)
    change_threshold = Column(String, nullable=False, default="medium")
    enabled = Column(Integer, nullable=False, default=1)
    execution_mode = Column(String, nullable=False, default="cloud")
    compare_mode = Column(String, nullable=False, default="last")
    baseline_inspection_id = Column(String, nullable=True)
    baseline_set_at = Column(String, nullable=True)
    baseline_updated_by = Column(String, nullable=True)
    last_status = Column(String, nullable=True)
    last_effective_change_level = Column(String, nullable=True)
    last_visual_change_level = Column(String, nullable=True)
    last_alert_at = Column(String, nullable=True)
    last_run_error = Column(Text, nullable=True)
    created_at = Column(String, nullable=False)
    updated_at = Column(String, nullable=False)
    last_run_at = Column(String, nullable=True)
    last_inspection_id = Column(String, nullable=True)
    last_diff_id = Column(String, nullable=True)


class BrowserInspectionWatchEventRow(Base):
    __tablename__ = "browser_inspection_watch_events"

    event_id = Column(String, primary_key=True)
    watch_id = Column(String, nullable=False, index=True)
    event_type = Column(String, nullable=False, default="diff_generated")
    base_inspection_id = Column(String, nullable=True)
    target_inspection_id = Column(String, nullable=False)
    change_level = Column(String, nullable=False)
    summary = Column(Text, nullable=False)
    regression_signals_json = Column(Text, nullable=False, default="[]")
    improvement_signals_json = Column(Text, nullable=False, default="[]")
    alert_triggered = Column(Integer, nullable=False, default=0)
    alert_kind = Column(String, nullable=True)
    created_at = Column(String, nullable=False)
    visual_meta_json = Column(Text, nullable=True)


class BrowserInspectionWatchRepository:
    def create_watch(
        self,
        *,
        url: str,
        project_id: Optional[str],
        interval_minutes: int,
        change_threshold: str,
        enabled: bool,
        execution_mode: str,
        compare_mode: str = "last",
    ) -> str:
        wid = str(uuid.uuid4())
        now = _utc_iso()
        row = BrowserInspectionWatchRow(
            watch_id=wid,
            url=url.strip(),
            project_id=(project_id or "").strip() or None,
            interval_minutes=max(5, min(int(interval_minutes), 24 * 60)),
            change_threshold=change_threshold,
            enabled=1 if enabled else 0,
            execution_mode=execution_mode,
            compare_mode=(compare_mode or "last").strip().lower() or "last",
            baseline_inspection_id=None,
            baseline_set_at=None,
            baseline_updated_by=None,
            last_status="never_run",
            last_effective_change_level=None,
            last_visual_change_level=None,
            last_alert_at=None,
            last_run_error=None,
            created_at=now,
            updated_at=now,
            last_run_at=None,
            last_inspection_id=None,
            last_diff_id=None,
        )
        with get_session() as s:
            s.add(row)
        logger.debug("browser_inspection_watch: created %s", wid)
        return wid

    def list_watches(self, *, project_id: Optional[str] = None, limit: int = 100) -> List[Dict[str, Any]]:
        limit = max(1, min(int(limit or 100), 500))
        with get_session() as s:
            q = (
                s.query(BrowserInspectionWatchRow)
                .order_by(desc(BrowserInspectionWatchRow.created_at))
                .limit(limit)
            )
            rows = q.all()
        out: List[Dict[str, Any]] = []
        for r in rows:
            d = self._row_watch_to_dict(r)
            if project_id and str(project_id).strip():
                if (d.get("project_id") or "") != str(project_id).strip():
                    continue
            out.append(d)
        return out

    def get_watch(self, watch_id: str) -> Optional[Dict[str, Any]]:
        wid = (watch_id or "").strip()
        if not wid:
            return None
        with get_session() as s:
            row = s.query(BrowserInspectionWatchRow).filter_by(watch_id=wid).first()
            return self._row_watch_to_dict(row) if row else None

    def update_watch(self, watch_id: str, **fields: Any) -> bool:
        wid = (watch_id or "").strip()
        if not wid:
            return False
        with get_session() as s:
            row = s.query(BrowserInspectionWatchRow).filter_by(watch_id=wid).first()
            if not row:
                return False
            if "url" in fields and fields["url"] is not None:
                row.url = str(fields["url"]).strip()
            if "project_id" in fields:
                v = fields["project_id"]
                row.project_id = (str(v).strip() or None) if v is not None else row.project_id
            if "interval_minutes" in fields and fields["interval_minutes"] is not None:
                row.interval_minutes = max(5, min(int(fields["interval_minutes"]), 24 * 60))
            if "change_threshold" in fields and fields["change_threshold"] is not None:
                row.change_threshold = str(fields["change_threshold"])
            if "enabled" in fields and fields["enabled"] is not None:
                row.enabled = 1 if bool(fields["enabled"]) else 0
            if "execution_mode" in fields and fields["execution_mode"] is not None:
                row.execution_mode = str(fields["execution_mode"])
            if "compare_mode" in fields and fields["compare_mode"] is not None:
                row.compare_mode = str(fields["compare_mode"]).strip().lower() or "last"
            if "baseline_inspection_id" in fields:
                row.baseline_inspection_id = fields["baseline_inspection_id"]
            if "baseline_set_at" in fields:
                row.baseline_set_at = fields["baseline_set_at"]
            if "baseline_updated_by" in fields:
                row.baseline_updated_by = fields["baseline_updated_by"]
            if "last_status" in fields:
                row.last_status = fields["last_status"]
            if "last_effective_change_level" in fields:
                row.last_effective_change_level = fields["last_effective_change_level"]
            if "last_visual_change_level" in fields:
                row.last_visual_change_level = fields["last_visual_change_level"]
            if "last_alert_at" in fields:
                row.last_alert_at = fields["last_alert_at"]
            if "last_run_error" in fields:
                row.last_run_error = fields["last_run_error"]
            if "last_run_at" in fields:
                row.last_run_at = fields["last_run_at"]
            if "last_inspection_id" in fields:
                row.last_inspection_id = fields["last_inspection_id"]
            if "last_diff_id" in fields:
                row.last_diff_id = fields["last_diff_id"]
            row.updated_at = _utc_iso()
        return True

    def insert_event(
        self,
        *,
        event_id: str,
        watch_id: str,
        base_inspection_id: Optional[str],
        target_inspection_id: str,
        change_level: str,
        summary: str,
        regression_signals: List[str],
        improvement_signals: List[str],
        alert_triggered: bool,
        alert_kind: Optional[str],
        visual_meta: Optional[Dict[str, Any]] = None,
        event_type: str = "diff_generated",
    ) -> None:
        cap = 8000
        vis_blob = None
        if visual_meta is not None:
            try:
                vis_blob = json.dumps(visual_meta, ensure_ascii=True, default=str)[:2000]
            except Exception:
                vis_blob = None
        tid = (target_inspection_id or "").strip() or _SENTINEL_NO_INSPECTION
        row = BrowserInspectionWatchEventRow(
            event_id=event_id,
            watch_id=watch_id,
            event_type=(event_type or "diff_generated").strip()[:64],
            base_inspection_id=base_inspection_id,
            target_inspection_id=tid,
            change_level=change_level,
            summary=(summary or "")[:cap],
            regression_signals_json=json.dumps(regression_signals[:50], ensure_ascii=True)[:cap],
            improvement_signals_json=json.dumps(improvement_signals[:50], ensure_ascii=True)[:cap],
            alert_triggered=1 if alert_triggered else 0,
            alert_kind=alert_kind,
            created_at=_utc_iso(),
            visual_meta_json=vis_blob,
        )
        with get_session() as s:
            s.add(row)

    def list_events(self, watch_id: str, *, limit: int = 100) -> List[Dict[str, Any]]:
        items, _ = self.list_events_page(watch_id, limit=limit, cursor=None)
        return items

    def list_events_page(
        self,
        watch_id: str,
        *,
        limit: int,
        cursor: Optional[str],
    ) -> Tuple[List[Dict[str, Any]], Optional[str]]:
        wid = (watch_id or "").strip()
        if not wid:
            return [], None
        limit = max(1, min(int(limit), 500))
        with get_session() as s:
            q = s.query(BrowserInspectionWatchEventRow).filter_by(watch_id=wid)
            if cursor:
                try:
                    ca_e, eid_e = _decode_event_cursor(cursor)
                except Exception:
                    raise ValueError("invalid cursor") from None
                q = q.filter(
                    or_(
                        BrowserInspectionWatchEventRow.created_at < ca_e,
                        and_(
                            BrowserInspectionWatchEventRow.created_at == ca_e,
                            BrowserInspectionWatchEventRow.event_id < eid_e,
                        ),
                    )
                )
            rows = (
                q.order_by(
                    desc(BrowserInspectionWatchEventRow.created_at),
                    desc(BrowserInspectionWatchEventRow.event_id),
                )
                .limit(limit + 1)
                .all()
            )
        has_more = len(rows) > limit
        rows = rows[:limit]
        out: List[Dict[str, Any]] = []
        for r in rows:
            out.append(
                {
                    "event_id": r.event_id,
                    "event_type": getattr(r, "event_type", None) or "diff_generated",
                    "created_at": r.created_at,
                    "target_inspection_id": None
                    if r.target_inspection_id == _SENTINEL_NO_INSPECTION
                    else r.target_inspection_id,
                    "base_inspection_id": r.base_inspection_id,
                    "change_level": r.change_level,
                    "summary": (r.summary or "")[:2000],
                    "alert_triggered": bool(r.alert_triggered),
                }
            )
        next_cur: Optional[str] = None
        if has_more and rows:
            last = rows[-1]
            next_cur = _encode_event_cursor(str(last.created_at), str(last.event_id))
        return out, next_cur

    def aggregate_metrics(self, watch_id: str) -> Dict[str, Any]:
        wid = (watch_id or "").strip()
        if not wid:
            return {}
        with get_session() as s:
            counts_rows = s.execute(
                text(
                    "SELECT event_type, COUNT(*) FROM browser_inspection_watch_events "
                    "WHERE watch_id = :wid GROUP BY event_type"
                ),
                {"wid": wid},
            ).fetchall()
            counts: Dict[str, int] = {str(r[0]): int(r[1]) for r in counts_rows if r[0] is not None}

            ar = s.execute(
                text(
                    "SELECT COUNT(*) FROM browser_inspection_watch_events "
                    "WHERE watch_id = :wid AND event_type = 'diff_generated' AND alert_triggered = 1"
                ),
                {"wid": wid},
            ).scalar()
            alerts_triggered = int(ar or 0)

            last_row = s.execute(
                text(
                    "SELECT change_level, visual_meta_json, created_at FROM browser_inspection_watch_events "
                    "WHERE watch_id = :wid AND event_type = 'diff_generated' "
                    "ORDER BY created_at DESC, event_id DESC LIMIT 1"
                ),
                {"wid": wid},
            ).fetchone()

            last_ev = s.execute(
                text("SELECT MAX(created_at) FROM browser_inspection_watch_events WHERE watch_id = :wid"),
                {"wid": wid},
            ).scalar()

            last_alert_row = s.execute(
                text(
                    "SELECT created_at FROM browser_inspection_watch_events "
                    "WHERE watch_id = :wid AND event_type = 'diff_generated' AND alert_triggered = 1 "
                    "ORDER BY created_at DESC, event_id DESC LIMIT 1"
                ),
                {"wid": wid},
            ).fetchone()

        last_diff_level: Optional[str] = None
        last_visual: Optional[str] = None
        if last_row:
            last_diff_level = str(last_row[0]) if last_row[0] is not None else None
            if last_row[1]:
                try:
                    vm = json.loads(last_row[1])
                    if isinstance(vm, dict) and vm.get("visual_change_level"):
                        last_visual = str(vm.get("visual_change_level"))
                except Exception:
                    pass

        last_alert_ev_at: Optional[str] = None
        if last_alert_row and last_alert_row[0]:
            last_alert_ev_at = str(last_alert_row[0])

        return {
            "counts": counts,
            "alerts_triggered": alerts_triggered,
            "last_diff_change_level": last_diff_level,
            "last_visual_change_level": last_visual,
            "last_event_at": str(last_ev) if last_ev else None,
            "last_alert_from_diff_at": last_alert_ev_at,
        }

    def _row_watch_to_dict(self, row: BrowserInspectionWatchRow) -> Dict[str, Any]:
        lec = getattr(row, "last_effective_change_level", None)
        return {
            "watch_id": row.watch_id,
            "url": row.url,
            "project_id": row.project_id,
            "interval_minutes": int(row.interval_minutes or 60),
            "change_threshold": row.change_threshold,
            "enabled": bool(row.enabled),
            "execution_mode": row.execution_mode,
            "compare_mode": getattr(row, "compare_mode", None) or "last",
            "baseline_inspection_id": getattr(row, "baseline_inspection_id", None),
            "baseline_set_at": getattr(row, "baseline_set_at", None),
            "baseline_updated_by": getattr(row, "baseline_updated_by", None),
            "last_status": getattr(row, "last_status", None) or "never_run",
            "current_status": getattr(row, "last_status", None) or "never_run",
            "last_effective_change_level": lec,
            "last_change_level": lec,
            "last_visual_change_level": getattr(row, "last_visual_change_level", None),
            "last_alert_at": getattr(row, "last_alert_at", None),
            "last_run_error": getattr(row, "last_run_error", None),
            "created_at": row.created_at,
            "updated_at": row.updated_at,
            "last_run_at": row.last_run_at,
            "last_inspection_id": row.last_inspection_id,
            "last_diff_id": row.last_diff_id,
        }


browser_inspection_watch_repo = BrowserInspectionWatchRepository()
