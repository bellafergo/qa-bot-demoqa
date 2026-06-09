# services/incident_timeline_service.py
"""Build chronological incident timeline from real QA sources (deterministic)."""
from __future__ import annotations

from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional, Set

from models.incident_models import IncidentTimelineEvent, RelatedPRAnalysisSummary, RelatedRunSummary


def _parse_iso(ts: Optional[str]) -> Optional[datetime]:
    if not ts or not str(ts).strip():
        return None
    s = str(ts).strip().replace("Z", "+00:00")
    try:
        dt = datetime.fromisoformat(s)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return dt
    except Exception:
        return None


def _within_window(ts: Optional[str], *, cutoff: datetime) -> bool:
    dt = _parse_iso(ts)
    if dt is None:
        return False
    return dt >= cutoff


def gather_browser_watch_events(
    project_id: str,
    *,
    time_window_hours: int,
    limit: int = 30,
) -> List[Dict[str, Any]]:
    """Collect Browser Watch alert events for the project within the time window."""
    try:
        from services.browser_inspection_watch_service import list_watches, list_watch_events

        pid = (project_id or "").strip()
        cutoff = datetime.now(timezone.utc) - timedelta(hours=max(1, int(time_window_hours)))
        watches = list_watches(project_id=pid, limit=20)
        out: List[Dict[str, Any]] = []
        for w in watches:
            wid = getattr(w, "watch_id", None) or getattr(w, "id", None)
            if not wid:
                continue
            events = list_watch_events(str(wid), limit=limit)
            for ev in events:
                if not getattr(ev, "alert_triggered", False):
                    continue
                ts = getattr(ev, "created_at", None)
                if not _within_window(ts, cutoff=cutoff):
                    continue
                out.append({
                    "timestamp": ts,
                    "event_type": getattr(ev, "event_type", "alert"),
                    "summary": getattr(ev, "summary", "") or "Browser Watch alert",
                    "watch_id": str(wid),
                    "change_level": getattr(ev, "change_level", None),
                })
        return out
    except Exception:
        return []


def build_incident_timeline(
    *,
    time_window_hours: int,
    related_runs: List[RelatedRunSummary],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    browser_events: List[Dict[str, Any]],
    incident_reported_at: str,
    clusters: Optional[List[Dict[str, Any]]] = None,
) -> List[IncidentTimelineEvent]:
    cutoff = datetime.now(timezone.utc) - timedelta(hours=max(1, int(time_window_hours)))
    events: List[IncidentTimelineEvent] = []
    seen: Set[str] = set()

    for pr in related_pr_analysis:
        ts = pr.analyzed_at or ""
        if ts and not _within_window(ts, cutoff=cutoff):
            continue
        key = f"pr:{pr.provider}:{pr.pr_number}:{ts}"
        if key in seen:
            continue
        seen.add(key)
        events.append(IncidentTimelineEvent(
            timestamp=ts or incident_reported_at,
            event_type="pr_analyzed",
            title=f"PR #{pr.pr_number} analyzed ({pr.pr_risk_score:.0f}/100)",
            details=f"Risk level {pr.risk_level}. {pr.reason}".strip(),
            source=f"pr_analysis:{pr.provider}",
        ))

    for r in related_runs:
        ts = r.started_at or ""
        if ts and not _within_window(ts, cutoff=cutoff):
            continue
        key = f"run:{r.run_id}"
        if key in seen:
            continue
        seen.add(key)
        err = r.error_summary or r.rca_summary or r.status
        events.append(IncidentTimelineEvent(
            timestamp=ts or incident_reported_at,
            event_type="run_failed",
            title=f"Run failed: {r.test_name or r.test_id}",
            details=err or "",
            source=f"run:{r.run_id}",
        ))

    for ev in browser_events:
        ts = str(ev.get("timestamp") or "")
        if ts and not _within_window(ts, cutoff=cutoff):
            continue
        key = f"watch:{ev.get('watch_id')}:{ts}"
        if key in seen:
            continue
        seen.add(key)
        events.append(IncidentTimelineEvent(
            timestamp=ts or incident_reported_at,
            event_type="browser_watch_alert",
            title="Browser Watch alert",
            details=str(ev.get("summary") or ""),
            source=f"browser_watch:{ev.get('watch_id', '')}",
        ))

    for c in (clusters or [])[:3]:
        mod = str(c.get("module") or "")
        fails = int(c.get("total_failures") or 0)
        if not mod or fails <= 0:
            continue
        key = f"cluster:{mod}"
        if key in seen:
            continue
        seen.add(key)
        events.append(IncidentTimelineEvent(
            timestamp=incident_reported_at,
            event_type="failure_cluster",
            title=f"Failure cluster in {mod}",
            details=f"{fails} recent failure(s), category {c.get('root_cause_category', 'unknown')}",
            source="failure_intelligence",
        ))

    events.append(IncidentTimelineEvent(
        timestamp=incident_reported_at,
        event_type="incident_reported",
        title="Incident reported",
        details="User submitted incident for investigation",
        source="incident_investigator",
    ))

    def _sort_key(e: IncidentTimelineEvent) -> datetime:
        dt = _parse_iso(e.timestamp)
        return dt or datetime.min.replace(tzinfo=timezone.utc)

    events.sort(key=_sort_key)
    return events
