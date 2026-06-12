# services/endpoint_timing_service.py
"""In-memory endpoint timing aggregation for slow-endpoint reporting."""
from __future__ import annotations

import logging
import threading
import time
from collections import defaultdict, deque
from datetime import datetime, timezone
from typing import Any, Deque, Dict, List, Optional

from models.performance_models import EndpointTimingRecord, SlowEndpointStat, SlowEndpointsReport

logger = logging.getLogger("vanya.endpoint_timing")

_MAX_RECORDS = 5000
_lock = threading.Lock()
_records: Deque[EndpointTimingRecord] = deque(maxlen=_MAX_RECORDS)


def _utc_now() -> datetime:
    return datetime.now(timezone.utc)


def record_endpoint_timing(
    *,
    endpoint: str,
    method: str,
    duration_ms: float,
    status_code: int = 200,
    request_id: Optional[str] = None,
) -> None:
    rec = EndpointTimingRecord(
        endpoint=(endpoint or "").strip() or "/",
        method=(method or "GET").upper(),
        duration_ms=float(duration_ms),
        status_code=int(status_code),
        timestamp=_utc_now(),
        request_id=(request_id or "").strip() or None,
    )
    with _lock:
        _records.append(rec)


def build_slow_endpoints_report(*, limit: int = 10) -> SlowEndpointsReport:
    limit = max(1, min(int(limit), 50))
    with _lock:
        snapshot = list(_records)

    if not snapshot:
        return SlowEndpointsReport(total_requests=0, endpoints=[])

    grouped: Dict[str, Dict[str, Any]] = defaultdict(
        lambda: {
            "count": 0,
            "total_ms": 0.0,
            "max_ms": 0.0,
            "latest_at": None,
        }
    )

    for rec in snapshot:
        key = f"{rec.method} {rec.endpoint}"
        bucket = grouped[key]
        bucket["count"] += 1
        bucket["total_ms"] += rec.duration_ms
        bucket["max_ms"] = max(bucket["max_ms"], rec.duration_ms)
        bucket["latest_at"] = rec.timestamp
        bucket["method"] = rec.method
        bucket["endpoint"] = rec.endpoint

    ranked: List[Dict[str, Any]] = []
    for key, bucket in grouped.items():
        count = bucket["count"]
        avg_ms = round(bucket["total_ms"] / count, 2) if count else 0.0
        ranked.append({
            "method": bucket["method"],
            "endpoint": bucket["endpoint"],
            "request_count": count,
            "avg_duration_ms": avg_ms,
            "max_duration_ms": round(bucket["max_ms"], 2),
            "latest_at": bucket["latest_at"],
        })

    ranked.sort(key=lambda row: (row["avg_duration_ms"], row["max_duration_ms"]), reverse=True)

    top = [
        SlowEndpointStat(
            method=row["method"],
            endpoint=row["endpoint"],
            request_count=row["request_count"],
            avg_duration_ms=row["avg_duration_ms"],
            max_duration_ms=row["max_duration_ms"],
            latest_at=row["latest_at"],
        )
        for row in ranked[:limit]
    ]

    return SlowEndpointsReport(
        total_requests=len(snapshot),
        endpoints=top,
        generated_at=_utc_now(),
    )


def reset_endpoint_timings() -> None:
    """Test helper."""
    with _lock:
        _records.clear()
