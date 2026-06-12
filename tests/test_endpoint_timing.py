# tests/test_endpoint_timing.py
"""V1.2 — Endpoint timing instrumentation."""
from __future__ import annotations

from services.endpoint_timing_service import (
    build_slow_endpoints_report,
    record_endpoint_timing,
    reset_endpoint_timings,
)


def test_slow_endpoints_report_ranks_by_avg_duration():
    reset_endpoint_timings()
    record_endpoint_timing(endpoint="/dashboard/summary", method="GET", duration_ms=120.0)
    record_endpoint_timing(endpoint="/dashboard/summary", method="GET", duration_ms=180.0)
    record_endpoint_timing(endpoint="/projects/x/value-dashboard", method="GET", duration_ms=400.0)

    report = build_slow_endpoints_report(limit=10)
    assert report.total_requests == 3
    assert len(report.endpoints) >= 2
    assert report.endpoints[0].endpoint == "/projects/x/value-dashboard"
    assert report.endpoints[0].avg_duration_ms == 400.0

    top_paths = {row.endpoint for row in report.endpoints}
    assert "/dashboard/summary" in top_paths
