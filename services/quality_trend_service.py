# services/quality_trend_service.py
"""
Observability OBS-01B — Quality Trends (read-only).

Historical quality evolution from stored incident reports and current health scores.
No new persistence, execution, external calls, or schedulers.
"""
from __future__ import annotations

import hashlib
import re
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

from models.incident_models import (
    ExecutiveQualityReport,
    HistoricalLearningReport,
    QualityHealthReport,
    QualityTrend,
    QualityTrendPoint,
    QualityTrendReport,
)

_SCOPE_TYPES = ("project", "environment", "module", "journey")
_MAX_HISTORY = 12


def build_trend_id(scope_type: str, scope_name: str) -> str:
    type_slug = re.sub(r"[^a-z0-9]+", "_", scope_type.strip().lower()).strip("_")
    name_slug = re.sub(r"[^a-z0-9]+", "_", scope_name.strip().lower()).strip("_")
    return f"quality_trend:{type_slug}:{name_slug}"


def _status_from_score(score: int) -> str:
    if score >= 90:
        return "EXCELLENT"
    if score >= 75:
        return "GOOD"
    if score >= 50:
        return "ATTENTION"
    return "HIGH_RISK"


def _direction(score_change: int) -> str:
    if score_change >= 3:
        return "IMPROVING"
    if score_change <= -3:
        return "DEGRADING"
    if score_change == 0:
        return "STABLE"
    return "STABLE"


def _trend_confidence(points: List[QualityTrendPoint], base: float = 0.55) -> float:
    if len(points) < 2:
        return 0.0
    n_boost = min(0.25, (len(points) - 2) * 0.05)
    scores = [p.score for p in points]
    deltas = [scores[i + 1] - scores[i] for i in range(len(scores) - 1)]
    if not deltas:
        return round(min(0.95, base + n_boost), 2)
    same_sign = all(d >= 0 for d in deltas) or all(d <= 0 for d in deltas)
    consistency_boost = 0.12 if same_sign else 0.04
    return round(min(0.95, base + n_boost + consistency_boost), 2)


def _load_historical_reports(project_id: str, limit: int = _MAX_HISTORY) -> List[Dict[str, Any]]:
    from services.db.incident_report_repository import incident_report_repo

    rows = incident_report_repo.list_reports(project_id=project_id, limit=limit)
    reports: List[Dict[str, Any]] = []
    for row in rows:
        report_id = str(row.get("id") or "").strip()
        if not report_id:
            continue
        full = incident_report_repo.get(report_id)
        if full:
            reports.append(full)
    reports.sort(key=lambda r: str(r.get("created_at") or ""))
    return reports


def _scope_scores_from_report(report: Dict[str, Any]) -> Dict[str, Tuple[int, str]]:
    scores: Dict[str, Tuple[int, str]] = {}
    qh = report.get("quality_health")
    if isinstance(qh, dict) and qh.get("overall_score") is not None:
        for item in qh.get("scores") or []:
            scope_type = str(item.get("scope_type") or "").strip()
            if scope_type not in _SCOPE_TYPES:
                continue
            scope_name = str(item.get("scope_name") or scope_type).strip()
            key = f"{scope_type}:{scope_name}"
            scores[key] = (int(item.get("score") or 0), str(item.get("status") or _status_from_score(int(item.get("score") or 0))))
        scores["project:Project"] = (
            int(qh["overall_score"]),
            str(qh.get("overall_status") or _status_from_score(int(qh["overall_score"]))),
        )
        return scores

    eqr = report.get("executive_quality_report")
    if isinstance(eqr, dict) and eqr.get("overall_quality_score") is not None:
        score = int(eqr["overall_quality_score"])
        scores["project:Project"] = (score, _status_from_score(score))
    return scores


def _snapshot_series(
    reports: List[Dict[str, Any]],
    *,
    current_quality_health: Optional[QualityHealthReport] = None,
    current_timestamp: Optional[str] = None,
) -> Dict[str, List[QualityTrendPoint]]:
    series: Dict[str, List[QualityTrendPoint]] = {}

    for report in reports:
        ts = str(report.get("created_at") or "").strip()
        if not ts:
            continue
        for key, (score, status) in _scope_scores_from_report(report).items():
            series.setdefault(key, []).append(
                QualityTrendPoint(timestamp=ts, score=score, status=status)
            )

    if current_quality_health:
        ts = current_timestamp or datetime.now(timezone.utc).isoformat()
        series.setdefault("project:Project", []).append(
            QualityTrendPoint(
                timestamp=ts,
                score=current_quality_health.overall_score,
                status=current_quality_health.overall_status,
            )
        )
        for item in current_quality_health.scores:
            if item.scope_type not in _SCOPE_TYPES:
                continue
            key = f"{item.scope_type}:{item.scope_name}"
            series.setdefault(key, []).append(
                QualityTrendPoint(timestamp=ts, score=item.score, status=item.status)
            )

    for key, points in series.items():
        deduped: List[QualityTrendPoint] = []
        seen_ts: set[str] = set()
        for point in sorted(points, key=lambda p: p.timestamp):
            if point.timestamp in seen_ts:
                continue
            seen_ts.add(point.timestamp)
            deduped.append(point)
        series[key] = deduped[-_MAX_HISTORY:]
    return series


def _build_trend(scope_type: str, scope_name: str, points: List[QualityTrendPoint]) -> Optional[QualityTrend]:
    if len(points) < 2:
        return None
    score_change = points[-1].score - points[0].score
    direction = _direction(score_change)
    return QualityTrend(
        trend_id=build_trend_id(scope_type, scope_name),
        scope_type=scope_type,
        scope_name=scope_name,
        trend_direction=direction,
        score_change=score_change,
        confidence=_trend_confidence(points),
        points=points,
    )


def _overall_from_trends(trends: List[QualityTrend]) -> str:
    project = next((t for t in trends if t.scope_type == "project"), None)
    if project:
        return project.trend_direction
    if not trends:
        return "UNKNOWN"
    directions = [t.trend_direction for t in trends]
    if directions.count("DEGRADING") > directions.count("IMPROVING"):
        return "DEGRADING"
    if directions.count("IMPROVING") > directions.count("DEGRADING"):
        return "IMPROVING"
    return "STABLE"


def build_quality_trend_report(
    project_id: str,
    *,
    quality_health: Optional[QualityHealthReport] = None,
    executive_quality_report: Optional[ExecutiveQualityReport] = None,
    historical_learning: Optional[HistoricalLearningReport] = None,
) -> Optional[QualityTrendReport]:
    pid = (project_id or "").strip().lower()
    if not pid:
        return None

    reports = _load_historical_reports(pid)
    if not reports and not quality_health and not executive_quality_report:
        return None

    series = _snapshot_series(reports, current_quality_health=quality_health)

    if not any(len(points) >= 2 for points in series.values()):
        if quality_health and executive_quality_report:
            fallback_score = quality_health.overall_score or executive_quality_report.overall_quality_score
            eq_ts = executive_quality_report.generated_at or datetime.now(timezone.utc).isoformat()
            series["project:Project"] = [
                QualityTrendPoint(
                    timestamp=eq_ts,
                    score=int(executive_quality_report.overall_quality_score),
                    status=_status_from_score(int(executive_quality_report.overall_quality_score)),
                ),
                QualityTrendPoint(
                    timestamp=datetime.now(timezone.utc).isoformat(),
                    score=int(fallback_score),
                    status=quality_health.overall_status,
                ),
            ]
        elif quality_health and len(reports) == 1:
            hist = reports[0]
            hist_scores = _scope_scores_from_report(hist)
            if hist_scores.get("project:Project") and quality_health.overall_score is not None:
                hist_score, hist_status = hist_scores["project:Project"]
                series["project:Project"] = [
                    QualityTrendPoint(timestamp=str(hist.get("created_at") or ""), score=hist_score, status=hist_status),
                    QualityTrendPoint(
                        timestamp=datetime.now(timezone.utc).isoformat(),
                        score=quality_health.overall_score,
                        status=quality_health.overall_status,
                    ),
                ]

    trends: List[QualityTrend] = []
    for key, points in sorted(series.items()):
        if len(points) < 2:
            continue
        scope_type, scope_name = key.split(":", 1)
        if scope_type not in _SCOPE_TYPES:
            continue
        trend = _build_trend(scope_type, scope_name, points)
        if trend:
            trends.append(trend)

    if not trends:
        return None

    trends.sort(key=lambda t: (0 if t.scope_type == "project" else 1, t.scope_name))
    overall_trend = _overall_from_trends(trends)
    confidences = [t.confidence for t in trends if t.confidence > 0]
    confidence = round(sum(confidences) / len(confidences), 2) if confidences else 0.55

    if historical_learning and historical_learning.similar_incidents:
        confidence = round(min(0.95, confidence + 0.05), 2)

    degrading = [t for t in trends if t.trend_direction == "DEGRADING"]
    improving = [t for t in trends if t.trend_direction == "IMPROVING"]
    digest = hashlib.sha256("|".join(t.trend_id for t in trends).encode("utf-8")).hexdigest()[:8]

    if overall_trend == "DEGRADING" and degrading:
        names = ", ".join(t.scope_name for t in degrading[:3])
        summary = f"Overall quality trend is DEGRADING. Downward areas: {names}. Ref: {digest}."
    elif overall_trend == "IMPROVING" and improving:
        names = ", ".join(t.scope_name for t in improving[:3])
        summary = f"Overall quality trend is IMPROVING. Upward areas: {names}. Ref: {digest}."
    else:
        summary = f"Overall quality trend is {overall_trend} across {len(trends)} tracked scope(s). Ref: {digest}."

    return QualityTrendReport(
        overall_trend=overall_trend,
        confidence=confidence,
        trends=trends,
        summary=summary,
    )
