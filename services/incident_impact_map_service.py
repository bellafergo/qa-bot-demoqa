# services/incident_impact_map_service.py
"""
Incident Investigator II-03C — Impact Map (read-only).

Deterministic aggregation of impacted areas from existing incident evidence.
No scanners, LLM calls, execution paths, or background jobs.
"""
from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any, Dict, List, Literal, Optional, Set, Tuple

from models.incident_models import (
    BlastRadiusModule,
    CorrelatedEvidence,
    EvidenceCorrelationSummary,
    IncidentHypothesis,
    IncidentImpactNode,
    IncidentStorylineStep,
    IncidentTimelineEvent,
    RelatedPRAnalysisSummary,
    RelatedRunSummary,
    TemporalCorrelationSummary,
)

ImpactSeverity = Literal["high", "medium", "low"]

_SEVERITY_RANK = {"high": 0, "medium": 1, "low": 2}

_AREA_SUFFIXES = (" flow", " module", " page", " suite", " component", " ui")

_NAVIGABLE_TYPES = frozenset({"run", "pr_analysis", "browser_watch", "failure_cluster"})


def _normalize_area_key(name: str) -> str:
    s = (name or "").strip().lower()
    for suffix in _AREA_SUFFIXES:
        if s.endswith(suffix):
            s = s[: -len(suffix)].strip()
    s = re.sub(r"\s+", " ", s)
    return s or ""


def _display_title(name: str) -> str:
    s = (name or "").strip()
    if not s:
        return "Unknown"
    if s.isupper() and len(s) <= 6:
        return s
    parts = re.split(r"[\s_-]+", s)
    return " ".join(p.capitalize() if p else "" for p in parts if p)


def _extract_area_from_text(text: str) -> Optional[str]:
    t = (text or "").strip()
    if not t:
        return None
    patterns = [
        r"^([A-Za-z][A-Za-z0-9 _-]{1,40}?)\s+UI\b",
        r"module\s+['\"]?([a-zA-Z0-9 _-]+)",
        r"same module affected:\s*(\w+)",
        r"impacted module[s]?\s+([a-zA-Z0-9 _-]+)",
        r"cluster in\s+([a-zA-Z0-9 _-]+)",
        r"failure cluster in\s+([a-zA-Z0-9 _-]+)",
    ]
    for pat in patterns:
        m = re.search(pat, t, re.I)
        if m:
            return m.group(1).strip()
    return None


def _watch_id_to_area(watch_id: str) -> Optional[str]:
    wid = (watch_id or "").strip()
    if not wid:
        return None
    if wid.isupper() and "_" in wid:
        return _display_title(wid.replace("_", " "))
    return _display_title(wid)


@dataclass
class _ImpactSignal:
    source: str
    confidence: float
    entity_type: Optional[str] = None
    entity_id: Optional[str] = None


@dataclass
class _AreaAccumulator:
    display_title: str
    signals: List[_ImpactSignal] = field(default_factory=list)
    max_correlation_confidence: float = 0.0
    max_hypothesis_confidence: float = 0.0
    has_failure_cluster: bool = False
    has_failed_run: bool = False
    has_browser_alert: bool = False

    def add(
        self,
        *,
        source: str,
        confidence: float,
        entity_type: Optional[str] = None,
        entity_id: Optional[str] = None,
    ) -> None:
        conf = max(0.0, min(1.0, float(confidence)))
        self.signals.append(
            _ImpactSignal(
                source=source,
                confidence=conf,
                entity_type=entity_type,
                entity_id=entity_id,
            )
        )
        if source in ("failure_cluster",):
            self.has_failure_cluster = True
        if source in ("failed_run", "run", "api_evidence"):
            self.has_failed_run = True
        if source in ("browser_watch", "browser_probe"):
            self.has_browser_alert = True


def _pick_display_title(existing: str, candidate: str) -> str:
    ex = (existing or "").strip()
    cand = (candidate or "").strip()
    if not ex:
        return _display_title(cand)
    if not cand:
        return _display_title(ex)
    if len(cand) > len(ex) and _normalize_area_key(cand) == _normalize_area_key(ex):
        return _display_title(cand)
    return _display_title(ex)


def _add_area(
    pool: Dict[str, _AreaAccumulator],
    area_name: str,
    *,
    source: str,
    confidence: float,
    entity_type: Optional[str] = None,
    entity_id: Optional[str] = None,
) -> None:
    key = _normalize_area_key(area_name)
    if not key or key == "unknown":
        return
    acc = pool.get(key)
    if acc is None:
        acc = _AreaAccumulator(display_title=_display_title(area_name))
        pool[key] = acc
    else:
        acc.display_title = _pick_display_title(acc.display_title, area_name)
    acc.add(
        source=source,
        confidence=confidence,
        entity_type=entity_type,
        entity_id=entity_id,
    )


def _confidence_for_area(acc: _AreaAccumulator) -> float:
    count = len(acc.signals)
    strongest = max((s.confidence for s in acc.signals), default=0.0)
    score = max(0.35 + 0.04 * count, strongest * 0.92)
    if acc.max_hypothesis_confidence > 0:
        score = max(score, acc.max_hypothesis_confidence * 0.85)
    if acc.max_correlation_confidence > 0:
        score = max(score, acc.max_correlation_confidence * 0.9)
    if acc.has_failure_cluster:
        score += 0.05
    if acc.has_failed_run and count >= 2:
        score += 0.04
    if acc.has_browser_alert:
        score += 0.02
    if count >= 5:
        score += 0.03
    return round(min(1.0, score), 2)


def _severity_for_area(acc: _AreaAccumulator, confidence: float) -> ImpactSeverity:
    count = len(acc.signals)
    if confidence >= 0.75 or count >= 8 or (acc.has_failure_cluster and acc.has_failed_run):
        return "high"
    if confidence >= 0.5 or count >= 3:
        return "medium"
    return "low"


def _description_for_area(title: str, severity: ImpactSeverity, count: int) -> str:
    if severity == "high" or count >= 8:
        return f"Multiple signals point to {title} as an impacted area."
    if severity == "medium" or count >= 3:
        return f"Several incident evidence signals appear related to {title}."
    return f"Some incident evidence appears related to {title}."


def _best_drilldown(acc: _AreaAccumulator) -> Tuple[Optional[str], Optional[str]]:
    navigable = [
        s for s in acc.signals
        if s.entity_type in _NAVIGABLE_TYPES and (s.entity_id or "").strip()
    ]
    if not navigable:
        return None, None
    best = max(navigable, key=lambda s: (s.confidence, s.source))
    return best.entity_type, best.entity_id


def _link_hypothesis_confidence(
    pool: Dict[str, _AreaAccumulator],
    hypotheses: List[IncidentHypothesis],
) -> None:
    for h in hypotheses:
        conf = float(h.confidence)
        refs = " ".join(h.supporting_refs or [])
        statement = h.statement or ""
        for key, acc in pool.items():
            title_low = acc.display_title.lower()
            if title_low in statement.lower() or title_low in refs.lower():
                acc.max_hypothesis_confidence = max(acc.max_hypothesis_confidence, conf)
            if key in _normalize_area_key(statement):
                acc.max_hypothesis_confidence = max(acc.max_hypothesis_confidence, conf)


def build_incident_impact_map(
    *,
    hypotheses: List[IncidentHypothesis],
    evidence_correlation: Optional[EvidenceCorrelationSummary],
    related_runs: List[RelatedRunSummary],
    related_pr_analysis: List[RelatedPRAnalysisSummary],
    browser_events: List[Dict[str, Any]],
    clusters: List[Dict[str, Any]],
    timeline: List[IncidentTimelineEvent],
    storyline: List[IncidentStorylineStep],
    impacted_modules: List[str],
    impacted_modules_ranked: List[BlastRadiusModule],
    temporal_correlation: Optional[TemporalCorrelationSummary] = None,
    meta: Optional[Dict[str, Any]] = None,
    limit: int = 12,
) -> List[IncidentImpactNode]:
    """Build read-only impact map nodes from gathered investigation signals."""
    pool: Dict[str, _AreaAccumulator] = {}

    for mod in impacted_modules:
        _add_area(pool, mod, source="impacted_module", confidence=0.65)

    for ranked in impacted_modules_ranked:
        conf = min(1.0, float(ranked.score) / 100.0)
        _add_area(pool, ranked.module, source="blast_radius", confidence=max(0.55, conf))

    for pra in related_pr_analysis:
        entity_id = f"{pra.provider}:{pra.pr_number}"
        for mod in pra.impacted_modules or []:
            _add_area(
                pool,
                mod,
                source="pr_analysis",
                confidence=min(0.95, 0.55 + float(pra.pr_risk_score) / 200.0),
                entity_type="pr_analysis",
                entity_id=entity_id,
            )
        extracted = _extract_area_from_text(pra.reason)
        if extracted:
            _add_area(
                pool,
                extracted,
                source="pr_analysis",
                confidence=min(0.9, 0.5 + float(pra.pr_risk_score) / 200.0),
                entity_type="pr_analysis",
                entity_id=entity_id,
            )

    for run in related_runs:
        mod = (run.module or "").strip()
        label = mod or run.test_name or run.test_id
        if not label:
            continue
        conf = 0.78 if run.error_summary else 0.68
        _add_area(
            pool,
            label,
            source="failed_run",
            confidence=conf,
            entity_type="run",
            entity_id=run.run_id,
        )

    for cluster in clusters:
        mod = str(cluster.get("module") or "").strip()
        if not mod:
            continue
        fails = int(cluster.get("total_failures") or 0)
        cid = str(cluster.get("cluster_id") or mod).strip()
        _add_area(
            pool,
            mod,
            source="failure_cluster",
            confidence=min(0.95, 0.6 + fails * 0.04),
            entity_type="failure_cluster",
            entity_id=cid,
        )

    for ev in browser_events:
        summary = str(ev.get("summary") or "")
        watch_id = str(ev.get("watch_id") or "").strip()
        area = _extract_area_from_text(summary) or _watch_id_to_area(watch_id)
        if not area:
            continue
        _add_area(
            pool,
            area,
            source="browser_watch",
            confidence=0.74,
            entity_type="browser_watch" if watch_id else None,
            entity_id=watch_id or None,
        )

    for item in (evidence_correlation.evidence if evidence_correlation else []):
        conf = float(item.confidence)
        mods: Set[str] = set()
        for text in (item.detail, item.reason, item.title):
            extracted = _extract_area_from_text(text or "")
            if extracted:
                mods.add(extracted)
        for mod in mods:
            key = _normalize_area_key(mod)
            if key in pool:
                pool[key].max_correlation_confidence = max(pool[key].max_correlation_confidence, conf)
            _add_area(
                pool,
                mod,
                source=item.source,
                confidence=conf,
                entity_type=item.related_entity_type,
                entity_id=item.related_entity_id,
            )

    for step in storyline:
        if step.related_entity_type == "conclusion":
            continue
        for text in (step.title, step.description):
            extracted = _extract_area_from_text(text or "")
            if extracted:
                _add_area(
                    pool,
                    extracted,
                    source="storyline",
                    confidence=float(step.confidence),
                    entity_type=step.related_entity_type,
                    entity_id=step.related_entity_id,
                )

    for ev in timeline:
        if ev.event_type == "incident_reported":
            continue
        extracted = _extract_area_from_text(f"{ev.title} {ev.details}")
        if extracted:
            entity_type: Optional[str] = None
            entity_id: Optional[str] = None
            src = (ev.source or "").strip()
            if src.startswith("run:"):
                entity_type, entity_id = "run", src[4:].strip()
            elif src.startswith("browser_watch:"):
                entity_type, entity_id = "browser_watch", src.split(":", 1)[-1].strip()
            _add_area(
                pool,
                extracted,
                source=ev.event_type,
                confidence=0.7,
                entity_type=entity_type,
                entity_id=entity_id,
            )

    for hint in (meta or {}).get("topic_hints") or []:
        if isinstance(hint, str) and hint.strip():
            _add_area(pool, hint.strip(), source="topic_hint", confidence=0.55)

    if temporal_correlation and temporal_correlation.signal in ("strong", "medium"):
        for key, acc in pool.items():
            if len(acc.signals) >= 2:
                acc.max_correlation_confidence = max(acc.max_correlation_confidence, 0.72)

    if not pool:
        return []

    _link_hypothesis_confidence(pool, hypotheses)

    nodes: List[IncidentImpactNode] = []
    for acc in pool.values():
        if not acc.signals:
            continue
        confidence = _confidence_for_area(acc)
        if confidence < 0.45 and len(acc.signals) < 2:
            continue
        severity = _severity_for_area(acc, confidence)
        entity_type, entity_id = _best_drilldown(acc)
        nodes.append(
            IncidentImpactNode(
                title=acc.display_title,
                description=_description_for_area(acc.display_title, severity, len(acc.signals)),
                severity=severity,
                confidence=confidence,
                related_entity_count=len(acc.signals),
                related_entity_type=entity_type,
                related_entity_id=entity_id,
            )
        )

    if not nodes:
        return []

    nodes.sort(
        key=lambda n: (
            _SEVERITY_RANK.get(n.severity, 9),
            -n.confidence,
            -n.related_entity_count,
            n.title.lower(),
        )
    )
    return nodes[: max(1, int(limit))]
