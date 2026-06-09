# services/incident_qa_context_service.py
"""
Gather real QA context for project-scoped incident investigation.

Reuses (read-only):
  - run_history_service
  - failure_intelligence_service
  - project_knowledge_service
  - github_integration_service / azure_devops_integration_service (optional PRs)
"""
from __future__ import annotations

import logging
import re
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, List, Optional, Set, Tuple

from models.incident_models import RelatedEvidenceSummary, RelatedPRAnalysisSummary, RelatedPRSummary, RelatedRunSummary

logger = logging.getLogger("vanya.incident_qa_context")

_MODULE_KEYWORDS: List[Tuple[re.Pattern[str], str]] = [
    (re.compile(r"\b(login|sign[\s-]?in|credenciales|autentic|auth)\b", re.I), "auth"),
    (re.compile(r"\b(dashboard|panel)\b", re.I), "dashboard"),
    (re.compile(r"\b(candidat|vacante|vacancy|job)\b", re.I), "candidates"),
    (re.compile(r"\b(cat[aá]logo|catalog)\b", re.I), "catalog"),
    (re.compile(r"\b(api|endpoint|backend)\b", re.I), "api"),
    (re.compile(r"\b(deploy|release|merge)\b", re.I), "deploy"),
]


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
        return True
    return dt >= cutoff


def extract_topic_hints(description: str, *, module: Optional[str] = None) -> Set[str]:
    hints: Set[str] = set()
    desc = description or ""
    mod = (module or "").strip()
    if mod:
        hints.add(mod.lower())
    for pattern, label in _MODULE_KEYWORDS:
        if pattern.search(desc):
            hints.add(label)
    tokens = re.findall(r"[a-zA-ZáéíóúÁÉÍÓÚñÑ]{4,}", desc.lower())
    for tok in tokens[:12]:
        if tok not in ("after", "before", "fails", "falla", "error", "deploy"):
            hints.add(tok)
    return hints


def _text_matches_hints(text: str, hints: Set[str]) -> bool:
    if not hints:
        return False
    low = (text or "").lower()
    return any(h in low for h in hints if len(h) >= 3)


def _catalog_module_lookup(project_id: str) -> Dict[str, str]:
    """test_case_id → module from catalog (read-only)."""
    pid = (project_id or "").strip().lower()
    if not pid:
        return {}
    try:
        from services.db.catalog_repository import catalog_repo

        return {tc_id: mod for tc_id, mod in catalog_repo.all_modules_for_project(pid)}
    except Exception as e:
        logger.debug("incident_qa: catalog module lookup failed: %s", e)
        return {}


def gather_failed_runs(
    project_id: str,
    *,
    time_window_hours: int,
    limit: int = 100,
) -> List[RelatedRunSummary]:
    from services.run_history_service import run_history_service
    from services.run_mapper import normalize_storage_status

    pid = (project_id or "").strip().lower()
    cutoff = datetime.now(timezone.utc) - timedelta(hours=max(1, int(time_window_hours)))
    tc_modules = _catalog_module_lookup(pid)
    label_map: Dict[str, str] = {}
    try:
        from services.module_canonical import build_module_label_map

        label_map = build_module_label_map(tc_modules.values())
    except Exception:
        pass

    try:
        rows = run_history_service.list_runs(project_id=pid, limit=limit)
    except Exception as e:
        logger.warning("incident_qa: list_runs failed: %s", e)
        return []

    out: List[RelatedRunSummary] = []
    for r in rows:
        st = normalize_storage_status(str(r.status or ""))
        if st not in ("fail", "error"):
            continue
        if not _within_window(r.started_at, cutoff=cutoff):
            continue
        arts = r.artifacts
        module = ""
        if r.meta and isinstance(r.meta, dict):
            module = str(r.meta.get("module") or r.meta.get("module_name") or "").strip()
        tc_id = str(r.test_id or "").strip()
        if not module and tc_id:
            module = str(tc_modules.get(tc_id) or "").strip()
        if module and label_map:
            from services.module_canonical import module_display_label

            module = module_display_label(module, labels_by_key=label_map)
        out.append(RelatedRunSummary(
            run_id=str(r.run_id or ""),
            test_id=tc_id,
            test_name=str(r.test_name or ""),
            status=st,
            started_at=r.started_at,
            error_summary=r.error_summary,
            rca_summary=r.rca_summary,
            module=module,
            evidence_url=(arts.evidence_url if arts else None) or getattr(r, "evidence_url", None),
        ))
    return out


def gather_related_evidence(failed_runs: List[RelatedRunSummary]) -> List[RelatedEvidenceSummary]:
    out: List[RelatedEvidenceSummary] = []
    for r in failed_runs:
        arts_ev = r.evidence_url
        if not arts_ev and not r.error_summary:
            continue
        out.append(RelatedEvidenceSummary(
            run_id=r.run_id,
            test_name=r.test_name,
            evidence_url=r.evidence_url,
            report_url=r.evidence_url,
            status=r.status,
            started_at=r.started_at,
        ))
    return out


def gather_failure_clusters(
    project_id: str,
    *,
    time_window_hours: int = 72,
    in_window_run_ids: Optional[List[str]] = None,
    limit: int = 50,
) -> List[Dict[str, Any]]:
    try:
        from services.failure_intelligence_service import failure_intelligence_service

        clusters = failure_intelligence_service.get_clusters(
            project_id=project_id,
            limit=limit,
        )
        raw = [c.model_dump() if hasattr(c, "model_dump") else dict(c) for c in clusters]
        if in_window_run_ids is None:
            return raw
        window_ids = {str(rid).strip() for rid in in_window_run_ids if str(rid).strip()}
        if not window_ids:
            return []
        filtered: List[Dict[str, Any]] = []
        for c in raw:
            cluster_run_ids = {str(rid).strip() for rid in (c.get("run_ids") or []) if str(rid).strip()}
            if cluster_run_ids & window_ids:
                filtered.append(c)
        return filtered
    except Exception as e:
        logger.debug("incident_qa: failure clusters unavailable: %s", e)
        return []


def gather_regressions(project_id: str) -> List[Dict[str, Any]]:
    try:
        from services.failure_intelligence_service import failure_intelligence_service

        regs = failure_intelligence_service.get_regressions(project_id=project_id)
        return [r.model_dump() if hasattr(r, "model_dump") else dict(r) for r in regs]
    except Exception as e:
        logger.debug("incident_qa: regressions unavailable: %s", e)
        return []


def gather_knowledge_context(
    project_id: str,
    *,
    description: str,
    target_url: Optional[str] = None,
) -> Optional[Any]:
    try:
        from services.project_knowledge_service import get_knowledge_context

        return get_knowledge_context(
            project_id,
            target_url=target_url,
            incident_description=description,
        )
    except Exception as e:
        logger.debug("incident_qa: knowledge context unavailable: %s", e)
        return None


def gather_open_prs(project_id: str, *, description: str, limit: int = 15) -> List[RelatedPRSummary]:
    """Best-effort PR listing from connected SCM providers (read-only)."""
    hints = extract_topic_hints(description)
    out: List[RelatedPRSummary] = []
    pid = (project_id or "").strip().lower()

    try:
        from services.github_integration_service import list_pull_requests

        prs = list_pull_requests(pid, limit=limit)
        for pr in prs.pull_requests:
            blob = f"{pr.title} {pr.branch} {pr.author}".lower()
            if hints and not _text_matches_hints(blob, hints):
                continue
            out.append(RelatedPRSummary(
                provider="github",
                pr_id=str(pr.number),
                title=pr.title,
                branch=pr.branch,
                author=pr.author,
                html_url=pr.html_url,
                updated_at=pr.updated_at,
                match_reason="keyword_match" if hints else "recent_open_pr",
            ))
    except Exception:
        logger.debug("incident_qa: github PRs not available project_id=%s", pid)

    try:
        from services.azure_devops_integration_service import list_pull_requests as az_list

        prs = az_list(pid, limit=limit)
        for pr in prs.pull_requests:
            blob = f"{pr.title} {pr.branch} {pr.author}".lower()
            if hints and not _text_matches_hints(blob, hints):
                continue
            out.append(RelatedPRSummary(
                provider="azure_devops",
                pr_id=str(pr.pull_request_id),
                title=pr.title,
                branch=pr.branch,
                author=pr.author,
                html_url=pr.html_url,
                updated_at=pr.updated_at,
                match_reason="keyword_match" if hints else "recent_open_pr",
            ))
    except Exception:
        logger.debug("incident_qa: azure PRs not available project_id=%s", pid)

    return out[:10]


def gather_related_pr_analysis(
    project_id: str,
    *,
    related_prs: List[RelatedPRSummary],
    impacted_modules: List[str],
    hints: Set[str],
    time_window_hours: int,
    limit: int = 10,
) -> List[RelatedPRAnalysisSummary]:
    """Match stored PR Analysis reports — no re-analysis."""
    from services.pr_analysis_report_store import list_stored_pr_reports

    pid = (project_id or "").strip().lower()
    cutoff = datetime.now(timezone.utc) - timedelta(hours=max(1, int(time_window_hours)))
    stored = list_stored_pr_reports(pid, limit=50)
    if not stored:
        return []

    open_pr_keys = {
        (p.provider, str(p.pr_id))
        for p in related_prs
    }
    impacted_lower = {m.lower() for m in impacted_modules if m}

    out: List[RelatedPRAnalysisSummary] = []
    seen: Set[str] = set()

    for row in stored:
        created = row.get("created_at")
        if created and not _within_window(created, cutoff=cutoff):
            continue

        pr_id = str(row.get("pr_id") or "")
        provider = str(row.get("provider") or "manual")
        report = row.get("report") or {}
        if not isinstance(report, dict):
            continue

        report_mods = [
            str(m.get("module") or m)
            for m in (report.get("impacted_modules") or [])
            if isinstance(m, dict) and m.get("module")
        ]
        report_mods_lower = {m.lower() for m in report_mods if m}

        reason = ""
        matched = False
        if (provider, pr_id) in open_pr_keys:
            matched = True
            reason = "matched open PR from SCM"
        elif impacted_lower and report_mods_lower & impacted_lower:
            overlap = sorted(report_mods_lower & impacted_lower)
            matched = True
            reason = f"same module affected: {', '.join(overlap)}"
        elif hints and report_mods_lower & hints:
            matched = True
            reason = f"module keyword overlap: {', '.join(sorted(report_mods_lower & hints))}"
        elif hints:
            summary_blob = str(report.get("summary") or "").lower()
            if _text_matches_hints(summary_blob, hints):
                matched = True
                reason = "keyword match in stored PR analysis summary"

        if not matched:
            continue

        key = f"{provider}:{pr_id}"
        if key in seen:
            continue
        seen.add(key)

        rec_tests = [
            str(t.get("test_case_id") or t)
            for t in (report.get("recommended_tests") or [])
            if isinstance(t, dict) and t.get("test_case_id")
        ][:8]
        signals = [
            str(s.get("signal") or s.get("label") or "")
            for s in (report.get("risk_signals") or [])
            if isinstance(s, dict) and (s.get("signal") or s.get("label"))
        ][:5]

        out.append(RelatedPRAnalysisSummary(
            pr_number=pr_id,
            provider=provider,
            pr_risk_score=float(report.get("pr_risk_score") or report.get("risk_score") or 0),
            risk_level=str(report.get("pr_risk_level") or report.get("risk_level") or "LOW"),
            impacted_modules=report_mods[:8],
            recommended_tests=rec_tests,
            risk_signals=signals,
            analyzed_at=created,
            reason=reason,
        ))
        if len(out) >= limit:
            break

    return out
