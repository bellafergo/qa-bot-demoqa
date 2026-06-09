# services/project_risk_service.py
"""
Risk Engine v1 orchestration — reads existing Vanya stores, no duplicate persistence.

Integrates with System Memory (``project_knowledge``) by enriching the knowledge
document with ``risk_score``, ``risk_level``, ``module_risks``, and
``recommended_tests``.
"""
from __future__ import annotations

import logging
from collections import defaultdict
from typing import Any, Dict, List, Optional

from models.project_knowledge_models import ProjectKnowledge
from models.risk_engine_models import ModuleRisk, RecommendedTest, RiskAssessment
from services.module_canonical import canonical_module_key
from services.risk_engine_service import compute_project_risk

logger = logging.getLogger("vanya.project_risk")

_SEVERITY_WEIGHT = {
    "critical": 6.0,
    "high": 4.0,
    "medium": 2.5,
    "low": 1.0,
    "info": 0.5,
}


def _norm_module(name: str) -> str:
    return canonical_module_key(name)


def _module_from_incident(inc: Dict[str, Any], tc_module: Dict[str, str]) -> str:
    mod = str(inc.get("module") or "").strip()
    if mod:
        return _norm_module(mod)
    desc = str(inc.get("description") or inc.get("incident_description") or "").lower()
    for m in set(tc_module.values()):
        if m and m.lower() in desc:
            return m
    for raw in set(tc_module.values()):
        if raw and canonical_module_key(raw).lower() in desc:
            return canonical_module_key(raw)
    return ""


def gather_risk_inputs(
    project_id: str,
    knowledge: Optional[ProjectKnowledge] = None,
) -> Dict[str, Any]:
    """
    Collect pre-aggregated signals from existing stores (no new tables).

    Reuses: failure_intelligence, run_analytics, catalog, incidents, project_knowledge.
    """
    pid = (project_id or "").strip()
    if not pid:
        raise ValueError("project_id is required")

    from services.db.catalog_repository import catalog_repo
    from services.failure_intelligence_service import failure_intelligence_service
    from services.run_analytics_service import get_runs_dashboard

    mem = knowledge
    if mem is None:
        try:
            from services.project_memory_service import get_memory
            mem = get_memory(pid)
        except Exception:
            mem = None

    regressions = failure_intelligence_service.get_regressions(project_id=pid)
    flaky_tests = failure_intelligence_service.get_flaky_tests(project_id=pid)

    dashboard = get_runs_dashboard(project_id=pid)
    pass_rate = float((dashboard.get("summary") or {}).get("pass_rate") or 0.0)
    run_fail_rate = float((mem.metadata or {}).get("run_fail_rate") or 0.0) if mem else 0.0
    if pass_rate == 0.0 and run_fail_rate > 0:
        pass_rate = round(100.0 - run_fail_rate * 100.0, 1)

    tc_module: Dict[str, str] = {}
    module_test_count: Dict[str, int] = defaultdict(int)
    try:
        for tc_id, mod in catalog_repo.all_modules_for_project(pid):
            tc_module[tc_id] = _norm_module(mod) or canonical_module_key("General")
            module_test_count[tc_module[tc_id]] += 1
    except Exception:
        logger.debug("risk: catalog module map unavailable", exc_info=True)

    failure_history: List[Dict] = []
    incident_history: List[Dict] = []
    related_tests: List[Dict] = []
    if mem:
        failure_history = [f.model_dump() for f in mem.failure_history]
        incident_history = [i.model_dump() for i in mem.incident_history]
        related_tests = [t.model_dump() for t in mem.related_tests]

    if not related_tests:
        try:
            cases = catalog_repo.list_test_cases(project_id=pid, limit=500)
            related_tests = [
                {
                    "test_case_id": c.test_case_id,
                    "name": c.name,
                    "module": c.module,
                    "priority": c.priority,
                    "last_run_status": "",
                    "last_run_at": "",
                }
                for c in (cases or [])
            ]
        except Exception:
            logger.debug("risk: catalog list failed", exc_info=True)

    if not incident_history:
        try:
            from services.db.incident_investigation_repository import incident_investigation_repo

            for row in incident_investigation_repo.list_runs(limit=50, project_id=pid) or []:
                payload = row if isinstance(row, dict) else getattr(row, "payload_json", None) or {}
                if isinstance(payload, str):
                    continue
                incident_history.append({
                    "id": str(payload.get("id") or ""),
                    "description": str(payload.get("incident_description") or "")[:400],
                    "severity": str(payload.get("severity") or "info"),
                    "module": str(payload.get("module") or ""),
                    "created_at": str(payload.get("created_at") or ""),
                })
        except Exception:
            logger.debug("risk: incident list failed", exc_info=True)

    module_stats: Dict[str, Dict[str, Any]] = defaultdict(lambda: {
        "test_count": 0,
        "regression_count": 0,
        "failure_count": 0,
        "flaky_count": 0,
        "incident_count": 0,
        "incident_severity_sum": 0.0,
        "recent_failure_count": 0,
        "pass_total": 0,
        "run_total": 0,
    })

    for mod, cnt in module_test_count.items():
        if mod:
            module_stats[mod]["test_count"] = cnt

    if mem:
        for m in mem.modules:
            if m.name:
                mod_key = _norm_module(m.name)
                module_stats[mod_key]["test_count"] = max(
                    int(module_stats[mod_key]["test_count"]),
                    int(m.test_count or 0),
                )

    for reg in regressions:
        mod = _norm_module(reg.module) or tc_module.get(reg.test_case_id, canonical_module_key("General"))
        module_stats[mod]["regression_count"] += 1

    for flaky in flaky_tests:
        if not flaky.suspected_flaky:
            continue
        mod = tc_module.get(flaky.test_case_id, canonical_module_key("General"))
        module_stats[mod]["flaky_count"] += 1

    for fail in failure_history:
        mod = _norm_module(str(fail.get("module") or "")) or canonical_module_key("General")
        module_stats[mod]["failure_count"] += int(fail.get("count") or 1)
        created = str(fail.get("last_failed_at") or "")
        if created:
            from services.risk_engine_service import is_recent
            if is_recent(created):
                module_stats[mod]["recent_failure_count"] += 1

    for inc in incident_history:
        mod = _module_from_incident(inc, tc_module) or canonical_module_key("General")
        sev = str(inc.get("severity") or "info").lower()
        module_stats[mod]["incident_count"] += 1
        module_stats[mod]["incident_severity_sum"] += _SEVERITY_WEIGHT.get(sev, 0.5)

    try:
        from services.run_history_service import run_history_service
        from services.run_mapper import normalize_storage_status

        runs = run_history_service.list_runs(project_id=pid, limit=300)
        for run in runs:
            tc_id = str(getattr(run, "test_id", None) or (run.meta or {}).get("test_case_id") or "")
            mod = tc_module.get(tc_id, canonical_module_key("General"))
            st = normalize_storage_status(str(run.status or ""))
            if st in ("pass", "fail", "error"):
                module_stats[mod]["run_total"] += 1
                if st == "pass":
                    module_stats[mod]["pass_total"] += 1
    except Exception:
        logger.debug("risk: run history for module pass rate failed", exc_info=True)

    for mod, stats in module_stats.items():
        total = int(stats.get("run_total") or 0)
        if total > 0:
            stats["pass_rate"] = round(int(stats.get("pass_total") or 0) / total * 100.0, 1)

    return {
        "pass_rate": pass_rate,
        "run_fail_rate": run_fail_rate,
        "regressions": [r.model_dump() for r in regressions],
        "flaky_tests": [f.model_dump() for f in flaky_tests],
        "failure_history": failure_history,
        "incidents": incident_history,
        "module_stats": dict(module_stats),
        "related_tests": related_tests,
    }


def assess_project_risk(
    project_id: str,
    knowledge: Optional[ProjectKnowledge] = None,
) -> RiskAssessment:
    """Compute full risk assessment on demand."""
    pid = (project_id or "").strip()
    inputs = gather_risk_inputs(pid, knowledge=knowledge)
    return compute_project_risk(
        pid,
        pass_rate=inputs["pass_rate"],
        run_fail_rate=inputs["run_fail_rate"],
        regressions=inputs["regressions"],
        flaky_tests=inputs["flaky_tests"],
        failure_history=inputs["failure_history"],
        incidents=inputs["incidents"],
        module_stats=inputs["module_stats"],
        related_tests=inputs["related_tests"],
    )


def apply_risk_to_knowledge(knowledge: ProjectKnowledge) -> ProjectKnowledge:
    """Enrich a ProjectKnowledge document with Risk Engine v1 fields."""
    assessment = assess_project_risk(knowledge.project_id, knowledge=knowledge)
    knowledge.risk_score = assessment.risk_score
    knowledge.risk_level = assessment.risk_level
    knowledge.module_risks = [
        ModuleRisk.model_validate(m.model_dump()) for m in assessment.module_risks
    ]
    knowledge.recommended_tests = [
        RecommendedTest.model_validate(r.model_dump()) for r in assessment.recommended_tests
    ]
    knowledge.risk_explanation = list(assessment.explanation)
    meta = dict(knowledge.metadata or {})
    meta["risk_engine_version"] = assessment.engine_version
    meta["risk_factors"] = [f.model_dump() for f in assessment.factors]
    meta["risk_pass_rate"] = assessment.pass_rate
    meta["risk_regression_count"] = assessment.regression_count
    meta["risk_flaky_count"] = assessment.flaky_count
    meta["risk_recent_incidents"] = assessment.recent_incident_count
    knowledge.metadata = meta
    return knowledge
