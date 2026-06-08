# services/incident_investigator_service.py
"""
Autonomous Incident Investigator (MVP).

Reuses:
  - ``runners.browser_inspector_runner.run_browser_inspection`` (Playwright, console/network)
  - ``core.target_url_validation.validate_target_url`` (SSRF policy)
  - ``services.browser_inspector_service.normalize_raw_runner_output`` (screenshot upload)
  - ``services.incident_diagnosis.build_incident_diagnosis`` (heuristic RCA)
  - ``services.db.project_repository.project_repo`` (base_url inference)

Non-destructive by default: single navigation, no form submits, no clicks.
"""
from __future__ import annotations

import logging
import re
from typing import Any, Dict, List, Optional, Tuple
from urllib.parse import urljoin

from core.target_url_validation import TargetURLNotAllowed, validate_target_url
from models.incident_models import IncidentInvestigationRun, InvestigateIncidentRequest
from services.db.incident_investigation_repository import incident_investigation_repo
from services.incident_diagnosis import build_incident_diagnosis

logger = logging.getLogger("vanya.incident_investigator")

_KEYWORD_PATHS: List[Tuple[re.Pattern[str], str]] = [
    (re.compile(r"\b(login|sign[\s-]?in|credenciales|autentic)\b", re.I), "/login"),
    (re.compile(r"\b(dashboard|panel)\b", re.I), "/dashboard"),
    (re.compile(r"\b(evidencia|evidence)\b", re.I), "/evidence"),
    (re.compile(r"\b(vacante|vacancy|vacantes|job|trabajo)\b", re.I), "/jobs"),
    (re.compile(r"\b(cat[aá]logo|catalog)\b", re.I), "/catalog"),
    (re.compile(r"\b(batch|ejecuci[oó]n)\b", re.I), "/batch"),
    (re.compile(r"\b(runs|historial|history)\b", re.I), "/runs"),
    (re.compile(r"\b(insights|inteligencia)\b", re.I), "/insights"),
]


def infer_target_url(
    description: str,
    *,
    target_url: Optional[str],
    project_id: Optional[str],
    module: Optional[str],
) -> Tuple[Optional[str], str]:
    """Return (url, inference_source)."""
    explicit = (target_url or "").strip()
    if explicit:
        return explicit, "explicit"

    base: Optional[str] = None
    pid = (project_id or "").strip().lower()
    if pid:
        try:
            from services.db.project_repository import project_repo

            proj = project_repo.get_project(pid)
            if proj is not None:
                bu = getattr(proj, "base_url", None) or (
                    proj.get("base_url") if isinstance(proj, dict) else None
                )
                if bu and str(bu).strip():
                    base = str(bu).strip()
        except Exception as e:
            logger.debug("incident: project lookup failed: %s", e)

    url_in_text = re.search(r"https?://[^\s<>\"']+", description or "")
    if url_in_text:
        return url_in_text.group(0).rstrip(".,;)'\"]"), "extracted_from_text"

    path = ""
    mod = (module or "").strip()
    if mod:
        path = mod if mod.startswith("/") else f"/{mod}"
    else:
        for pattern, p in _KEYWORD_PATHS:
            if pattern.search(description or ""):
                path = p
                break

    if base:
        joined = urljoin(base.rstrip("/") + "/", (path or "").lstrip("/"))
        return joined, "inferred_from_project"

    return None, "none"


def _clip_errors(rows: List[Any], *, max_items: int = 40) -> List[Dict[str, Any]]:
    out: List[Dict[str, Any]] = []
    for r in rows or []:
        if isinstance(r, dict):
            out.append(r)
        if len(out) >= max_items:
            break
    return out


def investigate_incident(req: InvestigateIncidentRequest) -> IncidentInvestigationRun:
    """Run a synchronous incident investigation and persist the result."""
    if req.allow_destructive_actions:
        raise ValueError(
            "Destructive actions are not supported in MVP — keep allow_destructive_actions=false"
        )
    if (req.credentials_mode or "none").strip().lower() not in ("none", ""):
        raise ValueError("Only credentials_mode='none' is supported in MVP")

    inferred_url, inference_source = infer_target_url(
        req.incident_description,
        target_url=req.target_url,
        project_id=req.project_id,
        module=req.module,
    )

    run_id = incident_investigation_repo.create_running(
        incident_description=req.incident_description.strip(),
        project_id=req.project_id,
        module=req.module,
        target_url=inferred_url,
    )

    steps: List[str] = ["investigation_started", f"url_inference:{inference_source}"]
    payload: Dict[str, Any] = incident_investigation_repo.get(run_id) or {"id": run_id}

    try:
        if not inferred_url:
            steps.append("no_target_url")
            (
                severity,
                reproduced,
                suspected_area,
                suspected_endpoint,
                diagnosis_summary,
                symptom,
                probable_cause,
                recommendations,
                reproduction_steps,
            ) = build_incident_diagnosis(
                incident_description=req.incident_description,
                target_url=None,
                navigation_error="No target URL could be inferred — provide target_url or project base_url",
                console_errors=[],
                network_errors=[],
                http_errors=[],
            )
            payload.update(
                {
                    "status": "completed",
                    "severity": severity,
                    "reproduced": reproduced,
                    "suspected_area": suspected_area,
                    "suspected_endpoint": suspected_endpoint,
                    "symptom_observed": symptom,
                    "probable_cause": probable_cause,
                    "diagnosis_summary": diagnosis_summary,
                    "recommendations": recommendations,
                    "reproduction_steps": reproduction_steps,
                    "steps_executed": steps,
                    "meta": {"url_inference": inference_source},
                }
            )
            incident_investigation_repo.save_payload(run_id, payload)
            return IncidentInvestigationRun.model_validate(payload)

        steps.append("validate_target_url")
        validated = validate_target_url(inferred_url)
        steps.append("launch_browser")

        from runners.browser_inspector_runner import run_browser_inspection

        timeout_ms = min(int(req.timeout_ms), 120_000)
        raw = run_browser_inspection(url=validated, timeout_ms=timeout_ms, headless=True)
        steps.extend(
            [
                "navigate_to_url",
                "capture_screenshot",
                "collect_console_errors",
                "collect_network_errors",
                "collect_http_error_responses",
            ]
        )

        from services.browser_inspector_service import normalize_raw_runner_output

        inspection = normalize_raw_runner_output(
            request_url=inferred_url,
            raw=raw,
            validated_navigation_url=validated,
            upload_hosted_screenshot=True,
        )

        console_errors = _clip_errors(raw.get("console_errors") or [])
        network_errors = _clip_errors(raw.get("network_errors") or [])
        http_errors = _clip_errors(raw.get("http_error_responses") or [])

        steps.append("consult_project_knowledge")
        knowledge_ctx = None
        knowledge_hints: List[str] = []
        try:
            from services.project_knowledge_service import get_knowledge_context

            knowledge_ctx = get_knowledge_context(
                req.project_id,
                target_url=validated,
                incident_description=req.incident_description,
            )
            if knowledge_ctx and knowledge_ctx.hints:
                knowledge_hints = list(knowledge_ctx.hints)
        except Exception as e:
            logger.debug("incident: knowledge context unavailable: %s", e)

        steps.append("generate_diagnosis")
        (
            severity,
            reproduced,
            suspected_area,
            suspected_endpoint,
            diagnosis_summary,
            symptom,
            probable_cause,
            recommendations,
            reproduction_steps,
        ) = build_incident_diagnosis(
            incident_description=req.incident_description,
            target_url=validated,
            navigation_error=raw.get("navigation_error"),
            console_errors=console_errors,
            network_errors=network_errors,
            http_errors=http_errors,
            page_title=raw.get("title"),
            status_code=raw.get("status_code"),
        )
        if knowledge_hints:
            recommendations = list(recommendations) + [f"[Memory] {h}" for h in knowledge_hints[:5]]

        screenshot_b64 = raw.get("screenshot_b64")
        payload.update(
            {
                "status": "completed",
                "target_url": validated,
                "severity": severity,
                "reproduced": reproduced,
                "suspected_area": suspected_area,
                "suspected_endpoint": suspected_endpoint,
                "symptom_observed": symptom,
                "probable_cause": probable_cause,
                "console_errors": console_errors,
                "network_errors": network_errors,
                "http_errors": http_errors,
                "screenshot_url": inspection.screenshot_url,
                "screenshot_b64": None if inspection.screenshot_url else screenshot_b64,
                "steps_executed": steps[: max(1, int(req.max_steps)) + 5],
                "diagnosis_summary": diagnosis_summary,
                "recommendations": recommendations,
                "reproduction_steps": reproduction_steps,
                "raw_evidence": {
                    "final_url": raw.get("final_url"),
                    "title": raw.get("title"),
                    "status_code": raw.get("status_code"),
                    "navigation_error": raw.get("navigation_error"),
                    "inspection_id": inspection.inspection_id,
                    "inventory_counts": inspection.inventory_counts,
                    "warnings": inspection.warnings,
                },
                "meta": {
                    "url_inference": inference_source,
                    "inspection_succeeded": inspection.inspection_succeeded,
                    "knowledge_context": knowledge_ctx.model_dump() if knowledge_ctx else None,
                    "knowledge_risk_score": knowledge_ctx.risk_score if knowledge_ctx else None,
                },
            }
        )
        incident_investigation_repo.save_payload(run_id, payload)
        result = IncidentInvestigationRun.model_validate(payload)
        try:
            from services.project_knowledge_service import ingest_incident_completed

            ingest_incident_completed(req.project_id, result.model_dump())
        except Exception:
            logger.debug("incident: knowledge ingest failed", exc_info=True)
        return result

    except TargetURLNotAllowed as e:
        steps.append("target_url_rejected")
        payload.update(
            {
                "status": "failed",
                "error_message": str(e) or "Target URL not allowed",
                "steps_executed": steps,
                "diagnosis_summary": "Investigation blocked by URL safety policy.",
                "recommendations": [
                    "Provide an allowed public HTTPS target_url.",
                    "Configure ALLOWED_TARGET_HOSTS if using a private staging domain.",
                ],
            }
        )
        incident_investigation_repo.save_payload(run_id, payload)
        return IncidentInvestigationRun.model_validate(payload)

    except Exception as e:
        logger.exception("incident investigation failed run_id=%s", run_id)
        steps.append("investigation_failed")
        payload.update(
            {
                "status": "failed",
                "error_message": f"{type(e).__name__}: {e}",
                "steps_executed": steps,
                "diagnosis_summary": "Investigation failed due to an unexpected error.",
                "recommendations": ["Retry with a shorter timeout or explicit target_url."],
            }
        )
        incident_investigation_repo.save_payload(run_id, payload)
        return IncidentInvestigationRun.model_validate(payload)


def list_incident_runs(
    *,
    limit: int = 50,
    project_id: Optional[str] = None,
) -> List[IncidentInvestigationRun]:
    rows = incident_investigation_repo.list_runs(limit=limit, project_id=project_id)
    return [IncidentInvestigationRun.model_validate(r) for r in rows]


def get_incident_run(run_id: str) -> Optional[IncidentInvestigationRun]:
    row = incident_investigation_repo.get(run_id)
    if not row:
        return None
    return IncidentInvestigationRun.model_validate(row)
