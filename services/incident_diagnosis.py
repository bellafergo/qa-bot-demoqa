# services/incident_diagnosis.py
"""
Heuristic diagnosis for Autonomous Incident Investigator (MVP — no LLM).

Maps captured browser signals to severity, suspected area, and recommendations.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List, Optional, Tuple
from urllib.parse import urlparse

from models.incident_models import IncidentReproduced, IncidentSeverity, SuspectedArea


def _pick_suspected_endpoint(
    http_errors: List[Dict[str, Any]],
    network_errors: List[Dict[str, Any]],
) -> Optional[str]:
    for row in http_errors or []:
        if not isinstance(row, dict):
            continue
        url = str(row.get("url") or "").strip()
        if url:
            return url
    for row in network_errors or []:
        if not isinstance(row, dict):
            continue
        url = str(row.get("url") or "").strip()
        if url:
            return url
    return None


def _has_api_path(url: str) -> bool:
    p = urlparse(url).path.lower()
    return "/api" in p or p.startswith("/v1/") or p.startswith("/v2/")


def build_incident_diagnosis(
    *,
    incident_description: str,
    target_url: Optional[str],
    navigation_error: Optional[str],
    console_errors: List[Dict[str, Any]],
    network_errors: List[Dict[str, Any]],
    http_errors: List[Dict[str, Any]],
    page_title: Optional[str] = None,
    status_code: Optional[int] = None,
) -> Tuple[
    IncidentSeverity,
    IncidentReproduced,
    SuspectedArea,
    Optional[str],
    str,
    str,
    str,
    List[str],
    List[str],
]:
    """
    Returns:
      severity, reproduced, suspected_area, suspected_endpoint,
      diagnosis_summary, symptom_observed, probable_cause,
      recommendations, reproduction_steps
    """
    console_errors = console_errors or []
    network_errors = network_errors or []
    http_errors = http_errors or []

    server_errors = [h for h in http_errors if int(h.get("status") or 0) >= 500]
    client_errors = [h for h in http_errors if 400 <= int(h.get("status") or 0) < 500]
    auth_errors = [h for h in http_errors if int(h.get("status") or 0) in (401, 403)]
    not_found = [h for h in http_errors if int(h.get("status") or 0) == 404]

    suspected_endpoint = _pick_suspected_endpoint(http_errors, network_errors)
    suspected_area: SuspectedArea = "unknown"
    probable_cause = ""
    recommendations: List[str] = []
    severity: IncidentSeverity = "info"

    desc_lower = (incident_description or "").lower()
    loading_keywords = bool(
        re.search(r"\b(cargando|loading|spinner|infinite|indefinid|colgad|stuck)\b", desc_lower)
    )

    # ── Reproduced heuristic ───────────────────────────────────────────────────
    signals = bool(
        navigation_error
        or console_errors
        or network_errors
        or http_errors
        or (status_code is not None and status_code >= 400)
    )
    if navigation_error and not (console_errors or network_errors or http_errors):
        reproduced: IncidentReproduced = "unknown"
    elif signals:
        reproduced = "true"
    elif target_url:
        reproduced = "false"
    else:
        reproduced = "unknown"

    # ── Symptom ───────────────────────────────────────────────────────────────
    parts: List[str] = []
    if navigation_error:
        parts.append(f"Navigation failed: {navigation_error[:200]}")
    if server_errors:
        parts.append(f"{len(server_errors)} HTTP 5xx response(s) detected")
    if auth_errors:
        parts.append(f"{len(auth_errors)} HTTP 401/403 response(s) detected")
    if not_found:
        parts.append(f"{len(not_found)} HTTP 404 response(s) detected")
    if client_errors and not auth_errors and not not_found:
        parts.append(f"{len(client_errors)} HTTP 4xx response(s) detected")
    if console_errors:
        parts.append(f"{len(console_errors)} console error(s) captured")
    if network_errors:
        parts.append(f"{len(network_errors)} failed network request(s) captured")
    if not parts and target_url:
        parts.append(f"Page loaded at {target_url} without obvious browser errors")
    elif not parts:
        parts.append("Could not navigate — no target URL resolved")
    symptom_observed = "; ".join(parts)

    # ── Area + cause + severity ───────────────────────────────────────────────
    if server_errors:
        suspected_area = "backend"
        probable_cause = "Server-side error (HTTP 5xx) — likely API or backend service failure."
        severity = "critical" if len(server_errors) >= 2 else "high"
        recommendations.append("Inspect server logs for the failing endpoint and stack trace.")
        recommendations.append("Reproduce with curl or API client against the suspected endpoint.")
    elif auth_errors:
        suspected_area = "auth"
        probable_cause = "Authentication or authorization failure (HTTP 401/403)."
        severity = "high"
        recommendations.append("Verify session/token validity and RBAC rules for the affected route.")
        recommendations.append("Check auth middleware and cookie/session configuration.")
    elif not_found:
        suspected_area = "backend" if suspected_endpoint and _has_api_path(suspected_endpoint) else "frontend"
        probable_cause = "Resource or route not found (HTTP 404)."
        severity = "medium"
        recommendations.append("Confirm the route exists in frontend router and backend API registry.")
    elif console_errors:
        suspected_area = "frontend"
        first = str(console_errors[0].get("text") or "")[:160]
        probable_cause = f"JavaScript runtime error in the browser: {first or 'see console_errors'}."
        severity = "high" if len(console_errors) >= 3 else "medium"
        recommendations.append("Open DevTools console on the target page and fix the thrown exception.")
        recommendations.append("Add error boundaries and empty/error states for failed fetches.")
    elif network_errors:
        suspected_area = "network"
        probable_cause = "Network request failed before receiving a response (timeout, CORS, DNS, or connection reset)."
        severity = "high"
        recommendations.append("Verify API availability, CORS headers, and CDN/firewall rules.")
    elif loading_keywords and not signals:
        suspected_area = "frontend"
        probable_cause = (
            "User-reported loading issue — page loaded without captured errors; "
            "possible hung fetch, missing empty/error state, or slow backend."
        )
        severity = "medium"
        reproduced = "unknown"
        recommendations.append("Audit frontend loading flags — ensure empty arrays and 404/timeouts clear loading state.")
        recommendations.append("Profile network waterfall for pending requests on the target view.")
    elif navigation_error:
        suspected_area = "network"
        probable_cause = f"Could not reach target URL: {navigation_error[:200]}."
        severity = "high"
        recommendations.append("Verify URL correctness, DNS, TLS certificate, and SSRF allowlist if applicable.")
    else:
        suspected_area = "unknown"
        probable_cause = "No definitive failure signal captured during passive observation."
        severity = "info"
        recommendations.append("Try providing an explicit target_url or run a catalog test to generate run evidence.")
        recommendations.append("Enable Browser Watch on the affected URL for continuous monitoring.")

    diagnosis_summary = probable_cause
    if reproduced == "false":
        diagnosis_summary = (
            f"Could not reproduce obvious failure signals. {probable_cause}"
        )

    reproduction_steps = []
    if target_url:
        reproduction_steps.append(f"Open {target_url} in a browser.")
    reproduction_steps.append(f"Observe: {symptom_observed}")
    if console_errors:
        reproduction_steps.append("Check browser DevTools → Console for matching errors.")
    if http_errors or network_errors:
        reproduction_steps.append("Check DevTools → Network for failed or 4xx/5xx requests.")

    return (
        severity,
        reproduced,
        suspected_area,
        suspected_endpoint,
        diagnosis_summary,
        symptom_observed,
        probable_cause,
        recommendations,
        reproduction_steps,
    )
