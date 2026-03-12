# services/openapi_parser_service.py
"""
OpenAPI / Swagger Spec Parser
================================

Supports:
  - OpenAPI 3.x  (openapi: "3.x.x")
  - Swagger 2.0  (swagger: "2.0")
  - JSON and YAML input

Input sources:
  - spec_text  — raw spec string (JSON or YAML)
  - spec_url   — fetched via httpx if present

Extracts per-operation:
  path, method, summary, description, tags, parameters,
  request body schema, success response schema, auth/security requirements.
"""
from __future__ import annotations

import json
import logging
from typing import Any, Dict, List, Optional, Tuple

from models.api_testing_models import EndpointInfo, OpenAPISpecRequest

logger = logging.getLogger("vanya.openapi_parser")


# ── Format detection / loading ────────────────────────────────────────────────

def _load_spec(text: str, format_hint: Optional[str] = None) -> Dict[str, Any]:
    """Parse a spec string as JSON or YAML. Returns a dict."""
    text = text.strip()

    # Try JSON first (fast and unambiguous)
    if format_hint != "yaml":
        try:
            return json.loads(text)
        except json.JSONDecodeError:
            pass

    # Try YAML
    try:
        import yaml  # pyyaml
        data = yaml.safe_load(text)
        if isinstance(data, dict):
            return data
    except Exception as exc:
        raise ValueError(f"Could not parse spec as JSON or YAML: {exc}") from exc

    raise ValueError("Spec parsed to a non-dict value — check the input format.")


def _fetch_url(url: str) -> str:
    """Fetch spec text from a URL via httpx."""
    try:
        import httpx
        with httpx.Client(timeout=15) as client:
            resp = client.get(url)
            resp.raise_for_status()
            return resp.text
    except Exception as exc:
        raise ValueError(f"Failed to fetch spec from '{url}': {exc}") from exc


# ── Version detection ─────────────────────────────────────────────────────────

def _spec_version(spec: Dict[str, Any]) -> str:
    """Return "3" for OpenAPI 3.x, "2" for Swagger 2.0."""
    if "openapi" in spec:
        return "3"
    if "swagger" in spec:
        return "2"
    # Best-effort guess by structure
    if "paths" in spec:
        return "3"
    return "unknown"


# ── Parameter extraction ──────────────────────────────────────────────────────

def _extract_parameters(operation: Dict[str, Any], path_params: List[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
    """Merge path-level and operation-level parameters."""
    merged: Dict[str, Dict] = {}
    for p in (path_params or []):
        key = f"{p.get('in','')}.{p.get('name','')}"
        merged[key] = p
    for p in operation.get("parameters", []):
        key = f"{p.get('in','')}.{p.get('name','')}"
        merged[key] = p
    return list(merged.values())


# ── Schema extraction ─────────────────────────────────────────────────────────

def _openapi3_request_schema(operation: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    rb = operation.get("requestBody", {})
    content = rb.get("content", {})
    # Prefer application/json
    for ct in ("application/json", "application/x-www-form-urlencoded", "*/*"):
        if ct in content:
            return content[ct].get("schema")
    # Fallback: first content type
    for v in content.values():
        return v.get("schema")
    return None


def _openapi3_response_schema(operation: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    responses = operation.get("responses", {})
    for code in ("200", "201", "default"):
        resp = responses.get(code, {})
        content = resp.get("content", {})
        for ct in ("application/json", "*/*"):
            if ct in content:
                return content[ct].get("schema")
        for v in content.values():
            return v.get("schema")
    return None


def _swagger2_request_schema(operation: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    for p in operation.get("parameters", []):
        if p.get("in") == "body":
            return p.get("schema")
    return None


def _swagger2_response_schema(operation: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    responses = operation.get("responses", {})
    for code in ("200", "201", "default"):
        resp = responses.get(code, {})
        if "schema" in resp:
            return resp["schema"]
    return None


# ── Security detection ────────────────────────────────────────────────────────

def _requires_auth(operation: Dict[str, Any], global_security: List[Any]) -> Tuple[bool, List[str]]:
    """Return (requires_auth, scheme_names) for an operation."""
    sec = operation.get("security")
    if sec is None:
        sec = global_security or []
    if not sec:
        return False, []
    # sec is a list of {scheme_name: [scopes]} dicts
    schemes = []
    for item in sec:
        if isinstance(item, dict):
            schemes.extend(item.keys())
    return bool(schemes), schemes


# ── OpenAPI 3.x parser ────────────────────────────────────────────────────────

def _parse_openapi3(spec: Dict[str, Any]) -> List[EndpointInfo]:
    endpoints: List[EndpointInfo] = []
    global_security = spec.get("security", [])
    paths = spec.get("paths", {})

    for path, path_item in paths.items():
        if not isinstance(path_item, dict):
            continue
        path_params = path_item.get("parameters", [])

        for method in ("get", "post", "put", "patch", "delete", "head", "options"):
            operation = path_item.get(method)
            if not isinstance(operation, dict):
                continue

            params = _extract_parameters(operation, path_params)
            req_schema = _openapi3_request_schema(operation)
            resp_schema = _openapi3_response_schema(operation)
            auth, schemes = _requires_auth(operation, global_security)

            endpoints.append(EndpointInfo(
                operation_id        = operation.get("operationId"),
                path                = path,
                method              = method.upper(),
                summary             = operation.get("summary", ""),
                description         = operation.get("description", ""),
                tags                = operation.get("tags", []),
                parameters          = params,
                request_body_schema = req_schema,
                response_schema     = resp_schema,
                requires_auth       = auth,
                security_schemes    = schemes,
            ))

    return endpoints


# ── Swagger 2.0 parser ────────────────────────────────────────────────────────

def _parse_swagger2(spec: Dict[str, Any]) -> List[EndpointInfo]:
    endpoints: List[EndpointInfo] = []
    global_security = spec.get("security", [])
    paths = spec.get("paths", {})

    for path, path_item in paths.items():
        if not isinstance(path_item, dict):
            continue
        path_params = path_item.get("parameters", [])

        for method in ("get", "post", "put", "patch", "delete", "head", "options"):
            operation = path_item.get(method)
            if not isinstance(operation, dict):
                continue

            # Swagger 2 body parameters are part of parameters list
            all_params = _extract_parameters(operation, path_params)
            non_body = [p for p in all_params if p.get("in") != "body"]
            req_schema = _swagger2_request_schema(operation)
            resp_schema = _swagger2_response_schema(operation)
            auth, schemes = _requires_auth(operation, global_security)

            endpoints.append(EndpointInfo(
                operation_id        = operation.get("operationId"),
                path                = path,
                method              = method.upper(),
                summary             = operation.get("summary", ""),
                description         = operation.get("description", ""),
                tags                = operation.get("tags", []),
                parameters          = non_body,
                request_body_schema = req_schema,
                response_schema     = resp_schema,
                requires_auth       = auth,
                security_schemes    = schemes,
            ))

    return endpoints


# ── Public API ────────────────────────────────────────────────────────────────

class OpenAPIParserService:

    def parse(self, req: OpenAPISpecRequest) -> List[EndpointInfo]:
        """Parse an OpenAPI/Swagger spec and return discovered endpoints."""
        text = req.spec_text
        if not text and req.spec_url:
            text = _fetch_url(req.spec_url)
        if not text:
            raise ValueError("No spec content found — provide spec_text or a reachable spec_url.")

        spec = _load_spec(text, format_hint=req.format_hint)
        version = _spec_version(spec)

        if version == "3":
            endpoints = _parse_openapi3(spec)
        elif version == "2":
            endpoints = _parse_swagger2(spec)
        else:
            # Optimistic: try 3.x parser as fallback
            logger.warning("openapi_parser: unknown spec version, trying OpenAPI 3.x parser")
            endpoints = _parse_openapi3(spec)

        logger.info("openapi_parser: parsed %d endpoints (version=%s)", len(endpoints), version)
        return endpoints


openapi_parser_service = OpenAPIParserService()
