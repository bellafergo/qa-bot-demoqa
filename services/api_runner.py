# services/api_runner.py
"""
API Test Runner
================

Executes api_request steps using httpx (synchronous client).
Evaluates assertions against HTTP responses.
Returns a result dict compatible with TestRun fields.

Supported step format:
  {
    "action":   "api_request",
    "method":   "POST",
    "endpoint": "/users",
    "headers":  {"Content-Type": "application/json"},
    "params":   {"page": 1},
    "body":     {"email": "test@example.com"}
  }

Supported assertion types:
  {"type": "status_code_equals",  "value": 200}
  {"type": "status_code_in",      "value": [200, 201]}
  {"type": "response_contains",   "target": "email", "value": "test@example.com"}
  {"type": "response_schema_match"}
  {"type": "header_exists",       "value": "Content-Type"}

Auth config:
  {"type": "bearer",   "token": "..."}
  {"type": "api_key",  "header": "X-API-Key", "token": "..."}
  {"type": "basic",    "username": "...", "password": "..."}
"""
from __future__ import annotations

import json
import logging
import time
from typing import Any, Dict, List, Optional

logger = logging.getLogger("vanya.api_runner")


# ── Auth header builder ───────────────────────────────────────────────────────

def _build_auth_headers(auth_config: Optional[Dict[str, Any]]) -> Dict[str, str]:
    if not auth_config:
        return {}
    auth_type = (auth_config.get("type") or "").lower()
    if auth_type == "bearer":
        token = auth_config.get("token", "")
        return {"Authorization": f"Bearer {token}"}
    if auth_type == "api_key":
        header = auth_config.get("header", "X-API-Key")
        token  = auth_config.get("token", "")
        return {header: token}
    if auth_type == "basic":
        import base64
        user = auth_config.get("username", "")
        pwd  = auth_config.get("password", "")
        encoded = base64.b64encode(f"{user}:{pwd}".encode()).decode()
        return {"Authorization": f"Basic {encoded}"}
    return {}


# ── Assertion evaluator ───────────────────────────────────────────────────────

def _evaluate_assertions(
    assertions: List[Dict[str, Any]],
    response,                          # httpx.Response
    logs: List[str],
) -> bool:
    """Evaluate all assertions against an httpx response. Returns True if all pass."""
    all_pass = True

    for assertion in assertions:
        atype = assertion.get("type", "")

        try:
            if atype == "status_code_equals":
                expected = int(assertion["value"])
                passed   = response.status_code == expected
                if not passed:
                    logs.append(f"FAIL status_code_equals: expected {expected}, got {response.status_code}")
                    all_pass = False

            elif atype == "status_code_in":
                raw = assertion["value"]
                # Support list[int], list[str], or comma-separated string "401,403"
                if isinstance(raw, str):
                    allowed = [int(v.strip()) for v in raw.split(",") if v.strip()]
                else:
                    allowed = [int(v) for v in raw]
                passed  = response.status_code in allowed
                if not passed:
                    logs.append(f"FAIL status_code_in: expected one of {allowed}, got {response.status_code}")
                    all_pass = False

            elif atype == "response_contains":
                target = assertion.get("target")
                value  = assertion.get("value", "")
                try:
                    body = response.json()
                except Exception:
                    body = {}
                if target:
                    # Check nested key using dot-notation
                    parts = target.split(".")
                    node: Any = body
                    for p in parts:
                        if isinstance(node, dict):
                            node = node.get(p)
                        else:
                            node = None
                            break
                    passed = node is not None and str(value) in str(node)
                else:
                    passed = str(value) in response.text
                if not passed:
                    logs.append(f"FAIL response_contains: '{value}' not found at '{target}'")
                    all_pass = False

            elif atype == "response_schema_match":
                # Minimal structural check: response must be valid JSON
                try:
                    response.json()
                    passed = True
                except Exception:
                    passed = False
                    logs.append("FAIL response_schema_match: response is not valid JSON")
                    all_pass = False

            elif atype == "header_exists":
                header_name = str(assertion.get("value", ""))
                # Case-insensitive header check
                passed = any(
                    h.lower() == header_name.lower()
                    for h in response.headers
                )
                if not passed:
                    logs.append(f"FAIL header_exists: header '{header_name}' not present")
                    all_pass = False

            else:
                logs.append(f"WARN unknown assertion type: '{atype}' — skipped")

        except Exception as exc:
            logs.append(f"ERROR evaluating assertion '{atype}': {exc}")
            all_pass = False

    return all_pass


# ── Step executor ─────────────────────────────────────────────────────────────

def _execute_steps(
    steps: List[Dict[str, Any]],
    base_url: str,
    auth_headers: Dict[str, str],
    timeout_s: int,
    logs: List[str],
    steps_result: List[Dict[str, Any]],
):
    """
    Execute api_request steps sequentially.
    Returns (last_response, error_message).
    """
    import httpx

    last_response = None
    with httpx.Client(base_url=base_url or "", timeout=float(timeout_s)) as client:
        for i, step in enumerate(steps):
            action = (step.get("action") or "").lower()
            if action != "api_request":
                logs.append(f"[STEP {i}] skipped non-api_request action: {action!r}")
                continue

            method   = step.get("method", "GET").upper()
            endpoint = step.get("endpoint", "/")
            headers  = dict(step.get("headers") or {})
            params   = step.get("params") or None
            body     = step.get("body")    # None → no body

            # Merge auth headers (step headers take precedence)
            merged_headers = {**auth_headers, **headers}

            logs.append(f"[STEP {i}] {method} {endpoint}")

            try:
                kwargs: Dict[str, Any] = {
                    "method":  method,
                    "url":     endpoint,
                    "headers": merged_headers,
                }
                if params:
                    kwargs["params"] = params
                if body is not None:
                    kwargs["json"] = body

                resp = client.request(**kwargs)
                last_response = resp

                step_result = {
                    "step":        i,
                    "method":      method,
                    "endpoint":    endpoint,
                    "status_code": resp.status_code,
                    "ok":          resp.is_success,
                }
                try:
                    step_result["response_body"] = resp.json()
                except Exception:
                    step_result["response_text"] = resp.text[:500]

                steps_result.append(step_result)
                logs.append(f"[STEP {i}] → {resp.status_code}")

            except Exception as exc:
                err = f"[STEP {i}] request error: {type(exc).__name__}: {exc}"
                logs.append(err)
                steps_result.append({"step": i, "error": str(exc)})
                return last_response, str(exc)

    return last_response, None


# ── Public runner ─────────────────────────────────────────────────────────────

def run_api_test(
    steps: List[Dict[str, Any]],
    *,
    base_url: str = "",
    assertions: Optional[List[Dict[str, Any]]] = None,
    auth_config: Optional[Dict[str, Any]] = None,
    timeout_s: int = 30,
) -> Dict[str, Any]:
    """
    Execute api_request steps and evaluate assertions.

    Returns a result dict with keys:
      ok, status, logs, steps, duration_ms, reason
    """
    t0 = time.time()
    logs: List[str] = []
    steps_result: List[Dict[str, Any]] = []
    assertions = assertions or []

    auth_headers = _build_auth_headers(auth_config)

    try:
        last_response, step_error = _execute_steps(
            steps, base_url, auth_headers, timeout_s, logs, steps_result
        )
    except Exception as exc:
        duration_ms = int((time.time() - t0) * 1000)
        logs.append(f"Runner fatal error: {type(exc).__name__}: {exc}")
        return {
            "ok":          False,
            "status":      "error",
            "logs":        logs,
            "steps":       steps_result,
            "duration_ms": duration_ms,
            "reason":      str(exc),
        }

    if step_error:
        duration_ms = int((time.time() - t0) * 1000)
        return {
            "ok":          False,
            "status":      "error",
            "logs":        logs,
            "steps":       steps_result,
            "duration_ms": duration_ms,
            "reason":      step_error,
        }

    if last_response is None:
        duration_ms = int((time.time() - t0) * 1000)
        logs.append("No api_request steps were executed.")
        return {
            "ok":          False,
            "status":      "error",
            "logs":        logs,
            "steps":       steps_result,
            "duration_ms": duration_ms,
            "reason":      "no api_request steps found",
        }

    # Evaluate assertions
    assertions_pass = _evaluate_assertions(assertions, last_response, logs)

    duration_ms = int((time.time() - t0) * 1000)
    passed = last_response.is_success and assertions_pass
    status = "pass" if passed else "fail"

    reason = None
    if not last_response.is_success:
        reason = f"HTTP {last_response.status_code}"
    elif not assertions_pass:
        reason = "one or more assertions failed"

    return {
        "ok":          passed,
        "status":      status,
        "logs":        logs,
        "steps":       steps_result,
        "duration_ms": duration_ms,
        "reason":      reason,
    }
