# runners/api_runner.py
"""
HTTP API test runner for catalog test_type=api.

Uses httpx only (no Playwright). Steps execute sequentially; context holds
variables, last response, parsed JSON, and response time.

Example flow (catalog steps JSON):
  {"action": "request", "method": "GET", "url": "{{base_url}}/api/candidates"}
  {"action": "assert_status", "expected": 200}
  {"action": "assert_json_type", "field": "data", "expected_type": "array"}
  {"action": "set_variable", "name": "first_id", "field": "data.0.id"}
  {"action": "request", "method": "GET", "url": "{{base_url}}/api/candidates/{{first_id}}"}
  {"action": "assert_response_time", "max_ms": 2000}

NextAuth (Credentials) session bootstrap (cookies preserved for following request steps):
  {"action": "nextauth_login", "email": "{{project_email}}", "password": "{{project_password}}", "base_url": "{{base_url}}"}
  Optional: csrf_path (default /api/auth/csrf), credentials_path (/api/auth/callback/credentials),
  callback_url (default {base_url}/).

Legacy: action "api_request" with "endpoint" is treated like "request" with "url".
After steps, optional catalog-style assertions (status_code_equals, …) are evaluated
against the last response if provided.
"""
from __future__ import annotations

import json
import logging
import re
import time
from typing import Any, Dict, List, Optional

import httpx

logger = logging.getLogger("vanya.runners.api")

_VAR_RE = re.compile(r"\{\{([^}]+)\}\}")

_ALLOWED_METHODS = frozenset(
    {"GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS"}
)

_JSON_TYPES = frozenset({"array", "object", "string", "number", "boolean", "null"})


def _safe_cookie_jar_summary(client: httpx.Client) -> Dict[str, Any]:
    """Names / domains / paths only — never cookie values."""
    names: List[str] = []
    domains: List[str] = []
    paths: List[str] = []
    try:
        cj = getattr(client.cookies, "jar", None)
        if cj is not None:
            for c in cj:
                try:
                    n = getattr(c, "name", None)
                    if n:
                        names.append(str(n))
                    d = (getattr(c, "domain", None) or "").strip()
                    if d:
                        domains.append(d.lstrip("."))
                    p = (getattr(c, "path", None) or "").strip()
                    if p:
                        paths.append(p)
                except Exception:
                    continue
    except Exception:
        pass
    return {
        "names": sorted(set(names)),
        "domains": sorted(set(domains)),
        "paths": sorted(set(paths)),
    }


def _log_cookie_jar(tag: str, client: httpx.Client) -> None:
    s = _safe_cookie_jar_summary(client)
    logger.debug(
        "api_runner cookies %s names=%s domains=%s paths=%s",
        tag,
        s.get("names"),
        s.get("domains"),
        s.get("paths"),
    )


def _request_base_for_url(variables: Dict[str, Any], merged_base: str) -> str:
    """Prefer origin used for NextAuth login so relative URLs match the cookie jar."""
    auth_o = str(variables.get("_session_origin") or "").strip().rstrip("/")
    if auth_o:
        return auth_o
    return merged_base


def _body_preview_for_log(resp: httpx.Response, max_len: int = 200) -> str:
    try:
        t = (resp.text or "")[:max_len]
    except Exception:
        return "(unreadable)"
    return t.replace("\n", " ").replace("\r", " ")


def _mask_email_for_log(email: str) -> str:
    e = (email or "").strip()
    if not e:
        return "(empty)"
    if "@" not in e:
        return "***"
    local, _, domain = e.partition("@")
    if len(local) <= 1:
        return f"*@{domain}"
    return f"{local[0]}***@{domain}"


def _build_auth_headers(auth_config: Optional[Dict[str, Any]]) -> Dict[str, str]:
    if not auth_config:
        return {}
    auth_type = (auth_config.get("type") or "").lower()
    if auth_type == "bearer":
        token = auth_config.get("token", "")
        return {"Authorization": f"Bearer {token}"}
    if auth_type == "api_key":
        header = auth_config.get("header", "X-API-Key")
        token = auth_config.get("token", "")
        return {header: token}
    if auth_type == "basic":
        import base64

        user = auth_config.get("username", "")
        pwd = auth_config.get("password", "")
        encoded = base64.b64encode(f"{user}:{pwd}".encode()).decode()
        return {"Authorization": f"Basic {encoded}"}
    return {}


def _interpolate_str(s: str, ctx: Dict[str, Any]) -> str:
    def repl(m: re.Match) -> str:
        key = m.group(1).strip()
        if key not in ctx:
            raise KeyError(f"Interpolation variable not defined: {key!r}")
        return str(ctx[key])

    return _VAR_RE.sub(repl, s)


def interpolate_value(obj: Any, ctx: Dict[str, Any]) -> Any:
    if isinstance(obj, str):
        if "{{" in obj:
            return _interpolate_str(obj, ctx)
        return obj
    if isinstance(obj, dict):
        return {k: interpolate_value(v, ctx) for k, v in obj.items()}
    if isinstance(obj, list):
        return [interpolate_value(v, ctx) for v in obj]
    return obj


def resolve_request_url(base_url: str, url: str) -> str:
    u = (url or "").strip()
    if not u:
        raise ValueError("request url is empty")
    if u.startswith("http://") or u.startswith("https://"):
        return u
    b = (base_url or "").strip().rstrip("/")
    path = u.lstrip("/")
    if not b:
        raise ValueError(f"Relative url {url!r} requires base_url")
    return f"{b}/{path}"


def get_json_path(data: Any, field: str) -> Any:
    if not field or not str(field).strip():
        raise ValueError("JSON field path is empty")
    cur: Any = data
    for part in str(field).split("."):
        if cur is None:
            raise KeyError(f"Path {field!r}: null before segment {part!r}")
        if isinstance(cur, list):
            try:
                idx = int(part)
            except ValueError as e:
                raise KeyError(f"Path {field!r}: expected numeric index, got {part!r}") from e
            if idx < 0 or idx >= len(cur):
                raise IndexError(f"Path {field!r}: index {idx} out of range (len={len(cur)})")
            cur = cur[idx]
        elif isinstance(cur, dict):
            if part not in cur:
                raise KeyError(f"Path {field!r}: missing key {part!r}")
            cur = cur[part]
        else:
            raise TypeError(f"Path {field!r}: cannot traverse {type(cur).__name__} at {part!r}")
    return cur


def _value_matches_json_type(value: Any, expected_type: str) -> bool:
    et = (expected_type or "").strip().lower()
    if et not in _JSON_TYPES:
        return False
    if et == "null":
        return value is None
    if et == "array":
        return isinstance(value, list)
    if et == "object":
        return isinstance(value, dict)
    if et == "string":
        return isinstance(value, str)
    if et == "number":
        return isinstance(value, (int, float)) and not isinstance(value, bool)
    if et == "boolean":
        return isinstance(value, bool)
    return False


def _normalize_step(step: Dict[str, Any]) -> Dict[str, Any]:
    s = dict(step)
    act = (s.get("action") or "").strip().lower()
    if act == "api_request":
        s["action"] = "request"
        if not s.get("url") and s.get("endpoint"):
            s["url"] = s["endpoint"]
    return s


def _evaluate_legacy_assertions(
    assertions: List[Dict[str, Any]],
    response: httpx.Response,
    logs: List[str],
) -> bool:
    """Catalog assertion dicts (type/value/…) against last httpx.Response."""
    all_pass = True
    for assertion in assertions:
        atype = assertion.get("type", "")
        try:
            if atype == "status_code_equals":
                expected = int(assertion["value"])
                if response.status_code != expected:
                    logs.append(
                        f"FAIL status_code_equals: expected {expected}, got {response.status_code}"
                    )
                    all_pass = False
            elif atype == "status_code_in":
                raw = assertion["value"]
                if isinstance(raw, str):
                    allowed = [int(v.strip()) for v in raw.split(",") if v.strip()]
                else:
                    allowed = [int(v) for v in raw]
                if response.status_code not in allowed:
                    logs.append(
                        f"FAIL status_code_in: expected one of {allowed}, got {response.status_code}"
                    )
                    all_pass = False
            elif atype == "response_contains":
                target = assertion.get("target")
                value = assertion.get("value", "")
                try:
                    body = response.json()
                except Exception:
                    body = {}
                if target:
                    parts = str(target).split(".")
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
                    logs.append(f"FAIL response_contains: {value!r} not found at {target!r}")
                    all_pass = False
            elif atype == "response_schema_match":
                try:
                    response.json()
                except Exception:
                    logs.append("FAIL response_schema_match: response is not valid JSON")
                    all_pass = False
            elif atype == "header_exists":
                header_name = str(assertion.get("value", ""))
                if not any(h.lower() == header_name.lower() for h in response.headers):
                    logs.append(f"FAIL header_exists: header {header_name!r} not present")
                    all_pass = False
            else:
                logs.append(f"WARN unknown legacy assertion type: {atype!r} — skipped")
        except Exception as exc:
            logs.append(f"ERROR evaluating assertion {atype!r}: {exc}")
            all_pass = False
    return all_pass


def _step_result(
    *,
    index: int,
    action: str,
    status: str,
    duration_ms: int,
    log: str,
    error: Optional[str] = None,
    extra: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    row: Dict[str, Any] = {
        "step": index,
        "action": action,
        "status": status,
        "duration_ms": duration_ms,
        "log": log,
    }
    if error:
        row["error"] = error
    if extra:
        row.update(extra)
    logger.info(
        "api_runner step %s action=%s status=%s duration_ms=%s log=%s",
        index,
        action,
        status,
        duration_ms,
        log[:500] if log else "",
    )
    return row


def run_api_test(
    steps: List[Dict[str, Any]],
    *,
    base_url: str = "",
    assertions: Optional[List[Dict[str, Any]]] = None,
    auth_config: Optional[Dict[str, Any]] = None,
    timeout_s: int = 30,
    initial_variables: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Run API catalog steps. Returns dict compatible with catalog execution:
    ok, status, logs, steps, duration_ms, reason, error_summary (on failure).

    *initial_variables* seeds the interpolation context (e.g. project_email, project_password);
    explicit *base_url* wins when both are set.
    """
    t0 = time.time()
    logs: List[str] = []
    steps_result: List[Dict[str, Any]] = []
    assertions = assertions or []

    if not steps:
        duration_ms = int((time.time() - t0) * 1000)
        return {
            "ok": False,
            "status": "error",
            "logs": ["No API steps to execute."],
            "steps": [],
            "duration_ms": duration_ms,
            "reason": "no steps",
            "error_summary": "no steps",
        }

    iv = dict(initial_variables or {})
    merged_base = (base_url or "").strip().rstrip("/")
    if not merged_base:
        merged_base = str(iv.get("base_url") or "").strip().rstrip("/")
    variables: Dict[str, Any] = {**iv, "base_url": merged_base}
    last_response: Optional[httpx.Response] = None
    last_response_json: Any = None
    last_response_time_ms: int = 0
    auth_headers = _build_auth_headers(auth_config)
    default_timeout = float(timeout_s)

    try:
        with httpx.Client(
            follow_redirects=True,
            timeout=httpx.Timeout(default_timeout),
        ) as client:
            for i, raw in enumerate(steps):
                if not isinstance(raw, dict):
                    msg = f"Step {i} is not an object"
                    logs.append(msg)
                    steps_result.append(
                        _step_result(
                            index=i,
                            action="?",
                            status="error",
                            duration_ms=0,
                            log=msg,
                            error=msg,
                        )
                    )
                    return _finish_fail(
                        logs, steps_result, t0, msg, steps_result[-1].get("error", msg)
                    )

                step = _normalize_step(raw)
                action = (step.get("action") or "").strip().lower()
                t_step = time.time()

                try:
                    ctx = {**variables, "base_url": variables.get("base_url", "")}

                    if action == "wait_ms":
                        ms = int(step.get("ms") or step.get("value") or 0)
                        if ms < 0:
                            raise ValueError("wait_ms: ms must be >= 0")
                        time.sleep(ms / 1000.0)
                        dur = int((time.time() - t_step) * 1000)
                        log = f"wait_ms {ms}"
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    if action == "request":
                        method = str(step.get("method") or "GET").upper().strip()
                        if method not in _ALLOWED_METHODS:
                            raise ValueError(
                                f"Unsupported HTTP method: {method!r}. Allowed: {sorted(_ALLOWED_METHODS)}"
                            )
                        url_raw = str(step.get("url") or "")
                        url_base = _request_base_for_url(variables, merged_base)
                        url = resolve_request_url(url_base, interpolate_value(url_raw, ctx))
                        if logger.isEnabledFor(logging.DEBUG):
                            _log_cookie_jar(f"before step {i} request", client)
                            logger.debug(
                                "api_runner request step=%s resolve_base=%s merged_base=%s",
                                i,
                                url_base,
                                merged_base,
                            )
                        headers = dict(auth_headers)
                        headers.update(interpolate_value(step.get("headers") or {}, ctx))
                        params = interpolate_value(step.get("params"), ctx) if step.get("params") else None
                        body = step.get("body")
                        if body is not None:
                            body = interpolate_value(body, ctx)
                        req_timeout = step.get("timeout_ms")
                        if req_timeout is not None:
                            req_timeout_f = max(0.001, float(req_timeout) / 1000.0)
                        else:
                            req_timeout_f = default_timeout

                        kwargs: Dict[str, Any] = {
                            "method": method,
                            "url": url,
                            "headers": headers,
                            "timeout": req_timeout_f,
                        }
                        if params is not None:
                            kwargs["params"] = params
                        if body is not None:
                            if isinstance(body, (dict, list)):
                                kwargs["json"] = body
                            else:
                                kwargs["content"] = (
                                    body if isinstance(body, (bytes, str)) else str(body)
                                )

                        tr = time.time()
                        try:
                            resp = client.request(**kwargs)
                        except httpx.TimeoutException as e:
                            raise TimeoutError(f"Request timeout: {e}") from e
                        last_response_time_ms = int((time.time() - tr) * 1000)
                        last_response = resp
                        try:
                            last_response_json = resp.json()
                        except Exception:
                            last_response_json = None

                        if logger.isEnabledFor(logging.DEBUG) and resp.status_code >= 400:
                            logger.debug(
                                "api_runner request step=%s status=%s url=%s body_preview=%s",
                                i,
                                resp.status_code,
                                url,
                                _body_preview_for_log(resp),
                            )

                        dur = int((time.time() - t_step) * 1000)
                        log = f"{method} {url} → HTTP {resp.status_code} ({last_response_time_ms}ms)"
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                                extra={
                                    "status_code": resp.status_code,
                                    "url": url,
                                },
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    if action == "nextauth_login":
                        email_i = str(interpolate_value(step.get("email") or "", ctx)).strip()
                        password_i = str(interpolate_value(step.get("password") or "", ctx)).strip()
                        if not email_i or not password_i:
                            raise ValueError(
                                "nextauth_login: email and password must be non-empty after interpolation "
                                "(set project variables EMAIL/PASSWORD or env fallbacks)."
                            )
                        raw_step_base = step.get("base_url")
                        if raw_step_base is not None and str(raw_step_base).strip():
                            step_origin = str(interpolate_value(raw_step_base, ctx)).strip().rstrip(
                                "/"
                            )
                        else:
                            step_origin = str(variables.get("base_url") or "").strip().rstrip("/")
                        if not step_origin:
                            raise ValueError(
                                "nextauth_login requires base_url on the run, from the project, or on the step."
                            )
                        csrf_path = str(
                            interpolate_value(step.get("csrf_path") or "/api/auth/csrf", ctx)
                        ).strip()
                        cred_path = str(
                            interpolate_value(
                                step.get("credentials_path") or "/api/auth/callback/credentials",
                                ctx,
                            )
                        ).strip()
                        raw_cb = step.get("callback_url")
                        if raw_cb is None or (isinstance(raw_cb, str) and not raw_cb.strip()):
                            callback_url = f"{step_origin}/"
                        else:
                            callback_url = str(interpolate_value(raw_cb, ctx)).strip()
                        csrf_url = resolve_request_url(step_origin, csrf_path)
                        csrf_resp = client.get(csrf_url, timeout=default_timeout)
                        logger.debug(
                            "nextauth_login: email_masked=%s csrf_http=%s",
                            _mask_email_for_log(email_i),
                            csrf_resp.status_code,
                        )
                        try:
                            csrf_json = csrf_resp.json()
                        except Exception as e:
                            raise ValueError(
                                f"nextauth_login: CSRF response is not JSON (HTTP {csrf_resp.status_code})"
                            ) from e
                        csrf_token = (
                            csrf_json.get("csrfToken") if isinstance(csrf_json, dict) else None
                        )
                        logger.debug(
                            "nextauth_login: csrfToken_found=%s",
                            bool(csrf_token),
                        )
                        if not csrf_token:
                            raise ValueError("nextauth_login: missing csrfToken in CSRF response")
                        login_url = resolve_request_url(step_origin, cred_path)
                        form: Dict[str, str] = {
                            "csrfToken": str(csrf_token),
                            "email": email_i,
                            "password": password_i,
                            "callbackUrl": callback_url,
                            "json": "true",
                        }
                        tr = time.time()
                        post_resp = client.post(login_url, data=form, timeout=default_timeout)
                        logger.debug(
                            "nextauth_login: credentials_post_http=%s",
                            post_resp.status_code,
                        )
                        last_response_time_ms = int((time.time() - tr) * 1000)
                        last_response = post_resp
                        try:
                            last_response_json = post_resp.json()
                        except Exception:
                            last_response_json = None
                        dur = int((time.time() - t_step) * 1000)
                        em = _mask_email_for_log(email_i)
                        log = (
                            f"nextauth_login csrf HTTP {csrf_resp.status_code} → "
                            f"POST credentials HTTP {post_resp.status_code} ({last_response_time_ms}ms) email={em}"
                        )
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                                extra={
                                    "status_code": post_resp.status_code,
                                    "url": login_url,
                                },
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        # Align interpolation base_url with the origin that received Set-Cookie
                        # so later {{base_url}} / relative URLs match the cookie jar host.
                        variables["_session_origin"] = step_origin
                        variables["base_url"] = step_origin
                        _log_cookie_jar("after nextauth_login", client)
                        continue

                    if action == "assert_authenticated":
                        raw_step_base = step.get("base_url")
                        if raw_step_base is not None and str(raw_step_base).strip():
                            step_origin = str(interpolate_value(raw_step_base, ctx)).strip().rstrip(
                                "/"
                            )
                        else:
                            step_origin = str(variables.get("base_url") or "").strip().rstrip("/")
                        if not step_origin:
                            raise ValueError(
                                "assert_authenticated requires base_url on the run, from the project, or on the step."
                            )
                        path = str(
                            interpolate_value(step.get("session_path") or "/api/auth/session", ctx)
                        ).strip()
                        sess_url = resolve_request_url(step_origin, path)
                        tr = time.time()
                        try:
                            resp = client.get(sess_url, timeout=default_timeout)
                        except httpx.TimeoutException as e:
                            raise TimeoutError(f"Request timeout: {e}") from e
                        last_response_time_ms = int((time.time() - tr) * 1000)
                        last_response = resp
                        try:
                            last_response_json = resp.json()
                        except Exception:
                            last_response_json = None
                        if resp.status_code != 200:
                            raise AssertionError(
                                f"assert_authenticated: expected HTTP 200, got {resp.status_code}"
                            )
                        sess = last_response_json if isinstance(last_response_json, dict) else {}
                        user = sess.get("user")
                        if user is None or user == {}:
                            raise AssertionError(
                                "assert_authenticated: session has no user (not logged in)"
                            )
                        dur = int((time.time() - t_step) * 1000)
                        log = f"assert_authenticated GET {sess_url} → HTTP 200 (session ok)"
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                                extra={"status_code": resp.status_code, "url": sess_url},
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    if action == "assert_status":
                        if last_response is None:
                            raise RuntimeError("No previous HTTP response; run a 'request' step first.")
                        exp = interpolate_value(step.get("expected"), ctx)
                        expected = int(exp)
                        got = last_response.status_code
                        if got != expected:
                            raise AssertionError(
                                f"assert_status: expected status {expected}, got {got}"
                            )
                        dur = int((time.time() - t_step) * 1000)
                        log = f"assert_status {got} == {expected}"
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    if action in ("assert_json_field", "assert_json_contains", "assert_json_type"):
                        if last_response is None:
                            raise RuntimeError("No previous HTTP response; run a 'request' step first.")
                        if last_response_json is None:
                            try:
                                last_response_json = last_response.json()
                            except Exception as e:
                                raise ValueError(
                                    "Response body is not valid JSON; cannot run JSON assertion."
                                ) from e
                        field = str(step.get("field") or "").strip()
                        if action == "assert_json_field":
                            exp = interpolate_value(step.get("expected"), ctx)
                            actual = get_json_path(last_response_json, field)
                            if actual != exp and not (
                                isinstance(actual, (int, float))
                                and isinstance(exp, (int, float))
                                and float(actual) == float(exp)
                            ):
                                raise AssertionError(
                                    f"assert_json_field {field!r}: expected {exp!r}, got {actual!r}"
                                )
                            log = f"assert_json_field {field} == {exp!r}"
                        elif action == "assert_json_contains":
                            sub = str(interpolate_value(step.get("expected"), ctx))
                            actual = get_json_path(last_response_json, field)
                            hay = "" if actual is None else str(actual)
                            if sub not in hay:
                                raise AssertionError(
                                    f"assert_json_contains {field!r}: expected substring {sub!r} not in {hay!r}"
                                )
                            log = f"assert_json_contains {field} contains {sub!r}"
                        else:
                            et = str(step.get("expected_type") or "").strip().lower()
                            if et not in _JSON_TYPES:
                                raise ValueError(
                                    f"assert_json_type: invalid expected_type {et!r}; use one of {sorted(_JSON_TYPES)}"
                                )
                            actual = get_json_path(last_response_json, field)
                            if not _value_matches_json_type(actual, et):
                                raise AssertionError(
                                    f"assert_json_type {field!r}: expected type {et!r}, value was {actual!r} ({type(actual).__name__})"
                                )
                            log = f"assert_json_type {field} is {et}"
                        dur = int((time.time() - t_step) * 1000)
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    if action == "assert_header":
                        if last_response is None:
                            raise RuntimeError("No previous HTTP response; run a 'request' step first.")
                        hname = str(interpolate_value(step.get("header") or "", ctx)).strip()
                        exp = interpolate_value(step.get("expected"), ctx)
                        exp_s = "" if exp is None else str(exp)
                        found_val = None
                        for k, v in last_response.headers.items():
                            if k.lower() == hname.lower():
                                found_val = v
                                break
                        if found_val is None:
                            raise AssertionError(
                                f"assert_header: header {hname!r} not present in response"
                            )
                        if str(found_val) != exp_s:
                            raise AssertionError(
                                f"assert_header {hname!r}: expected {exp_s!r}, got {found_val!r}"
                            )
                        dur = int((time.time() - t_step) * 1000)
                        log = f"assert_header {hname} == {exp_s!r}"
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    if action == "set_variable":
                        if last_response_json is None and last_response is not None:
                            try:
                                last_response_json = last_response.json()
                            except Exception as e:
                                raise ValueError(
                                    "set_variable: response body is not valid JSON"
                                ) from e
                        if last_response_json is None:
                            raise RuntimeError("No JSON response; run a successful JSON request first.")
                        name = str(step.get("name") or "").strip()
                        field = str(step.get("field") or "").strip()
                        if not name:
                            raise ValueError("set_variable requires 'name'")
                        if not field:
                            raise ValueError("set_variable requires 'field'")
                        val = get_json_path(last_response_json, field)
                        variables[name] = val
                        ctx[name] = val
                        dur = int((time.time() - t_step) * 1000)
                        log = f"set_variable {name} ← {field} ({val!r})"
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    if action == "assert_response_time":
                        max_ms = step.get("max_ms")
                        if max_ms is None:
                            raise ValueError("assert_response_time requires max_ms")
                        max_ms_i = int(max_ms)
                        if last_response_time_ms <= 0:
                            raise RuntimeError(
                                "assert_response_time: no response time recorded; run a request first."
                            )
                        if last_response_time_ms > max_ms_i:
                            raise AssertionError(
                                f"assert_response_time: {last_response_time_ms}ms > max {max_ms_i}ms"
                            )
                        dur = int((time.time() - t_step) * 1000)
                        log = f"assert_response_time {last_response_time_ms}ms <= {max_ms_i}ms"
                        steps_result.append(
                            _step_result(
                                index=i,
                                action=action,
                                status="pass",
                                duration_ms=dur,
                                log=log,
                            )
                        )
                        logs.append(f"[{i}] {log}")
                        continue

                    msg = f"Unsupported action for API runner: {action!r}"
                    raise ValueError(msg)

                except Exception as exc:
                    dur = int((time.time() - t_step) * 1000)
                    err = f"{type(exc).__name__}: {exc}"
                    logs.append(f"[{i}] FAIL {action}: {err}")
                    steps_result.append(
                        _step_result(
                            index=i,
                            action=action,
                            status="fail" if isinstance(exc, AssertionError) else "error",
                            duration_ms=dur,
                            log=f"{action} failed: {exc}",
                            error=err,
                        )
                    )
                    summary = f"Step {i} ({action}): {exc}"
                    return _finish_fail(logs, steps_result, t0, summary, err)

            # Legacy catalog assertions (non-step) after last request
            if assertions and last_response is not None:
                if not _evaluate_legacy_assertions(assertions, last_response, logs):
                    summary = "One or more legacy catalog assertions failed"
                    return _finish_fail(logs, steps_result, t0, summary, summary)

            duration_ms = int((time.time() - t0) * 1000)
            if last_response is None and not any(
                str((s or {}).get("action") or "").lower()
                in ("request", "api_request", "nextauth_login", "assert_authenticated")
                for s in (steps or [])
            ):
                return {
                    "ok": False,
                    "status": "error",
                    "logs": logs + ["No request step was executed."],
                    "steps": steps_result,
                    "duration_ms": duration_ms,
                    "reason": "no request executed",
                    "error_summary": "no request executed",
                }

            has_explicit_http_asserts = any(
                str((s or {}).get("action") or "").lower()
                in (
                    "assert_status",
                    "assert_json_field",
                    "assert_json_contains",
                    "assert_json_type",
                    "assert_header",
                    "assert_response_time",
                    "assert_authenticated",
                )
                for s in steps
            )
            if last_response is not None and not assertions and not has_explicit_http_asserts:
                passed = last_response.is_success
            else:
                passed = True

            status = "pass" if passed else "fail"
            reason = None if passed else (
                f"HTTP {last_response.status_code}" if last_response else None
            )
            return {
                "ok": passed,
                "status": status,
                "logs": logs,
                "steps": steps_result,
                "duration_ms": duration_ms,
                "reason": reason,
                "error_summary": reason,
            }

    except Exception as exc:
        duration_ms = int((time.time() - t0) * 1000)
        err = f"{type(exc).__name__}: {exc}"
        logs.append(f"Runner fatal: {err}")
        return {
            "ok": False,
            "status": "error",
            "logs": logs,
            "steps": steps_result,
            "duration_ms": duration_ms,
            "reason": err,
            "error_summary": err,
        }


def _finish_fail(
    logs: List[str],
    steps_result: List[Dict[str, Any]],
    t0: float,
    reason: str,
    error_summary: str,
) -> Dict[str, Any]:
    duration_ms = int((time.time() - t0) * 1000)
    return {
        "ok": False,
        "status": "fail",
        "logs": logs,
        "steps": steps_result,
        "duration_ms": duration_ms,
        "reason": reason,
        "error_summary": error_summary,
    }
