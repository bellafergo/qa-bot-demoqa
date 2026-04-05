# services/test_catalog_service.py
"""
Test Catalog Service
====================
Manages the test case catalog and drives test execution.

Persistence is backed by SQLite via repository classes.
In-memory caching is not used — the DB is the single source of truth.

Architecture:
  TestCatalogService
      ↓  converts steps
  execute_test()            ← existing Playwright runner
      ↓  produces runner result
  TestRun record            ← persisted via TestRunRepository

Thread-safe: repository sessions use per-operation commits.

Upgrade path: replace sqlite engine URL with Postgres/Supabase DSN.
"""
from __future__ import annotations

import logging
import time
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from models.test_case import TestCase, TestCaseCreate, TestStep, TestAssertion
from models.test_run import TestRun, SuiteRunResult
from services.db.catalog_repository import catalog_repo
from services.db.test_run_repository import test_run_repo

logger = logging.getLogger("vanya.test_catalog")


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


# ── Step format normalizer ────────────────────────────────────────────────────

_ACTION_ALIASES: Dict[str, str] = {
    "input":  "fill",
    "type":   "fill",
    "enter":  "fill",
    "navigate": "goto",
    "open":   "goto",
    "wait":   "wait_ms",
    "sleep":  "wait_ms",
    "assert_text": "assert_text_contains",
    "assert_visible_text": "assert_text_contains",
    "text_visible": "assert_text_contains",
    "check_text": "assert_text_contains",
    "verify_text": "assert_text_contains",
    "assert_url": "assert_url_contains",
    "url_contains": "assert_url_contains",
    "visible": "assert_visible",
    "element_visible": "assert_visible",
    "not_visible": "assert_not_visible",
    "element_not_visible": "assert_not_visible",
}


def _normalize_action(action: str) -> str:
    a = (action or "").strip().lower()
    return _ACTION_ALIASES.get(a, a)


def _step_to_runner(step: Dict[str, Any]) -> Dict[str, Any]:
    """
    Convert one catalog step dict to the runner's expected format.

    Catalog format:
      {"action": "goto",   "value": "https://..."}
      {"action": "input",  "target": "username", "value": "testuser"}
      {"action": "click",  "target": "login button"}
      {"action": "wait_ms","ms": 500}

    Runner format:
      {"action": "goto",   "url": "https://..."}
      {"action": "fill",   "selector": "username", "value": "testuser"}
      {"action": "click",  "selector": "login button"}
      {"action": "wait_ms","ms": 500}
    """
    s = dict(step)
    action = _normalize_action(s.get("action", ""))
    s["action"] = action

    if action == "goto":
        if not s.get("url") and s.get("value"):
            s["url"] = s.pop("value")
        if not s.get("url") and s.get("target"):
            s["url"] = s.pop("target")

    elif action in ("fill", "click", "press", "assert_visible", "assert_not_visible"):
        if not s.get("selector") and s.get("target"):
            s["selector"] = s.pop("target")

    elif action == "assert_text_contains":
        if not s.get("text") and s.get("value"):
            s["text"] = s.pop("value")
        if not s.get("selector") and s.get("target"):
            s["selector"] = s.pop("target")
        s.setdefault("selector", "body")

    elif action == "assert_url_contains":
        if not s.get("value") and s.get("target"):
            s["value"] = s.pop("target")

    elif action == "wait_ms":
        if s.get("ms") is None and s.get("value") is not None:
            try:
                s["ms"] = int(s.pop("value"))
            except (ValueError, TypeError):
                s.pop("value", None)

    return s


def _assertion_to_step(assertion: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """Convert a TestCase assertion dict to a runner step dict."""
    atype  = _normalize_action(str(assertion.get("type") or ""))
    value  = assertion.get("value")
    target = assertion.get("target") or assertion.get("selector")

    if atype == "assert_text_contains":
        if not value:
            return None
        return {"action": "assert_text_contains", "selector": target or "body", "text": str(value)}

    if atype == "assert_url_contains":
        if not value:
            return None
        return {"action": "assert_url_contains", "value": str(value)}

    if atype == "assert_visible":
        if not target:
            return None
        return {"action": "assert_visible", "selector": str(target)}

    if atype == "assert_not_visible":
        if not target:
            return None
        return {"action": "assert_not_visible", "selector": str(target)}

    logger.warning("test_catalog: unknown assertion type %r — skipped", atype)
    return None


def _build_runner_steps(
    test_case: TestCase,
    base_url: Optional[str] = None,
) -> List[Dict[str, Any]]:
    """Produce the full ordered step list ready for execute_test()."""
    steps: List[Dict[str, Any]] = []
    effective_base = base_url or test_case.base_url or ""

    raw_steps = [s.model_dump() if hasattr(s, "model_dump") else dict(s) for s in test_case.steps]

    # Expand generate_test_data steps and substitute {{entity.field}} placeholders
    # before the steps reach the Playwright runner.
    try:
        from services.test_data_service import preprocess_data_steps
        raw_steps, _ = preprocess_data_steps(raw_steps)
    except Exception:
        logger.warning("test_catalog: test_data preprocessing failed (non-fatal), continuing with raw steps")

    runner_steps = [_step_to_runner(s) for s in raw_steps]

    has_goto = any(s.get("action") == "goto" for s in runner_steps)
    if not has_goto and effective_base:
        steps.append({"action": "goto", "url": effective_base})
    elif not has_goto and not effective_base:
        logger.warning("test_catalog: %s has no goto and no base_url", test_case.test_case_id)

    steps.extend(runner_steps)

    raw_assertions = [a.model_dump() if hasattr(a, "model_dump") else dict(a) for a in test_case.assertions]
    for a in raw_assertions:
        step = _assertion_to_step(a)
        if step:
            steps.append(step)

    return steps


# ── Desktop step builder ──────────────────────────────────────────────────────

_DESKTOP_ALIASES: Dict[str, str] = {
    "input":      "input_text",
    "fill":       "input_text",
    "type":       "input_text",
    "assert_text": "assert_text_contains",
    "check_text": "assert_text_contains",
    "verify_text": "assert_text_contains",
    "exists":     "assert_exists",
    "wait":       "wait_for",
    "focus":      "focus_window",
    "attach":     "attach_window",
    "launch":     "launch_app",
    "start":      "launch_app",
    "open":       "launch_app",
    "keys":       "type_keys",
    "sendkeys":   "type_keys",
    "sleep":      "wait_ms",
}


def _normalize_desktop_action(action: str) -> str:
    a = (action or "").strip().lower()
    return _DESKTOP_ALIASES.get(a, a)


def _build_desktop_steps(test_case: TestCase) -> List[Dict[str, Any]]:
    """
    Convert catalog steps to the desktop runner format.

    Unlike _build_runner_steps(), this function:
      - Does NOT inject a goto step (desktop apps don't use URLs)
      - Preserves 'target' as-is (not renamed to 'selector')
      - Normalizes action names using desktop aliases
      - Appends assertion steps as assert_text_contains / assert_exists
    """
    raw_steps = [s.model_dump() if hasattr(s, "model_dump") else dict(s) for s in test_case.steps]

    desktop_steps: List[Dict[str, Any]] = []
    for s in raw_steps:
        step = dict(s)
        step["action"] = _normalize_desktop_action(step.get("action", ""))
        desktop_steps.append(step)

    # Append assertions as desktop assert steps
    raw_assertions = [a.model_dump() if hasattr(a, "model_dump") else dict(a) for a in test_case.assertions]
    for a in raw_assertions:
        atype  = _normalize_desktop_action(str(a.get("type") or ""))
        target = a.get("target") or a.get("selector") or ""
        value  = a.get("value") or ""
        if atype in ("assert_text_contains", "assert_text"):
            if value:
                desktop_steps.append({"action": "assert_text_contains", "target": target, "value": value})
        elif atype in ("assert_exists", "visible", "assert_visible"):
            if target:
                desktop_steps.append({"action": "assert_exists", "target": target})

    return desktop_steps


# ── Public service API ────────────────────────────────────────────────────────

class TestCatalogService:
    """
    Stateless service class. All state is persisted via repositories.
    """

    # ── Catalog CRUD ──────────────────────────────────────────────────────────

    def list_test_cases(
        self,
        *,
        module: Optional[str] = None,
        type_: Optional[str] = None,
        priority: Optional[str] = None,
        status: Optional[str] = "active",
        test_type: Optional[str] = None,
        search: Optional[str] = None,
        tags: Optional[List[str]] = None,
        project_id: Optional[str] = None,
        limit: int = 200,
    ) -> List[TestCase]:
        return catalog_repo.list_test_cases(
            module=module, type_=type_, priority=priority,
            status=status, test_type=test_type, search=search, tags=tags,
            project_id=project_id, limit=limit,
        )

    def get_test_case(self, test_case_id: str) -> Optional[TestCase]:
        return catalog_repo.get_test_case(test_case_id)

    def create_test_case(
        self,
        payload: TestCaseCreate,
        *,
        source:      str = "manual",
        change_note: str = "Initial version",
    ) -> TestCase:
        if catalog_repo.get_test_case(payload.test_case_id) is not None:
            raise ValueError(
                f"test_case_id '{payload.test_case_id}' already exists. "
                "Use a unique ID or delete the existing one first."
            )
        tc = TestCase(
            test_case_id = payload.test_case_id,
            name         = payload.name,
            module       = payload.module,
            type         = payload.type,
            priority     = payload.priority,
            status       = payload.status,
            test_type    = getattr(payload, "test_type", "ui") or "ui",
            version      = payload.version,
            tags         = payload.tags,
            base_url     = payload.base_url,
            project_id   = getattr(payload, "project_id", None) or "default",
            steps        = [TestStep(**s) for s in payload.steps],
            assertions   = [TestAssertion(**a) for a in payload.assertions],
        )
        catalog_repo.create_test_case(tc)
        # Always snapshot v1 on creation
        try:
            from services.db.test_version_repository import test_version_repo
            pass  # source / change_note come from the keyword arguments above
            test_version_repo.snapshot(tc, version_number=1, source=source, change_note=change_note)
        except Exception:
            logger.warning("test_catalog: failed to snapshot v1 for %s (non-fatal)", tc.test_case_id)
        logger.info("test_catalog: created %s — %s", tc.test_case_id, tc.name)
        return tc

    def update_test_case(
        self,
        test_case_id: str,
        data: dict,
        *,
        source:      str = "manual",
        change_note: str = "",
    ) -> Optional[TestCase]:
        """
        Update mutable fields of an existing test case and create a new version snapshot.

        *data* keys: name, module, type, priority, status, test_type, project_id,
                     steps, assertions, tags, base_url.

        Returns the updated TestCase, or None if not found.
        The version counter in test_cases is incremented automatically.
        """
        current = catalog_repo.get_test_case(test_case_id)
        if current is None:
            return None

        new_version_number = current.version + 1
        data["version"] = new_version_number

        # Coerce steps/assertions to plain dicts for storage
        if "steps" in data and data["steps"] is not None:
            data["steps"] = [
                s.model_dump() if hasattr(s, "model_dump") else dict(s)
                for s in data["steps"]
            ]
        if "assertions" in data and data["assertions"] is not None:
            data["assertions"] = [
                a.model_dump() if hasattr(a, "model_dump") else dict(a)
                for a in data["assertions"]
            ]

        # Auto-fix deterministic rules (no LLM)
        from services.test_auto_fixer import auto_fix_test
        fix_result = auto_fix_test(
            data.get("steps",      []),
            data.get("assertions", []),
        )
        data["steps"]      = fix_result["steps"]
        data["assertions"] = fix_result["assertions"]
        if fix_result["changes"]:
            fix_summary = "; ".join(c["message"] for c in fix_result["changes"])
            change_note = f"{change_note} [auto-fix: {fix_summary}]".strip() if change_note else f"[auto-fix: {fix_summary}]"

        updated = catalog_repo.update_test_case(test_case_id, data)
        if updated is None:
            return None

        try:
            from services.db.test_version_repository import test_version_repo
            test_version_repo.snapshot(
                updated,
                version_number = new_version_number,
                source         = source,
                change_note    = change_note or f"Updated to v{new_version_number}",
            )
        except Exception:
            logger.warning("test_catalog: failed to snapshot v%s for %s (non-fatal)", new_version_number, test_case_id)

        logger.info("test_catalog: updated %s → v%s", test_case_id, new_version_number)
        return updated

    def list_versions(self, test_case_id: str):
        """Return all version snapshots for a test, newest first."""
        from services.db.test_version_repository import test_version_repo
        return test_version_repo.list_versions(test_case_id)

    def get_version(self, test_case_id: str, version_number: int):
        """Return a single version snapshot, or None."""
        from services.db.test_version_repository import test_version_repo
        return test_version_repo.get_version(test_case_id, version_number)

    def diff_versions(self, test_case_id: str, from_version: int, to_version: int):
        """
        Compare two version snapshots of a test case.

        Returns a VersionDiff with:
          - diff.steps      — added / removed step entries
          - diff.assertions — added / removed assertion entries
          - diff.fields     — changed scalar fields (name, module, priority, …)
          - identical       — True when nothing changed

        No external libraries used. Steps and assertions are compared by their
        JSON-serialised canonical form (sort_keys=True), so order matters for
        detecting changes but field-order within each object does not.
        """
        import json
        from models.test_version_models import VersionDiff

        from services.db.test_version_repository import test_version_repo

        a = test_version_repo.get_version(test_case_id, from_version)
        b = test_version_repo.get_version(test_case_id, to_version)

        if a is None:
            raise ValueError(f"Version {from_version} not found for '{test_case_id}'")
        if b is None:
            raise ValueError(f"Version {to_version} not found for '{test_case_id}'")

        # ── Step / assertion diff ────────────────────────────────────────────
        def _label_item(item: dict) -> str:
            """Human-readable one-liner for a step or assertion dict."""
            action = item.get("action") or item.get("type") or "?"
            parts  = [action]
            for key in ("url", "selector", "target", "text", "value"):
                v = item.get(key)
                if v and str(v) not in parts:
                    parts.append(str(v))
            return " ".join(parts)

        def _list_diff(old: list, new: list) -> list:
            """Return added/removed entries preserving order of appearance."""
            old_keys = {json.dumps(x, sort_keys=True) for x in old}
            new_keys = {json.dumps(x, sort_keys=True) for x in new}
            result   = []
            for x in old:
                if json.dumps(x, sort_keys=True) not in new_keys:
                    result.append({"type": "removed", "value": _label_item(x)})
            for x in new:
                if json.dumps(x, sort_keys=True) not in old_keys:
                    result.append({"type": "added", "value": _label_item(x)})
            return result

        steps_diff      = _list_diff(a.steps      or [], b.steps      or [])
        assertions_diff = _list_diff(a.assertions or [], b.assertions or [])

        # ── Scalar field diff ────────────────────────────────────────────────
        TRACKED = ("name", "module", "priority", "status", "test_type", "base_url")
        fields_diff: dict = {}
        for field in TRACKED:
            va = str(getattr(a, field) or "")
            vb = str(getattr(b, field) or "")
            if va != vb:
                fields_diff[field] = {"from": va, "to": vb}

        identical = not steps_diff and not assertions_diff and not fields_diff

        return VersionDiff(
            test_case_id = test_case_id,
            from_version = from_version,
            to_version   = to_version,
            identical    = identical,
            diff         = {
                "steps":      steps_diff,
                "assertions": assertions_diff,
                "fields":     fields_diff,
            },
        )

    def rollback_test_case(
        self,
        test_case_id: str,
        version_number: int,
        reason: str = "",
    ):
        """
        Restore a test case to the content of *version_number*.

        Mechanics:
          1. Fetch the target version snapshot.
          2. Fetch the current HEAD test case.
          3. Update HEAD with the snapshot's content, incrementing version counter.
          4. Write a new snapshot with source="rollback" — history is preserved.

        Returns RollbackResponse.
        Raises ValueError if test or version not found.
        """
        from models.test_version_models import RollbackResponse
        from services.db.test_version_repository import test_version_repo

        target = test_version_repo.get_version(test_case_id, version_number)
        if target is None:
            raise ValueError(f"Version {version_number} not found for '{test_case_id}'")

        current = catalog_repo.get_test_case(test_case_id)
        if current is None:
            raise ValueError(f"Test case '{test_case_id}' not found")

        new_vnum = current.version + 1
        change_note = reason or f"Rollback to v{version_number}"

        data = dict(
            name        = target.name,
            module      = target.module,
            type        = target.type,
            priority    = target.priority,
            status      = target.status,
            test_type   = target.test_type,
            steps       = target.steps,
            assertions  = target.assertions,
            tags        = target.tags,
            base_url    = target.base_url,
            version     = new_vnum,
        )

        updated = catalog_repo.update_test_case(test_case_id, data)
        if updated is None:
            raise RuntimeError(f"Failed to update '{test_case_id}' during rollback")

        try:
            test_version_repo.snapshot(
                updated,
                version_number = new_vnum,
                source         = "rollback",
                change_note    = change_note,
            )
        except Exception:
            logger.warning("test_catalog: rollback snapshot failed for %s (non-fatal)", test_case_id)

        logger.info(
            "test_catalog: rollback %s v%s → restored from v%s → new v%s",
            test_case_id, current.version, version_number, new_vnum,
        )

        return RollbackResponse(
            ok                       = True,
            test_case_id             = test_case_id,
            rolled_back_from_version = version_number,
            new_version              = new_vnum,
            message                  = f"Rolled back to v{version_number} content. New version is v{new_vnum}. History preserved.",
        )

    def delete_test_case(self, test_case_id: str) -> bool:
        ok = catalog_repo.delete_test_case(test_case_id)
        if ok:
            logger.info("test_catalog: deleted %s", test_case_id)
        return ok

    # ── Execution ─────────────────────────────────────────────────────────────

    def run_test_case(
        self,
        test_case_id: str,
        *,
        environment: str = "default",
        base_url: Optional[str] = None,
        headless: bool = True,
        timeout_s: Optional[int] = None,
        extra_meta: Optional[Dict[str, Any]] = None,
        correlation_id: Optional[str] = None,
        client_id: Optional[str] = None,
        workspace_id: Optional[str] = None,
    ) -> TestRun:
        tc = self.get_test_case(test_case_id)
        if tc is None:
            raise ValueError(f"Test case '{test_case_id}' not found in catalog")
        if tc.status == "inactive":
            raise ValueError(f"Test case '{test_case_id}' is inactive.")

        return self._execute(
            tc,
            environment=environment,
            base_url=base_url,
            headless=headless,
            timeout_s=timeout_s,
            extra_meta=extra_meta,
            correlation_id=correlation_id,
            client_id=client_id,
            workspace_id=workspace_id,
        )

    def run_suite(
        self,
        *,
        environment: str = "default",
        base_url: Optional[str] = None,
        headless: bool = True,
        timeout_s: Optional[int] = None,
        module: Optional[str] = None,
        type_: Optional[str] = None,
        priority: Optional[str] = None,
        tags: Optional[List[str]] = None,
        test_case_ids: Optional[List[str]] = None,
        limit: int = 50,
        correlation_id: Optional[str] = None,
        client_id: Optional[str] = None,
        workspace_id: Optional[str] = None,
    ) -> SuiteRunResult:
        t0 = time.time()

        if test_case_ids:
            cases = [self.get_test_case(tcid) for tcid in test_case_ids]
            cases = [c for c in cases if c is not None and c.status == "active"]
            filter_applied: Dict[str, Any] = {"test_case_ids": test_case_ids}
        else:
            cases = self.list_test_cases(
                module=module, type_=type_, priority=priority,
                tags=tags, status="active", limit=limit,
            )
            filter_applied = {
                k: v for k, v in
                {"module": module, "type": type_, "priority": priority, "tags": tags}.items()
                if v is not None
            }

        suite_id = str(uuid.uuid4())
        runs: List[TestRun] = []
        passed = failed = errors = 0

        for tc in cases:
            try:
                run = self._execute(tc, environment=environment, base_url=base_url,
                                    headless=headless, timeout_s=timeout_s, correlation_id=correlation_id,
                                    client_id=client_id, workspace_id=workspace_id)
                runs.append(run)
                if run.status == "pass":
                    passed += 1
                elif run.status == "fail":
                    failed += 1
                else:
                    errors += 1
            except Exception as e:
                logger.exception("test_catalog: suite run error on %s", tc.test_case_id)
                err_run = TestRun(
                    test_case_id=tc.test_case_id, test_name=tc.name,
                    environment=environment, status="error",
                    logs=[f"Suite runner error: {type(e).__name__}: {e}"],
                )
                runs.append(err_run)
                errors += 1

        total_ms = int((time.time() - t0) * 1000)
        result = SuiteRunResult(
            suite_run_id=suite_id, finished_at=_now_utc(), environment=environment,
            total=len(runs), passed=passed, failed=failed, errors=errors,
            duration_ms=total_ms, runs=runs, filter_applied=filter_applied,
        )
        logger.info(
            "test_catalog: suite %s — %d total, %d pass, %d fail, %d error, %dms",
            suite_id, len(runs), passed, failed, errors, total_ms,
        )
        return result

    # ── Run history ───────────────────────────────────────────────────────────

    def list_runs(
        self,
        *,
        test_case_id: Optional[str] = None,
        limit: int = 100,
    ) -> List[TestRun]:
        return test_run_repo.list_runs(test_case_id=test_case_id, limit=limit)

    def get_run(self, run_id: str) -> Optional[TestRun]:
        return test_run_repo.get_run(run_id)

    # ── Internal ──────────────────────────────────────────────────────────────

    def _execute(
        self,
        tc: TestCase,
        *,
        environment: str,
        base_url: Optional[str],
        headless: bool,
        timeout_s: Optional[int],
        auth_config: Optional[Dict[str, Any]] = None,
        extra_meta: Optional[Dict[str, Any]] = None,
        correlation_id: Optional[str] = None,
        client_id: Optional[str] = None,
        workspace_id: Optional[str] = None,
    ) -> TestRun:
        """Build steps, run via appropriate runner (UI or API), persist result."""
        t0 = time.time()
        run_id     = str(uuid.uuid4())
        evidence_id = f"TC-{uuid.uuid4().hex[:10].upper()}"

        # Enrich meta for traceability across runner/logs/persistence
        if correlation_id or client_id or workspace_id:
            extra_meta = dict(extra_meta or {})
            extra_meta.setdefault("correlation_id", correlation_id)
            if client_id:
                extra_meta["client_id"] = client_id
            if workspace_id:
                extra_meta["workspace_id"] = workspace_id

        logger.info("test_catalog: executing %s (%s) — run_id=%s env=%s test_type=%s",
                    tc.test_case_id, tc.name, run_id, environment,
                    getattr(tc, "test_type", "ui"))

        steps = _build_runner_steps(tc, base_url=base_url)
        logger.debug("test_catalog: %s → %d steps: %s",
                     tc.test_case_id, len(steps), [s.get("action") for s in steps])

        tc_test_type = getattr(tc, "test_type", "ui") or "ui"

        try:
            if tc_test_type == "api":
                from services.api_runner import run_api_test
                assertions_raw = [a.model_dump() for a in tc.assertions]
                result = run_api_test(
                    steps      = steps,
                    base_url   = base_url or tc.base_url or "",
                    assertions = assertions_raw,
                    auth_config = auth_config,
                    timeout_s  = timeout_s or 30,
                )
            elif tc_test_type == "desktop":
                from runners.desktop_runner import run_desktop_test
                desktop_steps = _build_desktop_steps(tc)
                result = run_desktop_test(
                    steps     = desktop_steps,
                    timeout_s = timeout_s,
                )
            else:
                from runner import execute_test
                result = execute_test(
                    steps=steps,
                    base_url=base_url or tc.base_url,
                    headless=headless,
                    timeout_s=timeout_s,
                    correlation_id=correlation_id,
                )
            if not isinstance(result, dict):
                result = {}

            _ok      = result.get("ok")
            _outcome = str(result.get("outcome") or "").lower()
            _status  = str(result.get("status")  or "").lower()

            run_status: Literal["pass", "fail", "error", "running"]
            if _ok is True:
                run_status = "pass"
            elif _outcome == "pass" or _status == "passed":
                run_status = "pass"
            elif _status == "error":
                run_status = "error"
            else:
                run_status = "fail"

            duration_ms = result.get("duration_ms") or int((time.time() - t0) * 1000)

            evidence_url = result.get("evidence_url")
            report_url = result.get("report_url")
            if result.get("screenshot_b64"):
                try:
                    from core.settings import settings
                    from services.evidence_pipeline import process_evidence
                    if getattr(settings, "HAS_CLOUDINARY", False):
                        ev = process_evidence(
                            runner=result,
                            prompt=tc.name or "",
                            base_url=(base_url or tc.base_url or ""),
                            steps=result.get("steps") or steps,
                            evidence_id=evidence_id,
                            status=run_status,
                            duration_ms=duration_ms,
                            meta={"source": "catalog", "test_case_id": tc.test_case_id},
                        )
                        evidence_url = ev.get("evidence_url")
                        report_url = ev.get("report_url")
                except Exception as e:
                    logger.warning("test_catalog: evidence_pipeline failed for %s — %s", tc.test_case_id, e)

            run = TestRun(
                run_id=run_id, test_case_id=tc.test_case_id, test_name=tc.name,
                environment=environment, status=run_status, duration_ms=int(duration_ms),
                evidence_id=evidence_id, evidence_url=evidence_url,
                report_url=report_url,
                logs=result.get("logs") or [], steps_result=result.get("steps") or [],
                meta={
                    "runner_ok":      run_status == "pass",
                    "runner_outcome": _outcome,
                    "runner_reason":  result.get("reason"),
                    "tc_module":      tc.module,
                    "tc_type":        tc.type,
                    "tc_priority":    tc.priority,
                    "tc_version":     tc.version,
                    "evidence":       result.get("evidence"),
                    "screenshot_b64": result.get("screenshot_b64"),
                    **(extra_meta or {}),
                },
            )

        except Exception as e:
            logger.exception("test_catalog: runner error for %s", tc.test_case_id)
            run = TestRun(
                run_id=run_id, test_case_id=tc.test_case_id, test_name=tc.name,
                environment=environment, status="error",
                duration_ms=int((time.time() - t0) * 1000),
                evidence_id=evidence_id,
                logs=[f"Runner error: {type(e).__name__}: {e}"],
                meta=extra_meta or {},
            )

        self._save_run(run)
        return run

    def _save_run(self, run: TestRun) -> None:
        test_run_repo.create_run(run)
        try:
            from services.alerting import schedule_slack_alert_on_failed_run

            schedule_slack_alert_on_failed_run(run)
        except Exception:
            pass


# Module-level singleton — all routes import this instance
catalog_service = TestCatalogService()


# ── Test isolation helpers ────────────────────────────────────────────────────

def _reset_for_testing() -> None:
    """Wipe all catalog and run data. For use in tests only."""
    catalog_repo.clear_all()
    test_run_repo.clear_all()


# ── Seed loader ───────────────────────────────────────────────────────────────

def load_seed_catalog() -> None:
    """Load demo test cases only if the catalog table is empty."""
    from services.test_catalog_seed import SEED_TEST_CASES

    if not catalog_repo.is_empty():
        logger.debug("test_catalog: seed already loaded, skipped")
        return

    loaded = 0
    for payload_dict in SEED_TEST_CASES:
        try:
            payload = TestCaseCreate(**payload_dict)
            catalog_service.create_test_case(payload)
            loaded += 1
        except Exception as e:
            logger.warning("test_catalog: seed failed for %s — %s",
                           payload_dict.get("test_case_id", "?"), e)

    if loaded:
        logger.info("test_catalog: loaded %d seed test cases", loaded)
