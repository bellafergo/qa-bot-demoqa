# services/test_catalog_service.py
"""
Test Catalog Service
====================
Manages the in-memory test case catalog and drives test execution.

Architecture:
  TestCatalogService
      ↓  converts steps
  execute_test()            ← existing Playwright runner (runners/generic_steps.py)
      ↓  produces runner result
  TestRun record            ← stored in _RUN_STORE (in-memory, TTL)

Thread-safe. Designed to be a module-level singleton so imports don't
duplicate state.

Upgrade path: swap _CATALOG / _RUN_STORE with a real DB by replacing
the four private CRUD helpers at the bottom of this file.
"""
from __future__ import annotations

import logging
import threading
import time
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Literal, Optional

from models.test_case import TestCase, TestCaseCreate, TestStep, TestAssertion
from models.test_run import TestRun, SuiteRunResult

logger = logging.getLogger("vanya.test_catalog")


# ── In-memory stores (replace with DB for production scale) ───────────────────

_lock = threading.RLock()

# test_case_id -> TestCase
_CATALOG: Dict[str, TestCase] = {}

# run_id -> TestRun  (kept for TTL-style cleanup; list_runs returns recent ones)
_RUN_STORE: Dict[str, TestRun] = {}

# Ordered list of run_ids for list operations (most recent first)
_RUN_ORDER: List[str] = []

_MAX_RUNS_KEPT = 500   # GC threshold — prune oldest when exceeded


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


# ── Step format normalizer ────────────────────────────────────────────────────

# Maps catalog action names → runner action names
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

    Catalog format (what test authors write):
      {"action": "goto",   "value": "https://..."}
      {"action": "input",  "target": "username", "value": "testuser"}
      {"action": "click",  "target": "login button"}
      {"action": "wait_ms","ms": 500}

    Runner format (what execute_test() expects):
      {"action": "goto",   "url": "https://..."}
      {"action": "fill",   "selector": "username", "value": "testuser"}
      {"action": "click",  "selector": "login button"}
      {"action": "wait_ms","ms": 500}
    """
    s = dict(step)
    action = _normalize_action(s.get("action", ""))
    s["action"] = action

    # goto: "value" -> "url"
    if action == "goto":
        if not s.get("url") and s.get("value"):
            s["url"] = s.pop("value")
        # "target" can also hold the URL in some authoring styles
        if not s.get("url") and s.get("target"):
            s["url"] = s.pop("target")

    # fill/click/press/assert_visible: "target" -> "selector"
    elif action in ("fill", "click", "press", "assert_visible", "assert_not_visible"):
        if not s.get("selector") and s.get("target"):
            s["selector"] = s.pop("target")

    # assert_text_contains: "value" -> "text", default selector "body"
    elif action == "assert_text_contains":
        if not s.get("text") and s.get("value"):
            s["text"] = s.pop("value")
        if not s.get("selector") and s.get("target"):
            s["selector"] = s.pop("target")
        s.setdefault("selector", "body")

    # assert_url_contains: "value" stays as "value"
    elif action == "assert_url_contains":
        if not s.get("value") and s.get("target"):
            s["value"] = s.pop("target")

    # wait_ms: "value" can carry the milliseconds
    elif action == "wait_ms":
        if s.get("ms") is None and s.get("value") is not None:
            try:
                s["ms"] = int(s.pop("value"))
            except (ValueError, TypeError):
                s.pop("value", None)

    return s


def _assertion_to_step(assertion: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    """
    Convert a TestCase assertion dict to a runner step dict.

    Assertion format:
      {"type": "text_visible",       "value": "Welcome"}
      {"type": "url_contains",       "value": "/inventory"}
      {"type": "element_visible",    "target": "#submit"}
      {"type": "element_not_visible","target": ".error-msg"}
    """
    atype = _normalize_action(str(assertion.get("type") or ""))
    value = assertion.get("value")
    target = assertion.get("target") or assertion.get("selector")

    if atype == "assert_text_contains":
        if not value:
            return None
        return {
            "action": "assert_text_contains",
            "selector": target or "body",
            "text": str(value),
        }

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

    # Unknown assertion type — skip
    logger.warning("test_catalog: unknown assertion type %r — skipped", atype)
    return None


def _build_runner_steps(
    test_case: TestCase,
    base_url: Optional[str] = None,
) -> List[Dict[str, Any]]:
    """
    Produce the full ordered step list ready for execute_test().

    Order:
      1. goto (from first step or base_url)
      2. remaining steps (normalized)
      3. assertion steps (converted from test_case.assertions)
    """
    steps: List[Dict[str, Any]] = []

    # Determine effective base_url
    effective_base = base_url or test_case.base_url or ""

    raw_steps = [
        s.model_dump() if hasattr(s, "model_dump") else dict(s)
        for s in test_case.steps
    ]
    runner_steps = [_step_to_runner(s) for s in raw_steps]

    # Ensure the first step is goto
    has_goto = any(s.get("action") == "goto" for s in runner_steps)
    if not has_goto and effective_base:
        steps.append({"action": "goto", "url": effective_base})
    elif not has_goto and not effective_base:
        logger.warning(
            "test_catalog: test case %s has no goto and no base_url",
            test_case.test_case_id,
        )

    steps.extend(runner_steps)

    # Convert assertions → steps
    raw_assertions = [
        a.model_dump() if hasattr(a, "model_dump") else dict(a)
        for a in test_case.assertions
    ]
    for a in raw_assertions:
        step = _assertion_to_step(a)
        if step:
            steps.append(step)

    return steps


# ── Public service API ────────────────────────────────────────────────────────

class TestCatalogService:
    """
    Stateless service class. All state lives in the module-level dicts
    so it survives class re-instantiation.
    """

    # ── Catalog CRUD ──────────────────────────────────────────────────────────

    def list_test_cases(
        self,
        *,
        module: Optional[str] = None,
        type_: Optional[str] = None,
        priority: Optional[str] = None,
        status: Optional[str] = "active",
        tags: Optional[List[str]] = None,
        limit: int = 200,
    ) -> List[TestCase]:
        """Return test cases matching the given filters, most recently created first."""
        with _lock:
            cases = list(_CATALOG.values())

        if module:
            cases = [c for c in cases if c.module.lower() == module.lower()]
        if type_:
            cases = [c for c in cases if c.type == type_]
        if priority:
            cases = [c for c in cases if c.priority == priority]
        if status:
            cases = [c for c in cases if c.status == status]
        if tags:
            tag_set = {t.lower() for t in tags}
            cases = [c for c in cases if tag_set.issubset({t.lower() for t in c.tags})]

        cases.sort(key=lambda c: c.created_at, reverse=True)
        return cases[:limit]

    def get_test_case(self, test_case_id: str) -> Optional[TestCase]:
        with _lock:
            return _CATALOG.get(test_case_id)

    def create_test_case(self, payload: TestCaseCreate) -> TestCase:
        with _lock:
            if payload.test_case_id in _CATALOG:
                raise ValueError(
                    f"test_case_id '{payload.test_case_id}' already exists. "
                    "Use a unique ID or delete the existing one first."
                )
            tc = TestCase(
                test_case_id=payload.test_case_id,
                name=payload.name,
                module=payload.module,
                type=payload.type,
                priority=payload.priority,
                status=payload.status,
                version=payload.version,
                tags=payload.tags,
                base_url=payload.base_url,
                steps=[TestStep(**s) for s in payload.steps],
                assertions=[TestAssertion(**a) for a in payload.assertions],
            )
            _CATALOG[tc.test_case_id] = tc
            logger.info("test_catalog: created %s — %s", tc.test_case_id, tc.name)
            return tc

    def delete_test_case(self, test_case_id: str) -> bool:
        with _lock:
            if test_case_id not in _CATALOG:
                return False
            del _CATALOG[test_case_id]
            logger.info("test_catalog: deleted %s", test_case_id)
            return True

    # ── Execution ─────────────────────────────────────────────────────────────

    def run_test_case(
        self,
        test_case_id: str,
        *,
        environment: str = "default",
        base_url: Optional[str] = None,
        headless: bool = True,
        timeout_s: Optional[int] = None,
    ) -> TestRun:
        """
        Execute a single test case and return a TestRun record.

        Raises ValueError if test_case_id not found.
        """
        tc = self.get_test_case(test_case_id)
        if tc is None:
            raise ValueError(f"Test case '{test_case_id}' not found in catalog")
        if tc.status == "inactive":
            raise ValueError(
                f"Test case '{test_case_id}' is inactive. Activate it before running."
            )

        return self._execute(tc, environment=environment, base_url=base_url,
                             headless=headless, timeout_s=timeout_s)

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
    ) -> SuiteRunResult:
        """
        Execute multiple test cases and aggregate results.

        If test_case_ids is provided it takes precedence over filters.
        Otherwise filters (module, type_, priority, tags) are applied.
        """
        import time as _time
        t0 = _time.time()

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
                run = self._execute(
                    tc, environment=environment, base_url=base_url,
                    headless=headless, timeout_s=timeout_s,
                )
                runs.append(run)
                if run.status == "pass":
                    passed += 1
                elif run.status == "fail":
                    failed += 1
                else:
                    errors += 1
            except Exception as e:
                logger.exception(
                    "test_catalog: suite run error on %s", tc.test_case_id
                )
                err_run = TestRun(
                    test_case_id=tc.test_case_id,
                    test_name=tc.name,
                    environment=environment,
                    status="error",
                    logs=[f"Suite runner error: {type(e).__name__}: {e}"],
                )
                runs.append(err_run)
                errors += 1

        total_ms = int((_time.time() - t0) * 1000)
        result = SuiteRunResult(
            suite_run_id=suite_id,
            finished_at=_now_utc(),
            environment=environment,
            total=len(runs),
            passed=passed,
            failed=failed,
            errors=errors,
            duration_ms=total_ms,
            runs=runs,
            filter_applied=filter_applied,
        )
        logger.info(
            "test_catalog: suite %s finished — %d total, %d pass, %d fail, %d error, %dms",
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
        """Return recent TestRun records, most recent first."""
        with _lock:
            ordered = [_RUN_STORE[rid] for rid in reversed(_RUN_ORDER) if rid in _RUN_STORE]

        if test_case_id:
            ordered = [r for r in ordered if r.test_case_id == test_case_id]

        return ordered[:limit]

    def get_run(self, run_id: str) -> Optional[TestRun]:
        with _lock:
            return _RUN_STORE.get(run_id)

    # ── Internal ──────────────────────────────────────────────────────────────

    def _execute(
        self,
        tc: TestCase,
        *,
        environment: str,
        base_url: Optional[str],
        headless: bool,
        timeout_s: Optional[int],
    ) -> TestRun:
        """Build steps, run via Playwright runner, record result."""
        import time as _time
        from runner import execute_test  # import here to avoid circular at module load

        t0 = _time.time()
        run_id = str(uuid.uuid4())
        evidence_id = f"TC-{uuid.uuid4().hex[:10].upper()}"

        logger.info(
            "test_catalog: executing %s (%s) — run_id=%s env=%s",
            tc.test_case_id, tc.name, run_id, environment,
        )

        steps = _build_runner_steps(tc, base_url=base_url)
        logger.debug(
            "test_catalog: %s → %d runner steps: %s",
            tc.test_case_id,
            len(steps),
            [s.get("action") for s in steps],
        )

        try:
            result = execute_test(
                steps=steps,
                base_url=base_url or tc.base_url,
                headless=headless,
                timeout_s=timeout_s,
            )
            if not isinstance(result, dict):
                result = {}

            runner_status = str(result.get("status") or "").lower()
            run_status: Literal["pass", "fail", "error", "running"]
            if runner_status in ("passed", "pass", "ok"):
                run_status = "pass"
            elif runner_status in ("failed", "fail"):
                run_status = "fail"
            else:
                run_status = "error"

            duration_ms = result.get("duration_ms") or int((_time.time() - t0) * 1000)

            run = TestRun(
                run_id=run_id,
                test_case_id=tc.test_case_id,
                test_name=tc.name,
                environment=environment,
                status=run_status,
                duration_ms=int(duration_ms),
                evidence_id=evidence_id,
                evidence_url=result.get("evidence_url"),
                report_url=result.get("report_url"),
                logs=result.get("logs") or [],
                steps_result=result.get("steps") or [],
                meta={
                    "runner_ok": result.get("ok"),
                    "runner_outcome": result.get("outcome"),
                    "runner_reason": result.get("reason"),
                    "tc_module": tc.module,
                    "tc_type": tc.type,
                    "tc_priority": tc.priority,
                    "tc_version": tc.version,
                },
            )

        except Exception as e:
            logger.exception(
                "test_catalog: runner error for %s", tc.test_case_id
            )
            run = TestRun(
                run_id=run_id,
                test_case_id=tc.test_case_id,
                test_name=tc.name,
                environment=environment,
                status="error",
                duration_ms=int((_time.time() - t0) * 1000),
                evidence_id=evidence_id,
                logs=[f"Runner error: {type(e).__name__}: {e}"],
            )

        self._save_run(run)
        return run

    def _save_run(self, run: TestRun) -> None:
        with _lock:
            _RUN_STORE[run.run_id] = run
            if run.run_id not in _RUN_ORDER:
                _RUN_ORDER.append(run.run_id)
            # GC: prune oldest entries beyond threshold
            while len(_RUN_ORDER) > _MAX_RUNS_KEPT:
                oldest = _RUN_ORDER.pop(0)
                _RUN_STORE.pop(oldest, None)


# Module-level singleton — all routes import this instance
catalog_service = TestCatalogService()


# ── Seed loader ───────────────────────────────────────────────────────────────

def load_seed_catalog() -> None:
    """Load demo test cases if the catalog is empty."""
    from services.test_catalog_seed import SEED_TEST_CASES

    loaded = 0
    for payload_dict in SEED_TEST_CASES:
        try:
            payload = TestCaseCreate(**payload_dict)
            if catalog_service.get_test_case(payload.test_case_id) is None:
                catalog_service.create_test_case(payload)
                loaded += 1
        except Exception as e:
            logger.warning(
                "test_catalog: seed load failed for %s — %s",
                payload_dict.get("test_case_id", "?"), e,
            )

    if loaded:
        logger.info("test_catalog: loaded %d seed test cases", loaded)
    else:
        logger.debug("test_catalog: seed already loaded, skipped")
