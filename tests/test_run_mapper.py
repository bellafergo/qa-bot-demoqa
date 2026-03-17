# tests/test_run_mapper.py
"""
Unit tests for services/run_mapper.py

Coverage
--------
- normalize_status: all raw status variants
- run_from_catalog_testrun: field mapping, artifacts, meta, error_summary
- suite_from_catalog_suite_result: aggregate + nested runs
- run_from_orchestrator_job: job-level mapping
- run_from_legacy_store: run_store dict format
- normalize_run: dispatch heuristic (canonical / TestRun-like / legacy)
- normalize_run_list: heterogeneous list handling

These tests use only in-process logic — no network, DB, or Playwright needed.
"""
from __future__ import annotations

import unittest
from datetime import datetime, timezone
from types import SimpleNamespace
from typing import Any, Dict


# ── Import helpers ────────────────────────────────────────────────────────────

from services.run_mapper import (
    normalize_status,
    run_from_catalog_testrun,
    run_from_legacy_store,
    run_from_orchestrator_job,
    normalize_run,
    normalize_run_list,
    suite_from_catalog_suite_result,
)
from models.run_contract import CanonicalRun, RunArtifacts, RunMeta


def _make_test_run(**overrides) -> Any:
    """Create a minimal TestRun-like object using a namespace (avoids full Pydantic parse)."""
    from models.test_run import TestRun
    defaults = dict(
        run_id="run-001",
        test_case_id="TC-LOGIN-001",
        test_name="Login test",
        executed_at=datetime(2024, 6, 1, 12, 0, 0, tzinfo=timezone.utc),
        environment="staging",
        status="pass",
        duration_ms=1200,
        evidence_url="https://example.com/ev",
        report_url="https://example.com/rp",
        evidence_id="ev-abc",
        logs=[],
        steps_result=[{"action": "goto", "status": "passed"}],
        meta={},
    )
    defaults.update(overrides)
    return TestRun(**defaults)


# ══════════════════════════════════════════════════════════════════════════════
# 1. normalize_status
# ══════════════════════════════════════════════════════════════════════════════

class TestNormalizeStatus(unittest.TestCase):

    def test_pass_variants(self):
        for v in ("pass", "passed", "completed", "PASS", "PASSED", "Completed"):
            self.assertEqual(normalize_status(v), "passed", f"failed for {v!r}")

    def test_fail_variants(self):
        for v in ("fail", "failed", "error", "partial", "FAIL", "ERROR"):
            self.assertEqual(normalize_status(v), "failed", f"failed for {v!r}")

    def test_running(self):
        self.assertEqual(normalize_status("running"), "running")

    def test_queued(self):
        for v in ("queued", "pending", "QUEUED"):
            self.assertEqual(normalize_status(v), "queued")

    def test_canceled(self):
        for v in ("canceled", "cancelled", "aborted"):
            self.assertEqual(normalize_status(v), "canceled")

    def test_unknown_defaults_to_failed(self):
        self.assertEqual(normalize_status(""),        "failed")
        self.assertEqual(normalize_status("unknown"),  "failed")
        self.assertEqual(normalize_status("whatever"), "failed")


# ══════════════════════════════════════════════════════════════════════════════
# 2. run_from_catalog_testrun
# ══════════════════════════════════════════════════════════════════════════════

class TestRunFromCatalogTestRun(unittest.TestCase):

    def test_basic_field_mapping(self):
        tr = _make_test_run()
        r = run_from_catalog_testrun(tr)
        self.assertIsInstance(r, CanonicalRun)
        self.assertEqual(r.run_id,    "run-001")
        self.assertEqual(r.test_id,   "TC-LOGIN-001")
        self.assertEqual(r.test_name, "Login test")
        self.assertEqual(r.source,    "catalog")
        self.assertEqual(r.status,    "passed")
        self.assertEqual(r.duration_ms, 1200)

    def test_status_pass_maps_to_passed(self):
        r = run_from_catalog_testrun(_make_test_run(status="pass"))
        self.assertEqual(r.status, "passed")

    def test_status_fail_maps_to_failed(self):
        r = run_from_catalog_testrun(_make_test_run(status="fail"))
        self.assertEqual(r.status, "failed")

    def test_status_error_maps_to_failed(self):
        r = run_from_catalog_testrun(_make_test_run(status="error"))
        self.assertEqual(r.status, "failed")

    def test_artifacts_screenshot_from_meta(self):
        tr = _make_test_run(meta={"screenshot_b64": "abc123"})
        r = run_from_catalog_testrun(tr)
        self.assertEqual(r.artifacts.screenshot_b64, "abc123")

    def test_artifacts_evidence_url(self):
        r = run_from_catalog_testrun(_make_test_run())
        self.assertEqual(r.artifacts.evidence_url, "https://example.com/ev")
        self.assertEqual(r.artifacts.report_url,   "https://example.com/rp")

    def test_artifacts_null_when_no_evidence(self):
        tr = _make_test_run(evidence_url=None, report_url=None, meta={})
        r = run_from_catalog_testrun(tr)
        self.assertIsNone(r.artifacts.screenshot_b64)
        self.assertIsNone(r.artifacts.evidence_url)
        self.assertIsNone(r.artifacts.report_url)

    def test_meta_environment(self):
        r = run_from_catalog_testrun(_make_test_run(environment="staging"))
        self.assertEqual(r.meta.environment, "staging")

    def test_meta_base_url_from_meta_dict(self):
        tr = _make_test_run(meta={"base_url": "https://myapp.com"})
        r = run_from_catalog_testrun(tr)
        self.assertEqual(r.meta.base_url, "https://myapp.com")

    def test_steps_mapped_from_steps_result(self):
        steps = [{"action": "goto", "status": "passed"}, {"action": "click", "status": "passed"}]
        tr = _make_test_run(steps_result=steps)
        r = run_from_catalog_testrun(tr)
        self.assertEqual(len(r.steps), 2)
        self.assertEqual(r.steps[0]["action"], "goto")

    def test_started_at_is_iso_string(self):
        r = run_from_catalog_testrun(_make_test_run())
        self.assertIsInstance(r.started_at, str)
        self.assertIn("2024-06-01", r.started_at)

    def test_error_summary_populated_on_fail(self):
        tr = _make_test_run(
            status="fail",
            meta={"runner_reason": "Element not found: #submit"},
        )
        r = run_from_catalog_testrun(tr)
        self.assertIsNotNone(r.error_summary)
        self.assertIn("Element not found", r.error_summary)

    def test_error_summary_none_on_pass(self):
        r = run_from_catalog_testrun(_make_test_run(status="pass"))
        self.assertIsNone(r.error_summary)

    def test_error_summary_from_logs_when_no_reason(self):
        tr = _make_test_run(
            status="fail",
            logs=["Step 1 ok", "Step 2 failed: timeout waiting for element"],
        )
        r = run_from_catalog_testrun(tr)
        self.assertIsNotNone(r.error_summary)
        self.assertIn("failed", r.error_summary.lower())

    def test_duration_ms_defaults_to_zero(self):
        r = run_from_catalog_testrun(_make_test_run(duration_ms=None))
        self.assertEqual(r.duration_ms, 0)

    def test_job_id_from_meta(self):
        tr = _make_test_run(meta={"job_id": "job-xyz"})
        r = run_from_catalog_testrun(tr)
        self.assertEqual(r.job_id, "job-xyz")

    def test_source_from_meta_override(self):
        tr = _make_test_run(meta={"source": "orchestrator"})
        r = run_from_catalog_testrun(tr)
        self.assertEqual(r.source, "orchestrator")

    def test_source_defaults_to_catalog(self):
        r = run_from_catalog_testrun(_make_test_run(meta={}))
        self.assertEqual(r.source, "catalog")


# ══════════════════════════════════════════════════════════════════════════════
# 3. suite_from_catalog_suite_result
# ══════════════════════════════════════════════════════════════════════════════

class TestSuiteFromCatalogSuiteResult(unittest.TestCase):

    def _make_suite(self, **overrides):
        from models.test_run import SuiteRunResult, TestRun
        runs = [
            TestRun(test_case_id="TC-001", status="pass", environment="default"),
            TestRun(test_case_id="TC-002", status="fail", environment="default"),
        ]
        sr = SuiteRunResult(
            suite_run_id="suite-abc",
            environment="default",
            total=2, passed=1, failed=1, errors=0,
            duration_ms=3000,
            runs=runs,
        )
        for k, v in overrides.items():
            object.__setattr__(sr, k, v)
        return sr

    def test_top_level_counters_preserved(self):
        sr = self._make_suite()
        r = suite_from_catalog_suite_result(sr)
        self.assertEqual(r.suite_run_id, "suite-abc")
        self.assertEqual(r.total,   2)
        self.assertEqual(r.passed,  1)
        self.assertEqual(r.failed,  1)
        self.assertEqual(r.errors,  0)
        self.assertEqual(r.duration_ms, 3000)

    def test_runs_are_canonical(self):
        sr = self._make_suite()
        r = suite_from_catalog_suite_result(sr)
        self.assertEqual(len(r.runs), 2)
        self.assertIsInstance(r.runs[0], CanonicalRun)

    def test_first_run_status_passed(self):
        sr = self._make_suite()
        r = suite_from_catalog_suite_result(sr)
        self.assertEqual(r.runs[0].status, "passed")

    def test_second_run_status_failed(self):
        sr = self._make_suite()
        r = suite_from_catalog_suite_result(sr)
        self.assertEqual(r.runs[1].status, "failed")

    def test_environment_propagated(self):
        sr = self._make_suite()
        r = suite_from_catalog_suite_result(sr)
        self.assertEqual(r.environment, "default")


# ══════════════════════════════════════════════════════════════════════════════
# 4. run_from_orchestrator_job
# ══════════════════════════════════════════════════════════════════════════════

class TestRunFromOrchestratorJob(unittest.TestCase):

    def _make_job(self, **overrides):
        defaults = dict(
            job_id="job-abc",
            status="completed",
            total_count=5,
            passed_count=4,
            failed_count=1,
            error_count=0,
            error_message=None,
            environment="staging",
            started_at=datetime(2024, 6, 1, 10, 0, 0, tzinfo=timezone.utc),
            finished_at=datetime(2024, 6, 1, 10, 5, 0, tzinfo=timezone.utc),
            duration_ms=300_000,
        )
        defaults.update(overrides)
        return SimpleNamespace(**defaults)

    def test_run_id_equals_job_id(self):
        r = run_from_orchestrator_job(self._make_job())
        self.assertEqual(r.run_id, "job-abc")
        self.assertEqual(r.job_id, "job-abc")

    def test_source_is_orchestrator(self):
        r = run_from_orchestrator_job(self._make_job())
        self.assertEqual(r.source, "orchestrator")

    def test_completed_maps_to_passed(self):
        r = run_from_orchestrator_job(self._make_job(status="completed"))
        self.assertEqual(r.status, "passed")

    def test_failed_maps_to_failed(self):
        r = run_from_orchestrator_job(self._make_job(status="failed"))
        self.assertEqual(r.status, "failed")

    def test_queued_maps_to_queued(self):
        r = run_from_orchestrator_job(self._make_job(status="queued"))
        self.assertEqual(r.status, "queued")

    def test_summary_is_populated(self):
        r = run_from_orchestrator_job(self._make_job())
        self.assertIsNotNone(r.summary)
        self.assertIn("5 test(s)", r.summary)
        self.assertIn("4 passed",  r.summary)

    def test_error_summary_from_error_message(self):
        r = run_from_orchestrator_job(self._make_job(error_message="DB timeout"))
        self.assertEqual(r.error_summary, "DB timeout")

    def test_environment_in_meta(self):
        r = run_from_orchestrator_job(self._make_job())
        self.assertEqual(r.meta.environment, "staging")

    def test_started_at_is_iso(self):
        r = run_from_orchestrator_job(self._make_job())
        self.assertIn("2024-06-01", r.started_at)

    def test_duration_ms(self):
        r = run_from_orchestrator_job(self._make_job())
        self.assertEqual(r.duration_ms, 300_000)

    def test_no_steps(self):
        r = run_from_orchestrator_job(self._make_job())
        self.assertEqual(r.steps, [])


# ══════════════════════════════════════════════════════════════════════════════
# 5. run_from_legacy_store
# ══════════════════════════════════════════════════════════════════════════════

class TestRunFromLegacyStore(unittest.TestCase):

    def _base_payload(self, **overrides) -> Dict:
        p = dict(
            evidence_id="ev-123",
            status="completed",
            created_at=1_717_243_200_000,   # ms epoch
            kind="steps",
            duration_ms=800,
            meta={},
        )
        p.update(overrides)
        return p

    def test_run_id_from_evidence_id(self):
        r = run_from_legacy_store(self._base_payload())
        self.assertEqual(r.run_id, "ev-123")

    def test_status_completed_maps_to_passed(self):
        r = run_from_legacy_store(self._base_payload(status="completed"))
        self.assertEqual(r.status, "passed")

    def test_started_at_from_ms_epoch(self):
        r = run_from_legacy_store(self._base_payload())
        self.assertIsNotNone(r.started_at)
        self.assertIn("2024", r.started_at)

    def test_duration_ms(self):
        r = run_from_legacy_store(self._base_payload())
        self.assertEqual(r.duration_ms, 800)

    def test_artifacts_evidence_url_from_payload(self):
        p = self._base_payload(evidence_url="https://cdn.example.com/ev.png")
        r = run_from_legacy_store(p)
        self.assertEqual(r.artifacts.evidence_url, "https://cdn.example.com/ev.png")

    def test_artifacts_evidence_url_from_meta(self):
        p = self._base_payload(meta={"evidence_url": "https://cdn.example.com/meta.png"})
        r = run_from_legacy_store(p)
        self.assertEqual(r.artifacts.evidence_url, "https://cdn.example.com/meta.png")

    def test_artifacts_screenshot_b64(self):
        p = self._base_payload(screenshot_b64="BASE64DATA")
        r = run_from_legacy_store(p)
        self.assertEqual(r.artifacts.screenshot_b64, "BASE64DATA")

    def test_meta_base_url_from_meta_dict(self):
        p = self._base_payload(meta={"base_url": "https://myapp.io"})
        r = run_from_legacy_store(p)
        self.assertEqual(r.meta.base_url, "https://myapp.io")

    def test_error_summary_from_error_message(self):
        p = self._base_payload(error_message="Navigation failed: net::ERR_NAME_NOT_RESOLVED")
        r = run_from_legacy_store(p)
        self.assertIn("Navigation failed", r.error_summary)

    def test_source_defaults_to_chat(self):
        r = run_from_legacy_store(self._base_payload())
        self.assertEqual(r.source, "chat")

    def test_run_id_from_run_id_field(self):
        p = self._base_payload(run_id="explicit-run-id")
        r = run_from_legacy_store(p)
        self.assertEqual(r.run_id, "explicit-run-id")


# ══════════════════════════════════════════════════════════════════════════════
# 6. normalize_run (dispatch)
# ══════════════════════════════════════════════════════════════════════════════

class TestNormalizeRun(unittest.TestCase):

    def test_already_canonical_with_artifacts_dict(self):
        raw = {
            "run_id": "already-canonical",
            "status": "passed",
            "source": "catalog",
            "duration_ms": 500,
            "artifacts": {"evidence_url": "https://cdn.example.com", "screenshot_b64": None},
            "meta": {},
        }
        r = normalize_run(raw)
        self.assertIsInstance(r, CanonicalRun)
        self.assertEqual(r.run_id, "already-canonical")
        self.assertEqual(r.status, "passed")

    def test_testrun_like_dict_dispatched_to_catalog_mapper(self):
        raw = {
            "run_id": "tr-001",
            "test_case_id": "TC-SMOKE-001",
            "test_name": "Smoke test",
            "status": "pass",
            "environment": "default",
            "executed_at": "2024-06-01T12:00:00+00:00",
            "duration_ms": 1000,
            "logs": [],
            "steps_result": [],
            "meta": {},
        }
        r = normalize_run(raw)
        self.assertEqual(r.test_id, "TC-SMOKE-001")
        self.assertEqual(r.status,  "passed")

    def test_legacy_store_dict_dispatched_to_legacy_mapper(self):
        raw = {
            "evidence_id": "ev-legacy",
            "status": "completed",
            "kind": "steps",
            "duration_ms": 200,
            "meta": {},
        }
        r = normalize_run(raw)
        self.assertEqual(r.run_id, "ev-legacy")
        self.assertEqual(r.status, "passed")

    def test_non_dict_input_returns_unknown(self):
        r = normalize_run("not-a-dict")  # type: ignore[arg-type]
        self.assertIsInstance(r, CanonicalRun)
        self.assertEqual(r.run_id, "unknown")


# ══════════════════════════════════════════════════════════════════════════════
# 7. normalize_run_list
# ══════════════════════════════════════════════════════════════════════════════

class TestNormalizeRunList(unittest.TestCase):

    def test_mixed_list_all_mapped(self):
        tr = _make_test_run(run_id="r1")
        legacy = {"evidence_id": "ev-2", "status": "failed", "kind": "steps", "meta": {}}
        result = normalize_run_list([tr, legacy])
        self.assertEqual(len(result), 2)
        self.assertIsInstance(result[0], CanonicalRun)
        self.assertIsInstance(result[1], CanonicalRun)

    def test_empty_list(self):
        self.assertEqual(normalize_run_list([]), [])

    def test_pydantic_model_handled(self):
        tr = _make_test_run(run_id="r-pydantic")
        result = normalize_run_list([tr])
        self.assertEqual(result[0].run_id, "r-pydantic")


# ══════════════════════════════════════════════════════════════════════════════
# 8. Canonical contract invariants
# ══════════════════════════════════════════════════════════════════════════════

class TestContractInvariants(unittest.TestCase):
    """Every normalisation path must produce a valid CanonicalRun with correct types."""

    def _check(self, r: CanonicalRun):
        self.assertIsInstance(r, CanonicalRun)
        self.assertIsInstance(r.run_id, str)
        self.assertIsInstance(r.duration_ms, int)
        self.assertIsInstance(r.steps, list)
        self.assertIsInstance(r.artifacts, RunArtifacts)
        self.assertIsInstance(r.meta, RunMeta)
        self.assertIn(r.status, ("queued", "running", "passed", "failed", "canceled"))
        self.assertIn(r.source, ("chat", "catalog", "orchestrator", "api"))

    def test_catalog_run(self):
        self._check(run_from_catalog_testrun(_make_test_run()))

    def test_catalog_run_fail(self):
        self._check(run_from_catalog_testrun(_make_test_run(status="fail")))

    def test_catalog_run_error(self):
        self._check(run_from_catalog_testrun(_make_test_run(status="error")))

    def test_legacy_store_run(self):
        self._check(run_from_legacy_store({"evidence_id": "ev-x", "status": "completed", "meta": {}}))

    def test_orchestrator_job_run(self):
        self._check(run_from_orchestrator_job(SimpleNamespace(
            job_id="j1", status="running",
            total_count=3, passed_count=1, failed_count=0, error_count=0,
            error_message=None, environment="default",
            started_at=None, finished_at=None, duration_ms=0,
        )))

    def test_normalize_run_generic(self):
        self._check(normalize_run({"evidence_id": "ev-y", "status": "queued", "meta": {}}))


if __name__ == "__main__":
    unittest.main()
