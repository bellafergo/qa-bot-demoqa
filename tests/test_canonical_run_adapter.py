# tests/test_canonical_run_adapter.py
"""
``normalize_storage_status`` + ``test_run_from_canonical`` (CanonicalRun → TestRun).

RCA / Failure Intelligence still read SQLite directly; these tests lock the adapter contract
before those services switch to ``run_history_service``.
"""
from __future__ import annotations

import unittest
from datetime import datetime, timezone

from models.run_contract import CanonicalRun, RunArtifacts, RunMeta
from models.test_run import TestRun
import services.run_mapper as run_mapper_mod
from services.run_mapper import (
    canonical_run_to_test_run,
    normalize_storage_status,
    run_from_catalog_testrun,
)


def _make_test_run(**overrides) -> TestRun:
    defaults = dict(
        run_id="run-001",
        test_case_id="TC-001",
        test_name="Example",
        executed_at=datetime(2026, 3, 1, 10, 0, 0, tzinfo=timezone.utc),
        environment="staging",
        status="pass",
        duration_ms=100,
        evidence_url=None,
        report_url=None,
        evidence_id=None,
        logs=[],
        steps_result=[],
        meta={},
    )
    defaults.update(overrides)
    return TestRun(**defaults)


class TestNormalizeStorageStatus(unittest.TestCase):

    def test_canonical_failed_to_fail(self):
        self.assertEqual(normalize_storage_status("failed"), "fail")

    def test_canonical_passed_to_pass(self):
        self.assertEqual(normalize_storage_status("passed"), "pass")

    def test_success_alias(self):
        self.assertEqual(normalize_storage_status("success"), "pass")

    def test_sqlite_pass_through(self):
        self.assertEqual(normalize_storage_status("pass"), "pass")
        self.assertEqual(normalize_storage_status("fail"), "fail")

    def test_lifecycle_statuses(self):
        self.assertEqual(normalize_storage_status("running"), "running")
        self.assertEqual(normalize_storage_status("queued"), "queued")
        self.assertEqual(normalize_storage_status("canceled"), "canceled")

    def test_empty_defaults_to_fail(self):
        self.assertEqual(normalize_storage_status(""), "fail")
        self.assertEqual(normalize_storage_status(None), "fail")


class TestTestRunFromCanonical(unittest.TestCase):
    """Uses ``canonical_run_to_test_run`` to avoid pytest collecting an imported ``test_*`` callable."""

    def test_alias_matches_implementation(self):
        self.assertIs(
            run_mapper_mod.test_run_from_canonical,
            run_mapper_mod.canonical_run_to_test_run,
        )

    def test_roundtrip_preserves_logs_and_steps(self):
        tr = _make_test_run(
            status="fail",
            logs=["selector not found"],
            steps_result=[{"action": "click", "status": "failed", "error": "not visible"}],
        )
        cr = run_from_catalog_testrun(tr)
        back = canonical_run_to_test_run(cr)
        self.assertEqual(back.status, "fail")
        self.assertTrue(any("selector" in x for x in back.logs))
        self.assertEqual(len(back.steps_result), 1)

    def test_missing_test_id_becomes_unknown(self):
        cr = CanonicalRun(
            run_id="r1",
            test_id=None,
            test_name="orphan",
            status="failed",
            started_at="2026-01-01T00:00:00+00:00",
            steps=[],
            logs=[],
            artifacts=RunArtifacts(),
            meta=RunMeta(),
        )
        tr = canonical_run_to_test_run(cr)
        self.assertEqual(tr.test_case_id, "unknown")

    def test_error_summary_becomes_log_line_when_logs_empty(self):
        cr = CanonicalRun(
            run_id="r2",
            test_id="TC-Z",
            status="failed",
            started_at="2026-01-02T00:00:00+00:00",
            error_summary="Service unavailable",
            steps=[],
            logs=[],
            artifacts=RunArtifacts(),
            meta=RunMeta(),
        )
        tr = canonical_run_to_test_run(cr)
        self.assertTrue(any("Service unavailable" in x for x in tr.logs))

    def test_canonical_passed_maps_to_test_run_pass(self):
        tr0 = _make_test_run(status="pass")
        cr = run_from_catalog_testrun(tr0)
        tr = canonical_run_to_test_run(cr)
        self.assertEqual(tr.status, "pass")

    def test_rca_consumes_adapter_output(self):
        """Adapter output must be a valid ``TestRun`` for ``RCAService.analyze``."""
        from services.rca_service import RCAService

        tr = _make_test_run(
            status="fail",
            logs=["locator not found for selector .btn"],
            steps_result=[],
        )
        cr = run_from_catalog_testrun(tr)
        run = canonical_run_to_test_run(cr)
        res = RCAService().analyze(run)
        self.assertEqual(res.root_cause_category, "selector_issue")


if __name__ == "__main__":
    unittest.main()
