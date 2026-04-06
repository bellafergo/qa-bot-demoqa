# tests/test_run_history_service.py
"""
Unit tests for services/run_history_service.py

These tests verify that RunHistoryService:
  1. Reads from test_run_repo (SQLite) as the official source.
  2. Returns CanonicalRun objects — the canonical mapper is applied at the service level.
  3. Handles "not found" cleanly.
  4. Passes filters and limit through to the repository.
  5. Applies status normalisation (pass→passed, fail→failed; error stays error).
  6. Populates artifacts and meta correctly.

All tests patch test_run_repo so no real DB or Playwright execution is needed.
"""
from __future__ import annotations

import unittest
from datetime import datetime, timezone
from unittest.mock import MagicMock, patch

from models.run_contract import CanonicalRun, RunArtifacts, RunMeta
from models.test_run import TestRun


# ── Test-run factory ──────────────────────────────────────────────────────────

def _tr(
    run_id: str = "run-001",
    test_case_id: str = "TC-SMOKE-001",
    status: str = "pass",
    duration_ms: int = 1200,
    environment: str = "staging",
    **extra,
) -> TestRun:
    return TestRun(
        run_id=run_id,
        test_case_id=test_case_id,
        test_name="Smoke test",
        executed_at=datetime(2024, 6, 1, 12, 0, 0, tzinfo=timezone.utc),
        environment=environment,
        status=status,
        duration_ms=duration_ms,
        evidence_url=extra.get("evidence_url"),
        report_url=extra.get("report_url"),
        evidence_id=extra.get("evidence_id"),
        logs=extra.get("logs", []),
        steps_result=extra.get("steps_result", []),
        meta=extra.get("meta", {}),
    )


# ── Helpers ───────────────────────────────────────────────────────────────────

def _make_service():
    """Return a fresh RunHistoryService with a mocked test_run_repo."""
    from services.run_history_service import RunHistoryService
    svc = RunHistoryService()
    svc._repo = MagicMock()
    return svc


# ══════════════════════════════════════════════════════════════════════════════
# 1. list_runs
# ══════════════════════════════════════════════════════════════════════════════

class TestListRuns(unittest.TestCase):

    def _run_list(self, repo_returns, **kwargs):
        with patch(
            "services.run_history_service.test_run_repo.list_runs",
            return_value=repo_returns,
        ) as mock_list:
            from services.run_history_service import RunHistoryService
            svc = RunHistoryService()
            result = svc.list_runs(**kwargs)
            return result, mock_list

    def test_returns_list_of_canonical_run(self):
        result, _ = self._run_list([_tr()])
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 1)
        self.assertIsInstance(result[0], CanonicalRun)

    def test_empty_repo_returns_empty_list(self):
        result, _ = self._run_list([])
        self.assertEqual(result, [])

    def test_multiple_runs_all_mapped(self):
        result, _ = self._run_list([_tr("r1"), _tr("r2"), _tr("r3")])
        self.assertEqual(len(result), 3)
        run_ids = {r.run_id for r in result}
        self.assertEqual(run_ids, {"r1", "r2", "r3"})

    def test_passes_limit_to_repo(self):
        result, mock_list = self._run_list([], limit=42)
        mock_list.assert_called_once_with(test_case_id=None, project_id=None, limit=42)

    def test_passes_test_case_id_filter(self):
        result, mock_list = self._run_list([], test_case_id="TC-LOGIN-001")
        mock_list.assert_called_once_with(
            test_case_id="TC-LOGIN-001", project_id=None, limit=100
        )

    def test_limit_clamped_to_500(self):
        result, mock_list = self._run_list([], limit=9999)
        called_limit = mock_list.call_args[1]["limit"]
        self.assertLessEqual(called_limit, 500)

    def test_limit_minimum_is_1(self):
        result, mock_list = self._run_list([], limit=0)
        called_limit = mock_list.call_args[1]["limit"]
        self.assertGreaterEqual(called_limit, 1)

    def test_status_pass_normalised_to_passed(self):
        result, _ = self._run_list([_tr(status="pass")])
        self.assertEqual(result[0].status, "passed")

    def test_status_fail_normalised_to_failed(self):
        result, _ = self._run_list([_tr(status="fail")])
        self.assertEqual(result[0].status, "failed")

    def test_status_error_preserved_as_error(self):
        result, _ = self._run_list([_tr(status="error")])
        self.assertEqual(result[0].status, "error")

    def test_test_id_mapped_from_test_case_id(self):
        result, _ = self._run_list([_tr(test_case_id="TC-CART-003")])
        self.assertEqual(result[0].test_id, "TC-CART-003")

    def test_duration_ms_preserved(self):
        result, _ = self._run_list([_tr(duration_ms=4200)])
        self.assertEqual(result[0].duration_ms, 4200)

    def test_started_at_is_iso_string(self):
        result, _ = self._run_list([_tr()])
        self.assertIsInstance(result[0].started_at, str)
        self.assertIn("2024-06-01", result[0].started_at)


# ══════════════════════════════════════════════════════════════════════════════
# 2. get_run
# ══════════════════════════════════════════════════════════════════════════════

class TestGetRun(unittest.TestCase):

    def _call_get(self, repo_returns, run_id="run-001"):
        with patch(
            "services.run_history_service.test_run_repo.get_run",
            return_value=repo_returns,
        ) as mock_get:
            from services.run_history_service import RunHistoryService
            svc = RunHistoryService()
            result = svc.get_run(run_id)
            return result, mock_get

    def test_returns_canonical_run_when_found(self):
        result, _ = self._call_get(_tr())
        self.assertIsInstance(result, CanonicalRun)
        self.assertEqual(result.run_id, "run-001")

    def test_returns_none_when_not_found(self):
        result, _ = self._call_get(None)
        self.assertIsNone(result)

    def test_passes_run_id_to_repo(self):
        result, mock_get = self._call_get(None, run_id="my-run-id")
        mock_get.assert_called_once_with("my-run-id")

    def test_status_mapped_to_canonical(self):
        result, _ = self._call_get(_tr(status="fail"))
        self.assertEqual(result.status, "failed")

    def test_artifacts_populated_from_evidence_url(self):
        tr = _tr(evidence_url="https://cdn.example.com/ev.png")
        result, _ = self._call_get(tr)
        self.assertEqual(result.artifacts.evidence_url, "https://cdn.example.com/ev.png")

    def test_artifacts_report_url(self):
        tr = _tr(report_url="https://cdn.example.com/report.pdf")
        result, _ = self._call_get(tr)
        self.assertEqual(result.artifacts.report_url, "https://cdn.example.com/report.pdf")

    def test_artifacts_screenshot_b64_from_meta(self):
        tr = _tr(meta={"screenshot_b64": "BASE64STRING"})
        result, _ = self._call_get(tr)
        self.assertEqual(result.artifacts.screenshot_b64, "BASE64STRING")

    def test_meta_environment(self):
        result, _ = self._call_get(_tr(environment="prod"))
        self.assertEqual(result.meta.environment, "prod")

    def test_meta_base_url_from_meta_dict(self):
        tr = _tr(meta={"base_url": "https://myapp.com"})
        result, _ = self._call_get(tr)
        self.assertEqual(result.meta.base_url, "https://myapp.com")

    def test_steps_mapped_from_steps_result(self):
        steps = [{"action": "goto", "status": "passed"}, {"action": "click", "status": "passed"}]
        tr = _tr(steps_result=steps)
        result, _ = self._call_get(tr)
        self.assertEqual(len(result.steps), 2)

    def test_error_summary_from_runner_reason(self):
        tr = _tr(status="fail", meta={"runner_reason": "Element #login not found"})
        result, _ = self._call_get(tr)
        self.assertIsNotNone(result.error_summary)
        self.assertIn("Element #login", result.error_summary)

    def test_no_error_summary_on_pass(self):
        result, _ = self._call_get(_tr(status="pass"))
        self.assertIsNone(result.error_summary)


# ══════════════════════════════════════════════════════════════════════════════
# 3. count_by_status and get_last_run_at
# ══════════════════════════════════════════════════════════════════════════════

class TestAggregates(unittest.TestCase):

    def test_count_by_status_delegates_to_repo(self):
        expected = {"pass": 10, "fail": 3, "error": 1}
        with patch(
            "services.run_history_service.test_run_repo.count_by_status",
            return_value=expected,
        ):
            from services.run_history_service import RunHistoryService
            svc = RunHistoryService()
            result = svc.count_by_status()
        self.assertEqual(result, expected)

    def test_get_last_run_at_delegates_to_repo(self):
        expected = "2024-06-01T12:00:00+00:00"
        with patch(
            "services.run_history_service.test_run_repo.get_last_executed_at",
            return_value=expected,
        ):
            from services.run_history_service import RunHistoryService
            svc = RunHistoryService()
            result = svc.get_last_run_at()
        self.assertEqual(result, expected)

    def test_get_last_run_at_none_when_no_runs(self):
        with patch(
            "services.run_history_service.test_run_repo.get_last_executed_at",
            return_value=None,
        ):
            from services.run_history_service import RunHistoryService
            svc = RunHistoryService()
            result = svc.get_last_run_at()
        self.assertIsNone(result)


# ══════════════════════════════════════════════════════════════════════════════
# 4. Architecture invariants
# ══════════════════════════════════════════════════════════════════════════════

class TestArchitectureInvariants(unittest.TestCase):
    """
    Confirm that RunHistoryService ONLY reads from test_run_repo (SQLite).
    It must never access run_store directly.
    """

    def test_service_does_not_import_run_store_at_module_level(self):
        """Lazy imports inside methods (e.g. get_run_unified) are allowed."""
        import ast
        from pathlib import Path

        src = (Path(__file__).resolve().parent.parent / "services/run_history_service.py").read_text()
        tree = ast.parse(src)
        for node in tree.body:
            if not isinstance(node, (ast.Import, ast.ImportFrom)):
                continue
            names = []
            if isinstance(node, ast.ImportFrom) and node.module:
                names.append(node.module)
            if isinstance(node, ast.Import):
                names += [alias.name for alias in node.names]
            for name in names:
                self.assertNotIn(
                    "run_store",
                    name,
                    f"run_history_service should not top-level import run_store — found: {name}",
                )

    def test_list_runs_returns_only_canonical_run_instances(self):
        with patch(
            "services.run_history_service.test_run_repo.list_runs",
            return_value=[_tr("r1"), _tr("r2")],
        ):
            from services.run_history_service import RunHistoryService
            result = RunHistoryService().list_runs()
        for item in result:
            self.assertIsInstance(item, CanonicalRun, f"{item!r} is not CanonicalRun")

    def test_get_run_returns_canonical_or_none(self):
        for repo_val in (_tr(), None):
            with patch(
                "services.run_history_service.test_run_repo.get_run",
                return_value=repo_val,
            ):
                from services.run_history_service import RunHistoryService
                result = RunHistoryService().get_run("any-id")
            if repo_val is None:
                self.assertIsNone(result)
            else:
                self.assertIsInstance(result, CanonicalRun)

    def test_canonical_run_status_in_valid_set(self):
        valid = {"queued", "planning", "compiled", "running", "passed", "failed", "error", "canceled"}
        for raw_status in ("pass", "fail", "error", "running"):
            with patch(
                "services.run_history_service.test_run_repo.list_runs",
                return_value=[_tr(status=raw_status)],
            ):
                from services.run_history_service import RunHistoryService
                result = RunHistoryService().list_runs()
            self.assertIn(result[0].status, valid, f"Bad status for raw={raw_status!r}")

    def test_canonical_run_has_artifacts_and_meta(self):
        with patch(
            "services.run_history_service.test_run_repo.list_runs",
            return_value=[_tr()],
        ):
            from services.run_history_service import RunHistoryService
            result = RunHistoryService().list_runs()
        self.assertIsInstance(result[0].artifacts, RunArtifacts)
        self.assertIsInstance(result[0].meta, RunMeta)


if __name__ == "__main__":
    unittest.main()
