# tests/test_run_bridge.py
"""
Unit tests for services/run_bridge.py

Coverage
--------
1. bridge_run_to_sqlite — field mapping from run_store payload to TestRun
2. Status filtering — queued/running payloads are skipped
3. Status normalisation — arbitrary strings mapped to TestRun status set
4. Evidence-id handling — run_id / evidence_id resolution
5. Meta enrichment — source=chat, bridge_persisted=True, screenshot_b64
6. Failure resilience — exceptions are caught and False returned
7. Missing fields — None values do not crash the bridge
8. Integration with run_store.save_run() — bridge is called for final states,
   skipped for intermediate states, run_store still returns evidence_id
"""
from __future__ import annotations

import unittest
from datetime import datetime, timezone
from unittest.mock import MagicMock, call, patch


# ── Helper ────────────────────────────────────────────────────────────────────

def _payload(
    evidence_id: str = "ev-abc",
    status: str = "completed",
    **extra,
) -> dict:
    base = dict(
        evidence_id  = evidence_id,
        status       = status,
        started_at   = 1_717_243_200_000,   # ms epoch
        duration_ms  = 1500,
        steps        = [{"action": "goto", "status": "passed"}],
        logs         = ["step 1 ok"],
        meta         = {},
    )
    base.update(extra)
    return base


# ══════════════════════════════════════════════════════════════════════════════
# 1. bridge_run_to_sqlite — field mapping
# ══════════════════════════════════════════════════════════════════════════════

class TestBridgeFieldMapping(unittest.TestCase):

    def _call(self, payload, captured_run=None):
        """Call bridge_run_to_sqlite with test_run_repo mocked out."""
        runs = []

        def fake_create(tr):
            runs.append(tr)

        with patch("services.run_bridge.test_run_repo") as mock_repo:
            mock_repo.create_run.side_effect = fake_create
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(payload)

        return ok, runs

    # ── Returns True on success ───────────────────────────────────────────────

    def test_returns_true_on_success(self):
        ok, runs = self._call(_payload())
        self.assertTrue(ok)
        self.assertEqual(len(runs), 1)

    # ── run_id == evidence_id ─────────────────────────────────────────────────

    def test_run_id_equals_evidence_id(self):
        ok, runs = self._call(_payload(evidence_id="ev-xyz"))
        self.assertEqual(runs[0].run_id, "ev-xyz")

    def test_evidence_id_field_set(self):
        ok, runs = self._call(_payload(evidence_id="ev-xyz"))
        self.assertEqual(runs[0].evidence_id, "ev-xyz")

    # ── test_case_id sentinel ─────────────────────────────────────────────────

    def test_test_case_id_defaults_to_chat(self):
        ok, runs = self._call(_payload())
        self.assertEqual(runs[0].test_case_id, "_chat")

    def test_test_case_id_from_meta(self):
        p = _payload(meta={"test_case_id": "TC-CUSTOM-001"})
        ok, runs = self._call(p)
        self.assertEqual(runs[0].test_case_id, "TC-CUSTOM-001")

    def test_test_case_id_from_payload(self):
        p = _payload()
        p["test_case_id"] = "TC-PAYLOAD-002"
        ok, runs = self._call(p)
        self.assertEqual(runs[0].test_case_id, "TC-PAYLOAD-002")

    # ── test_name resolution ──────────────────────────────────────────────────

    def test_test_name_from_meta_test_name(self):
        p = _payload(meta={"test_name": "Login flow"})
        ok, runs = self._call(p)
        self.assertEqual(runs[0].test_name, "Login flow")

    def test_test_name_from_meta_domain(self):
        p = _payload(meta={"domain": "checkout"})
        ok, runs = self._call(p)
        self.assertEqual(runs[0].test_name, "checkout")

    def test_test_name_from_prompt(self):
        p = _payload(prompt="Go to login page and click submit")
        ok, runs = self._call(p)
        self.assertIn("login", runs[0].test_name.lower())

    def test_test_name_default_fallback(self):
        ok, runs = self._call(_payload())
        self.assertEqual(runs[0].test_name, "Chat / Execute Run")

    # ── status mapping ────────────────────────────────────────────────────────

    def test_status_completed_maps_to_pass(self):
        ok, runs = self._call(_payload(status="completed"))
        self.assertEqual(runs[0].status, "pass")

    def test_status_passed_maps_to_pass(self):
        ok, runs = self._call(_payload(status="passed"))
        self.assertEqual(runs[0].status, "pass")

    def test_status_failed_maps_to_fail(self):
        ok, runs = self._call(_payload(status="failed"))
        self.assertEqual(runs[0].status, "fail")

    def test_status_fail_maps_to_fail(self):
        ok, runs = self._call(_payload(status="fail"))
        self.assertEqual(runs[0].status, "fail")

    def test_status_error_maps_to_error(self):
        ok, runs = self._call(_payload(status="error"))
        self.assertEqual(runs[0].status, "error")

    def test_unknown_status_maps_to_error(self):
        ok, runs = self._call(_payload(status="weird_value"))
        self.assertEqual(runs[0].status, "error")

    # ── duration_ms ───────────────────────────────────────────────────────────

    def test_duration_ms_preserved(self):
        ok, runs = self._call(_payload(duration_ms=4200))
        self.assertEqual(runs[0].duration_ms, 4200)

    def test_duration_ms_defaults_to_zero_when_missing(self):
        p = _payload()
        del p["duration_ms"]
        ok, runs = self._call(p)
        self.assertEqual(runs[0].duration_ms, 0)

    # ── steps_result ──────────────────────────────────────────────────────────

    def test_steps_mapped_from_steps(self):
        p = _payload(steps=[{"action": "click", "status": "passed"}])
        ok, runs = self._call(p)
        self.assertEqual(len(runs[0].steps_result), 1)
        self.assertEqual(runs[0].steps_result[0]["action"], "click")

    def test_steps_mapped_from_steps_result(self):
        p = _payload()
        p.pop("steps", None)
        p["steps_result"] = [{"action": "goto", "status": "passed"}]
        ok, runs = self._call(p)
        self.assertEqual(len(runs[0].steps_result), 1)

    # ── logs ──────────────────────────────────────────────────────────────────

    def test_logs_preserved(self):
        p = _payload(logs=["line 1", "line 2"])
        ok, runs = self._call(p)
        self.assertEqual(runs[0].logs, ["line 1", "line 2"])

    def test_logs_capped_at_500(self):
        p = _payload(logs=[f"log {i}" for i in range(600)])
        ok, runs = self._call(p)
        self.assertLessEqual(len(runs[0].logs), 500)

    # ── evidence / report URLs ────────────────────────────────────────────────

    def test_evidence_url_from_payload(self):
        p = _payload(evidence_url="https://cdn.example.com/ev.png")
        ok, runs = self._call(p)
        self.assertEqual(runs[0].evidence_url, "https://cdn.example.com/ev.png")

    def test_evidence_url_from_meta(self):
        p = _payload(meta={"evidence_url": "https://meta.example.com/ev.png"})
        ok, runs = self._call(p)
        self.assertEqual(runs[0].evidence_url, "https://meta.example.com/ev.png")

    def test_report_url_from_payload(self):
        p = _payload(report_url="https://cdn.example.com/report.pdf")
        ok, runs = self._call(p)
        self.assertEqual(runs[0].report_url, "https://cdn.example.com/report.pdf")

    # ── meta enrichment ───────────────────────────────────────────────────────

    def test_meta_source_set_to_chat(self):
        ok, runs = self._call(_payload())
        self.assertEqual(runs[0].meta.get("source"), "chat")

    def test_meta_bridge_persisted_flag(self):
        ok, runs = self._call(_payload())
        self.assertTrue(runs[0].meta.get("bridge_persisted"))

    def test_meta_evidence_id_set(self):
        ok, runs = self._call(_payload(evidence_id="ev-meta-test"))
        self.assertEqual(runs[0].meta.get("evidence_id"), "ev-meta-test")

    def test_screenshot_b64_moved_to_meta(self):
        p = _payload(screenshot_b64="BASE64CONTENT")
        ok, runs = self._call(p)
        self.assertEqual(runs[0].meta.get("screenshot_b64"), "BASE64CONTENT")

    def test_screenshot_b64_already_in_meta_preserved(self):
        p = _payload(meta={"screenshot_b64": "ALREADY_IN_META"})
        ok, runs = self._call(p)
        self.assertEqual(runs[0].meta.get("screenshot_b64"), "ALREADY_IN_META")

    def test_base_url_moved_to_meta(self):
        p = _payload(base_url="https://myapp.io")
        ok, runs = self._call(p)
        self.assertEqual(runs[0].meta.get("base_url"), "https://myapp.io")

    def test_environment_from_meta(self):
        p = _payload(meta={"environment": "staging"})
        ok, runs = self._call(p)
        self.assertEqual(runs[0].environment, "staging")

    def test_environment_default(self):
        ok, runs = self._call(_payload())
        self.assertEqual(runs[0].environment, "default")

    # ── executed_at (timestamp conversion) ───────────────────────────────────

    def test_executed_at_from_ms_epoch(self):
        p = _payload(started_at=1_717_243_200_000)  # 2024-06-01 12:00:00 UTC
        ok, runs = self._call(p)
        self.assertIn("2024-06-01", runs[0].executed_at.isoformat())

    def test_executed_at_from_iso_string(self):
        p = _payload()
        p["started_at"] = "2024-06-01T12:00:00+00:00"
        ok, runs = self._call(p)
        self.assertIsNotNone(runs[0].executed_at)

    def test_executed_at_from_created_at_fallback(self):
        p = _payload()
        p.pop("started_at", None)
        p["created_at"] = 1_717_243_200_000
        ok, runs = self._call(p)
        self.assertIn("2024-06-01", runs[0].executed_at.isoformat())

    def test_executed_at_defaults_when_missing(self):
        p = _payload()
        p.pop("started_at", None)
        ok, runs = self._call(p)
        self.assertIsNotNone(runs[0].executed_at)


# ══════════════════════════════════════════════════════════════════════════════
# 2. Status filtering — skip intermediate states
# ══════════════════════════════════════════════════════════════════════════════

class TestStatusFiltering(unittest.TestCase):

    def _call(self, status):
        with patch("services.run_bridge.test_run_repo") as mock_repo:
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(_payload(status=status))
        return ok, mock_repo.create_run.called

    def test_queued_is_skipped(self):
        ok, created = self._call("queued")
        self.assertFalse(ok)
        self.assertFalse(created)

    def test_running_is_skipped(self):
        ok, created = self._call("running")
        self.assertFalse(ok)
        self.assertFalse(created)

    def test_pending_is_skipped(self):
        ok, created = self._call("pending")
        self.assertFalse(ok)
        self.assertFalse(created)

    def test_completed_is_persisted(self):
        with patch("services.run_bridge.test_run_repo"):
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(_payload(status="completed"))
        self.assertTrue(ok)

    def test_failed_is_persisted(self):
        with patch("services.run_bridge.test_run_repo"):
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(_payload(status="failed"))
        self.assertTrue(ok)

    def test_error_is_persisted(self):
        with patch("services.run_bridge.test_run_repo"):
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(_payload(status="error"))
        self.assertTrue(ok)


# ══════════════════════════════════════════════════════════════════════════════
# 3. Missing / edge-case inputs
# ══════════════════════════════════════════════════════════════════════════════

class TestEdgeCases(unittest.TestCase):

    def test_no_evidence_id_returns_false(self):
        p = {"status": "completed", "duration_ms": 0, "meta": {}}
        from services.run_bridge import bridge_run_to_sqlite
        ok = bridge_run_to_sqlite(p)
        self.assertFalse(ok)

    def test_empty_dict_returns_false(self):
        from services.run_bridge import bridge_run_to_sqlite
        ok = bridge_run_to_sqlite({})
        self.assertFalse(ok)

    def test_none_logs_handled(self):
        p = _payload(logs=None)
        with patch("services.run_bridge.test_run_repo"):
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(p)
        self.assertTrue(ok)

    def test_none_steps_handled(self):
        p = _payload()
        p.pop("steps", None)
        with patch("services.run_bridge.test_run_repo"):
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(p)
        self.assertTrue(ok)

    def test_none_meta_handled(self):
        p = _payload(meta=None)
        with patch("services.run_bridge.test_run_repo"):
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(p)
        self.assertTrue(ok)

    def test_run_id_used_when_no_evidence_id(self):
        p = {"run_id": "run-fallback", "status": "completed", "duration_ms": 0, "meta": {}}
        runs = []
        with patch("services.run_bridge.test_run_repo") as m:
            m.create_run.side_effect = lambda tr: runs.append(tr)
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(p)
        self.assertTrue(ok)
        self.assertEqual(runs[0].run_id, "run-fallback")


# ══════════════════════════════════════════════════════════════════════════════
# 4. Failure resilience — bridge never raises
# ══════════════════════════════════════════════════════════════════════════════

class TestFailureResilience(unittest.TestCase):

    def test_repo_exception_returns_false_not_raises(self):
        with patch("services.run_bridge.test_run_repo") as mock_repo:
            mock_repo.create_run.side_effect = Exception("DB locked")
            from services.run_bridge import bridge_run_to_sqlite
            ok = bridge_run_to_sqlite(_payload())  # must not raise
        self.assertFalse(ok)

    def test_import_error_returns_false_not_raises(self):
        with patch.dict("sys.modules", {"services.db.test_run_repository": None}):
            from services.run_bridge import bridge_run_to_sqlite
            # Even if the import fails, bridge_run_to_sqlite should not raise
            try:
                result = bridge_run_to_sqlite(_payload())
                # If it gets here without raising, that's the goal
            except Exception:
                self.fail("bridge_run_to_sqlite raised an exception")


# ══════════════════════════════════════════════════════════════════════════════
# 5. Integration with run_store.save_run()
# ══════════════════════════════════════════════════════════════════════════════

class TestRunStoreSaveRunIntegration(unittest.TestCase):
    """
    Verify that run_store.save_run() calls bridge_run_to_sqlite for final states
    and bridge_async_run_to_sqlite for queued/running, without affecting the return value.
    """

    def _save(self, status: str, evidence_id: str = "ev-int-test"):
        payload = {
            "evidence_id": evidence_id,
            "status": status,
            "duration_ms": 100,
            "meta": {},
        }
        with patch("services.run_store.bridge_run_to_sqlite") as mock_bridge, patch(
            "services.run_store.bridge_async_run_to_sqlite"
        ) as mock_async:
            # Also suppress Supabase
            with patch("services.run_store.persist_run_supabase", side_effect=Exception):
                from services.run_store import save_run
                result = save_run(payload)
        return result, mock_bridge, mock_async

    def test_save_run_returns_evidence_id_for_final_state(self):
        result, _, _ = self._save("completed")
        self.assertEqual(result, "ev-int-test")

    def test_save_run_returns_evidence_id_for_queued(self):
        result, _, _ = self._save("queued", evidence_id="ev-q")
        self.assertEqual(result, "ev-q")

    def test_bridge_called_for_completed(self):
        _, mock_bridge, mock_async = self._save("completed")
        mock_bridge.assert_called_once()
        mock_async.assert_not_called()

    def test_bridge_called_for_failed(self):
        _, mock_bridge, mock_async = self._save("failed")
        mock_bridge.assert_called_once()
        mock_async.assert_not_called()

    def test_bridge_called_for_error(self):
        _, mock_bridge, mock_async = self._save("error")
        mock_bridge.assert_called_once()
        mock_async.assert_not_called()

    def test_queued_uses_async_bridge(self):
        _, mock_bridge, mock_async = self._save("queued")
        mock_async.assert_called_once()
        mock_bridge.assert_not_called()

    def test_bridge_failure_does_not_affect_save_run_result(self):
        payload = {"evidence_id": "ev-safe", "status": "completed", "meta": {}}
        with patch("services.run_bridge.bridge_run_to_sqlite", side_effect=Exception("crash")):
            with patch("services.run_store.persist_run_supabase", side_effect=Exception):
                from services.run_store import save_run
                result = save_run(payload)
        self.assertEqual(result, "ev-safe")   # run_store still works


# ══════════════════════════════════════════════════════════════════════════════
# 6. _to_datetime helper
# ══════════════════════════════════════════════════════════════════════════════

class TestToDatetime(unittest.TestCase):

    def setUp(self):
        from services.run_bridge import _to_datetime
        self._fn = _to_datetime

    def test_ms_epoch_int(self):
        dt = self._fn(1_717_243_200_000)
        self.assertIn("2024", dt.isoformat())

    def test_ms_epoch_float(self):
        dt = self._fn(1_717_243_200_000.0)
        self.assertIn("2024", dt.isoformat())

    def test_iso_string(self):
        dt = self._fn("2024-06-01T12:00:00+00:00")
        self.assertEqual(dt.year, 2024)

    def test_none_returns_now(self):
        dt = self._fn(None)
        self.assertIsInstance(dt, datetime)

    def test_invalid_string_returns_now(self):
        dt = self._fn("not-a-date")
        self.assertIsInstance(dt, datetime)

    def test_datetime_passthrough(self):
        original = datetime(2024, 1, 1, tzinfo=timezone.utc)
        result = self._fn(original)
        self.assertEqual(result, original)

    def test_naive_datetime_gets_utc(self):
        naive = datetime(2024, 1, 1)
        result = self._fn(naive)
        self.assertIsNotNone(result.tzinfo)


if __name__ == "__main__":
    unittest.main()
