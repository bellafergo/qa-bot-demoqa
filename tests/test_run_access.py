# tests/test_run_access.py
"""Thin facade tests — run_access delegates without changing behavior."""
from __future__ import annotations

import unittest
from unittest.mock import MagicMock, patch

from models.run_contract import CanonicalRun


class TestRunAccess(unittest.TestCase):

    def test_persist_run_payload_delegates_to_save_run(self):
        payload = {"evidence_id": "e1", "status": "queued"}
        with patch("services.run_access.save_run", return_value="e1") as m:
            from services.run_access import persist_run_payload

            self.assertEqual(persist_run_payload(payload), "e1")
            m.assert_called_once_with(payload)

    def test_get_canonical_run_delegates_to_get_run_unified(self):
        cr = CanonicalRun(run_id="r1", status="passed")
        with patch(
            "services.run_history_service.run_history_service.get_run_unified",
            return_value=cr,
        ) as m:
            from services.run_access import get_canonical_run

            self.assertIs(get_canonical_run("r1"), cr)
            m.assert_called_once_with("r1")


if __name__ == "__main__":
    unittest.main()
