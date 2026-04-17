# tests/test_qa_runs_read.py
"""qa_runs row → legacy payload: canonical run_id must not collapse to evidence_id."""
from __future__ import annotations

import unittest

from services.qa_runs_read import qa_runs_row_to_legacy_payload


class TestQaRunsRowToLegacyPayload(unittest.TestCase):
    def test_result_dict_prefers_embedded_run_id_over_evidence_pk(self):
        row = {
            "evidence_id": "TC-ABC12345",
            "run_id": "",
            "result": {
                "run_id": "11111111-1111-1111-1111-111111111111",
                "status": "passed",
            },
            "meta": {},
            "status": "failed",
        }
        p = qa_runs_row_to_legacy_payload(row)
        self.assertEqual(p["run_id"], "11111111-1111-1111-1111-111111111111")
        self.assertEqual(p.get("evidence_id"), "TC-ABC12345")

    def test_column_run_id_wins_over_result(self):
        row = {
            "evidence_id": "TC-OLD",
            "run_id": "22222222-2222-2222-2222-222222222222",
            "result": {"run_id": "33333333-3333-3333-3333-333333333333"},
            "meta": {},
            "status": "passed",
        }
        p = qa_runs_row_to_legacy_payload(row)
        self.assertEqual(p["run_id"], "22222222-2222-2222-2222-222222222222")

    def test_no_result_uses_meta_run_id_before_evidence_id(self):
        row = {
            "evidence_id": "EV-deadbeef",
            "run_id": "",
            "result": None,
            "meta": {"run_id": "44444444-4444-4444-4444-444444444444"},
            "status": "error",
        }
        p = qa_runs_row_to_legacy_payload(row)
        self.assertEqual(p["run_id"], "44444444-4444-4444-4444-444444444444")


if __name__ == "__main__":
    unittest.main()
