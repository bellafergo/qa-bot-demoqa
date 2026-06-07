# tests/test_run_history_merge.py
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import patch

from models.run_contract import CanonicalRun, RunMeta
from models.test_run import TestRun
from services.db.test_run_repository import test_run_repo
from services.run_history_service import run_history_service


def _cr(run_id: str, *, started: str = "2026-06-01T12:00:00+00:00") -> CanonicalRun:
    return CanonicalRun(
        run_id=run_id,
        test_id="TC-1",
        status="pass",
        started_at=datetime.fromisoformat(started),
        meta=RunMeta(source="catalog"),
    )


def test_merge_prefers_sqlite_when_supabase_empty():
    from services.run_history_merge import merge_sqlite_supabase_runs

    tr = TestRun(
        run_id="run-sql-1",
        test_case_id="TC-1",
        test_name="T",
        status="pass",
        duration_ms=100,
        evidence_id="EV-1",
    )
    out = merge_sqlite_supabase_runs(
        supabase_runs=[],
        sqlite_runs=[tr],
        limit=10,
    )
    assert len(out) == 1
    assert out[0].run_id == "run-sql-1"


@patch("services.run_history_service.list_qa_runs_canonical", return_value=[])
def test_list_runs_includes_sqlite_when_supabase_empty(mock_list):
    from services.db.init_db import init_catalog_db

    init_catalog_db()
    tr = TestRun(
        run_id="run-merge-1",
        test_case_id="TC-MERGE-001",
        test_name="Merge test",
        status="pass",
        duration_ms=50,
        evidence_id="EV-MERGE-1",
        meta={"source": "catalog", "correlation_id": "corr-abc-123"},
    )
    test_run_repo.create_run(tr)

    with patch("services.run_history_service.supabase_qa_runs_enabled", return_value=True):
        rows = run_history_service.list_runs(limit=20)
    ids = {r.run_id for r in rows}
    assert "run-merge-1" in ids


def test_find_by_correlation_id():
    from services.db.init_db import init_catalog_db

    init_catalog_db()
    tr = TestRun(
        run_id="run-corr-1",
        test_case_id="TC-CORR",
        test_name="Corr",
        status="pass",
        duration_ms=10,
        evidence_id="EV-CORR-1",
        meta={"correlation_id": "d369870b8d89"},
    )
    test_run_repo.create_run(tr)

    hit = test_run_repo.find_by_lookup_id("d369870b8d89")
    assert hit is not None
    assert hit.run_id == "run-corr-1"

    with patch("services.run_history_service.supabase_qa_runs_enabled", return_value=False):
        cr = run_history_service.get_run("d369870b8d89")
    assert cr is not None
    assert cr.run_id == "run-corr-1"


def test_persist_run_supabase_strips_screenshot_b64():
    from services.run_store_supabase import _lean_result_for_supabase

    payload = {
        "run_id": "r1",
        "evidence_id": "e1",
        "meta": {"screenshot_b64": "BIG", "evidence": {"screenshot_b64": "BIG2"}},
        "screenshot_b64": "BIG3",
    }
    lean = _lean_result_for_supabase(payload)
    assert "screenshot_b64" not in lean
    assert "screenshot_b64" not in lean["meta"]
    assert "screenshot_b64" not in lean["meta"]["evidence"]
