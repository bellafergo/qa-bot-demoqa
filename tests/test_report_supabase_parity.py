# tests/test_report_supabase_parity.py
"""Supabase parity for incident_investigation_reports and pr_analysis_reports."""
from __future__ import annotations

from unittest.mock import MagicMock, patch


# ── merge: newest created_at wins ─────────────────────────────────────────────


def test_merge_list_sqlite_newer_wins():
    from services.report_history_merge import merge_sqlite_supabase_records

    sqlite_rows = [
        {"id": "a", "created_at": "2026-06-06T12:00:00+00:00", "summary": "sqlite-newer"},
    ]
    supa_rows = [
        {"id": "a", "created_at": "2026-06-06T08:00:00+00:00", "summary": "supa-stale"},
    ]
    merged = merge_sqlite_supabase_records(
        supabase_rows=supa_rows,
        sqlite_rows=sqlite_rows,
        limit=10,
    )
    assert merged[0]["summary"] == "sqlite-newer"


def test_merge_list_supabase_newer_wins():
    from services.report_history_merge import merge_sqlite_supabase_records

    sqlite_rows = [
        {"id": "a", "created_at": "2026-06-06T08:00:00+00:00", "summary": "sqlite-stale"},
    ]
    supa_rows = [
        {"id": "a", "created_at": "2026-06-06T12:00:00+00:00", "summary": "supa-newer"},
    ]
    merged = merge_sqlite_supabase_records(
        supabase_rows=supa_rows,
        sqlite_rows=sqlite_rows,
        limit=10,
    )
    assert merged[0]["summary"] == "supa-newer"


def test_merge_list_tie_supabase_wins():
    from services.report_history_merge import merge_sqlite_supabase_records

    sqlite_rows = [
        {"id": "a", "created_at": "2026-06-06T10:00:00+00:00", "summary": "sqlite-a"},
        {"id": "b", "created_at": "2026-06-06T09:00:00+00:00", "summary": "sqlite-b"},
    ]
    supa_rows = [
        {"id": "a", "created_at": "2026-06-06T10:00:00+00:00", "summary": "supa-a"},
    ]
    merged = merge_sqlite_supabase_records(
        supabase_rows=supa_rows,
        sqlite_rows=sqlite_rows,
        limit=10,
    )
    assert [r["id"] for r in merged] == ["a", "b"]
    assert merged[0]["summary"] == "supa-a"


def test_merge_list_invalid_timestamp_fallback_supabase():
    from services.report_history_merge import merge_sqlite_supabase_records

    sqlite_rows = [
        {"id": "a", "created_at": "not-a-date", "summary": "sqlite-invalid-ts"},
    ]
    supa_rows = [
        {"id": "a", "created_at": "2026-06-06T10:00:00+00:00", "summary": "supa-valid-ts"},
    ]
    merged = merge_sqlite_supabase_records(
        supabase_rows=supa_rows,
        sqlite_rows=sqlite_rows,
        limit=10,
    )
    assert merged[0]["summary"] == "supa-valid-ts"


def test_merge_list_orders_by_created_at_desc():
    from services.report_history_merge import merge_sqlite_supabase_records

    sqlite_rows = [
        {"id": "old", "created_at": "2026-06-05T10:00:00+00:00"},
        {"id": "new", "created_at": "2026-06-06T15:00:00+00:00"},
    ]
    merged = merge_sqlite_supabase_records(
        supabase_rows=[],
        sqlite_rows=sqlite_rows,
        limit=10,
    )
    assert [r["id"] for r in merged] == ["new", "old"]


def test_merge_reports_sqlite_only_when_supabase_empty():
    from services.report_history_merge import merge_sqlite_supabase_records

    rows = [{"id": "x", "created_at": "2026-06-06T10:00:00+00:00"}]
    merged = merge_sqlite_supabase_records(
        supabase_rows=[],
        sqlite_rows=rows,
        limit=5,
    )
    assert merged == rows


def test_pick_newer_record_get_sqlite_wins():
    from services.report_history_merge import pick_newer_record

    winner = pick_newer_record(
        sqlite_row={"id": "1", "created_at": "2026-06-06T12:00:00+00:00", "v": "sqlite"},
        supabase_row={"id": "1", "created_at": "2026-06-06T08:00:00+00:00", "v": "supa"},
    )
    assert winner["v"] == "sqlite"


def test_pick_newer_record_get_supabase_wins():
    from services.report_history_merge import pick_newer_record

    winner = pick_newer_record(
        sqlite_row={"id": "1", "created_at": "2026-06-06T08:00:00+00:00", "v": "sqlite"},
        supabase_row={"id": "1", "created_at": "2026-06-06T12:00:00+00:00", "v": "supa"},
    )
    assert winner["v"] == "supa"


# ── supabase persist ──────────────────────────────────────────────────────────


@patch("services.incident_report_supabase._get_supabase")
def test_persist_incident_report_supabase_upserts(mock_get_sb):
    from services.incident_report_supabase import persist_incident_report_supabase

    table = MagicMock()
    sb = MagicMock()
    sb.table.return_value = table
    mock_get_sb.return_value = sb

    ok = persist_incident_report_supabase(
        report_id="inc-1",
        project_id="demo",
        description="Login fails",
        severity="high",
        summary="summary",
        confidence=0.55,
        created_at="2026-06-06T10:00:00+00:00",
        report_json={"project_id": "demo", "confidence": 0.55},
    )
    assert ok is True
    sb.table.assert_called_with("incident_investigation_reports")
    table.upsert.assert_called_once()
    kwargs = table.upsert.call_args.kwargs
    assert kwargs.get("on_conflict") == "id"


@patch("services.pr_analysis_report_supabase._get_supabase")
def test_persist_pr_analysis_report_supabase_upserts(mock_get_sb):
    from services.pr_analysis_report_supabase import persist_pr_analysis_report_supabase

    table = MagicMock()
    sb = MagicMock()
    sb.table.return_value = table
    mock_get_sb.return_value = sb

    ok = persist_pr_analysis_report_supabase(
        row_id="pr-row-1",
        project_id="demo",
        pr_id="142",
        provider="github",
        created_at="2026-06-06T10:00:00+00:00",
        report_json={"pr_risk_score": 62},
    )
    assert ok is True
    sb.table.assert_called_with("pr_analysis_reports")
    table.upsert.assert_called_once()
    kwargs = table.upsert.call_args.kwargs
    assert kwargs.get("on_conflict") == "project_id,pr_id,provider"


@patch("services.pr_analysis_report_supabase._get_supabase")
def test_persist_pr_analysis_reanalyze_natural_key_updates_row(mock_get_sb):
    """Same PR re-analyzed with a new SQLite id must upsert on natural key, not id."""
    from services.pr_analysis_report_supabase import persist_pr_analysis_report_supabase

    table = MagicMock()
    sb = MagicMock()
    sb.table.return_value = table
    mock_get_sb.return_value = sb

    base = {
        "project_id": "demo",
        "pr_id": "142",
        "provider": "github",
    }
    ok1 = persist_pr_analysis_report_supabase(
        row_id="supa-old-id",
        created_at="2026-06-06T10:00:00+00:00",
        report_json={"pr_risk_score": 62},
        **base,
    )
    ok2 = persist_pr_analysis_report_supabase(
        row_id="sqlite-new-id-after-reanalyze",
        created_at="2026-06-06T12:00:00+00:00",
        report_json={"pr_risk_score": 71},
        **base,
    )
    assert ok1 is True and ok2 is True
    assert table.upsert.call_count == 2
    for call in table.upsert.call_args_list:
        assert call.kwargs.get("on_conflict") == "project_id,pr_id,provider"
    second_row = table.upsert.call_args_list[1][0][0]
    assert second_row["id"] == "sqlite-new-id-after-reanalyze"
    assert second_row["project_id"] == "demo"
    assert second_row["pr_id"] == "142"
    assert second_row["provider"] == "github"
    assert second_row["report_json"]["pr_risk_score"] == 71


@patch("services.incident_report_supabase.persist_incident_report_supabase", return_value=True)
def test_incident_repo_save_mirrors_to_supabase(mock_persist):
    from services.db.incident_report_repository import IncidentReportRepository

    repo = IncidentReportRepository()
    rid = repo.save(
        project_id="demo",
        description="Issue",
        severity="medium",
        summary="Sum",
        confidence=0.4,
        report={"project_id": "demo", "description": "Issue"},
    )
    assert rid
    mock_persist.assert_called_once()
    assert mock_persist.call_args.kwargs["report_id"] == rid


@patch("services.pr_analysis_report_supabase.persist_pr_analysis_report_supabase", return_value=True)
def test_pr_analysis_repo_upsert_mirrors_to_supabase(mock_persist):
    from services.db.pr_analysis_report_repository import PRAnalysisReportRepository

    repo = PRAnalysisReportRepository()
    row_id = repo.upsert(
        project_id="demo",
        pr_id="99",
        provider="github",
        report={"pr_risk_score": 50, "project_id": "demo"},
    )
    assert row_id
    mock_persist.assert_called_once()
    assert mock_persist.call_args.kwargs["row_id"] == row_id


# ── repo get: compare both stores ─────────────────────────────────────────────


@patch("services.incident_report_supabase.supabase_incident_reports_enabled", return_value=True)
@patch("services.incident_report_supabase.fetch_incident_report_full_supabase")
@patch("services.db.incident_report_repository.IncidentReportRepository._get_sqlite")
def test_incident_repo_get_sqlite_newer_wins(mock_sqlite, mock_fetch, _enabled):
    from services.db.incident_report_repository import IncidentReportRepository

    mock_sqlite.return_value = {
        "id": "inc-9",
        "created_at": "2026-06-06T12:00:00+00:00",
        "summary": "sqlite-newer",
    }
    mock_fetch.return_value = {
        "id": "inc-9",
        "created_at": "2026-06-06T08:00:00+00:00",
        "summary": "supa-stale",
    }
    doc = IncidentReportRepository().get("inc-9")
    assert doc["summary"] == "sqlite-newer"


@patch("services.incident_report_supabase.supabase_incident_reports_enabled", return_value=True)
@patch("services.incident_report_supabase.fetch_incident_report_full_supabase")
@patch("services.db.incident_report_repository.IncidentReportRepository._get_sqlite")
def test_incident_repo_get_supabase_newer_wins(mock_sqlite, mock_fetch, _enabled):
    from services.db.incident_report_repository import IncidentReportRepository

    mock_sqlite.return_value = {
        "id": "inc-9",
        "created_at": "2026-06-06T08:00:00+00:00",
        "summary": "sqlite-stale",
    }
    mock_fetch.return_value = {
        "id": "inc-9",
        "created_at": "2026-06-06T12:00:00+00:00",
        "summary": "supa-newer",
    }
    doc = IncidentReportRepository().get("inc-9")
    assert doc["summary"] == "supa-newer"


@patch("services.pr_analysis_report_supabase.supabase_pr_analysis_reports_enabled", return_value=True)
@patch("services.pr_analysis_report_supabase.list_pr_analysis_reports_supabase")
@patch("services.db.pr_analysis_report_repository.PRAnalysisReportRepository._list_sqlite")
def test_pr_analysis_repo_list_merges_newest_wins(mock_sqlite, mock_supa_list, _enabled):
    from services.db.pr_analysis_report_repository import PRAnalysisReportRepository

    mock_sqlite.return_value = [
        {
            "id": "1",
            "created_at": "2026-06-06T12:00:00+00:00",
            "pr_id": "1",
            "report": {"pr_risk_score": 90},
        },
    ]
    mock_supa_list.return_value = [
        {
            "id": "1",
            "created_at": "2026-06-06T08:00:00+00:00",
            "pr_id": "1",
            "report": {"pr_risk_score": 80},
        },
        {"id": "2", "created_at": "2026-06-06T07:00:00+00:00", "pr_id": "2", "report": {}},
    ]
    rows = PRAnalysisReportRepository().list_for_project("demo", limit=10)
    assert len(rows) == 2
    assert rows[0]["report"]["pr_risk_score"] == 90


@patch("services.incident_report_supabase.supabase_incident_reports_enabled", return_value=True)
@patch("services.incident_report_supabase.list_incident_reports_supabase")
@patch("services.db.incident_report_repository.IncidentReportRepository._list_sqlite")
def test_incident_repo_list_merges_tie_supabase_wins(mock_sqlite, mock_supa_list, _enabled):
    from services.db.incident_report_repository import IncidentReportRepository

    mock_sqlite.return_value = [
        {"id": "a", "created_at": "2026-06-06T10:00:00+00:00", "confidence": 0.2},
    ]
    mock_supa_list.return_value = [
        {"id": "a", "created_at": "2026-06-06T10:00:00+00:00", "confidence": 0.9},
        {"id": "b", "created_at": "2026-06-06T09:00:00+00:00", "confidence": 0.5},
    ]
    rows = IncidentReportRepository().list_reports(project_id="demo", limit=10)
    assert rows[0]["confidence"] == 0.9
    assert any(r["id"] == "b" for r in rows)


def test_persist_incident_report_supabase_no_client_returns_false():
    from services.incident_report_supabase import persist_incident_report_supabase

    with patch("services.incident_report_supabase._get_supabase", return_value=None):
        ok = persist_incident_report_supabase(
            report_id="x",
            project_id="demo",
            description="d",
            severity="low",
            summary="s",
            confidence=0.1,
            created_at="2026-06-06T10:00:00+00:00",
            report_json={},
        )
    assert ok is False
