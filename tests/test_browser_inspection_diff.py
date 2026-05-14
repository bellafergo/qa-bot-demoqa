# tests/test_browser_inspection_diff.py
from __future__ import annotations

import uuid
from datetime import datetime, timezone
from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.test_run import TestRun as StoredTestRun
from services.browser_inspection_diff_service import compare_browser_inspections
from models.browser_inspection_diff_models import BrowserInspectionDiffRequest


def _bio_run(
    rid: str,
    *,
    title: str = "T",
    status_code: int = 200,
    counts: dict | None = None,
    console_n: int = 0,
    network_n: int = 0,
    warnings_n: int = 0,
    page_type: list | None = None,
    risk_notes: list | None = None,
    actions: list | None = None,
    flows: list | None = None,
    project_id: str | None = "p1",
):
    counts = counts or {
        "links_count": 2,
        "buttons_count": 2,
        "inputs_count": 1,
        "forms_count": 0,
        "images_without_alt_count": 0,
        "selector_candidates_count": 5,
    }
    page_type = page_type or ["dashboard"]
    if actions is None:
        actions_final = [{"text": "Save", "kind": "submit"}]
    else:
        actions_final = actions
    if flows is None:
        flows_final = ["flow-a"]
    else:
        flows_final = flows
    bis = {
        "inspection_id": rid,
        "url": "https://ex.com",
        "final_url": "https://ex.com/",
        "title": title,
        "status_code": status_code,
        "screenshot_url": None,
        "console_errors": [{"text": f"e{i}"} for i in range(console_n)],
        "network_errors": [{"url": f"u{i}", "failure": "x"} for i in range(network_n)],
        "counts": counts,
        "warnings": [f"w{i}" for i in range(warnings_n)],
        "inspection_succeeded": True,
    }
    ams = {
        "page_type": page_type,
        "confidence": "medium",
        "primary_actions_summary": actions_final,
        "suggested_test_flows": flows_final,
        "risk_notes": risk_notes or [],
    }
    meta = {
        "source": "browser_inspection",
        "test_case_id": "_browser_inspection",
        "execution_mode": "cloud",
        "project_id": project_id,
        "browser_inspection_summary": bis,
        "app_map_summary": ams,
    }
    return StoredTestRun(
        run_id=rid,
        test_case_id="_browser_inspection",
        test_name=f"[Browser inspection] {title}",
        executed_at=datetime.now(timezone.utc),
        environment="default",
        status="pass",
        duration_ms=0,
        evidence_url=None,
        evidence_id=rid,
        logs=[],
        steps_result=[],
        meta=meta,
    )


def test_diff_no_changes_none_level():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo

    test_run_repo.create_run(_bio_run(b))
    test_run_repo.create_run(_bio_run(t))
    out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert out.change_level == "none"
    assert not out.changes.title_changed


def test_diff_title_and_status_change_level_low_or_medium():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo

    rb = _bio_run(b, title="A")
    rt = _bio_run(t, title="B", status_code=500)
    test_run_repo.create_run(rb)
    test_run_repo.create_run(rt)
    out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert out.changes.title_changed is True
    assert out.changes.status_code_changed is True
    assert out.change_level in ("low", "medium", "high")
    assert "http_status_worsened" in out.regression_signals


def test_diff_console_network_regression():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo

    test_run_repo.create_run(_bio_run(b, console_n=0, network_n=0))
    test_run_repo.create_run(_bio_run(t, console_n=2, network_n=1))
    out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert "console_errors_increased" in out.regression_signals
    assert "network_errors_increased" in out.regression_signals


def test_diff_selector_candidates_drop_regression():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo

    hi = {"selector_candidates_count": 10, "links_count": 0, "buttons_count": 0, "inputs_count": 0, "forms_count": 0, "images_without_alt_count": 0}
    lo = {"selector_candidates_count": 2, "links_count": 0, "buttons_count": 0, "inputs_count": 0, "forms_count": 0, "images_without_alt_count": 0}
    test_run_repo.create_run(_bio_run(b, counts=hi))
    test_run_repo.create_run(_bio_run(t, counts=lo))
    out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert "selector_candidates_dropped_significantly" in out.regression_signals


def test_diff_page_type_unknown_to_dashboard_improvement():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo

    test_run_repo.create_run(_bio_run(b, page_type=["unknown"]))
    test_run_repo.create_run(_bio_run(t, page_type=["dashboard"]))
    out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert "page_type_became_known_category" in out.improvement_signals


def test_diff_primary_actions_removed_regression():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo

    test_run_repo.create_run(_bio_run(b, actions=[{"text": "Save", "kind": "submit"}]))
    test_run_repo.create_run(_bio_run(t, actions=[]))
    out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert "primary_actions_removed" in out.regression_signals


def test_diff_missing_inspection_404():
    from fastapi import HTTPException

    with pytest.raises(HTTPException) as ei:
        compare_browser_inspections(
            BrowserInspectionDiffRequest(
                base_inspection_id=str(uuid.uuid4()),
                target_inspection_id=str(uuid.uuid4()),
            )
        )
    assert ei.value.status_code == 404


def test_diff_response_has_no_heavy_keys():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo

    test_run_repo.create_run(_bio_run(b))
    test_run_repo.create_run(_bio_run(t))
    out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    blob = str(out.model_dump())
    assert "screenshot_b64" not in blob.lower()
    assert "<html" not in blob.lower()
    assert len(blob) < 20_000


def test_diff_includes_visual_fields_when_pair_succeeds():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo
    from models.browser_inspection_diff_models import BrowserInspectionDiffRequest
    from models.browser_visual_diff_models import BrowserVisualDiffResult

    test_run_repo.create_run(_bio_run(b))
    test_run_repo.create_run(_bio_run(t))

    fake = BrowserVisualDiffResult(
        visual_change_detected=False,
        visual_change_level="none",
        visual_hash_changed=False,
        visual_similarity_score=1.0,
        base_visual_hash="a" * 16,
        target_visual_hash="a" * 16,
    )
    with patch(
        "services.browser_inspection_diff_service.compare_browser_visual_pair",
        return_value=fake,
    ) as mock_v:
        out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    mock_v.assert_called_once()
    assert out.visual_similarity_score == 1.0
    assert out.visual_change_level == "none"


def test_diff_visual_adds_regression_signal_when_medium():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo
    from models.browser_inspection_diff_models import BrowserInspectionDiffRequest
    from models.browser_visual_diff_models import BrowserVisualDiffResult

    test_run_repo.create_run(_bio_run(b))
    test_run_repo.create_run(_bio_run(t))
    fake = BrowserVisualDiffResult(
        visual_change_detected=True,
        visual_change_level="medium",
        visual_hash_changed=True,
        visual_similarity_score=0.55,
        base_visual_hash="b" * 16,
        target_visual_hash="c" * 16,
    )
    with patch(
        "services.browser_inspection_diff_service.compare_browser_visual_pair",
        return_value=fake,
    ):
        out = compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert "visual_change_suspected" in out.regression_signals


def test_diff_project_mismatch_400():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo
    from fastapi import HTTPException

    test_run_repo.create_run(_bio_run(b, project_id="p-a"))
    test_run_repo.create_run(_bio_run(t, project_id="p-b"))
    with pytest.raises(HTTPException) as ei:
        compare_browser_inspections(BrowserInspectionDiffRequest(base_inspection_id=b, target_inspection_id=t))
    assert ei.value.status_code == 400


def test_diff_endpoint_ok():
    b, t = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo
    from app import app

    test_run_repo.create_run(_bio_run(b))
    test_run_repo.create_run(_bio_run(t))
    client = TestClient(app)
    r = client.post(
        "/browser-inspections/diff",
        json={"base_inspection_id": b, "target_inspection_id": t, "project_id": "p1"},
    )
    assert r.status_code == 200
    data = r.json()
    assert data["base_inspection_id"] == b
    assert "changes" in data
    assert "visual_change_detected" in data


def test_browser_inspection_not_in_test_runs_list():
    """Generic GET /test-runs must not include _browser_inspection rows (SQLite path)."""
    bio_id, cat_id = str(uuid.uuid4()), str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo
    from app import app

    test_run_repo.create_run(_bio_run(bio_id, project_id=None))
    test_run_repo.create_run(
        StoredTestRun(
            run_id=cat_id,
            test_case_id="TC-HYGIENE-001",
            test_name="Catalog",
            executed_at=datetime.now(timezone.utc),
            environment="default",
            status="pass",
            duration_ms=1,
            evidence_url=None,
            evidence_id=cat_id,
            logs=[],
            steps_result=[],
            meta={"source": "catalog"},
        )
    )
    with patch("services.run_history_service.supabase_qa_runs_enabled", return_value=False):
        client = TestClient(app)
        resp = client.get("/test-runs", params={"limit": 50})
    assert resp.status_code == 200
    ids = {x.get("run_id") for x in resp.json()}
    assert cat_id in ids
    assert bio_id not in ids


def test_browser_inspection_still_listed_dedicated_endpoint():
    rid = str(uuid.uuid4())
    from services.db.test_run_repository import test_run_repo
    from app import app

    test_run_repo.create_run(_bio_run(rid))
    client = TestClient(app)
    r = client.get("/browser-inspections", params={"limit": 20})
    assert r.status_code == 200
    ids = {x.get("inspection_id") for x in r.json().get("items", [])}
    assert rid in ids


def test_run_store_list_skips_browser_inspection():
    from services import run_store

    bid, cid = str(uuid.uuid4()), str(uuid.uuid4())
    run_store.save_run(
        {
            "run_id": bid,
            "evidence_id": bid,
            "status": "passed",
            "test_name": "bio",
            "meta": {"source": "browser_inspection", "test_case_id": "_browser_inspection"},
            "steps": [],
        }
    )
    run_store.save_run(
        {
            "run_id": cid,
            "evidence_id": cid,
            "status": "passed",
            "test_name": "cat",
            "meta": {"source": "catalog", "test_case_id": "TC-1"},
            "steps": [],
        }
    )
    rows = run_store.list_runs(limit=10)
    ids = {r.get("run_id") for r in rows}
    assert bid not in ids
    assert cid in ids
