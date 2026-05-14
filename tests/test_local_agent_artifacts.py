# tests/test_local_agent_artifacts.py
"""Phase 4D — Local agent artifact upload + cloud browser inspection persistence."""
from __future__ import annotations

import json
from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient

from models.browser_inspection_models import BrowserInspectionResult
from models.local_agent_models import LOCAL_AGENT_MAX_ARTIFACT_BYTES
from models.test_run import TestRun as StoredTestRun
from services.browser_inspection_persistence import build_lightweight_run_payload
from services.run_mapper import run_from_catalog_testrun

MINI_PNG = bytes.fromhex(
    "89504e470d0a1a0a0000000d49484452000000010000000108060000001f15c489"
    "0000000a49444154789c63000100000500001d0a2db40000000049454e44ae426082"
)


@pytest.fixture
def client() -> TestClient:
    from app import app

    return TestClient(app)


def _register(client: TestClient) -> tuple[str, str]:
    r = client.post("/local-agents/register", json={"project_id": "p-artifacts", "name": "agent-art"})
    assert r.status_code == 200, r.text
    j = r.json()
    return j["agent_id"], j["agent_token"]


def test_upload_valid_png_mock_cloudinary(client: TestClient):
    aid, tok = _register(client)
    with patch("services.cloudinary_service.upload_image_bytes") as up:
        up.return_value = {"secure_url": "https://res.cloudinary.com/demo/image/upload/v1/x.png", "url": ""}
        r = client.post(
            f"/agent-api/{aid}/artifacts/upload",
            params={"inspection_id": "insp-uuid-01"},
            files={"file": ("ok.png", MINI_PNG, "image/png")},
            headers={"Authorization": f"Bearer {tok}"},
        )
    assert r.status_code == 200, r.text
    body = r.json()
    assert body["content_type"] == "image/png"
    assert body["evidence_url"].startswith("https://")
    assert len(body["sha256"]) == 64


def test_upload_rejects_magic_mismatch(client: TestClient):
    aid, tok = _register(client)
    r = client.post(
        f"/agent-api/{aid}/artifacts/upload",
        params={"inspection_id": "insp-uuid-02"},
        files={"file": ("x.png", b"GIF89a" + b"\x00" * 20, "image/png")},
        headers={"Authorization": f"Bearer {tok}"},
    )
    assert r.status_code == 400


def test_upload_rejects_oversized(client: TestClient):
    aid, tok = _register(client)
    big = b"\x00" * (LOCAL_AGENT_MAX_ARTIFACT_BYTES + 1)
    r = client.post(
        f"/agent-api/{aid}/artifacts/upload",
        params={"inspection_id": "insp-uuid-03"},
        files={"file": ("huge.png", big, "image/png")},
        headers={"Authorization": f"Bearer {tok}"},
    )
    assert r.status_code == 413


def test_upload_rejects_invalid_token(client: TestClient):
    aid, _ = _register(client)
    r = client.post(
        f"/agent-api/{aid}/artifacts/upload",
        params={"inspection_id": "insp-uuid-04"},
        files={"file": ("ok.png", MINI_PNG, "image/png")},
        headers={"Authorization": "Bearer vla_wrong_token________________"},
    )
    assert r.status_code == 401


def test_upload_rejects_path_traversal_filename(client: TestClient):
    aid, tok = _register(client)
    r = client.post(
        f"/agent-api/{aid}/artifacts/upload",
        params={"inspection_id": "insp-uuid-05"},
        files={"file": ("../../evil.png", MINI_PNG, "image/png")},
        headers={"Authorization": f"Bearer {tok}"},
    )
    assert r.status_code == 400


def test_persist_stores_evidence_url_and_local_agent_meta(client: TestClient, monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    aid, tok = _register(client)
    captured: list[dict] = []

    def cap(payload):
        captured.append(dict(payload))
        return payload.get("evidence_id") or payload.get("run_id")

    ins = BrowserInspectionResult(
        inspection_id="persist-ins-1",
        url="https://example.com",
        final_url="https://example.com/",
        title="T",
        status_code=200,
        screenshot_url="https://cdn.example/shot.png",
        inventory_counts={"links_count": 1},
        inspection_succeeded=True,
    )
    body = {
        "project_id": "p-artifacts",
        "job_id": "job-1",
        "watch_id": "watch-9",
        "artifact_sha256": "a" * 64,
        "inspection": ins.model_dump(mode="json"),
    }
    with patch("services.run_access.persist_run_payload", side_effect=cap):
        r = client.post(
            f"/agent-api/{aid}/browser-inspections/persist",
            json=body,
            headers={"Authorization": f"Bearer {tok}"},
        )
    assert r.status_code == 200, r.text
    out = r.json()
    assert out["persisted"] is True
    assert out["evidence_url"] == "https://cdn.example/shot.png"
    assert captured
    p0 = captured[0]
    assert p0.get("evidence_url") == "https://cdn.example/shot.png"
    meta = p0.get("meta") or {}
    assert meta.get("source") == "local_agent"
    assert meta.get("execution_mode") == "local_agent"
    assert meta.get("local_agent_id") == aid
    assert meta.get("watch_id") == "watch-9"
    assert meta.get("job_id") == "job-1"
    assert "screenshot_b64" not in p0
    assert "screenshot_b64" not in meta
    bis = meta.get("browser_inspection_summary") or {}
    assert bis.get("screenshot_url") == "https://cdn.example/shot.png"


def test_persist_strips_screenshot_b64_from_nested_inspection(client: TestClient, monkeypatch):
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    aid, tok = _register(client)
    captured: list[dict] = []

    def cap(payload):
        captured.append(dict(payload))
        return "rid"

    d = BrowserInspectionResult(
        inspection_id="persist-ins-2",
        url="https://example.com",
        final_url="https://example.com/",
        title="T",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    ).model_dump(mode="json")
    d["screenshot_b64"] = "AAAABBBB"
    with patch("services.run_access.persist_run_payload", side_effect=cap):
        r = client.post(
            f"/agent-api/{aid}/browser-inspections/persist",
            json={"inspection": d},
            headers={"Authorization": f"Bearer {tok}"},
        )
    assert r.status_code == 200, r.text
    meta = (captured[0].get("meta") or {}) if captured else {}
    assert "screenshot_b64" not in meta
    assert "screenshot_b64" not in captured[0]


def test_upload_failure_still_persists_inspection(client: TestClient, monkeypatch):
    """Simulate Cloudinary failure on upload; separate persist call succeeds."""
    monkeypatch.setenv("VANYA_PERSIST_BROWSER_INSPECTION", "1")
    aid, tok = _register(client)
    with patch("services.cloudinary_service.upload_image_bytes", side_effect=RuntimeError("cloud down")):
        up = client.post(
            f"/agent-api/{aid}/artifacts/upload",
            params={"inspection_id": "insp-uuid-06"},
            files={"file": ("ok.png", MINI_PNG, "image/png")},
            headers={"Authorization": f"Bearer {tok}"},
        )
    assert up.status_code == 503

    captured: list[dict] = []

    def cap(payload):
        captured.append(payload)
        return "insp-no-shot"

    ins = BrowserInspectionResult(
        inspection_id="insp-no-shot",
        url="https://example.com",
        final_url="https://example.com/",
        title="T",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    with patch("services.run_access.persist_run_payload", side_effect=cap):
        pr = client.post(
            f"/agent-api/{aid}/browser-inspections/persist",
            json={"inspection": ins.model_dump(mode="json")},
            headers={"Authorization": f"Bearer {tok}"},
        )
    assert pr.status_code == 200
    assert pr.json().get("persisted") is True
    assert (captured[0].get("meta") or {}).get("source") == "local_agent"


def test_run_mapper_local_agent_row_has_evidence_url():
    tr = StoredTestRun(
        run_id="r1",
        test_case_id="_browser_inspection",
        test_name="[Browser inspection] x",
        status="pass",
        evidence_url="https://cdn.example/z.png",
        evidence_id="r1",
        meta={
            "source": "local_agent",
            "execution_mode": "local_agent",
            "run_type": "browser_inspection",
        },
    )
    cr = run_from_catalog_testrun(tr)
    assert cr.source == "local_agent"
    assert (cr.evidence_url or cr.artifacts.evidence_url) == "https://cdn.example/z.png"


def test_build_lightweight_payload_local_agent_execution_mode():
    ins = BrowserInspectionResult(
        inspection_id="lid-1",
        url="https://ex.com",
        final_url="https://ex.com/",
        title="T",
        status_code=200,
        screenshot_url="https://u/x.webp",
        inventory_counts={},
        inspection_succeeded=True,
    )
    p = build_lightweight_run_payload(
        ins,
        inv={},
        extras={},
        project_id="p1",
        app_map=None,
        meta_source="local_agent",
        execution_mode="local_agent",
        local_agent_id="agent-1",
        watch_id="w1",
        job_id="j1",
        artifact_sha256="b" * 64,
    )
    m = p.get("meta") or {}
    assert m.get("source") == "local_agent"
    assert m.get("execution_mode") == "local_agent"
    assert m.get("artifact_sha256") == "b" * 64


def test_result_ref_stays_bounded_with_cloud_fields():
    from local_agent.result_packager import pack_browser_inspection_result_ref

    r = BrowserInspectionResult(
        inspection_id="i-pack",
        url="https://ex.com",
        final_url="https://ex.com/",
        title="T",
        status_code=200,
        inventory_counts={},
        inspection_succeeded=True,
    )
    ref = pack_browser_inspection_result_ref(
        r,
        raw_runner={},
        cloud_evidence_url="https://cdn.example/s.png",
        persisted_run_id="i-pack",
        persisted=True,
        persistence_warning=None,
    )
    assert len(ref) <= 512
    d = json.loads(ref)
    assert d.get("evidence_url", "").startswith("https://")
