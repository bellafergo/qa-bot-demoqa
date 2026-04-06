# tests/test_rate_limit_middleware.py
from __future__ import annotations

import os
from unittest.mock import patch

import pytest
from starlette.testclient import TestClient


@pytest.fixture(autouse=True)
def _clear_rate_limit_buckets():
    import core.rate_limit_middleware as m

    with m._limiter._lock:
        m._limiter._windows.clear()
    yield
    with m._limiter._lock:
        m._limiter._windows.clear()


@patch.dict(
    os.environ,
    {"RATE_LIMIT_ENABLED": "1", "RATE_LIMIT_CHAT": "2/3600"},
    clear=False,
)
def test_health_unlimited_when_rate_limit_on():
    from app import app

    c = TestClient(app)
    for _ in range(5):
        assert c.get("/health").status_code == 200


@patch.dict(
    os.environ,
    {"RATE_LIMIT_ENABLED": "1", "RATE_LIMIT_CHAT": "2/3600"},
    clear=False,
)
def test_chat_run_returns_429_after_limit():
    from app import app

    c = TestClient(app)
    body = {"prompt": "ping"}
    r1 = c.post("/chat_run", json=body)
    r2 = c.post("/chat_run", json=body)
    assert r1.status_code != 429
    assert r2.status_code != 429
    r3 = c.post("/chat_run", json=body)
    assert r3.status_code == 429
    assert r3.json().get("detail") == "Rate limit exceeded"
