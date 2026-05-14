# tests/test_browser_watch_env.py
from __future__ import annotations

from services.browser_watch_env import (
    browser_watch_max_per_tick,
    browser_watch_scheduler_enabled,
    browser_watch_tick_seconds,
)


def test_new_env_overrides_legacy_tick(monkeypatch):
    monkeypatch.setenv("VANYA_BROWSER_WATCH_TICK_S", "45")
    monkeypatch.setenv("VANYA_BIO_WATCH_TICK_S", "200")
    assert browser_watch_tick_seconds() == 45


def test_legacy_tick_when_new_unset(monkeypatch):
    monkeypatch.delenv("VANYA_BROWSER_WATCH_TICK_S", raising=False)
    monkeypatch.setenv("VANYA_BIO_WATCH_TICK_S", "99")
    assert browser_watch_tick_seconds() == 99


def test_scheduler_disabled_legacy(monkeypatch):
    monkeypatch.delenv("VANYA_BROWSER_WATCH_SCHEDULER", raising=False)
    monkeypatch.setenv("VANYA_BIO_WATCH_SCHEDULER", "0")
    assert browser_watch_scheduler_enabled() is False


def test_new_scheduler_overrides_legacy_on(monkeypatch):
    monkeypatch.setenv("VANYA_BROWSER_WATCH_SCHEDULER", "1")
    monkeypatch.setenv("VANYA_BIO_WATCH_SCHEDULER", "0")
    assert browser_watch_scheduler_enabled() is True


def test_max_per_tick_legacy(monkeypatch):
    monkeypatch.delenv("VANYA_BROWSER_WATCH_MAX_PER_TICK", raising=False)
    monkeypatch.setenv("VANYA_BIO_WATCH_MAX_PER_TICK", "12")
    assert browser_watch_max_per_tick() == 12
