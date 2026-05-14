# tests/test_browser_watch_alert_dedupe.py
from __future__ import annotations

import time
from unittest.mock import patch

from services.browser_watch_alert_dedupe import (
    dedupe_window_seconds,
    reset_watch_alert_dedupe_for_tests,
    watch_alert_try_reserve_slot,
)


def setup_function() -> None:
    reset_watch_alert_dedupe_for_tests()


def test_dedupe_blocks_duplicate_within_window():
    kwargs = dict(
        watch_id="w1",
        change_level="high",
        regression_signals=["a"],
        summary="same",
        visual_change_level="none",
        visual_similarity_score=None,
    )
    ok1, _ = watch_alert_try_reserve_slot(**kwargs)
    ok2, reason = watch_alert_try_reserve_slot(**kwargs)
    assert ok1 is True
    assert ok2 is False
    assert "dedupe" in reason


def test_dedupe_expires_after_window(monkeypatch):
    monkeypatch.setenv("VANYA_BROWSER_ALERT_DEDUPE_MINUTES", "60")
    with patch("services.browser_watch_alert_dedupe.dedupe_window_seconds", return_value=0.05):
        kwargs = dict(
            watch_id="w2",
            change_level="medium",
            regression_signals=[],
            summary="x",
            visual_change_level="low",
            visual_similarity_score=0.9,
        )
        assert watch_alert_try_reserve_slot(**kwargs)[0] is True
        assert watch_alert_try_reserve_slot(**kwargs)[0] is False
        time.sleep(0.07)
        assert watch_alert_try_reserve_slot(**kwargs)[0] is True


def test_dedupe_fingerprint_includes_visual(monkeypatch):
    with patch("services.browser_watch_alert_dedupe.dedupe_window_seconds", return_value=3600.0):
        base = dict(
            watch_id="w3",
            change_level="high",
            regression_signals=["x"],
            summary="s",
            visual_change_level="none",
            visual_similarity_score=None,
        )
        assert watch_alert_try_reserve_slot(**base)[0] is True
        base2 = {**base, "visual_change_level": "high"}
        ok, _ = watch_alert_try_reserve_slot(**base2)
        assert ok is True
