"""Validation helpers for catalog AI edit (no OpenAI calls)."""
import pytest

from services.catalog_ai_edit import validate_ai_edit_payload


def test_validate_ai_edit_payload_ok():
    data = {
        "steps": [{"action": "goto", "url": "/login"}],
        "assertions": [{"type": "url_contains", "value": "/login"}],
        "change_summary": ["Añadido goto a /login"],
    }
    steps, assertions, summary = validate_ai_edit_payload(data)
    assert len(steps) == 1
    assert steps[0]["action"] == "goto"
    assert len(assertions) == 1
    assert assertions[0]["type"] == "url_contains"
    assert summary == ["Añadido goto a /login"]


def test_validate_ai_edit_missing_action():
    with pytest.raises(ValueError, match="action"):
        validate_ai_edit_payload(
            {
                "steps": [{}],
                "assertions": [],
            }
        )


def test_validate_change_summary_string_coerced():
    steps, assertions, summary = validate_ai_edit_payload(
        {
            "steps": [{"action": "wait_ms", "ms": 100}],
            "assertions": [{"type": "text_visible", "value": "ok"}],
            "change_summary": "solo un string",
        }
    )
    assert summary == ["solo un string"]
