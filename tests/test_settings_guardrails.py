# tests/test_settings_guardrails.py
"""
Tests for core/settings.py Settings.validate() fail-fast behavior.
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


class TestSettingsValidate:
    def test_validate_raises_without_openai_key(self):
        from core.settings import Settings
        s = Settings.__new__(Settings)
        object.__setattr__(s, "OPENAI_API_KEY", "")
        with pytest.raises(EnvironmentError, match="OPENAI_API_KEY"):
            s.validate()

    def test_validate_passes_with_openai_key(self):
        from core.settings import Settings
        s = Settings.__new__(Settings)
        object.__setattr__(s, "OPENAI_API_KEY", "sk-test-key")
        s.validate()  # must not raise

    def test_validate_lists_all_errors(self):
        from core.settings import Settings
        s = Settings.__new__(Settings)
        object.__setattr__(s, "OPENAI_API_KEY", "")
        try:
            s.validate()
            pytest.fail("Expected EnvironmentError")
        except EnvironmentError as e:
            assert "OPENAI_API_KEY" in str(e)

    def test_singleton_validate_method_exists(self):
        from core.settings import settings
        assert callable(settings.validate)
