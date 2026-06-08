# tests/test_project_repository_supabase_settings.py
"""Supabase project settings persistence — schema-cache fallback and errors."""
from __future__ import annotations

from unittest.mock import MagicMock, patch

import pytest

from services.db.project_errors import ProjectSettingsPersistError
from services.db.project_repository_supabase import ProjectRepositorySupabase


def _schema_cache_exc():
    return Exception("Could not find the 'settings_json' column of 'projects' in the schema cache")


def test_persist_settings_raises_when_both_columns_unavailable():
    repo = ProjectRepositorySupabase()
    sb = MagicMock()
    table = sb.table.return_value
    update = table.update.return_value
    update.eq.return_value.execute.side_effect = _schema_cache_exc()

    with pytest.raises(ProjectSettingsPersistError) as exc:
        repo._persist_settings(sb, "demo", {"github": {"installation_id": "1"}}, "2026-01-01T00:00:00+00:00")

    assert "settings_json" in str(exc.value).lower()
    assert update.eq.return_value.execute.call_count >= 2


def test_persist_settings_uses_settings_fallback_when_settings_json_blocked():
    repo = ProjectRepositorySupabase()
    sb = MagicMock()
    table = sb.table.return_value
    update = table.update.return_value
    eq = update.eq.return_value

    eq.execute.side_effect = [_schema_cache_exc(), MagicMock()]

    repo._persist_settings(sb, "demo", {"github": {"installation_id": "99"}}, "2026-01-01T00:00:00+00:00")

    assert eq.execute.call_count == 2
    sb.table.assert_called_with("projects")


def test_update_project_does_not_silently_drop_settings_on_schema_cache():
    repo = ProjectRepositorySupabase()
    sb = MagicMock()
    table = sb.table.return_value
    update = table.update.return_value
    update.eq.return_value.execute.side_effect = _schema_cache_exc()

    with patch.object(repo, "_sb", return_value=sb):
        with pytest.raises(ProjectSettingsPersistError):
            repo.update_project("demo", {"settings": {"github": {"installation_id": "123"}}})
