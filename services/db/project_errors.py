# services/db/project_errors.py
"""Shared errors for project persistence backends."""


class ProjectDuplicateIdError(Exception):
    """Raised when inserting a project whose id already exists (Postgres/Supabase or SQLite)."""


class ProjectSettingsPersistError(Exception):
    """Raised when project settings could not be written to the active backend (e.g. stale PostgREST schema cache)."""
