# services/db/project_errors.py
"""Shared errors for project persistence backends."""


class ProjectDuplicateIdError(Exception):
    """Raised when inserting a project whose id already exists (Postgres/Supabase or SQLite)."""
