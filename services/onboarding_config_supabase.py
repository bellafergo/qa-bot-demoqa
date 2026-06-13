# services/onboarding_config_supabase.py
"""Shared helpers for Supabase mirrors of onboarding configuration entities."""
from __future__ import annotations


def supabase_onboarding_config_enabled() -> bool:
    """True when Supabase REST client can serve durable onboarding config reads."""
    from services.qa_runs_read import supabase_qa_runs_enabled

    return supabase_qa_runs_enabled()
