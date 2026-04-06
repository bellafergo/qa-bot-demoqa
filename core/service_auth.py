# core/service_auth.py
"""
Phase 2 — machine / integration authentication (stubs).

Today: X-Service-Token validated in core.auth_middleware against VANYA_SERVICE_TOKEN.

Future: per-agent API keys, hashed key storage, scopes, rotation, mTLS hints.
User JWT validation stays in core.vanya_auth.verify_supabase_user_jwt — do not conflate.
"""

from __future__ import annotations

# Intentionally minimal; extend here when implementing API keys / agent auth.
