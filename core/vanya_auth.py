# core/vanya_auth.py
"""
User authentication (Supabase JWT via JWKS) + hooks for future machine auth.

Phase 1: Bearer JWT validated with ES256 against Supabase Auth JWKS
  GET {SUPABASE_URL}/auth/v1/.well-known/jwks.json
  Key selection uses header "kid" (handled by PyJWT PyJWKClient).

Phase 2: Optional X-Service-Token matching VANYA_SERVICE_TOKEN (machine-to-machine).

Corporate access: optional ``ALLOWED_EMAIL_DOMAINS`` (comma-separated, e.g.
``fahorro.com.mx,zuperio.com.mx``). Empty = no domain restriction.
User JWTs are checked in middleware; service token bypasses domain rules.
"""
from __future__ import annotations

import hmac
import logging
import os
import threading
from typing import Any, Dict, FrozenSet, Optional, Tuple

logger = logging.getLogger("vanya.auth")

# ── Environment ───────────────────────────────────────────────────────────────

_AUTH_ENABLED_RAW = (os.getenv("VANYA_AUTH_ENABLED") or "").strip().lower()
_SUPABASE_URL = (os.getenv("SUPABASE_URL") or "").strip()
_SERVICE_TOKEN = (os.getenv("VANYA_SERVICE_TOKEN") or "").strip()
# Comma-separated audiences; default "authenticated". Empty env = skip aud verification.
_JWT_AUDIENCES_RAW = (os.getenv("SUPABASE_JWT_AUDIENCES") or "authenticated").strip()

# JWKS client singleton (thread-safe lazy init)
_jwks_lock = threading.Lock()
_jwks_client: Any = None


def _jwks_url() -> str:
    base = _SUPABASE_URL.rstrip("/")
    if not base:
        return ""
    return f"{base}/auth/v1/.well-known/jwks.json"


def _get_jwks_client():
    """
    Lazily construct PyJWKClient. PyJWT caches the JWK set in memory
    (cache_jwk_set + lifespan) and matches keys by token header `kid`.
    """
    global _jwks_client
    url = _jwks_url()
    if not url:
        raise RuntimeError("SUPABASE_URL is not configured")

    with _jwks_lock:
        if _jwks_client is None:
            from jwt import PyJWKClient

            # lifespan: refresh JWKS periodically if keys rotate
            _jwks_client = PyJWKClient(
                url,
                cache_jwk_set=True,
                max_cached_keys=32,
                lifespan=3600,
            )
            logger.info("vanya.auth: JWKS client initialized (%s)", url)
        return _jwks_client


def log_auth_startup_warnings() -> None:
    """Call once at process startup to surface misconfiguration."""
    if _AUTH_ENABLED_RAW in ("1", "true", "yes", "on") and not _SUPABASE_URL:
        logger.error(
            "VANYA_AUTH_ENABLED is on but SUPABASE_URL is missing — "
            "JWKS URL cannot be built; JWT enforcement will stay OFF."
        )


def auth_enforcement_enabled() -> bool:
    """
    Mirrors legacy HS256 gating: explicit OFF always wins; explicit ON requires
    SUPABASE_URL (JWKS). If VANYA_AUTH_ENABLED is unset, auth stays OFF so a DB-only
    SUPABASE_URL does not suddenly enforce JWTs (legacy used unset + no secret → off).
    """
    raw = os.getenv("VANYA_AUTH_ENABLED")
    enabled = str(raw or "").strip().lower()
    supabase_url = os.getenv("SUPABASE_URL")
    if enabled in ("0", "false", "no", "off"):
        return False
    if enabled in ("1", "true", "yes", "on"):
        return bool(str(supabase_url or "").strip())
    return False


def allowed_email_domains() -> FrozenSet[str]:
    """
    Domains from ALLOWED_EMAIL_DOMAINS (comma-separated).
    Empty / unset env → empty frozenset → policy disabled.
    Entries are trimmed and lowercased for case-insensitive matching.

    Read from the environment on each call so tests and process managers can
    change policy without importing the module again.
    """
    raw = (os.getenv("ALLOWED_EMAIL_DOMAINS") or "").strip()
    if not raw:
        return frozenset()
    parts = [p.strip().lower() for p in raw.split(",")]
    return frozenset(p for p in parts if p)


def resolve_allowed_email_domains(
    *,
    claims: Optional[Dict[str, Any]] = None,
    workspace_id: Optional[str] = None,
) -> FrozenSet[str]:
    """
    Effective domain allowlist for the current request.

    Today: global policy from ALLOWED_EMAIL_DOMAINS only.
    Future (multi-tenant): may merge per-workspace or per-tenant lists from DB
    using ``workspace_id`` / ``claims`` without changing call sites that use
    :func:`check_email_domain_policy`.
    """
    _ = claims
    _ = workspace_id
    return allowed_email_domains()


def extract_user_email_from_claims(claims: Dict[str, Any]) -> Optional[str]:
    """
    Best-effort email from a Supabase access token (Google OAuth, etc.).
    Tries top-level ``email`` then ``user_metadata.email``.
    """
    if not isinstance(claims, dict):
        return None
    for key in ("email",):
        val = claims.get(key)
        if isinstance(val, str) and val.strip():
            return val.strip()
    um = claims.get("user_metadata")
    if isinstance(um, dict):
        val = um.get("email")
        if isinstance(val, str) and val.strip():
            return val.strip()
    return None


def _domain_part_from_email(email: str) -> Optional[str]:
    """Return lowercased domain or None if missing / malformed."""
    e = email.strip()
    if "@" not in e:
        return None
    local, domain = e.rsplit("@", 1)
    domain = domain.strip().lower()
    if not domain or not local.strip():
        return None
    return domain


def expected_jwt_issuer() -> str:
    base = _SUPABASE_URL.rstrip("/")
    if not base:
        return ""
    return f"{base}/auth/v1"


def _decode_audiences() -> Optional[Any]:
    """Return audience arg for jwt.decode, or None to skip aud check."""
    if not _JWT_AUDIENCES_RAW or _JWT_AUDIENCES_RAW.lower() in ("__skip__", "none", "off"):
        return None
    parts = [p.strip() for p in _JWT_AUDIENCES_RAW.split(",") if p.strip()]
    if not parts:
        return None
    if len(parts) == 1:
        return parts[0]
    return parts


# Paths that never require a user JWT (machine webhooks, health, static assets, docs in dev).
PUBLIC_PATH_PREFIXES: Tuple[str, ...] = (
    "/health",
    "/meta",
    "/webhooks",
    "/evidence",
    "/reports",
    "/favicon.ico",
)

PUBLIC_PATHS_EXACT: FrozenSet[str] = frozenset()


def is_public_path(path: str) -> bool:
    p = path or ""
    if p in PUBLIC_PATHS_EXACT:
        return True
    for prefix in PUBLIC_PATH_PREFIXES:
        if p == prefix or p.startswith(prefix + "/"):
            return True
    return False


def docs_allowed_in_this_deployment() -> bool:
    """OpenAPI UI only outside strict production (same heuristic as app error handler)."""
    if (os.getenv("DEBUG_ERRORS") or "").strip().lower() in ("1", "true", "yes"):
        return True
    env = (os.getenv("ENV") or os.getenv("ENVIRONMENT") or "").lower().strip()
    if env in ("prod", "production"):
        return False
    if (os.getenv("RENDER") or "").strip().lower() in ("1", "true", "yes"):
        return False
    return True


def machine_request_authorized(request_headers: Any) -> bool:
    """
    Phase 2 — service token (not user OAuth).
    Constant-time compare when token is configured.
    """
    if not _SERVICE_TOKEN:
        return False
    try:
        st = request_headers.get("x-service-token") or request_headers.get("X-Service-Token")
    except Exception:
        st = None
    if not st or not isinstance(st, str):
        return False
    return hmac.compare_digest(st.strip(), _SERVICE_TOKEN)


def verify_supabase_user_jwt(token: str) -> Dict[str, Any]:
    """
    Validate Supabase access token: ES256 signature via JWKS, exp, iss, aud (optional), role.

    Raises jwt.PyJWTError (or subclasses) / ValueError on failure.
    """
    import jwt

    issuer = expected_jwt_issuer()
    if not issuer:
        raise RuntimeError("SUPABASE_URL is not configured")

    jwks_client = _get_jwks_client()
    signing_key = jwks_client.get_signing_key_from_jwt(token)

    decode_kwargs: Dict[str, Any] = {
        "algorithms": ["ES256"],
        "issuer": issuer,
        "options": {"require": ["exp", "sub"], "verify_signature": True},
        "leeway": 60,
    }

    aud = _decode_audiences()
    if aud is not None:
        decode_kwargs["audience"] = aud

    payload = jwt.decode(
        token,
        signing_key.key,
        **decode_kwargs,
    )

    role = payload.get("role")
    if role != "authenticated":
        raise ValueError(f"Unexpected role {role!r}; expected 'authenticated'")

    return payload


def check_email_domain_policy(
    email: Optional[str],
    domains: FrozenSet[str],
) -> Tuple[bool, str]:
    """
    Pure domain gate. If ``domains`` is empty, allow (policy off).

    Matching is **case-insensitive** on the domain (allowlist and email domain
    are normalized to lowercase). Returns (True, "") or (False, detail_for_403).
    """
    if not domains:
        return True, ""

    if email is None or not str(email).strip():
        return (
            False,
            "Your sign-in token did not include an email address. "
            "Corporate access requires an email; try signing in again or contact support.",
        )

    domain = _domain_part_from_email(str(email))
    if domain is None:
        return (
            False,
            "Your sign-in token did not include a valid email address. "
            "Corporate access requires a valid corporate email.",
        )

    if domain in domains:
        return True, ""

    return (
        False,
        f"This email domain is not authorized ({domain}). "
        "Use your corporate Google account or ask an administrator to add your domain.",
    )


def email_domain_allowed(
    email: Optional[str],
    *,
    claims: Optional[Dict[str, Any]] = None,
    workspace_id: Optional[str] = None,
) -> Tuple[bool, str]:
    """
    If ALLOWED_EMAIL_DOMAINS is empty → allow all.
    Otherwise require email's domain to be in the effective allowlist.

    ``claims`` / ``workspace_id`` are reserved for future per-tenant policy;
    callers should pass them when available so resolution stays centralized.
    """
    domains = resolve_allowed_email_domains(claims=claims, workspace_id=workspace_id)
    return check_email_domain_policy(email, domains)


def apply_user_to_state(state: Any, claims: Dict[str, Any]) -> None:
    email = extract_user_email_from_claims(claims)
    domain = _domain_part_from_email(email) if email else None
    domain = domain or ""

    state.user_id = str(claims.get("sub") or "")
    state.email = email
    state.email_domain = domain
    state.auth_claims = claims
    state.auth_kind = "user"


def apply_service_to_state(state: Any) -> None:
    state.user_id = "service"
    state.email = None
    state.email_domain = ""
    state.auth_claims = {}
    state.auth_kind = "service"
