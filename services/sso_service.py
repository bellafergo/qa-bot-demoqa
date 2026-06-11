# services/sso_service.py
"""
Enterprise SEC-01D — SSO activation (configuration, validation, login URLs).

Preserves local authentication and Supabase JWT flows. No token exchange,
user provisioning, or SAML implementation in this sprint.
"""
from __future__ import annotations

import json
import logging
import re
import threading
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from urllib.parse import urlencode

from models.auth_models import (
    SSOProviderConfig,
    SSOProviderType,
    SSOProvidersResponse,
    SSOValidationResult,
    SSOLoginUrl,
)

logger = logging.getLogger("vanya.sso")

_LOCK = threading.RLock()
_CONFIG_PATH = Path("evidence/sso_config.json")
_ACTIVATED_PROVIDERS: Tuple[SSOProviderType, ...] = ("MICROSOFT", "GOOGLE", "OKTA")
_PROVIDER_LABELS = {
    "MICROSOFT": "Microsoft Entra ID",
    "GOOGLE": "Google Workspace",
    "OKTA": "Okta",
}
_CONFIGS: Dict[str, Dict[str, object]] = {}

_UUID_RE = re.compile(
    r"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
    re.IGNORECASE,
)
_MICROSOFT_TENANT_ALIASES = frozenset({"common", "organizations", "consumers"})


def _load_configs_from_disk() -> Dict[str, Dict[str, object]]:
    try:
        if _CONFIG_PATH.exists():
            raw = json.loads(_CONFIG_PATH.read_text())
            if isinstance(raw, dict):
                return {str(k): v for k, v in raw.items() if isinstance(v, dict)}
    except Exception as exc:
        logger.debug("sso_service: could not load config — %s", exc)
    return {}


def _save_configs_to_disk(configs: Dict[str, Dict[str, object]]) -> None:
    try:
        _CONFIG_PATH.parent.mkdir(parents=True, exist_ok=True)
        _CONFIG_PATH.write_text(json.dumps(configs, indent=2, default=str))
    except Exception as exc:
        logger.warning("sso_service: could not persist configs — %s", exc)


def _ensure_loaded() -> None:
    with _LOCK:
        if not _CONFIGS:
            _CONFIGS.update(_load_configs_from_disk())


def _redirect_uri() -> str:
    from core.settings import settings

    return (settings.SSO_REDIRECT_URI or "http://localhost:5173/auth/callback").strip().rstrip("/")


def _env_value(provider: str, field: str) -> str:
    from core.settings import settings

    mapping = {
        ("MICROSOFT", "client_id"): settings.SSO_MICROSOFT_CLIENT_ID,
        ("MICROSOFT", "tenant_id"): settings.SSO_MICROSOFT_TENANT_ID,
        ("GOOGLE", "client_id"): settings.SSO_GOOGLE_CLIENT_ID,
        ("OKTA", "client_id"): settings.SSO_OKTA_CLIENT_ID,
        ("OKTA", "issuer"): settings.SSO_OKTA_ISSUER,
    }
    return (mapping.get((provider, field)) or "").strip()


def _raw_config(provider: SSOProviderType) -> Dict[str, object]:
    _ensure_loaded()
    with _LOCK:
        stored = dict(_CONFIGS.get(provider, {}))
    for field in ("client_id", "tenant_id", "issuer"):
        if not str(stored.get(field) or "").strip():
            env_val = _env_value(provider, field)
            if env_val:
                stored[field] = env_val
    return stored


def _is_configured(provider: SSOProviderType, raw: Dict[str, object]) -> bool:
    client_id = str(raw.get("client_id") or "").strip()
    if not client_id:
        return False
    if provider == "MICROSOFT":
        return bool(str(raw.get("tenant_id") or "").strip())
    if provider == "OKTA":
        return bool(str(raw.get("issuer") or "").strip())
    return True


def _to_provider_config(provider: SSOProviderType) -> SSOProviderConfig:
    raw = _raw_config(provider)
    configured = _is_configured(provider, raw)
    validated = bool(raw.get("validated")) and configured
    return SSOProviderConfig(
        provider=provider,
        enabled=bool(raw.get("enabled")) and configured,
        client_id=str(raw.get("client_id") or "").strip() or None,
        tenant_id=str(raw.get("tenant_id") or "").strip() or None,
        issuer=str(raw.get("issuer") or "").strip() or None,
        configured=configured,
        validated=validated,
    )


def list_sso_provider_configs() -> SSOProvidersResponse:
    """Return enterprise SSO provider configuration snapshots."""
    providers = [_to_provider_config(provider) for provider in _ACTIVATED_PROVIDERS]
    return SSOProvidersResponse(providers=providers, total=len(providers))


def available_sso_providers() -> List[SSOProviderType]:
    return list(_ACTIVATED_PROVIDERS)


def configured_sso_providers() -> List[SSOProviderType]:
    return [p.provider for p in list_sso_provider_configs().providers if p.configured]


def validated_sso_providers() -> List[SSOProviderType]:
    return [
        p.provider
        for p in list_sso_provider_configs().providers
        if p.validated and p.enabled
    ]


def is_sso_ready() -> bool:
    return len(validated_sso_providers()) > 0


def is_provider_enabled(provider_type: str) -> bool:
    if provider_type not in _ACTIVATED_PROVIDERS:
        return False
    config = _to_provider_config(provider_type)  # type: ignore[arg-type]
    return config.enabled and config.validated


def validate_provider_config(
    *,
    provider: SSOProviderType,
    client_id: Optional[str] = None,
    tenant_id: Optional[str] = None,
    issuer: Optional[str] = None,
    enabled: bool = True,
) -> SSOValidationResult:
    """Validate provider configuration without performing login."""
    cid = (client_id or "").strip()
    tid = (tenant_id or "").strip()
    iss = (issuer or "").strip().rstrip("/")

    if provider == "MICROSOFT":
        if not cid:
            return SSOValidationResult(provider=provider, valid=False, message="Microsoft client_id is required.")
        if not tid:
            return SSOValidationResult(
                provider=provider,
                valid=False,
                message="Microsoft tenant_id is required.",
            )
        if tid.lower() not in _MICROSOFT_TENANT_ALIASES and not _UUID_RE.match(tid):
            return SSOValidationResult(
                provider=provider,
                valid=False,
                message="Microsoft tenant_id must be a GUID or one of common, organizations, consumers.",
            )
        message = "Microsoft Entra ID configuration is valid."
    elif provider == "GOOGLE":
        if not cid:
            return SSOValidationResult(provider=provider, valid=False, message="Google client_id is required.")
        message = "Google Workspace configuration is valid."
    elif provider == "OKTA":
        if not iss:
            return SSOValidationResult(provider=provider, valid=False, message="Okta issuer URL is required.")
        if not iss.startswith("https://"):
            return SSOValidationResult(
                provider=provider,
                valid=False,
                message="Okta issuer must be an https URL.",
            )
        if not cid:
            return SSOValidationResult(provider=provider, valid=False, message="Okta client_id is required.")
        message = "Okta configuration is valid."
    else:
        return SSOValidationResult(provider=provider, valid=False, message="Unsupported SSO provider.")

    _persist_provider_config(
        provider=provider,
        client_id=cid,
        tenant_id=tid or None,
        issuer=iss or None,
        enabled=enabled,
        validated=True,
    )
    return SSOValidationResult(provider=provider, valid=True, message=message)


def _persist_provider_config(
    *,
    provider: SSOProviderType,
    client_id: str,
    tenant_id: Optional[str],
    issuer: Optional[str],
    enabled: bool,
    validated: bool,
) -> None:
    _ensure_loaded()
    with _LOCK:
        _CONFIGS[provider] = {
            "provider": provider,
            "enabled": enabled,
            "client_id": client_id,
            "tenant_id": tenant_id,
            "issuer": issuer,
            "validated": validated,
        }
        _save_configs_to_disk(_CONFIGS)


def build_login_url(*, provider: SSOProviderType) -> SSOLoginUrl:
    """Generate an OAuth authorize URL for a validated provider."""
    config = _to_provider_config(provider)
    if not config.configured:
        raise ValueError(f"{_PROVIDER_LABELS[provider]} is not configured.")
    if not config.validated:
        raise ValueError(f"{_PROVIDER_LABELS[provider]} configuration has not been validated.")

    redirect_uri = _redirect_uri()
    state = f"sso:{provider.lower()}"

    if provider == "MICROSOFT":
        tenant = config.tenant_id or "common"
        base = f"https://login.microsoftonline.com/{tenant}/oauth2/v2.0/authorize"
        params = {
            "client_id": config.client_id,
            "response_type": "code",
            "redirect_uri": redirect_uri,
            "response_mode": "query",
            "scope": "openid profile email",
            "state": state,
        }
    elif provider == "GOOGLE":
        base = "https://accounts.google.com/o/oauth2/v2/auth"
        params = {
            "client_id": config.client_id,
            "response_type": "code",
            "redirect_uri": redirect_uri,
            "scope": "openid email profile",
            "access_type": "offline",
            "state": state,
        }
    else:
        base = f"{(config.issuer or '').rstrip('/')}/v1/authorize"
        params = {
            "client_id": config.client_id,
            "response_type": "code",
            "redirect_uri": redirect_uri,
            "scope": "openid profile email",
            "state": state,
        }

    return SSOLoginUrl(provider=provider, login_url=f"{base}?{urlencode(params)}")
