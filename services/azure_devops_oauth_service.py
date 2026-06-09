# services/azure_devops_oauth_service.py
"""Microsoft OAuth 2.0 authorization-code flow for Azure DevOps."""
from __future__ import annotations

import logging
from typing import Any, Dict
from urllib.parse import urlencode

import httpx

from core.settings import settings

logger = logging.getLogger("vanya.azure_devops_oauth")

AZURE_DEVOPS_RESOURCE_SCOPE = "vso.profile vso.project vso.code offline_access"


def is_azure_devops_oauth_configured() -> bool:
    return bool(
        settings.AZURE_CLIENT_ID
        and settings.AZURE_CLIENT_SECRET
        and settings.AZURE_REDIRECT_URI
        and settings.AZURE_AUTHORITY_TENANT
    )


def _oauth_authority_segment() -> str:
    return settings.AZURE_AUTHORITY_TENANT.strip()


def _token_url() -> str:
    authority = _oauth_authority_segment()
    return f"https://login.microsoftonline.com/{authority}/oauth2/v2.0/token"


def _authorize_url_base() -> str:
    authority = _oauth_authority_segment()
    return f"https://login.microsoftonline.com/{authority}/oauth2/v2.0/authorize"


def build_authorize_url(*, state: str) -> str:
    if not is_azure_devops_oauth_configured():
        raise ValueError("Azure DevOps OAuth is not configured on the server")
    params = {
        "client_id": settings.AZURE_CLIENT_ID,
        "response_type": "code",
        "redirect_uri": settings.AZURE_REDIRECT_URI,
        "response_mode": "query",
        "scope": AZURE_DEVOPS_RESOURCE_SCOPE,
        "state": state,
    }
    return f"{_authorize_url_base()}?{urlencode(params)}"


def exchange_code_for_tokens(code: str) -> Dict[str, Any]:
    if not is_azure_devops_oauth_configured():
        raise ValueError("Azure DevOps OAuth is not configured on the server")
    payload = {
        "client_id": settings.AZURE_CLIENT_ID,
        "client_secret": settings.AZURE_CLIENT_SECRET,
        "grant_type": "authorization_code",
        "code": (code or "").strip(),
        "redirect_uri": settings.AZURE_REDIRECT_URI,
        "scope": AZURE_DEVOPS_RESOURCE_SCOPE,
    }
    try:
        with httpx.Client(timeout=30) as client:
            res = client.post(_token_url(), data=payload)
    except httpx.RequestError as exc:
        logger.warning("azure oauth token exchange network error: %s", type(exc).__name__)
        raise ValueError("Could not reach Microsoft token endpoint") from exc

    if res.status_code >= 400:
        logger.warning("azure oauth token exchange failed status=%s", res.status_code)
        raise ValueError("Microsoft token exchange failed — check redirect URI and app registration")

    data = res.json()
    if not isinstance(data, dict) or not str(data.get("access_token") or "").strip():
        raise ValueError("Microsoft token response did not include access_token")
    return data


def refresh_access_token(refresh_token: str) -> Dict[str, Any]:
    if not is_azure_devops_oauth_configured():
        raise ValueError("Azure DevOps OAuth is not configured on the server")
    rt = (refresh_token or "").strip()
    if not rt:
        raise ValueError("refresh_token is required")

    payload = {
        "client_id": settings.AZURE_CLIENT_ID,
        "client_secret": settings.AZURE_CLIENT_SECRET,
        "grant_type": "refresh_token",
        "refresh_token": rt,
        "redirect_uri": settings.AZURE_REDIRECT_URI,
        "scope": AZURE_DEVOPS_RESOURCE_SCOPE,
    }
    try:
        with httpx.Client(timeout=30) as client:
            res = client.post(_token_url(), data=payload)
    except httpx.RequestError as exc:
        logger.warning("azure oauth refresh network error: %s", type(exc).__name__)
        raise ValueError("Could not reach Microsoft token endpoint") from exc

    if res.status_code >= 400:
        logger.warning("azure oauth refresh failed status=%s", res.status_code)
        raise ValueError("Azure DevOps token refresh failed — reconnect OAuth")

    data = res.json()
    if not isinstance(data, dict) or not str(data.get("access_token") or "").strip():
        raise ValueError("Microsoft refresh response did not include access_token")
    return data
