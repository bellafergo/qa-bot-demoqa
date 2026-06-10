# services/integration_service.py
"""
Integration Service — manages connector lifecycle, config, and health checks.

Persistence pattern: same as memory_store.py —
  - in-memory dict with RLock for thread safety (primary)
  - JSON file under evidence/integrations_config.json (cross-restart survival)
  - Supabase optional future layer (not wired here)

Secrets are NEVER written to disk or returned in API responses.
ConnectorConfigUpdate.token / .api_key set presence flags on disk-backed config and may
be retained in-memory (_SECRETS) for runtime connector use until process restart.
Env-var fallbacks (e.g. JIRA_API_TOKEN, SLACK_BOT_TOKEN) are supported by individual connectors.
"""
from __future__ import annotations

import json
import logging
import threading
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional

from connectors.registry import registry
from models.connector import (
    ConnectorConfig,
    ConnectorConfigUpdate,
    ConnectorStatus,
    ConnectorSummary,
)

logger = logging.getLogger("vanya.integration_service")

_LOCK = threading.RLock()
_CONFIGS: Dict[str, ConnectorConfig] = {}  # connector_id → ConnectorConfig
_LAST_HEALTH: Dict[str, ConnectorStatus] = {}  # connector_id → last health result
_SECRETS: Dict[str, Dict[str, str]] = {}  # connector_id → {token|api_key} (in-memory only)

_CONFIG_PATH = Path("evidence/integrations_config.json")


# ── Persistence helpers ────────────────────────────────────────────────────────

def _load_configs_from_disk() -> Dict[str, ConnectorConfig]:
    try:
        if _CONFIG_PATH.exists():
            raw = json.loads(_CONFIG_PATH.read_text())
            loaded: Dict[str, ConnectorConfig] = {}
            for cid, data in raw.items():
                try:
                    loaded[cid] = ConnectorConfig(**data)
                except Exception:
                    pass
            return loaded
    except Exception:
        pass
    return {}


def _save_configs_to_disk(configs: Dict[str, ConnectorConfig]) -> None:
    try:
        _CONFIG_PATH.parent.mkdir(parents=True, exist_ok=True)
        payload = {cid: cfg.model_dump() for cid, cfg in configs.items()}
        _CONFIG_PATH.write_text(json.dumps(payload, indent=2, default=str))
    except Exception as exc:
        logger.warning("integration_service: could not persist configs — %s", exc)


def _default_config(connector_id: str) -> ConnectorConfig:
    return ConnectorConfig(connector_id=connector_id)


def _get_config_locked(connector_id: str) -> ConnectorConfig:
    """Return stored config or a fresh default. Must be called under _LOCK."""
    if connector_id not in _CONFIGS:
        _CONFIGS[connector_id] = _default_config(connector_id)
    return _CONFIGS[connector_id]


# ── Service class ──────────────────────────────────────────────────────────────

class IntegrationService:
    """
    Manages all connector configs and health state.

    Follows the same singleton + method pattern as TestCatalogService.
    """

    def __init__(self) -> None:
        # Load persisted configs at startup (best-effort)
        with _LOCK:
            loaded = _load_configs_from_disk()
            _CONFIGS.update(loaded)
            logger.debug(
                "integration_service: loaded %d connector configs from disk", len(loaded)
            )

    # ── Listing ───────────────────────────────────────────────────────────────

    def list_connectors(self) -> List[ConnectorSummary]:
        """Return a summary for every registered connector."""
        summaries: List[ConnectorSummary] = []
        with _LOCK:
            for connector in registry.all():
                cfg = _get_config_locked(connector.connector_id)
                # Use cached health if available, else don't auto-run health checks on list
                last = _LAST_HEALTH.get(connector.connector_id)
                summary = connector.to_summary(cfg)
                if last:
                    summary.health = last.health
                    summary.last_check_at = last.last_check_at
                summaries.append(summary)
        return summaries

    # ── Single connector ──────────────────────────────────────────────────────

    def get_connector_status(self, connector_id: str) -> ConnectorStatus:
        """Return the current status for a connector (uses cached health if available)."""
        connector = registry.get(connector_id)
        if connector is None:
            raise KeyError(f"Unknown connector: {connector_id!r}")
        with _LOCK:
            cfg = _get_config_locked(connector_id)
            last = _LAST_HEALTH.get(connector_id)
        if last:
            return last
        return connector.get_status(cfg)

    def get_config(self, connector_id: str) -> ConnectorConfig:
        """Return the stored config for a connector (no secrets)."""
        if registry.get(connector_id) is None:
            raise KeyError(f"Unknown connector: {connector_id!r}")
        with _LOCK:
            return _get_config_locked(connector_id)

    # ── Config update ─────────────────────────────────────────────────────────

    def update_config(
        self, connector_id: str, update: ConnectorConfigUpdate
    ) -> ConnectorConfig:
        """
        Apply a config update. Secrets (token, api_key) are never echoed back.
        Presence flags are persisted to disk; raw values are kept in-memory only.
        """
        if registry.get(connector_id) is None:
            raise KeyError(f"Unknown connector: {connector_id!r}")

        with _LOCK:
            cfg = _get_config_locked(connector_id)
            data: Dict[str, Any] = cfg.model_dump()

            if update.enabled is not None:
                data["enabled"] = update.enabled
            if update.base_url is not None:
                data["base_url"] = update.base_url.strip() or None
            if update.auth_type is not None:
                data["auth_type"] = update.auth_type
            if update.workspace is not None:
                data["workspace"] = update.workspace.strip() or None
            if update.project_key is not None:
                data["project_key"] = update.project_key.strip() or None
            if update.channel is not None:
                data["channel"] = update.channel.strip() or None
            if update.extra is not None:
                data["extra"] = update.extra

            # Secrets: persist in-memory only; disk stores presence flags
            if update.token is not None:
                stripped = update.token.strip()
                data["token_present"] = bool(stripped)
                if stripped:
                    _SECRETS.setdefault(connector_id, {})["token"] = stripped
                elif connector_id in _SECRETS:
                    _SECRETS[connector_id].pop("token", None)
            if update.api_key is not None:
                stripped = update.api_key.strip()
                data["api_key_present"] = bool(stripped)
                if stripped:
                    _SECRETS.setdefault(connector_id, {})["api_key"] = stripped
                elif connector_id in _SECRETS:
                    _SECRETS[connector_id].pop("api_key", None)

            new_cfg = ConnectorConfig(**data)
            _CONFIGS[connector_id] = new_cfg
            _save_configs_to_disk(_CONFIGS)

        return new_cfg

    # ── Enable / disable ──────────────────────────────────────────────────────

    def enable(self, connector_id: str) -> ConnectorConfig:
        return self.update_config(connector_id, ConnectorConfigUpdate(enabled=True))

    def disable(self, connector_id: str) -> ConnectorConfig:
        return self.update_config(connector_id, ConnectorConfigUpdate(enabled=False))

    # ── Health check ──────────────────────────────────────────────────────────

    def run_health_check(self, connector_id: str) -> ConnectorStatus:
        """Run a fresh health check and cache the result."""
        connector = registry.get(connector_id)
        if connector is None:
            raise KeyError(f"Unknown connector: {connector_id!r}")

        with _LOCK:
            cfg = _get_config_locked(connector_id)

        status = connector.get_status(cfg)

        with _LOCK:
            _LAST_HEALTH[connector_id] = status

        return status

    # ── Actions ───────────────────────────────────────────────────────────────

    def get_actions(self, connector_id: str) -> List[str]:
        connector = registry.get(connector_id)
        if connector is None:
            raise KeyError(f"Unknown connector: {connector_id!r}")
        return connector.supported_actions()

    def get_connector_secret(self, connector_id: str, key: str) -> Optional[str]:
        """Return an in-memory connector secret (never persisted to disk)."""
        with _LOCK:
            return _SECRETS.get(connector_id, {}).get(key)


# Module-level singleton
integration_service = IntegrationService()
