from __future__ import annotations

from typing import Any, Dict, List, Optional, Tuple

from connectors.registry import registry
from services.integration_service import integration_service


class IntegrationDispatcher:
    """
    Centralized connector dispatcher for operational actions.
    Endpoints call this service after explicit user confirmation in the UI.
    """

    _ALERT_CONNECTORS = ("slack", "email", "teams")
    _TICKETING_CONNECTORS = ("itsm",)

    def readiness(self) -> Dict[str, Any]:
        items: Dict[str, Dict[str, Any]] = {}
        for cid in registry.ids():
            try:
                st = integration_service.get_connector_status(cid)
                items[cid] = {
                    "ready": st.enabled and st.health == "ok",
                    "enabled": st.enabled,
                    "health": st.health,
                    "message": st.last_check_message,
                    "actions": list(registry.get(cid).supported_actions()) if registry.get(cid) else [],
                }
            except Exception as exc:
                items[cid] = {
                    "ready": False,
                    "enabled": False,
                    "health": "unknown",
                    "message": str(exc),
                    "actions": [],
                }
        return items

    def alerting_ready_legacy(self) -> Dict[str, Any]:
        """Backward-compatible response used by current UI."""
        r = self.readiness().get("slack") or {}
        if not r.get("ready"):
            return {"ready": False, "reason": r.get("message") or "Slack connector not ready"}
        return {"ready": True, "connector_id": "slack"}

    def send_alert(
        self,
        *,
        connector_id: str,
        subject: Optional[str],
        message: str,
        recipients: Optional[List[str]] = None,
        channel: Optional[str] = None,
        context: Optional[Dict[str, Any]] = None,
    ) -> Tuple[bool, Dict[str, Any]]:
        if connector_id not in self._ALERT_CONNECTORS:
            return False, {"error": f"Connector '{connector_id}' does not support alerting"}
        connector = registry.get(connector_id)
        if connector is None:
            return False, {"error": f"Connector '{connector_id}' not found"}

        status = integration_service.get_connector_status(connector_id)
        if not status.enabled:
            return False, {"error": f"Connector '{connector_id}' is disabled"}
        if status.health != "ok":
            return False, {"error": f"Connector '{connector_id}' is not ready: {status.last_check_message or status.health}"}

        if connector_id == "slack":
            ch = (channel or integration_service.get_config("slack").channel or "").strip()
            if not ch:
                return False, {"error": "Slack channel is required"}
            ok, msg = connector.send_alert(channel=ch, text=message, run_id=(context or {}).get("run_id"))
            return ok, {"message": msg, "connector_id": "slack", "channel": ch}

        if connector_id == "email":
            ok, msg = connector.send_alert(subject=subject or "Vanya alert", message=message, recipients=recipients or [])
            return ok, {"message": msg, "connector_id": "email", "recipients": recipients or []}

        if connector_id == "teams":
            ok, msg = connector.send_alert(subject=subject or "Vanya alert", message=message)
            return ok, {"message": msg, "connector_id": "teams"}

        return False, {"error": f"Unsupported connector '{connector_id}'"}

    def create_ticket(
        self,
        *,
        title: str,
        description: str,
        priority: str = "medium",
        context: Optional[Dict[str, Any]] = None,
    ) -> Tuple[bool, Dict[str, Any]]:
        connector_id = "itsm"
        connector = registry.get(connector_id)
        if connector is None:
            return False, {"error": "ITSM connector not found"}
        status = integration_service.get_connector_status(connector_id)
        if not status.enabled:
            return False, {"error": "ITSM connector is disabled"}
        if status.health != "ok":
            return False, {"error": f"ITSM connector is not ready: {status.last_check_message or status.health}"}
        return connector.create_ticket(
            title=title,
            description=description,
            priority=priority,
            context=context or {},
        )


integration_dispatcher = IntegrationDispatcher()
