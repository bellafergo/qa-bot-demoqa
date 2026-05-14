# services/alerting.py
"""
Post-run Slack alerts (fire-and-forget). Does not block persistence.

Hook points: test_catalog_service._save_run and run_bridge.bridge_run_to_sqlite.
Not test_run_repo.create_run — orchestrator may upsert the same run twice for flaky meta.
"""
from __future__ import annotations

import asyncio
import logging
import threading
from typing import Any, Optional

logger = logging.getLogger("vanya.alerting")

# Accept API-style status strings too
_FAILED_STATUSES = frozenset({"fail", "failed", "error"})


def _run_status_is_failed(status: Any) -> bool:
    return str(status or "").strip().lower() in _FAILED_STATUSES


def _derive_error_summary(run: Any) -> str:
    meta = getattr(run, "meta", None) or {}
    if isinstance(meta, dict):
        r = meta.get("runner_reason") or meta.get("error_message")
        if r:
            return str(r)[:800]
    logs = getattr(run, "logs", None) or []
    if logs:
        return str(logs[0])[:800]
    for step in getattr(run, "steps_result", None) or []:
        if isinstance(step, dict) and step.get("ok") is False:
            return str(step.get("error") or step.get("message") or "step failed")[:800]
    em = getattr(run, "error_message", None)
    if em:
        return str(em)[:800]
    return "sin detalle"


def _send_slack_alert_if_configured_for_test_run(run: Any) -> None:
    if not _run_status_is_failed(getattr(run, "status", None)):
        return
    try:
        from connectors.registry import registry
        from connectors.slack_connector import SlackConnector
        from models.connector import ConnectorConfig
        from services.integration_service import integration_service

        cfg: ConnectorConfig = integration_service.get_config("slack")
        if not cfg.enabled:
            return

        conn = registry.get("slack")
        if not isinstance(conn, SlackConnector):
            return

        valid, msg = conn.validate_config(cfg)
        if not valid:
            logger.debug("slack auto-alert skipped (invalid config): %s", msg)
            return

        ch = (cfg.channel or "").strip()
        if not ch:
            return

        test_name = (getattr(run, "test_name", None) or getattr(run, "test_case_id", None) or "Test")
        test_name = str(test_name).strip() or "Test"
        duration_ms = getattr(run, "duration_ms", None)
        if duration_ms is None:
            duration_ms = 0
        err = _derive_error_summary(run)
        ev = getattr(run, "evidence_url", None)
        ev_str = (str(ev).strip() if ev else "") or "no disponible"
        subject_line = f"❌ Test fallido: {test_name}"
        body = (
            f"*Test:* {test_name}\n"
            f"*Estado:* {getattr(run, 'status', '')}\n"
            f"*Duración:* {duration_ms}ms\n"
            f"*Error:* {err}\n"
            f"*Evidencia:* {ev_str}"
        )
        full_text = f"{subject_line}\n\n{body}"
        ev_link: Optional[str] = None
        if ev and str(ev).strip().lower().startswith(("http://", "https://")):
            ev_link = str(ev).strip()

        st = str(getattr(run, "status", "") or "").lower()
        ok, send_msg = conn.send_alert(
            channel=ch,
            text=full_text,
            run_id=getattr(run, "run_id", None),
            status=st,
            evidence_url=ev_link,
        )
        if not ok:
            logger.warning("slack auto-alert failed run_id=%s: %s", getattr(run, "run_id", "?"), send_msg)
    except Exception as exc:
        logger.warning(
            "slack auto-alert error run_id=%s: %s",
            getattr(run, "run_id", "?"),
            exc,
        )


async def send_alert_if_configured(run: Any) -> None:
    """Async entry point; runs the sync sender in a thread (non-blocking)."""
    await asyncio.to_thread(_send_slack_alert_if_configured_for_test_run, run)


def schedule_slack_alert_on_failed_run(run: Any) -> None:
    """
    Fire-and-forget from synchronous run completion paths (catalog, run_bridge).
    Never raises.
    """
    meta = getattr(run, "meta", None) or {}
    if isinstance(meta, dict) and meta.get("source") == "browser_inspection":
        return

    def _worker() -> None:
        try:
            _send_slack_alert_if_configured_for_test_run(run)
        except Exception as exc:
            logger.warning("slack auto-alert worker error: %s", exc)

    try:
        t = threading.Thread(target=_worker, daemon=True, name="vanya-slack-auto-alert")
        t.start()
    except Exception as exc:
        logger.warning("slack auto-alert could not start thread: %s", exc)


def schedule_browser_inspection_watch_alert(
    *,
    alert_kind: str,
    watch_id: str,
    url: str,
    change_level: str,
    summary: str,
    regression_signals: list,
    base_inspection_id: str | None,
    target_inspection_id: str,
    visual_change_detected: Optional[bool] = None,
    visual_change_level: Optional[str] = None,
    visual_similarity_score: Optional[float] = None,
) -> None:
    """
    Fire-and-forget Slack summary for watch diff alerts. Never raises.
    Payload is intentionally tiny (no DOM, no screenshots).
    """

    def _worker() -> None:
        try:
            from connectors.registry import registry
            from connectors.slack_connector import SlackConnector
            from models.connector import ConnectorConfig
            from services.integration_service import integration_service

            cfg: ConnectorConfig = integration_service.get_config("slack")
            if not cfg.enabled:
                return
            conn = registry.get("slack")
            if not isinstance(conn, SlackConnector):
                return
            valid, msg = conn.validate_config(cfg)
            if not valid:
                logger.debug("browser watch alert skipped: %s", msg)
                return
            ch = (cfg.channel or "").strip()
            if not ch:
                return
            rs = ", ".join(str(x) for x in (regression_signals or [])[:12])
            if len(rs) > 600:
                rs = rs[:600] + "…"
            body = (
                f"*Vanya browser watch*\n"
                f"*Kind:* `{alert_kind}`\n"
                f"*URL:* {str(url)[:500]}\n"
                f"*Change level:* `{change_level}`\n"
                f"*Watch:* `{watch_id}`\n"
                f"*Inspections:* `{base_inspection_id or '—'}` → `{target_inspection_id}`\n"
                f"*Summary:* {str(summary)[:900]}\n"
                f"*Regression signals:* {rs or '—'}"
            )
            if visual_change_detected or (
                visual_change_level and str(visual_change_level).lower() not in ("none", "")
            ):
                sim = visual_similarity_score
                sim_s = f"{sim:.2f}" if isinstance(sim, (int, float)) else "—"
                body += (
                    f"\n*Visual:* detected={bool(visual_change_detected)} "
                    f"level=`{visual_change_level or '—'}` sim={sim_s}"
                )
            ok, send_msg = conn.send_alert(
                channel=ch,
                text=body,
                run_id=target_inspection_id,
                status="watch",
            )
            if not ok:
                logger.warning("browser watch slack alert failed: %s", send_msg)
        except Exception as exc:
            logger.warning("browser watch alert worker error: %s", exc)

    try:
        t = threading.Thread(target=_worker, daemon=True, name="vanya-browser-watch-alert")
        t.start()
    except Exception as exc:
        logger.warning("browser watch alert could not start thread: %s", exc)
