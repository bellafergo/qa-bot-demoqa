"""CLI entry for Vanya Local Agent (Phase 4B–4C)."""
from __future__ import annotations

import argparse
import logging
import sys

from local_agent import __version__
from local_agent.config import argparse_namespace_to_kwargs, build_config


def _setup_logging(verbose: bool) -> None:
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s %(levelname)s %(name)s %(message)s",
        stream=sys.stderr,
    )


def _parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        prog="vanya-local-agent",
        description="Vanya Local Agent — heartbeat, poll, jobs (Phase 4C: optional Playwright browser_inspection).",
    )
    p.add_argument("--base-url", default=None, help="Vanya Cloud base URL (or VANYA_CLOUD_URL)")
    p.add_argument("--agent-id", default=None, help="Agent id (or VANYA_AGENT_ID)")
    p.add_argument("--agent-token", default=None, help="Agent bearer token (or VANYA_AGENT_TOKEN)")
    p.add_argument(
        "--poll-interval",
        type=float,
        default=None,
        help="Seconds to sleep when queue empty (or VANYA_AGENT_POLL_INTERVAL, default 10)",
    )
    p.add_argument(
        "--http-timeout",
        type=float,
        default=None,
        help="HTTP timeout seconds (or VANYA_AGENT_HTTP_TIMEOUT, default 30)",
    )
    p.add_argument(
        "--agent-version",
        default=None,
        help="Sent as agent_version on heartbeat (default vanya-local-agent/4c)",
    )
    p.add_argument("--once", action="store_true", help="Run a single heartbeat+poll cycle then exit")
    p.add_argument(
        "--dry-run",
        action="store_true",
        help="Do not POST job results (still heartbeat + poll)",
    )
    p.add_argument(
        "--browser-enabled",
        action=argparse.BooleanOptionalAction,
        default=None,
        help="Run browser_inspection jobs with Playwright (or VANYA_AGENT_BROWSER_ENABLED=1). Default off.",
    )
    p.add_argument(
        "--allow-localhost",
        action=argparse.BooleanOptionalAction,
        default=None,
        help="Allow http(s) to localhost/127.0.0.1 (or VANYA_AGENT_ALLOW_LOCALHOST=1)",
    )
    p.add_argument(
        "--allow-private-ips",
        action=argparse.BooleanOptionalAction,
        default=None,
        help="Allow literal private/link-local IPs (or VANYA_AGENT_ALLOW_PRIVATE_IPS=1)",
    )
    p.add_argument(
        "--browser-headless",
        action=argparse.BooleanOptionalAction,
        default=None,
        help="Chromium headless (default true; or VANYA_AGENT_BROWSER_HEADLESS)",
    )
    p.add_argument(
        "--browser-timeout-ms",
        type=int,
        default=None,
        help="Default navigation timeout for jobs without payload.timeout_ms (VANYA_AGENT_BROWSER_TIMEOUT_MS)",
    )
    p.add_argument("-v", "--verbose", action="store_true", help="Debug logging")
    p.add_argument(
        "--show-agent-version",
        action="store_true",
        help="Print local_agent package version and exit",
    )
    return p.parse_args(argv)


def main(argv: list[str] | None = None) -> None:
    ns = _parse_args(argv)
    if ns.show_agent_version:
        print(__version__)
        raise SystemExit(0)
    _setup_logging(bool(ns.verbose))
    log = logging.getLogger("vanya.local_agent.cli")
    try:
        cfg = build_config(**argparse_namespace_to_kwargs(ns))
    except ValueError as e:
        log.error("configuration error: %s", e)
        raise SystemExit(2) from e

    log.info(
        "starting agent id=%s cloud=%s poll=%.1fs once=%s dry_run=%s browser=%s lh=%s priv=%s token=%s",
        cfg.agent_id,
        cfg.base_url,
        cfg.poll_interval_s,
        cfg.once,
        cfg.dry_run,
        cfg.browser_enabled,
        cfg.allow_localhost,
        cfg.allow_private_ips,
        cfg.token_log_label(),
    )

    from local_agent.runner import run

    code = run(cfg)
    raise SystemExit(code)


if __name__ == "__main__":
    main()
