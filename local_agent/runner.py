"""Agent main loop (heartbeat → poll → optional simulated results)."""
from __future__ import annotations

import logging
import time
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from local_agent.client import VanyaAgentClient
    from local_agent.config import AgentConfig

logger = logging.getLogger("vanya.local_agent.runner")


def run_cycle(client: "VanyaAgentClient", cfg: "AgentConfig") -> int:
    """
    One cycle: heartbeat, poll, process queued jobs (simulated success unless cfg.dry_run).
    Returns number of jobs seen this cycle.
    """
    logger.info("heartbeat token=%s", cfg.token_log_label())
    client.heartbeat(agent_version=cfg.agent_version)
    data = client.poll(limit=10)
    jobs = list(data.get("jobs") or [])
    caps = list(data.get("agent_capabilities") or [])
    if not jobs:
        logger.info("poll: no jobs in queue")
        return 0
    logger.info("poll: %d job(s) in queue", len(jobs))
    client.process_jobs(jobs, cfg=cfg, agent_capabilities=caps)
    return len(jobs)


def run(cfg: "AgentConfig", client: "VanyaAgentClient | None" = None) -> int:
    """
    Run until interrupted or cfg.once.
    Exit code 0 on clean exit, 1 on configuration/client error, 2 on auth errors.
    """
    from local_agent.client import AgentAuthError, AgentClientError, VanyaAgentClient

    owns = client is None
    http = client or VanyaAgentClient(
        cfg.base_url,
        cfg.agent_id,
        cfg.agent_token,
        timeout_s=cfg.http_timeout_s,
    )
    try:
        while True:
            try:
                n = run_cycle(http, cfg)
            except AgentAuthError as e:
                logger.error("%s", e)
                return 2
            except AgentClientError as e:
                logger.error("%s", e)
                return 1
            if cfg.once:
                logger.info("--once: exiting after single cycle")
                return 0
            if n == 0:
                logger.debug("sleeping %.1fs", cfg.poll_interval_s)
                time.sleep(cfg.poll_interval_s)
            else:
                time.sleep(min(2.0, cfg.poll_interval_s))
    except KeyboardInterrupt:
        logger.info("interrupted, exiting")
        return 0
    finally:
        if owns:
            http.close()
