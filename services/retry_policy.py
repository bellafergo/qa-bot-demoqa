# services/retry_policy.py
"""
Lightweight retry policy for suite execution.

Decision logic:
  - Transient failures (timeouts, navigation, network) → retry
  - Terminal failures (element not found, assertion mismatch) → do not retry
  - Succeeded on retry → mark as flaky, not failed

Usage:
    policy = RetryPolicy()
    if policy.should_retry(runner_result, attempt=1):
        # run again
"""
from __future__ import annotations

import re
from typing import Any, Dict, Optional

# ── Failure classification patterns ───────────────────────────────────────────

# Matches runner output (reason / logs / failure_context) for transient issues
_TRANSIENT_PATTERNS = [
    r"timeout",
    r"timed.?out",
    r"navigation",
    r"networkidle",
    r"net::",
    r"ERR_",
    r"connection.?refused",
    r"ECONNREFUSED",
    r"page closed",
    r"frame detached",
    r"execution context",
    r"context.?destroy",
    r"browser.?crash",
]

# Matches terminal failures — retrying would not help
_TERMINAL_PATTERNS = [
    r"locator_not_found",
    r"not_found",
    r"no.?element",
    r"invalid.?target",
    r"no primary selector",
    r"assertion.?failed",
    r"assert_text_contains",
    r"url.?mismatch",
    r"assert_url_contains",
    r"assert_not_visible",
    r"fill requiere",
]

_TRANSIENT_RE = re.compile("|".join(_TRANSIENT_PATTERNS), re.IGNORECASE)
_TERMINAL_RE  = re.compile("|".join(_TERMINAL_PATTERNS),  re.IGNORECASE)


def classify_runner_failure(runner_result: Dict[str, Any]) -> str:
    """
    Classify the failure in a runner result as 'transient', 'terminal', or 'unknown'.
    Reads: reason, logs, failure_context.classification, steps[*].error.
    """
    if runner_result.get("ok") or runner_result.get("status") == "passed":
        return "none"

    # Collect all text signals
    signals: list[str] = []

    reason = str(runner_result.get("reason") or "")
    if reason:
        signals.append(reason)

    for log in (runner_result.get("logs") or []):
        signals.append(str(log))

    fc = runner_result.get("failure_context")
    if isinstance(fc, dict):
        signals.append(str(fc.get("error_type") or ""))
        signals.append(str(fc.get("error") or ""))
        classification = fc.get("classification")
        if isinstance(classification, dict):
            signals.append(str(classification.get("failure_type") or ""))

    for step in (runner_result.get("steps") or []):
        if isinstance(step, dict) and step.get("error"):
            signals.append(str(step["error"]))

    combined = " ".join(signals)

    if _TERMINAL_RE.search(combined):
        return "terminal"
    if _TRANSIENT_RE.search(combined):
        return "transient"
    return "unknown"


class RetryPolicy:
    """
    Configurable retry policy for individual test jobs.

    Attributes:
        max_retries:     maximum number of additional attempts (default 2)
        retry_terminal:  if True, retry even terminal failures (default False)
        retry_unknown:   if True, retry unknown failure types (default True)
    """

    def __init__(
        self,
        max_retries: int = 2,
        retry_terminal: bool = False,
        retry_unknown: bool = True,
    ):
        self.max_retries   = max_retries
        self.retry_terminal = retry_terminal
        self.retry_unknown  = retry_unknown

    def should_retry(self, runner_result: Dict[str, Any], attempt: int) -> bool:
        """
        Return True if the job should be retried given the current result and attempt number.

        Args:
            runner_result: the raw dict returned by execute_test()
            attempt:       the attempt number just completed (1 = first run)
        """
        # Already used up all retries
        if attempt > self.max_retries:
            return False

        # Passed — no retry needed
        if runner_result.get("ok") or runner_result.get("status") == "passed":
            return False

        failure_type = classify_runner_failure(runner_result)

        if failure_type == "terminal":
            return self.retry_terminal
        if failure_type == "transient":
            return True
        # unknown
        return self.retry_unknown

    @classmethod
    def default(cls) -> "RetryPolicy":
        """Standard policy: retry transient up to 2 times, never retry terminal."""
        return cls(max_retries=2, retry_terminal=False, retry_unknown=True)

    @classmethod
    def strict(cls) -> "RetryPolicy":
        """No retries — every failure is final."""
        return cls(max_retries=0)

    @classmethod
    def aggressive(cls) -> "RetryPolicy":
        """Retry everything up to 3 times — useful for flaky environments."""
        return cls(max_retries=3, retry_terminal=True, retry_unknown=True)
