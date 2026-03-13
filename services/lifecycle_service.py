# services/lifecycle_service.py
"""
Test case lifecycle management service.

Manages transitions between lifecycle states and version tracking.

Allowed transitions
-------------------
  draft      → active, archived
  active     → deprecated, archived
  deprecated → active, archived
  archived   → (terminal)

Version bumping
---------------
Every call to transition() or bump_version() appends a TestCaseVersionInfo
entry to test_case.version_history and increments test_case.version.

Design
------
- Pure service — does NOT write to the DB itself.
- Caller (route or catalog_service) is responsible for persisting the result.
- No external dependencies.
"""
from __future__ import annotations

from datetime import datetime, timezone
from typing import List, Optional

from models.test_case import LifecycleState, TestCase, TestCaseVersionInfo


# ── Allowed transition graph ───────────────────────────────────────────────────

_TRANSITIONS: dict = {
    LifecycleState.draft:      {LifecycleState.active, LifecycleState.archived},
    LifecycleState.active:     {LifecycleState.deprecated, LifecycleState.archived},
    LifecycleState.deprecated: {LifecycleState.active, LifecycleState.archived},
    LifecycleState.archived:   set(),   # terminal state
}


def _now_utc() -> datetime:
    return datetime.now(timezone.utc)


# ── Public service ────────────────────────────────────────────────────────────

class LifecycleService:
    """Manages lifecycle state transitions and version history for TestCase objects."""

    # ── Transition ─────────────────────────────────────────────────────────────

    def transition(
        self,
        tc: TestCase,
        new_state: LifecycleState,
        change_summary: Optional[str] = None,
    ) -> TestCase:
        """
        Transition a test case to a new lifecycle state.

        Returns the mutated TestCase (same object — caller should persist it).
        Raises ValueError on invalid transitions.
        """
        current = tc.lifecycle_state
        allowed = _TRANSITIONS.get(current, set())

        if new_state == current:
            # idempotent — nothing to do
            return tc

        if new_state not in allowed:
            raise ValueError(
                f"Invalid lifecycle transition: {current.value!r} → {new_state.value!r}. "
                f"Allowed from {current.value!r}: "
                f"{[s.value for s in allowed] or 'none (terminal state)'}."
            )

        # Record the version snapshot BEFORE applying the change
        entry = TestCaseVersionInfo(
            version=tc.version,
            lifecycle_state=current.value,
            change_summary=change_summary or f"Transitioned from {current.value} to {new_state.value}",
            changed_at=_now_utc(),
        )
        tc.version_history.append(entry)

        # Apply transition
        tc.lifecycle_state = new_state
        tc.version += 1
        tc.updated_at = _now_utc()

        # Keep the legacy `status` field in sync for backward compatibility
        if new_state == LifecycleState.active:
            tc.status = "active"      # type: ignore[assignment]
        elif new_state in (LifecycleState.deprecated, LifecycleState.archived):
            tc.status = "inactive"    # type: ignore[assignment]
        # draft → keep current status (not yet promoted)

        return tc

    # ── Version bump ───────────────────────────────────────────────────────────

    def bump_version(
        self,
        tc: TestCase,
        change_summary: Optional[str] = None,
        created_from: Optional[str] = None,
        modified_from: Optional[str] = None,
    ) -> TestCase:
        """
        Increment the version counter and append a history entry without
        changing the lifecycle state.  Use this when steps/assertions change.
        """
        entry = TestCaseVersionInfo(
            version=tc.version,
            lifecycle_state=tc.lifecycle_state.value,
            created_from=created_from,
            modified_from=modified_from,
            change_summary=change_summary or "Content updated",
            changed_at=_now_utc(),
        )
        tc.version_history.append(entry)
        tc.version += 1
        tc.updated_at = _now_utc()
        return tc

    # ── Allowed transitions query ──────────────────────────────────────────────

    def get_allowed_transitions(self, state: LifecycleState) -> List[LifecycleState]:
        """Return the list of states reachable from `state`."""
        return list(_TRANSITIONS.get(state, set()))

    # ── Bulk filter ────────────────────────────────────────────────────────────

    def is_runnable(self, tc: TestCase) -> bool:
        """Returns True if the test case is in a runnable state (active)."""
        return tc.lifecycle_state == LifecycleState.active


# Module-level singleton
lifecycle_service = LifecycleService()
