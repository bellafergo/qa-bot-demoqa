# runners/desktop_runner.py
"""
Desktop UI Test Runner for Vanya.

Executes normalized desktop steps against Win32 / legacy applications
using DesktopAdapter (pywinauto backend or deterministic mock).

Key design principles:
  - ZERO LLM calls during execution (fully deterministic from catalog steps)
  - Compatible result dict with UI runner (generic_steps.py) and TestRun model
  - Gracefully degrades to mock on non-Windows / CI
  - Evidence captured via EvidenceCapture (screenshots, trace log)
  - Reuses runners/common.py helpers

Supported actions
-----------------
  launch_app            launch or attach the target application
  attach_window         attach to a named window
  focus_window          bring window to front
  click                 click a control
  input / fill          type text into a control
  type_keys             send raw key sequences (e.g. "{ENTER}", "^a")
  select                choose an item in a listbox / combo
  read_text             read control text into the step log
  assert_text_contains  assert control text contains expected value
  assert_exists         assert control is present
  wait_for              wait for control to become visible
  screenshot            capture an explicit screenshot mid-flow
  wait_ms               pause execution (ms)
"""
from __future__ import annotations

import logging
import time
import uuid
from typing import Any, Dict, List, Optional

from core.runner_contract import normalize_desktop_action
from runners.common import _as_int, _now_ms, _safe_str, _final_status
from runners.desktop_adapter import DesktopAdapter

logger = logging.getLogger("vanya.desktop_runner")


def _target_from_step(step: Dict[str, Any]) -> str:
    """Extract the primary target string from a step dict."""
    t = step.get("target") or step.get("selector") or step.get("loc") or ""
    return _safe_str(t).strip()


def _value_from_step(step: Dict[str, Any]) -> str:
    v = step.get("value") or step.get("text") or step.get("expected") or ""
    return _safe_str(v).strip()


def _ms_from_step(step: Dict[str, Any], default: int = 500) -> int:
    return _as_int(step.get("ms") or step.get("timeout_ms"), default)


# ── Result builder ─────────────────────────────────────────────────────────────

def _build_result(
    *,
    ok: bool,
    outcome: str,
    status: str,
    reason: str,
    evidence_id: str,
    report_steps: List[Dict[str, Any]],
    logs: List[str],
    screenshot_b64: Optional[str],
    duration_ms: int,
    adapter: DesktopAdapter,
    evidence_screenshots: List[Dict[str, Any]],
) -> Dict[str, Any]:
    """Build the result dict compatible with TestRun and the existing run storage."""
    return {
        "ok":             ok,
        "status":         status,
        "outcome":        outcome,
        "reason":         reason,
        "evidence_id":    evidence_id,
        "steps":          report_steps,
        "logs":           logs,
        "screenshot_b64": screenshot_b64,
        "duration_ms":    duration_ms,
        "meta": {
            "runner":          "desktop",
            "is_mock":         adapter.is_mock,
            "steps_count":     len(report_steps),
        },
        # Evidence bundle — compatible with run storage, mirroring generic_steps.py
        "evidence": {
            "run_id":         evidence_id,
            "screenshots":    evidence_screenshots,
            "network_events": [],    # desktop has no network events
            "dom_snapshots":  [],    # desktop has no DOM
            "video_path":     None,
            "trace_path":     None,
            "metadata": {
                "runner":      "desktop",
                "steps_count": len(report_steps),
                "is_mock":     adapter.is_mock,
            },
        },
    }


# ── Main runner ───────────────────────────────────────────────────────────────

def run_desktop_test(
    steps: List[Dict[str, Any]],
    *,
    use_mock: Optional[bool] = None,
    timeout_s: Optional[int] = None,
) -> Dict[str, Any]:
    """
    Execute a sequence of desktop automation steps.

    Parameters
    ----------
    steps      : List of step dicts from the catalog (already normalised)
    use_mock   : True → always use MockDesktopBackend (test / CI / non-Windows)
                 None → auto-detect (default)
    timeout_s  : Optional global timeout in seconds (advisory — not enforced per-step)

    Returns
    -------
    Dict compatible with TestRun (same contract as generic_steps.execute_test).

    Token cost
    ----------
    ZERO LLM calls. All execution is deterministic from the step list.
    """
    t0           = time.time()
    evidence_id  = f"DT-{uuid.uuid4().hex[:10].upper()}"
    report_steps: List[Dict[str, Any]] = []
    logs:         List[str]            = []
    screenshots:  List[Dict[str, Any]] = []
    outcome      = "pass"
    had_error    = False
    reason: Optional[str] = None
    final_screenshot_b64: Optional[str] = None

    if not isinstance(steps, list) or not steps:
        return _build_result(
            ok=False, outcome="fail", status="failed",
            reason="desktop runner: steps vacío o inválido",
            evidence_id=evidence_id, report_steps=[], logs=["Empty steps"],
            screenshot_b64=None, duration_ms=0,
            adapter=DesktopAdapter(use_mock=True),
            evidence_screenshots=[],
        )

    adapter = DesktopAdapter(use_mock=use_mock)

    logger.info(
        "run_desktop_test START — evidence_id=%s steps=%d backend=%s",
        evidence_id,
        len(steps),
        "mock" if adapter.is_mock else "pywinauto/real",
    )

    logs.append(f"[PLAN] {len(steps)} desktop steps — mock={adapter.is_mock}")
    logs.append(
        "[PLAN] " + ", ".join(
            f"{i}:{normalize_desktop_action(s.get('action','?'))}"
            for i, s in enumerate(steps)
        )
    )

    def _record_step(
        i: int,
        step: Dict[str, Any],
        st: str,
        err: Optional[str] = None,
        extra: Optional[Dict[str, Any]] = None,
    ) -> None:
        payload: Dict[str, Any] = {
            "index":  i,
            "action": normalize_desktop_action(step.get("action", "")),
            "target": _target_from_step(step),
            "value":  step.get("value"),
            "status": st,
            "error":  err,
            "ts_ms":  _now_ms(),
        }
        if extra:
            payload.update(extra)
        report_steps.append(payload)

    def _snap(step_i: int, action_name: str, label: Optional[str] = None) -> None:
        b64 = adapter.screenshot()
        if b64:
            screenshots.append({
                "step_index":   step_i,
                "action":       action_name,
                "label":        label or action_name,
                "b64":          b64,
                "timestamp_ms": _now_ms(),
            })

    # ── Step loop ─────────────────────────────────────────────────────────
    for i, step in enumerate(steps):
        raw_action = step.get("action", "")
        action     = normalize_desktop_action(raw_action)
        target     = _target_from_step(step)
        value      = _value_from_step(step)

        logs.append(f"[STEP] i={i} action={action!r} target={target!r}")

        try:
            # ── Launch app ────────────────────────────────────────────────
            if action == "launch_app":
                app_path = target or value or step.get("path") or ""
                if not app_path:
                    raise ValueError("launch_app requires target or value (app path)")
                adapter.launch_app(app_path)
                _record_step(i, step, "passed", extra={"app_path": app_path})
                _snap(i, action)
                continue

            # ── Attach window ─────────────────────────────────────────────
            if action == "attach_window":
                window_title = target or value
                if not window_title:
                    raise ValueError("attach_window requires target (window title)")
                timeout_s_win = _as_int(step.get("timeout_s"), 10)
                adapter.attach_window(window_title, timeout_s=timeout_s_win)
                _record_step(i, step, "passed", extra={"window_title": window_title})
                _snap(i, action)
                continue

            # ── Focus window ──────────────────────────────────────────────
            if action == "focus_window":
                window_title = target or value
                if not window_title:
                    raise ValueError("focus_window requires target (window title)")
                adapter.focus_window(window_title)
                _record_step(i, step, "passed", extra={"window_title": window_title})
                continue

            # ── Click ─────────────────────────────────────────────────────
            if action == "click":
                if not target:
                    raise ValueError("click requires target")
                adapter.click(target)
                _record_step(i, step, "passed")
                _snap(i, action)
                continue

            # ── Input text ────────────────────────────────────────────────
            if action == "input_text":
                if not target:
                    raise ValueError("input_text requires target")
                adapter.input_text(target, value)
                _record_step(i, step, "passed", extra={"value_len": len(value)})
                continue

            # ── Type keys ─────────────────────────────────────────────────
            if action == "type_keys":
                keys = value or step.get("keys") or ""
                if not target and not keys:
                    raise ValueError("type_keys requires target or value")
                adapter.type_keys(target, keys)
                _record_step(i, step, "passed", extra={"keys": keys})
                continue

            # ── Select ────────────────────────────────────────────────────
            if action == "select":
                if not target:
                    raise ValueError("select requires target")
                adapter.select(target, value)
                _record_step(i, step, "passed", extra={"selected": value})
                continue

            # ── Read text ─────────────────────────────────────────────────
            if action == "read_text":
                if not target:
                    raise ValueError("read_text requires target")
                text = adapter.read_text(target)
                logs.append(f"[READ] {target!r} → {text!r}")
                _record_step(i, step, "passed", extra={"read_text": text})
                continue

            # ── Assert text contains ──────────────────────────────────────
            if action == "assert_text_contains":
                if not target:
                    raise ValueError("assert_text_contains requires target")
                if not value:
                    raise ValueError("assert_text_contains requires value (expected text)")
                adapter.assert_text_contains(target, value)
                _record_step(i, step, "passed", extra={"expected": value})
                _snap(i, action, label=f"assert_{target}")
                continue

            # ── Assert exists ─────────────────────────────────────────────
            if action == "assert_exists":
                if not target:
                    raise ValueError("assert_exists requires target")
                adapter.assert_exists(target)
                _record_step(i, step, "passed")
                continue

            # ── Wait for ──────────────────────────────────────────────────
            if action == "wait_for":
                if not target:
                    raise ValueError("wait_for requires target")
                timeout_ms = _ms_from_step(step, default=5000)
                adapter.wait_for(target, timeout_ms=timeout_ms)
                _record_step(i, step, "passed", extra={"timeout_ms": timeout_ms})
                continue

            # ── Wait ms ───────────────────────────────────────────────────
            if action == "wait_ms":
                ms = _ms_from_step(step, default=500)
                time.sleep(ms / 1000)
                _record_step(i, step, "passed", extra={"ms": ms})
                continue

            # ── Explicit screenshot ───────────────────────────────────────
            if action == "screenshot":
                b64 = adapter.screenshot()
                _record_step(i, step, "passed", extra={"screenshot_taken": b64 is not None})
                if b64:
                    screenshots.append({
                        "step_index":   i,
                        "action":       "screenshot",
                        "label":        target or "explicit",
                        "b64":          b64,
                        "timestamp_ms": _now_ms(),
                    })
                    final_screenshot_b64 = b64
                continue

            # ── Unknown action ────────────────────────────────────────────
            raise ValueError(f"Unsupported desktop action: {action!r}")

        except AssertionError as e:
            outcome = "fail"
            reason  = f"Assertion failed at step {i + 1} ({action}): {e}"
            logs.append(f"[FAIL] {reason}")
            _record_step(i, step, "failed", err=reason)
            _snap(i, action, label=f"failure_step_{i}")
            break

        except (ValueError, TypeError) as e:
            outcome   = "fail"
            reason    = f"Step error at step {i + 1} ({action}): {type(e).__name__}: {e}"
            logs.append(f"[ERROR] {reason}")
            _record_step(i, step, "failed", err=reason)
            _snap(i, action, label=f"failure_step_{i}")
            break

        except Exception as e:
            outcome   = "fail"
            had_error = True
            if e.args and isinstance(e.args[0], dict):
                payload = e.args[0]
                reason  = (
                    f"Desktop target not found at step {i + 1} ({action}): "
                    f"classification={payload.get('classification')} "
                    f"errors={payload.get('errors')}"
                )
            else:
                reason = f"Unexpected error at step {i + 1} ({action}): {type(e).__name__}: {e}"
            logs.append(f"[ERROR] {reason}")
            _record_step(i, step, "error", err=reason)
            _snap(i, action, label=f"failure_step_{i}")
            break

    # ── Final screenshot ──────────────────────────────────────────────────────
    if final_screenshot_b64 is None:
        final_screenshot_b64 = adapter.screenshot()

    if final_screenshot_b64 and not screenshots:
        screenshots.append({
            "step_index":   len(steps) - 1,
            "action":       "final",
            "label":        "run_complete",
            "b64":          final_screenshot_b64,
            "timestamp_ms": _now_ms(),
        })

    # ── Finalize result ───────────────────────────────────────────────────────
    duration_ms = int((time.time() - t0) * 1000)
    status      = _final_status("pass", outcome, had_error)

    if outcome == "pass" and reason is None:
        reason = "OK"

    ok = status == "passed"

    logs.append(
        f"[DONE] status={status} outcome={outcome} duration={duration_ms}ms "
        f"steps={len(report_steps)} screenshots={len(screenshots)}"
    )
    logger.info(
        "run_desktop_test DONE — evidence_id=%s status=%s duration=%dms "
        "steps=%d screenshots=%d backend=%s",
        evidence_id, status, duration_ms, len(report_steps), len(screenshots),
        "mock" if adapter.is_mock else "pywinauto/real",
    )

    return _build_result(
        ok=ok,
        outcome=outcome,
        status=status,
        reason=reason or "unknown",
        evidence_id=evidence_id,
        report_steps=report_steps,
        logs=logs,
        screenshot_b64=final_screenshot_b64,
        duration_ms=duration_ms,
        adapter=adapter,
        evidence_screenshots=screenshots,
    )
