# runners/evidence_capture.py
"""
Enhanced execution evidence capture for Playwright test runs.

Complements evidence_service.py (Cloudinary upload) by providing the
*capture* side: screenshots per step, network event recording, DOM snapshots,
and Playwright tracing.

All features degrade gracefully — if Playwright is unavailable (unit tests,
CI without browsers), functions return empty/None without raising.

Usage
-----
    cfg  = EvidenceConfig(screenshot_timeline=True, dom_snapshot=True)
    cap  = EvidenceCapture(run_id="run-abc", config=cfg)

    # inside a with-page block:
    cap.start_trace(playwright_context)
    cap.capture_step_screenshot(page, step_index=0, action="goto")
    cap.capture_dom_snapshot(page, label="after_login")
    trace_path = cap.stop_trace(playwright_context)
    bundle = cap.finalize()
"""
from __future__ import annotations

import base64
import logging
import os
import time
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

logger = logging.getLogger("vanya.evidence_capture")


# ── Config ─────────────────────────────────────────────────────────────────────

@dataclass
class EvidenceConfig:
    """
    Controls which evidence artefacts are captured during a test run.

    screenshot_timeline : Take a screenshot after every step.
    video               : Enable Playwright video recording (set before browser launch).
    network_trace       : Record network request/response events.
    dom_snapshot        : Capture full DOM HTML on failures (or all steps if True).
    action_trace        : Enable Playwright trace (zip with screenshots + network).
    output_dir          : Directory for on-disk artefacts (traces, video).
    """
    screenshot_timeline: bool = False
    video:               bool = False
    network_trace:       bool = False
    dom_snapshot:        bool = False
    action_trace:        bool = False
    output_dir:          str  = "/tmp/vanya_evidence"


# ── Bundle ─────────────────────────────────────────────────────────────────────

@dataclass
class EvidenceBundle:
    """All artefacts captured during one test run."""
    run_id:          str
    screenshots:     List[Dict[str, Any]] = field(default_factory=list)
    # Each entry: {step_index, action, b64, timestamp_ms, label}
    network_events:  List[Dict[str, Any]] = field(default_factory=list)
    # Each entry: {url, method, status, timestamp_ms, body_size}
    dom_snapshots:   List[Dict[str, Any]] = field(default_factory=list)
    # Each entry: {label, html, timestamp_ms}
    video_path:      Optional[str]        = None
    trace_path:      Optional[str]        = None
    metadata:        Dict[str, Any]       = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "run_id":         self.run_id,
            "screenshots":    self.screenshots,
            "network_events": self.network_events,
            "dom_snapshots":  self.dom_snapshots,
            "video_path":     self.video_path,
            "trace_path":     self.trace_path,
            "metadata":       self.metadata,
        }


# ── Capture class ──────────────────────────────────────────────────────────────

class EvidenceCapture:
    """
    Stateful capture helper bound to one test run.

    Lifecycle
    ---------
    1. create EvidenceCapture(run_id, config)
    2. call start_trace(playwright_context)  ← optional
    3. for each step: capture_step_screenshot(page, ...)
    4. on failure or when needed: capture_dom_snapshot(page, label)
    5. call stop_trace(playwright_context)   ← optional
    6. call finalize() → EvidenceBundle
    """

    def __init__(self, run_id: str, config: Optional[EvidenceConfig] = None) -> None:
        self.run_id  = run_id
        self.config  = config or EvidenceConfig()
        self._bundle = EvidenceBundle(run_id=run_id)
        self._trace_started = False

    # ── Playwright trace ───────────────────────────────────────────────────────

    def start_trace(self, playwright_context: Any) -> None:
        """
        Start Playwright tracing on a BrowserContext if action_trace is enabled.
        Silently skips if context is None or tracing is not available.
        """
        if not self.config.action_trace:
            return
        if playwright_context is None:
            return
        try:
            playwright_context.tracing.start(
                screenshots=True,
                snapshots=True,
                sources=False,
            )
            self._trace_started = True
            logger.debug("evidence_capture: trace started for run %s", self.run_id)
        except Exception as exc:
            logger.warning("evidence_capture: could not start trace — %s", exc)

    def stop_trace(self, playwright_context: Any) -> Optional[str]:
        """
        Stop Playwright tracing and save the zip to output_dir.
        Returns the path to the trace zip, or None on failure.
        """
        if not self._trace_started or playwright_context is None:
            return None
        try:
            os.makedirs(self.config.output_dir, exist_ok=True)
            trace_path = os.path.join(
                self.config.output_dir, f"trace_{self.run_id}.zip"
            )
            playwright_context.tracing.stop(path=trace_path)
            self._bundle.trace_path = trace_path
            logger.debug("evidence_capture: trace saved to %s", trace_path)
            return trace_path
        except Exception as exc:
            logger.warning("evidence_capture: stop_trace failed — %s", exc)
            return None

    # ── Network recording ──────────────────────────────────────────────────────

    def attach_network_listener(self, page: Any) -> None:
        """
        Register a 'response' listener on the Playwright page to record
        network events.  Call once after page is created.
        """
        if not self.config.network_trace or page is None:
            return
        try:
            def _on_response(response: Any) -> None:
                try:
                    self._bundle.network_events.append({
                        "url":          response.url,
                        "method":       response.request.method,
                        "status":       response.status,
                        "timestamp_ms": int(time.time() * 1000),
                        "resource_type": response.request.resource_type,
                    })
                except Exception:
                    pass

            page.on("response", _on_response)
        except Exception as exc:
            logger.warning("evidence_capture: attach_network_listener failed — %s", exc)

    # ── Screenshot timeline ────────────────────────────────────────────────────

    def capture_step_screenshot(
        self,
        page: Any,
        step_index: int,
        action: str,
        label: Optional[str] = None,
    ) -> Optional[Dict[str, Any]]:
        """
        Capture a screenshot of the current page state and store as base64.
        Returns the entry dict or None if screenshot failed / not configured.
        """
        if not self.config.screenshot_timeline or page is None:
            return None
        try:
            png_bytes = page.screenshot(full_page=False)
            b64 = base64.b64encode(png_bytes).decode("ascii")
            entry = {
                "step_index":   step_index,
                "action":       action,
                "label":        label or action,
                "b64":          b64,
                "timestamp_ms": int(time.time() * 1000),
            }
            self._bundle.screenshots.append(entry)
            return entry
        except Exception as exc:
            logger.debug("evidence_capture: screenshot failed at step %d — %s", step_index, exc)
            return None

    # ── DOM snapshot ───────────────────────────────────────────────────────────

    def capture_dom_snapshot(
        self,
        page: Any,
        label: str = "snapshot",
    ) -> Optional[Dict[str, Any]]:
        """
        Capture the current DOM as HTML and store it in the bundle.
        Truncated to 512 KB to prevent huge payloads.
        """
        if not self.config.dom_snapshot or page is None:
            return None
        try:
            html = page.content()
            MAX_BYTES = 512 * 1024
            if len(html) > MAX_BYTES:
                html = html[:MAX_BYTES] + "\n<!-- TRUNCATED -->"
            entry = {
                "label":        label,
                "html":         html,
                "timestamp_ms": int(time.time() * 1000),
            }
            self._bundle.dom_snapshots.append(entry)
            return entry
        except Exception as exc:
            logger.debug("evidence_capture: dom_snapshot failed (%s) — %s", label, exc)
            return None

    # ── Finalise ───────────────────────────────────────────────────────────────

    def finalize(self, metadata: Optional[Dict[str, Any]] = None) -> EvidenceBundle:
        """
        Mark capture as complete and return the EvidenceBundle.
        Attaches optional extra metadata (run duration, step count, etc.).
        """
        if metadata:
            self._bundle.metadata.update(metadata)
        self._bundle.metadata.setdefault("screenshot_count", len(self._bundle.screenshots))
        self._bundle.metadata.setdefault("network_event_count", len(self._bundle.network_events))
        self._bundle.metadata.setdefault("dom_snapshot_count", len(self._bundle.dom_snapshots))
        return self._bundle


# ── Convenience builder ────────────────────────────────────────────────────────

def make_evidence_config_from_dict(d: Dict[str, Any]) -> EvidenceConfig:
    """
    Build an EvidenceConfig from an arbitrary dict (e.g. from a request body).
    Unknown keys are ignored.
    """
    return EvidenceConfig(
        screenshot_timeline=bool(d.get("screenshot_timeline", False)),
        video=bool(d.get("video", False)),
        network_trace=bool(d.get("network_trace", False)),
        dom_snapshot=bool(d.get("dom_snapshot", False)),
        action_trace=bool(d.get("action_trace", False)),
        output_dir=str(d.get("output_dir", "/tmp/vanya_evidence")),
    )
