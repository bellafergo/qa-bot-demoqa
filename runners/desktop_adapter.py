# runners/desktop_adapter.py
"""
Desktop Automation Adapter for Vanya.

Provides a unified interface over Win32/pywinauto desktop automation.
Degrades gracefully to a deterministic mock when:
  - running on non-Windows (Linux, macOS, CI)
  - pywinauto is not installed
  - DESKTOP_RUNNER_MOCK=1 env var is set

Architecture:
  DesktopAdapter          ← public API (used by desktop_runner.py)
      ├── PywinautoBackend   ← real Win32 via pywinauto (Windows only)
      └── MockDesktopBackend ← fully deterministic mock for tests / CI

No LLM calls are made here. Execution is fully deterministic from catalog steps.
"""
from __future__ import annotations

import base64
import logging
import os
import platform
import time
from typing import Any, Dict, List, Optional

logger = logging.getLogger("vanya.desktop_adapter")


# ── Backend detection ─────────────────────────────────────────────────────────

def _should_use_mock() -> bool:
    """Return True when real Win32 automation is unavailable or suppressed."""
    if os.getenv("DESKTOP_RUNNER_MOCK", "").lower() in ("1", "true", "yes"):
        logger.info("DesktopAdapter: mock forced via DESKTOP_RUNNER_MOCK env var")
        return True
    if platform.system() != "Windows":
        logger.info(
            "DesktopAdapter: mock active — not running on Windows (platform=%s)",
            platform.system(),
        )
        return True
    try:
        import pywinauto  # noqa: F401
        logger.info("DesktopAdapter: pywinauto available — real Win32 backend selected")
        return False
    except ImportError:
        logger.info("DesktopAdapter: mock active — pywinauto not installed (pip install pywinauto)")
        return True


# ── Mock backend ──────────────────────────────────────────────────────────────

class MockControl:
    """
    Simulates a pywinauto control handle.
    Records interactions; text is configurable for assert tests.
    """

    def __init__(self, name: str = "MockControl", text: str = "") -> None:
        self.name = name
        self._text = text
        self.interactions: List[str] = []

    def click(self) -> None:
        self.interactions.append(f"click:{self.name}")

    def set_edit_text(self, value: str) -> None:
        self.interactions.append(f"set_edit_text:{self.name}={value!r}")
        self._text = value

    def type_keys(self, keys: str, with_spaces: bool = True) -> None:
        self.interactions.append(f"type_keys:{self.name}={keys!r}")

    def select(self, value: str) -> None:
        self.interactions.append(f"select:{self.name}={value!r}")

    def window_text(self) -> str:
        return self._text or self.name

    def exists(self) -> bool:
        return True

    def is_visible(self) -> bool:
        return True

    def wait(self, state: str = "visible", timeout: int = 5) -> "MockControl":
        return self

    def __repr__(self) -> str:
        return f"MockControl({self.name!r})"


class MockDesktopBackend:
    """
    Fully deterministic mock backend.

    Behaviour:
      - All controls resolve and exist.
      - read_text returns the configured text or the target name.
      - assert_text_contains raises AssertionError only when text is genuinely absent.
      - screenshot returns a 1x1 transparent PNG as base64.
    """

    # Minimal 1×1 white PNG (base64)
    _STUB_PNG_B64 = (
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk"
        "YPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
    )

    def __init__(self) -> None:
        self._app:    Optional[MockControl] = None
        self._window: Optional[MockControl] = None
        self._controls: Dict[str, MockControl] = {}
        self._log: List[str] = []

    # ── App / window lifecycle ─────────────────────────────────────────────

    def launch_app(self, app_path: str) -> None:
        self._log.append(f"launch_app:{app_path!r}")
        self._app = MockControl(name="MockApp")
        logger.debug("MockDesktopBackend: launched app %r", app_path)

    def attach_window(self, window_title: str, timeout_s: int = 10) -> None:
        self._log.append(f"attach_window:{window_title!r}")
        self._window = MockControl(name=window_title)
        logger.debug("MockDesktopBackend: attached window %r", window_title)

    def focus_window(self, window_title: str) -> None:
        self._log.append(f"focus_window:{window_title!r}")
        logger.debug("MockDesktopBackend: focused window %r", window_title)

    # ── Control operations ────────────────────────────────────────────────

    def _get_or_create_control(self, target: str, text: str = "") -> MockControl:
        if target not in self._controls:
            self._controls[target] = MockControl(name=target, text=text)
        return self._controls[target]

    def click(self, target: str) -> None:
        ctrl = self._get_or_create_control(target)
        ctrl.click()
        self._log.append(f"click:{target!r}")

    def input_text(self, target: str, value: str) -> None:
        ctrl = self._get_or_create_control(target)
        ctrl.set_edit_text(value)
        self._log.append(f"input_text:{target!r}={value!r}")

    def type_keys(self, target: str, keys: str) -> None:
        ctrl = self._get_or_create_control(target)
        ctrl.type_keys(keys)
        self._log.append(f"type_keys:{target!r}={keys!r}")

    def select(self, target: str, value: str) -> None:
        ctrl = self._get_or_create_control(target)
        ctrl.select(value)
        self._log.append(f"select:{target!r}={value!r}")

    def read_text(self, target: str) -> str:
        ctrl = self._get_or_create_control(target)
        return ctrl.window_text()

    def assert_text_contains(self, target: str, expected: str) -> None:
        text = self.read_text(target)
        if expected not in text:
            raise AssertionError(
                f"assert_text_contains failed: expected {expected!r} in {text!r} "
                f"(control: {target!r})"
            )
        self._log.append(f"assert_text_contains:{target!r} ok")

    def assert_exists(self, target: str) -> None:
        ctrl = self._get_or_create_control(target)
        if not ctrl.exists():
            raise AssertionError(f"assert_exists failed: control {target!r} not found")
        self._log.append(f"assert_exists:{target!r} ok")

    def wait_for(self, target: str, timeout_ms: int = 5000) -> None:
        self._get_or_create_control(target)
        self._log.append(f"wait_for:{target!r}")

    def screenshot(self) -> Optional[str]:
        return self._STUB_PNG_B64

    def set_control_text(self, target: str, text: str) -> None:
        """Test helper: preset the text a control will return."""
        self._get_or_create_control(target, text=text)

    @property
    def call_log(self) -> List[str]:
        return list(self._log)


# ── pywinauto backend ─────────────────────────────────────────────────────────

class PywinautoBackend:
    """
    Real Win32 automation via pywinauto.

    Only instantiated on Windows when pywinauto is available.
    Each public method converts the string target to a pywinauto control
    via DesktopTargetResolver (see desktop_target.py).
    """

    def __init__(self) -> None:
        import pywinauto  # raises ImportError if unavailable
        from pywinauto.application import Application
        self._pywinauto = pywinauto
        self._Application = Application
        self._app: Any = None
        self._window: Any = None

    # ── App / window lifecycle ─────────────────────────────────────────────

    def launch_app(self, app_path: str) -> None:
        self._app = self._Application(backend="win32").start(app_path)
        logger.info("PywinautoBackend: launched %r", app_path)

    def attach_window(self, window_title: str, timeout_s: int = 10) -> None:
        if self._app is None:
            # No launch_app was called — connect to an existing process by window title.
            # Required for TC-POS-003 and any test that attaches without launching first.
            logger.info("PywinautoBackend: no app handle, connecting to existing window %r", window_title)
            self._app = self._Application(backend="win32").connect(title=window_title)
        self._window = self._app.window(title=window_title)
        self._window.wait("exists ready", timeout=timeout_s)
        logger.info("PywinautoBackend: attached window %r", window_title)

    def focus_window(self, window_title: str) -> None:
        if self._window is not None:
            self._window.set_focus()

    # ── Helpers ────────────────────────────────────────────────────────────

    def _resolve(self, target: str) -> Any:
        """Resolve a string target to a pywinauto control via the window."""
        if self._window is None:
            raise RuntimeError("No window attached. Call attach_window first.")
        from runners.desktop_target import resolve_desktop_target
        ctrl, _used, _resolved, _meta = resolve_desktop_target(self._window, target)
        return ctrl

    # ── Control operations ─────────────────────────────────────────────────

    def click(self, target: str) -> None:
        self._resolve(target).click_input()

    def input_text(self, target: str, value: str) -> None:
        ctrl = self._resolve(target)
        try:
            ctrl.set_edit_text(value)
        except Exception:
            # Fallback for legacy controls (VB6 TextBox, etc.) that ignore WM_SETTEXT.
            # click_input focuses the control, then type_keys simulates real keystrokes.
            logger.debug(
                "PywinautoBackend: set_edit_text failed for %r, falling back to type_keys", target
            )
            ctrl.click_input()
            ctrl.type_keys("^a{DELETE}", with_spaces=False)
            ctrl.type_keys(value, with_spaces=True)

    def type_keys(self, target: str, keys: str) -> None:
        ctrl = self._resolve(target)
        ctrl.type_keys(keys, with_spaces=True)

    def select(self, target: str, value: str) -> None:
        ctrl = self._resolve(target)
        ctrl.select(value)

    def read_text(self, target: str) -> str:
        return self._resolve(target).window_text()

    def assert_text_contains(self, target: str, expected: str) -> None:
        text = self.read_text(target)
        if expected not in text:
            raise AssertionError(
                f"assert_text_contains failed: expected {expected!r} in {text!r}"
            )

    def assert_exists(self, target: str) -> None:
        ctrl = self._resolve(target)
        if not ctrl.exists():
            raise AssertionError(f"assert_exists failed: {target!r} not found")

    def wait_for(self, target: str, timeout_ms: int = 5000) -> None:
        ctrl = self._resolve(target)
        ctrl.wait("exists visible", timeout=timeout_ms / 1000)

    def screenshot(self) -> Optional[str]:
        """Capture a screenshot of the active window as base64 PNG."""
        try:
            import io
            from PIL import ImageGrab
            img = ImageGrab.grab()
            buf = io.BytesIO()
            img.save(buf, format="PNG")
            return base64.b64encode(buf.getvalue()).decode("ascii")
        except Exception as exc:
            logger.warning("PywinautoBackend: screenshot failed — %s", exc)
            return None


# ── Public DesktopAdapter ─────────────────────────────────────────────────────

class DesktopAdapter:
    """
    Unified desktop automation adapter.

    Auto-selects PywinautoBackend (real Win32) or MockDesktopBackend (CI/non-Windows).
    The caller never needs to check which backend is active.

    Usage:
        adapter = DesktopAdapter()
        adapter.launch_app("C:/POS/app.exe")
        adapter.attach_window("POS Main Window")
        adapter.input_text("username_field", "cashier01")
        adapter.click("login_button")
        adapter.assert_text_contains("status_label", "Venta realizada")
    """

    def __init__(self, *, use_mock: Optional[bool] = None) -> None:
        force_mock = use_mock if use_mock is not None else _should_use_mock()
        if force_mock:
            self._backend: Any = MockDesktopBackend()
            self.is_mock = True
            logger.info("DesktopAdapter: backend=MockDesktopBackend (deterministic, no real UI)")
        else:
            self._backend = PywinautoBackend()
            self.is_mock = False
            logger.info("DesktopAdapter: backend=PywinautoBackend (real Win32 automation)")

    # ── Delegate all methods to backend ──────────────────────────────────

    def launch_app(self, app_path: str) -> None:
        self._backend.launch_app(app_path)

    def attach_window(self, window_title: str, timeout_s: int = 10) -> None:
        self._backend.attach_window(window_title, timeout_s=timeout_s)

    def focus_window(self, window_title: str) -> None:
        self._backend.focus_window(window_title)

    def click(self, target: str) -> None:
        self._backend.click(target)

    def input_text(self, target: str, value: str) -> None:
        self._backend.input_text(target, value)

    def type_keys(self, target: str, keys: str) -> None:
        self._backend.type_keys(target, keys)

    def select(self, target: str, value: str) -> None:
        self._backend.select(target, value)

    def read_text(self, target: str) -> str:
        return self._backend.read_text(target)

    def assert_text_contains(self, target: str, expected: str) -> None:
        self._backend.assert_text_contains(target, expected)

    def assert_exists(self, target: str) -> None:
        self._backend.assert_exists(target)

    def wait_for(self, target: str, timeout_ms: int = 5000) -> None:
        self._backend.wait_for(target, timeout_ms=timeout_ms)

    def screenshot(self) -> Optional[str]:
        return self._backend.screenshot()

    def set_control_text(self, target: str, text: str) -> None:
        """Test helper: only available on mock backend."""
        if self.is_mock:
            self._backend.set_control_text(target, text)

    @property
    def call_log(self) -> List[str]:
        """Test helper: call log from mock backend."""
        if self.is_mock:
            return self._backend.call_log
        return []
