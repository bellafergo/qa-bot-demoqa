# runner.py
from __future__ import annotations

import time
import base64
import uuid
from typing import Any, Dict, List, Optional
from urllib.parse import urljoin

from playwright.sync_api import sync_playwright, TimeoutError as PlaywrightTimeoutError


# ============================================================
# Errors
# ============================================================
class StepExecutionError(Exception):
    pass


# ============================================================
# Helpers
# ============================================================
def _ms() -> int:
    return int(time.time() * 1000)


def _safe_str(v: Any, max_len: int = 140) -> str:
    s = "" if v is None else str(v)
    return s if len(s) <= max_len else s[:max_len] + "..."


def _normalize_url(url: Optional[str], base_url: Optional[str]) -> str:
    u = (url or "").strip()
    if not u:
        return (base_url or "").strip()

    if u.startswith("http://") or u.startswith("https://"):
        return u

    b = (base_url or "").strip()
    if not b:
        return u

    # urljoin maneja /path, ?q= etc
    return urljoin(b if b.endswith("/") else b + "/", u)


def _pick_locator(page, step: Dict[str, Any]):
    """
    Locator fallback (robusto):
      1) selector (CSS/XPath)
      2) role + text (get_by_role)
      3) text (get_by_text)
    """
    selector = (step.get("selector") or "").strip()
    text = (step.get("text") or "").strip()
    role = (step.get("role") or "").strip()

    if selector:
        return page.locator(selector)

    if role and text:
        return page.get_by_role(role, name=text)

    if text:
        return page.get_by_text(text, exact=False)

    raise StepExecutionError("No locator: provee selector o text o role+text")


def _ensure_goto(steps: List[Dict[str, Any]], base_url: Optional[str]) -> List[Dict[str, Any]]:
    """
    Garantiza goto al inicio.
    - Si ya existe goto -> no toca
    - Si no existe goto y hay base_url -> inserta goto(base_url)
    - Si no existe goto y NO hay base_url -> deja igual (pero puede fallar después si requieren navegación)
    """
    if not steps:
        return steps

    has_goto = any(str(s.get("action", "")).strip().lower() == "goto" for s in steps)
    if has_goto:
        return steps

    if base_url and base_url.strip():
        steps.insert(0, {"action": "goto", "url": base_url.strip()})
    return steps


def _b64_png(img_bytes: bytes) -> str:
    return base64.b64encode(img_bytes).decode("utf-8")


# ============================================================
# Runner
# ============================================================
def execute_test(
    steps: List[Dict[str, Any]],
    base_url: Optional[str] = None,
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
) -> Dict[str, Any]:
    """
    Ejecuta steps Playwright.
    Soporta acciones:
      goto, fill, click, press, assert_visible, assert_text_contains, wait_ms
    Compatibilidad:
      wait_for, wait_for_selector -> treated as wait visible

    Retorna:
      {
        status: "passed" | "fail",
        error: str | None,
        evidence_id: str,
        steps: [...],
        logs: [...],
        screenshot_b64: str | None,
        duration_ms: int,
        meta: { headless, steps_count, base_url }
      }
    """
    t0 = time.time()

    screenshot_b64: Optional[str] = None
    report_steps: List[Dict[str, Any]] = []
    logs: List[str] = []
    evidence_id = f"EV-{uuid.uuid4().hex[:10]}"

    if not isinstance(steps, list) or not steps:
        return {
            "status": "fail",
            "error": "steps vacío o inválido",
            "evidence_id": evidence_id,
            "steps": [],
            "logs": ["Runner error: steps vacío o inválido"],
            "screenshot_b64": None,
            "duration_ms": int((time.time() - t0) * 1000),
            "meta": {
                "headless": headless,
                "steps_count": 0,
                "base_url": (base_url or None),
            },
        }

    base_url_norm = (base_url or "").strip() or None
    steps = _ensure_goto(steps, base_url_norm)

    status = "fail"
    error_msg: Optional[str] = None

    viewport = viewport or {"width": 1280, "height": 720}

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)
        context = browser.new_context(
            viewport=viewport,
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/123.0.0.0 Safari/537.36"
            ),
        )
        page = context.new_page()

        def take_screenshot_robust(retries: int = 3, delay_ms: int = 700) -> Optional[str]:
            """
            Evita screenshots en blanco:
            - espera domcontentloaded
            - intenta networkidle sin romper
            - espera body attached
            - espera un poco extra
            - reintenta si PNG muy chico
            """
            for attempt in range(1, retries + 1):
                try:
                    page.wait_for_load_state("domcontentloaded", timeout=60000)
                    try:
                        page.wait_for_load_state("networkidle", timeout=15000)
                    except Exception:
                        pass
                    page.wait_for_selector("body", state="attached", timeout=15000)
                    page.wait_for_timeout(delay_ms)

                    img_bytes = page.screenshot(full_page=True)
                    if img_bytes and len(img_bytes) > 2000:
                        return _b64_png(img_bytes)

                    logs.append(f"[screenshot] attempt={attempt} too-small-or-empty")
                except Exception as e:
                    logs.append(f"[screenshot] attempt={attempt} failed: {type(e).__name__}: {_safe_str(e)}")
            return None

        try:
            logs.append(f"Runner start. steps={len(steps)} headless={headless} base_url={base_url_norm}")

            for i, step in enumerate(steps, start=1):
                step_t0 = _ms()

                action = (step.get("action") or "").strip().lower()
                url = (step.get("url") or "").strip() if step.get("url") else None
                selector = step.get("selector")
                text = step.get("text")
                role = step.get("role")
                value = step.get("value")
                timeout_ms = int(step.get("timeout_ms") or 15000)

                # alias compatibility
                if action in ("wait_for", "wait_for_selector"):
                    action = "wait_visible"

                step_result: Dict[str, Any] = {
                    "i": i,
                    "action": action,
                    "selector": selector,
                    "text": text,
                    "role": role,
                    "url": url,
                    "status": "pass",
                    "duration_ms": 0,
                    "error": None,
                }

                try:
                    # -------------------------
                    # NAV
                    # -------------------------
                    if action == "goto":
                        resolved = _normalize_url(url, base_url_norm)
                        if not resolved:
                            raise StepExecutionError("goto requiere 'url' o base_url")
                        logs.append(f"Goto: {resolved}")
                        page.goto(resolved, wait_until="domcontentloaded", timeout=60000)
                        try:
                            page.wait_for_load_state("networkidle", timeout=15000)
                        except Exception:
                            pass

                    # -------------------------
                    # WAIT (visible)
                    # -------------------------
                    elif action == "wait_visible":
                        loc = _pick_locator(page, step)
                        logs.append(
                            f"Wait visible: {(_safe_str(selector) or _safe_str(role))} {_safe_str(text)} (timeout={timeout_ms}ms)"
                        )
                        loc.wait_for(state="visible", timeout=timeout_ms)

                    # -------------------------
                    # INPUT
                    # -------------------------
                    elif action == "fill":
                        if value is None and text is None:
                            raise StepExecutionError("fill requiere 'value' (o 'text' como fallback)")
                        fill_value = value if value is not None else text
                        loc = _pick_locator(page, step)
                        logs.append(f"Fill: {(_safe_str(selector) or _safe_str(text))} = {_safe_str(fill_value, 80)}")
                        loc.wait_for(state="visible", timeout=timeout_ms)
                        loc.fill(str(fill_value), timeout=timeout_ms)

                    # -------------------------
                    # CLICK
                    # -------------------------
                    elif action == "click":
                        loc = _pick_locator(page, step)
                        logs.append(f"Click: {(_safe_str(selector) or _safe_str(text))}")
                        loc.wait_for(state="visible", timeout=timeout_ms)
                        loc.click(timeout=timeout_ms)
                        try:
                            page.wait_for_load_state("networkidle", timeout=15000)
                        except Exception:
                            pass

                    # -------------------------
                    # PRESS
                    # -------------------------
                    elif action == "press":
                        key = (value or text or "Enter")
                        logs.append(
                            f"Press: {key}" + (f" on {(_safe_str(selector) or _safe_str(text))}" if (selector or text or role) else "")
                        )
                        try:
                            loc = _pick_locator(page, step)
                            loc.wait_for(state="visible", timeout=timeout_ms)
                            loc.press(str(key), timeout=timeout_ms)
                        except Exception:
                            page.keyboard.press(str(key))

                        try:
                            page.wait_for_load_state("networkidle", timeout=15000)
                        except Exception:
                            pass

                    # -------------------------
                    # ASSERTS
                    # -------------------------
                    elif action == "assert_visible":
                        loc = _pick_locator(page, step)
                        logs.append(f"Assert visible: {(_safe_str(selector) or _safe_str(text))}")
                        loc.wait_for(state="visible", timeout=timeout_ms)

                    elif action == "assert_text_contains":
                        expected = str(value or text or "").strip()
                        if not expected:
                            raise StepExecutionError("assert_text_contains requiere 'text' o 'value' esperado")
                        loc = _pick_locator(page, step)
                        logs.append(
                            f"Assert text contains: {(_safe_str(selector) or _safe_str(text))} has '{_safe_str(expected, 80)}'"
                        )
                        loc.wait_for(state="visible", timeout=timeout_ms)
                        actual = loc.inner_text(timeout=timeout_ms) or ""
                        if expected.lower() not in actual.lower():
                            raise StepExecutionError(
                                f"Texto esperado no encontrado. Esperado: '{expected}'. Actual: '{_safe_str(actual, 220)}'"
                            )

                    # -------------------------
                    # SLEEP
                    # -------------------------
                    elif action == "wait_ms":
                        # prioridad: timeout_ms explícito del step > value > text > 1000
                        ms = int(step.get("timeout_ms") or value or text or 1000)
                        logs.append(f"Wait ms: {ms}")
                        page.wait_for_timeout(ms)

                    else:
                        raise StepExecutionError(f"Acción no soportada: {action}")

                except PlaywrightTimeoutError as e:
                    step_result["status"] = "fail"
                    step_result["error"] = f"Timeout: {str(e)}"
                    raise StepExecutionError(step_result["error"])

                except StepExecutionError as e:
                    step_result["status"] = "fail"
                    step_result["error"] = str(e)
                    raise

                except Exception as e:
                    step_result["status"] = "fail"
                    step_result["error"] = f"{type(e).__name__}: {str(e)}"
                    raise

                finally:
                    step_result["duration_ms"] = _ms() - step_t0
                    report_steps.append(step_result)

            screenshot_b64 = take_screenshot_robust()
            status = "passed"

        except Exception as e:
            error_msg = str(e)
            logs.append(f"ERROR: {error_msg}")
            screenshot_b64 = screenshot_b64 or take_screenshot_robust()
            status = "fail"

        finally:
            try:
                context.close()
            except Exception:
                pass
            try:
                browser.close()
            except Exception:
                pass

    return {
        "status": status,
        "error": error_msg,
        "evidence_id": evidence_id,
        "steps": report_steps,
        "logs": logs,
        "screenshot_b64": screenshot_b64,
        "duration_ms": int((time.time() - t0) * 1000),
        "meta": {
            "headless": headless,
            "steps_count": len(steps),
            "base_url": base_url_norm,
        },
    }