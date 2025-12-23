import time
import base64
import uuid
from typing import Any, Dict, List, Optional

from playwright.sync_api import sync_playwright, TimeoutError as PlaywrightTimeoutError


class StepExecutionError(Exception):
    pass


def _ms() -> int:
    return int(time.time() * 1000)


def _safe_str(v: Any, max_len: int = 120) -> str:
    s = "" if v is None else str(v)
    return s if len(s) <= max_len else s[:max_len] + "..."


def _pick_locator(page, step: Dict[str, Any]):
    """
    Locator fallback:
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


def _ensure_goto(steps: List[Dict[str, Any]], default_url: Optional[str] = None) -> List[Dict[str, Any]]:
    """
    Garantiza que exista un paso goto al inicio.
    - Si no hay goto y default_url existe, lo inserta.
    - Si no hay goto y default_url NO existe, no hace nada.
    """
    if not steps:
        return steps

    has_goto = any(str(s.get("action", "")).strip().lower() == "goto" for s in steps)
    if has_goto:
        return steps

    if default_url:
        steps.insert(0, {"action": "goto", "url": default_url})

    return steps


def execute_test(steps: List[Dict[str, Any]], headless: bool = True) -> Dict[str, Any]:
    t0 = time.time()

    # ✅ SIEMPRE inicializa (evita NameError y resultados incompletos)
    screenshot_b64: Optional[str] = None
    report_steps: List[Dict[str, Any]] = []
    logs: List[str] = []
    evidence_id = f"EV-{uuid.uuid4().hex[:10]}"

    # Asegura navegación inicial si el modelo olvidó goto.
    # Aquí no tenemos base_url, así que confiamos en que venga un goto con url
    # o que app.py lo inserte antes de llamar al runner.
    steps = _ensure_goto(steps, default_url=None)

    status = "fail"
    error_msg: Optional[str] = None

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)
        context = browser.new_context(
            viewport={"width": 1280, "height": 720},
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/119.0.0.0 Safari/537.36"
            ),
        )
        page = context.new_page()

        def take_screenshot_robust(retries: int = 3, delay_ms: int = 700) -> Optional[str]:
            """
            Evita screenshots en blanco:
            - espera DOM + body
            - intenta networkidle pero no revienta si falla
            - full_page=True
            - reintenta si PNG sale demasiado pequeño
            """
            for _ in range(retries):
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
                        return base64.b64encode(img_bytes).decode("utf-8")
                except Exception:
                    pass
            return None

        try:
            logs.append(f"Runner start. steps={len(steps)} headless={headless}")

            for i, step in enumerate(steps, start=1):
                step_t0 = _ms()

                action = (step.get("action") or "").strip().lower()
                url = (step.get("url") or "").strip() if step.get("url") else None
                selector = step.get("selector")
                text = step.get("text")
                role = step.get("role")
                value = step.get("value")
                timeout_ms = int(step.get("timeout_ms") or 15000)

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
                        if not url:
                            raise StepExecutionError("goto requiere 'url'")
                        logs.append(f"Goto: {url}")
                        page.goto(url, wait_until="domcontentloaded", timeout=60000)
                        try:
                            page.wait_for_load_state("networkidle", timeout=15000)
                        except Exception:
                            pass

                    # -------------------------
                    # WAIT
                    # -------------------------
                    elif action in ("wait_for", "wait_for_selector"):
                        if action == "wait_for_selector" and not (selector or "").strip():
                            raise StepExecutionError("wait_for_selector requiere 'selector'")
                        loc = _pick_locator(page, step)
                        logs.append(
                            f"Wait visible: {(_safe_str(selector) or _safe_str(role))} {_safe_str(text)} "
                            f"(timeout={timeout_ms}ms)"
                        )
                        loc.wait_for(state="visible", timeout=timeout_ms)

                    # -------------------------
                    # INPUT
                    # -------------------------
                    elif action == "fill":
                        if value is None and text is None:
                            raise StepExecutionError("fill requiere 'value'")
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
                        key = (text or value or "Enter")
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
                        expected = (text or "").strip()
                        if not expected:
                            raise StepExecutionError("assert_text_contains requiere 'text'")
                        loc = _pick_locator(page, step)
                        logs.append(
                            f"Assert text contains: {(_safe_str(selector) or _safe_str(text))} has '{_safe_str(expected, 80)}'"
                        )
                        loc.wait_for(state="visible", timeout=timeout_ms)
                        actual = loc.inner_text(timeout=timeout_ms) or ""
                        if expected.lower() not in actual.lower():
                            raise StepExecutionError(
                                f"Texto esperado no encontrado. Esperado: '{expected}'. Actual: '{_safe_str(actual, 160)}'"
                            )

                    # -------------------------
                    # SLEEP
                    # -------------------------
                    elif action == "wait_ms":
                        ms = int(value or text or timeout_ms or 1000)
                        logs.append(f"Wait ms: {ms}")
                        time.sleep(ms / 1000.0)

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

            # Screenshot final (robusto)
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
        },
    }