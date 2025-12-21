import time
import base64
from typing import Any, Dict, List, Optional

from playwright.sync_api import sync_playwright, TimeoutError as PlaywrightTimeoutError


class StepExecutionError(Exception):
    pass


def _ms() -> int:
    return int(time.time() * 1000)


def execute_test(steps: List[Dict[str, Any]], headless: bool = True) -> Dict[str, Any]:
    t0 = time.time()
    screenshot_b64: Optional[str] = None
    report_steps: List[Dict[str, Any]] = []
    logs: List[str] = []

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)

        # Nota: User-Agent ayuda un poco, pero NO garantiza evitar CAPTCHA en Google.
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
            - espera a DOM + networkidle
            - espera a body
            - usa full_page=True
            - reintenta si el PNG sale demasiado pequeño (típico de blanco/bug)
            """
            for _ in range(retries):
                try:
                    # Asegura que haya algo renderizado
                    page.wait_for_load_state("domcontentloaded", timeout=60000)
                    page.wait_for_load_state("networkidle", timeout=60000)
                    page.wait_for_selector("body", state="attached", timeout=15000)
                    page.wait_for_timeout(delay_ms)

                    img_bytes = page.screenshot(full_page=True)
                    # Si el PNG es muy pequeño suele ser blanco/bug -> reintenta
                    if img_bytes and len(img_bytes) > 15000:  # ~15KB
                        return base64.b64encode(img_bytes).decode("utf-8")
                except Exception:
                    pass
            return None

        status = "fail"
        error_msg: Optional[str] = None

        try:
            for i, step in enumerate(steps, start=1):
                step_t0 = _ms()

                action = (step.get("action") or "").strip()
                url = step.get("url")
                selector = step.get("selector")
                value = step.get("value")
                text = step.get("text")
                timeout_ms = int(step.get("timeout_ms") or 15000)

                step_result: Dict[str, Any] = {
                    "i": i,
                    "action": action,
                    "selector": selector,
                    "url": url,
                    "status": "pass",
                    "duration_ms": 0,
                    "error": None,
                }

                try:
                    if action == "goto":
                        if not url:
                            raise StepExecutionError("goto requiere 'url'")
                        logs.append(f"Goto: {url}")
                        page.goto(url, wait_until="domcontentloaded", timeout=60000)
                        # espera extra por estabilidad
                        page.wait_for_load_state("networkidle", timeout=60000)

                    elif action == "wait_for_selector":
                        if not selector:
                            raise StepExecutionError("wait_for_selector requiere 'selector'")
                        logs.append(f"Wait for selector: {selector}")
                        page.wait_for_selector(selector, state="visible", timeout=timeout_ms)

                    elif action == "fill":
                        if not selector:
                            raise StepExecutionError("fill requiere 'selector'")
                        if value is None and text is None:
                            raise StepExecutionError("fill requiere 'value'")
                        fill_value = value if value is not None else text
                        logs.append(f"Fill: {selector} = {str(fill_value)[:80]}")
                        page.wait_for_selector(selector, state="visible", timeout=timeout_ms)
                        page.fill(selector, str(fill_value))

                    elif action == "click":
                        if not selector:
                            raise StepExecutionError("click requiere 'selector'")
                        logs.append(f"Click: {selector}")
                        page.wait_for_selector(selector, state="visible", timeout=timeout_ms)
                        page.click(selector, timeout=timeout_ms)
                        # a veces el click dispara cambios, damos chance
                        try:
                            page.wait_for_load_state("networkidle", timeout=30000)
                        except Exception:
                            pass

                    elif action == "press":
                        # press puede ser sobre elemento (selector) o global (keyboard)
                        key = (text or value or "Enter")
                        logs.append(f"Press: {key}" + (f" on {selector}" if selector else ""))
                        if selector:
                            page.wait_for_selector(selector, state="visible", timeout=timeout_ms)
                            page.focus(selector)
                        page.keyboard.press(str(key))
                        # para Enter, dejamos que navegue
                        try:
                            page.wait_for_load_state("networkidle", timeout=60000)
                        except Exception:
                            pass

                    elif action == "assert_visible":
                        if not selector:
                            raise StepExecutionError("assert_visible requiere 'selector'")
                        logs.append(f"Assert visible: {selector}")
                        page.wait_for_selector(selector, state="visible", timeout=timeout_ms)

                    elif action == "assert_text_contains":
                        if not selector:
                            raise StepExecutionError("assert_text_contains requiere 'selector'")
                        if text is None:
                            raise StepExecutionError("assert_text_contains requiere 'text'")
                        expected = str(text)
                        logs.append(f"Assert text contains: {selector} has '{expected[:80]}'")
                        page.wait_for_selector(selector, state="visible", timeout=timeout_ms)
                        actual = page.inner_text(selector, timeout=timeout_ms)
                        if expected not in actual:
                            raise StepExecutionError(
                                f"Texto esperado no encontrado. Esperado: '{expected}'"
                            )

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

                except Exception as e:
                    step_result["status"] = "fail"
                    step_result["error"] = str(e)
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
            # Screenshot en fallo (robusto)
            screenshot_b64 = screenshot_b64 or take_screenshot_robust()
            status = "fail"

        finally:
            browser.close()

    return {
        "status": status,
        "error": error_msg,
        "steps": report_steps,
        "logs": logs,
        "screenshot_b64": screenshot_b64,
        "duration_ms": int((time.time() - t0) * 1000),
        "meta": {
            "headless": headless,
            "steps_count": len(steps),
        },
    }