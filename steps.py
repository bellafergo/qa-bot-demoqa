from typing import Any, Dict, List, Optional, Callable
import time
from playwright.sync_api import Page

class StepExecutionError(Exception):
    pass

# on_step(i, step, status, duration_ms, error_message)
OnStepFn = Optional[Callable[[int, Dict[str, Any], str, int, Optional[str]], None]]

def run_steps(page: Page, steps: List[Dict[str, Any]], on_step: OnStepFn = None) -> None:
    for i, step in enumerate(steps, start=1):
        action = step.get("action")
        t0 = time.time()

        try:
            if action == "goto":
                page.goto(step["url"], wait_until="domcontentloaded")

            elif action == "wait_for_selector":
                page.wait_for_selector(
                    step["selector"],
                    timeout=int(step.get("timeout_ms", 10000))
                )

            elif action == "fill":
                page.fill(step["selector"], str(step["value"]))

            elif action == "click":
                page.click(step["selector"])

            elif action == "assert_visible":
                page.wait_for_selector(
                    step["selector"],
                    timeout=int(step.get("timeout_ms", 10000))
                )

            elif action == "assert_text_contains":
                actual = page.locator(step["selector"]).inner_text()
                expected = str(step["text"])
                if expected not in actual:
                    raise StepExecutionError(f"Expected '{expected}' in '{actual}'")

            else:
                raise StepExecutionError(f"Acción no soportada: {action}")

            duration_ms = int((time.time() - t0) * 1000)
            if on_step:
                on_step(i, step, "pass", duration_ms, None)

        except Exception as e:
            duration_ms = int((time.time() - t0) * 1000)
            err_msg = f"Error en paso #{i}: {step}. Causa: {e}"

            if on_step:
                on_step(i, step, "fail", duration_ms, err_msg)

            raise StepExecutionError(err_msg) from e