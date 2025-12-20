import os
import time
from datetime import datetime
from typing import Any, Dict, List, Optional

from playwright.sync_api import sync_playwright
from steps import run_steps, StepExecutionError


def execute_test(steps: List[Dict[str, Any]], headless: bool = True) -> Dict[str, Any]:
    os.makedirs("evidence", exist_ok=True)
    ts = datetime.now().strftime("%Y%m%d_%H%M%S")
    run_id = f"run_{ts}"

    started_at = datetime.now().isoformat()
    t0 = time.time()

    final_shot = f"evidence/{run_id}_final.png"
    fail_shot = f"evidence/{run_id}_fail.png"

    report_steps: List[Dict[str, Any]] = []

    def on_step(i: int, step: Dict[str, Any], status: str, duration_ms: int, error: Optional[str]):
        report_steps.append({
            "i": i,
            "action": step.get("action"),
            "selector": step.get("selector"),
            "url": step.get("url"),
            "status": status,
            "duration_ms": duration_ms,
            "error": error,
        })

    report: Dict[str, Any] = {
        "run_id": run_id,
        "status": "fail",
        "started_at": started_at,
        "duration_ms": None,
        "error": None,
        "steps": report_steps,  # 👈 desglose por paso
        "evidence": {
            "final_screenshot": None,
            "failure_screenshot": None,
        },
        "meta": {
            "headless": headless,
            "steps_count": len(steps),
        },
    }

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)
        page = browser.new_page()

        try:
            # Ejecuta y va llenando report_steps con el callback
            run_steps(page, steps, on_step=on_step)

            # ✅ evidencia al final aunque pase
            page.screenshot(path=final_shot, full_page=True)
            report["evidence"]["final_screenshot"] = final_shot

            report["status"] = "pass"

        except StepExecutionError as e:
            # ❌ evidencia cuando falla
            try:
                page.screenshot(path=fail_shot, full_page=True)
                report["evidence"]["failure_screenshot"] = fail_shot
            except Exception:
                pass

            report["error"] = str(e)
            report["status"] = "fail"

        finally:
            browser.close()

    report["duration_ms"] = int((time.time() - t0) * 1000)
    return report