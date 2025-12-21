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
        "steps": report_steps,
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
        context = browser.new_context(viewport={'width': 1280, 'height': 720})
        page = context.new_page()

        try:
            # Ejecuta los pasos de la prueba
            run_steps(page, steps, on_step=on_step)

            # --- MEJORA PARA EVITAR CAPTURAS EN BLANCO ---
            # 1. Esperamos a que la red esté inactiva (que no haya peticiones pendientes)
            page.wait_for_load_state("networkidle")
            
            # 2. Si es DemoQA o similar, intentamos esperar a que aparezca el cuadro de resultados
            try:
                # Espera hasta 3 segundos si aparece el div de salida
                page.wait_for_selector("#output", state="visible", timeout=3000)
            except:
                # Si no existe el selector #output, esperamos un segundo extra de cortesía
                time.sleep(1)
            # ---------------------------------------------

            # ✅ Evidencia final con la página ya renderizada
            page.screenshot(path=final_shot, full_page=False) # full_page=False suele ser más estable para capturas rápidas
            report["evidence"]["final_screenshot"] = final_shot
            report["status"] = "pass"

        except StepExecutionError as e:
            # ❌ Evidencia en caso de fallo
            try:
                # Esperamos un poco para capturar el mensaje de error en pantalla
                time.sleep(0.5)
                page.screenshot(path=fail_shot, full_page=False)
                report["evidence"]["failure_screenshot"] = fail_shot
            except Exception:
                pass

            report["error"] = str(e)
            report["status"] = "fail"

        finally:
            browser.close()

    report["duration_ms"] = int((time.time() - t0) * 1000)
    return report