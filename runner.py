import os
import time
import base64
from datetime import datetime
from typing import Any, Dict, List, Optional
from playwright.sync_api import sync_playwright

class StepExecutionError(Exception):
    pass

def execute_test(steps: List[Dict[str, Any]], headless: bool = True) -> Dict[str, Any]:
    t0 = time.time()
    screenshot_b64 = None
    report_steps = []

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)
        # Usamos un User Agent real para que Google no nos bloquee
        context = browser.new_context(
            viewport={'width': 1280, 'height': 720},
            user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
        )
        page = context.new_page()

        try:
            for i, step in enumerate(steps):
                action = step.get("action")
                url = step.get("url")
                selector = step.get("selector")
                value = step.get("value") or step.get("text")

                try:
                    if action == "goto":
                        # ✅ Forzamos espera a que la red esté quieta
                        print(f"Navegando a: {url}")
                        page.goto(url, wait_until="networkidle", timeout=60000)
                        page.wait_for_load_state("domcontentloaded")
                    
                    elif action in ["fill", "press"]:
                        page.wait_for_selector(selector, state="visible", timeout=15000)
                        if action == "fill":
                            page.fill(selector, value)
                        page.keyboard.press("Enter")
                        # Esperamos a que la búsqueda procese
                        time.sleep(2) 
                        page.wait_for_load_state("networkidle")

                    report_steps.append({"step": i, "action": action, "status": "pass"})
                except Exception as e:
                    report_steps.append({"step": i, "action": action, "status": "fail", "error": str(e)})
                    raise StepExecutionError(f"Error en {action}: {str(e)}")

            # ✅ CAPTURA FINAL: Espera extra de 2 segundos antes de la foto
            time.sleep(2) 
            img_bytes = page.screenshot(full_page=False)
            screenshot_b64 = base64.b64encode(img_bytes).decode('utf-8')
            status = "pass"

        except StepExecutionError as e:
            # Si falla, intentamos capturar lo que haya en pantalla
            try:
                img_bytes = page.screenshot()
                screenshot_b64 = base64.b64encode(img_bytes).decode('utf-8')
            except: pass
            status = "fail"
        finally:
            browser.close()

    return {
        "status": status,
        "steps": report_steps,
        "screenshot_b64": screenshot_b64,
        "duration_ms": int((time.time() - t0) * 1000)
    }