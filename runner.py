import os
import time
import base64
from datetime import datetime
from typing import Any, Dict, List, Optional
from playwright.sync_api import sync_playwright

# Excepción personalizada para el control de flujo
class StepExecutionError(Exception):
    pass

def execute_test(steps: List[Dict[str, Any]], headless: bool = True) -> Dict[str, Any]:
    """
    Ejecuta una secuencia de pasos de Playwright y devuelve un reporte con captura en Base64.
    """
    t0 = time.time()
    started_at = datetime.now().isoformat()
    screenshot_b64 = None
    report_steps = []

    with sync_playwright() as p:
        # Lanzamiento del navegador (Chromium)
        browser = p.chromium.launch(headless=headless)
        context = browser.new_context(
            viewport={'width': 1280, 'height': 720},
            user_agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
        )
        page = context.new_page()

        try:
            for i, step in enumerate(steps):
                action = step.get("action")
                selector = step.get("selector")
                value = step.get("value") or step.get("text")
                url = step.get("url")
                
                step_start = time.time()
                try:
                    # --- LÓGICA DE ACCIONES CORREGIDA ---
                    if action == "goto":
                        page.goto(url, wait_until="networkidle", timeout=30000)
                    
                    elif action == "wait_for_selector":
                        page.wait_for_selector(selector, state="visible", timeout=10000)
                    
                    elif action == "fill":
                        page.wait_for_selector(selector, state="visible", timeout=10000)
                        page.fill(selector, "") # Limpia primero
                        page.fill(selector, value)
                        # ✅ TRUCO PARA GOOGLE: Presionamos Enter tras escribir
                        page.keyboard.press("Enter")
                        page.wait_for_load_state("networkidle")
                    
                    elif action == "click":
                        page.wait_for_selector(selector, state="visible", timeout=10000)
                        page.click(selector)
                        page.wait_for_load_state("networkidle")
                    
                    elif action == "assert_text_contains":
                        page.wait_for_selector(selector, timeout=10000)
                        content = page.text_content(selector)
                        if value.lower() not in content.lower():
                            raise Exception(f"Texto '{value}' no encontrado. Encontrado: '{content}'")

                    # Registro de paso exitoso
                    report_steps.append({
                        "step": i,
                        "action": action,
                        "status": "pass",
                        "duration_ms": int((time.time() - step_start) * 1000)
                    })

                except Exception as e:
                    # Registro de paso fallido
                    report_steps.append({
                        "step": i,
                        "action": action,
                        "status": "fail",
                        "error": str(e)
                    })
                    raise StepExecutionError(f"Fallo en paso {i} ({action}): {str(e)}")

            # ✅ ÉXITO: Captura de pantalla final
            page.wait_for_load_state("networkidle")
            time.sleep(1) # Pausa de cortesía para renderizado final
            img_bytes = page.screenshot(full_page=False)
            screenshot_b64 = base64.b64encode(img_bytes).decode('utf-8')
            status = "pass"
            error_msg = None

        except StepExecutionError as e:
            # ❌ FALLO: Captura de pantalla del error
            try:
                img_bytes = page.screenshot(full_page=False)
                screenshot_b64 = base64.b64encode(img_bytes).decode('utf-8')
            except:
                pass
            status = "fail"
            error_msg = str(e)
        
        finally:
            browser.close()

    return {
        "status": status,
        "started_at": started_at,
        "duration_ms": int((time.time() - t0) * 1000),
        "error": error_msg,
        "steps": report_steps,
        "screenshot_b64": screenshot_b64 # La clave para que Vercel muestre la foto
    }