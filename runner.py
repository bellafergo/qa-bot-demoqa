# runner.py
# ============================================================
# Playwright Runner (Retail / E2E) - PRODUCT READY
# - Ejecuta steps Playwright
# - Devuelve screenshot_b64 para UI
# - Soporta timeout global (timeout_s) SIN afectar screenshot robust
# - ✅ Semántica QA: expected(pass/fail) + outcome(pass/fail) => status final passed/failed
# ============================================================

from __future__ import annotations
import re
import base64
import time
import uuid
import os
from typing import Any, Dict, List, Optional, Tuple

from playwright.sync_api import Error as PlaywrightError
from playwright.sync_api import TimeoutError as PlaywrightTimeoutError
from playwright.sync_api import sync_playwright
from dotenv import load_dotenv

# Carga variables de entorno (OPENAI, SUPABASE, HEB_EMAIL, HEB_PASSWORD, etc.)
load_dotenv()


# ============================================================
# Helpers
# ============================================================
def _b64_png(img_bytes: bytes) -> str:
    return base64.b64encode(img_bytes).decode("utf-8")


def _now_ms() -> int:
    return int(time.time() * 1000)


def _as_int(v: Any, default: int) -> int:
    try:
        return int(v)
    except Exception:
        return default


def _safe_str(v: Any) -> str:
    try:
        return "" if v is None else str(v)
    except Exception:
        return ""


def _normalize_action(step: Dict[str, Any]) -> str:
    a = (step.get("action") or step.get("type") or "").strip().lower()
    # compat aliases
    if a in ("wait_for_selector", "wait_for"):
        return "assert_visible"
    return a


def _pick_timeout_ms(step: Dict[str, Any], default_ms: int) -> int:
    return _as_int(step.get("timeout_ms"), default_ms)


def _selector_from_step(step: Dict[str, Any]) -> str:
    sel = step.get("selector") or step.get("target") or step.get("loc") or ""
    return _safe_str(sel).strip()


def _url_from_step(step: Dict[str, Any], base_url: Optional[str]) -> str:
    url = (step.get("url") or step.get("href") or "").strip()
    if url:
        return url

    # compat: {"action":"goto","path":"/login"}
    path = (step.get("path") or "").strip()
    if path and base_url:
        if base_url.endswith("/") and path.startswith("/"):
            return base_url[:-1] + path
        if (not base_url.endswith("/")) and (not path.startswith("/")):
            return base_url + "/" + path
        return base_url + path

    return base_url or ""


def _norm_expected(v: Any) -> str:
    """
    expected: "pass" | "fail"
    Acepta variantes: true/false, passed/failed, ok/fail, etc.
    """
    s = _safe_str(v).strip().lower()
    if s in ("", "none", "null"):
        return "pass"

    if s in ("pass", "passed", "ok", "true", "success"):
        return "pass"
    if s in ("fail", "failed", "false", "error", "negative"):
        return "fail"

    # default conservador
    return "pass"


def _final_status(expected: str, outcome: str, had_error: bool) -> str:
    """
    status final:
      - si hubo error técnico (Playwright/runner) => "error"
      - si expected == outcome => "passed"
      - si expected != outcome => "failed"
    """
    if had_error:
        return "error"
    return "passed" if expected == outcome else "failed"


# ============================================================
# Screenshot (NO depende de timeout global por decisión)
# ============================================================
def take_screenshot_robust(page) -> Tuple[Optional[str], List[str]]:
    """
    Toma screenshot con reintentos y timeouts propios.
    Importante: NO respeta timeout global (por diseño), para no perder evidencia.
    """
    logs: List[str] = []
    b64: Optional[str] = None

    # Timeouts propios (ms)
    NAV_STABILIZE_MS = 1200
    SHOT_TIMEOUT_MS = 15000
    RETRIES = 2

    # intenta estabilizar un poco DOM antes de screenshot
    try:
        page.wait_for_timeout(NAV_STABILIZE_MS)
    except Exception:
        pass

    # intenta esperar estado de carga sin colgarse
    try:
        page.wait_for_load_state("domcontentloaded", timeout=5000)
    except Exception:
        pass

    last_err = None
    for attempt in range(RETRIES + 1):
        try:
            png = page.screenshot(full_page=True, timeout=SHOT_TIMEOUT_MS)
            b64 = _b64_png(png)
            logs.append(f"Screenshot: ok (full_page) [attempt {attempt+1}]")
            return b64, logs
        except Exception as e:
            last_err = e
            logs.append(f"Screenshot full_page failed [attempt {attempt+1}]: {type(e).__name__}: {e}")

        try:
            png = page.screenshot(full_page=False, timeout=SHOT_TIMEOUT_MS)
            b64 = _b64_png(png)
            logs.append(f"Screenshot: ok (viewport) [attempt {attempt+1}]")
            return b64, logs
        except Exception as e:
            last_err = e
            logs.append(f"Screenshot viewport failed [attempt {attempt+1}]: {type(e).__name__}: {e}")

        try:
            page.wait_for_timeout(800)
        except Exception:
            pass

    logs.append(
        f"Screenshot: failed окончательно: {type(last_err).__name__}: {last_err}" if last_err else "Screenshot: failed"
    )
    return None, logs

# ============================================================
# Runner ESPECIAL HEB (flujo carrito o compra completa)
# ============================================================
def execute_heb_full_purchase(
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
    timeout_s: Optional[int] = None,
    expected: Optional[str] = None,
    only_add_to_cart: bool = False,  # 🔹 futuro: solo carrito
) -> Dict[str, Any]:
    """
    Flujo HEB:

    - Login con HEB_EMAIL / HEB_PASSWORD (variables de entorno)
    - Buscar "tomate" y "Coca Cola"
    - Agregar al carrito
    - Si only_add_to_cart = False: completar checkout y validar confirmación
    - Si only_add_to_cart = True: dejar productos en carrito sin completar compra

    Devuelve el MISMO contrato que execute_test, para no afectar la UI.
    """
    t0 = time.time()
    evidence_id = f"EV-{uuid.uuid4().hex[:10]}"
    logs: List[str] = []
    screenshot_b64: Optional[str] = None

    expected_norm = _norm_expected(expected)
    outcome = "pass"
    had_error = False
    reason: Optional[str] = None

    base_url = "https://www.heb.com.mx"

    # Credenciales desde .env
    email = os.getenv("HEB_EMAIL")
    password = os.getenv("HEB_PASSWORD")

    if not email or not password:
        reason = "Faltan variables de entorno HEB_EMAIL y/o HEB_PASSWORD"
        logs.append(reason)
        outcome = "fail"
        had_error = True
        duration_ms = int((time.time() - t0) * 1000)
        status = _final_status(expected_norm, outcome, had_error)
        ok = status == "passed"
        if reason is None:
            reason = "Error en configuración de credenciales HEB"

        return {
            "ok": ok,
            "status": status,
            "expected": expected_norm,
            "outcome": outcome,
            "reason": reason,
            "evidence_id": evidence_id,
            "steps": [],
            "logs": logs,
            "screenshot_b64": screenshot_b64,
            "duration_ms": duration_ms,
            "meta": {
                "headless": headless,
                "steps_count": 0,
                "base_url": base_url,
                "timeout_ms": None,
                "viewport": viewport or {"width": 1366, "height": 768},
            },
        }

    # Viewport defaults
    if not isinstance(viewport, dict):
        viewport = {"width": 1366, "height": 768}
    vw = _as_int(viewport.get("width"), 1366)
    vh = _as_int(viewport.get("height"), 768)

    # Timeout global opcional
    timeout_ms_global: Optional[int] = None
    if timeout_s is not None:
        timeout_ms_global = max(15000, int(timeout_s) * 1000)

    page = None
    browser = None
    context = None

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=headless)
            context = browser.new_context(viewport={"width": vw, "height": vh})

            if timeout_ms_global is not None:
                context.set_default_timeout(timeout_ms_global)
                context.set_default_navigation_timeout(timeout_ms_global)
                logs.append(f"[HEB] Global timeout applied: {timeout_ms_global}ms")

            page = context.new_page()

            try:
                # 1) Home HEB — navegación más robusta
                page.goto(
                    base_url,
                    wait_until="commit",   # ✅ evitar bloqueos por JS eterno
                    timeout=90000,         # hasta 90s para primera carga
                )
                page.wait_for_timeout(4000)

                # Verificar que cargó algo razonable
                try:
                    page.get_by_text("Iniciar sesión", exact=False).first.wait_for(timeout=20000)
                except Exception:
                    raise AssertionError("No apareció 'Iniciar sesión' en la home de HEB.")

                # 2) Abrir login (link o botón, con regex)
                try:
                    login_el = page.get_by_role(
                        "link",
                        name=re.compile(r"Iniciar sesión|Inicia sesión", re.IGNORECASE),
                    )
                    login_el.click(timeout=60000)
                except Exception:
                    try:
                        login_el = page.get_by_role(
                            "button",
                            name=re.compile(r"Iniciar sesión|Inicia sesión", re.IGNORECASE),
                        )
                        login_el.click(timeout=60000)
                    except Exception:
                        page.get_by_text(
                            re.compile(r"Iniciar sesión|Inicia sesión", re.IGNORECASE)
                        ).first.click(timeout=60000)

                # 3) Login – correo
                page.get_by_placeholder("Correo electrónico").fill(email)
                page.get_by_role(
                    "button",
                    name=re.compile(r"Continuar", re.IGNORECASE),
                ).click()

                # 4) Login – contraseña
                page.get_by_placeholder("Contraseña").wait_for(timeout=30000)
                page.get_by_placeholder("Contraseña").fill(password)

                # botón puede llamarse "Iniciar sesión" o "Continuar"
                try:
                    page.get_by_role(
                        "button",
                        name=re.compile(r"Iniciar sesión", re.IGNORECASE),
                    ).click()
                except Exception:
                    page.get_by_role(
                        "button",
                        name=re.compile(r"Continuar", re.IGNORECASE),
                    ).click()

                page.wait_for_url(
                    lambda url: "heb.com.mx" in url,
                    timeout=60000,
                )
                page.wait_for_timeout(4000)  # que cargue header, carrusel, etc.

                # Helper para buscar y agregar producto
                def buscar_y_agregar(termino: str, cantidad: int = 1) -> None:
                    logs.append(f"[HEB] Buscando producto: {termino} (cantidad={cantidad})")
                    sb = page.get_by_placeholder("Buscar productos")
                    sb.click()
                    sb.fill(termino)
                    sb.press("Enter")
                    page.wait_for_timeout(5000)

                    # Primer botón "Agregar"
                    add_btn = page.get_by_role(
                        "button",
                        name=re.compile(r"Agregar", re.IGNORECASE),
                    ).first
                    add_btn.click()
                    page.wait_for_timeout(1500)

                    # Intentar ajustar cantidad (si hay +)
                    if cantidad > 1:
                        try:
                            plus_btn = page.get_by_role(
                                "button",
                                name=re.compile(r"\+", re.IGNORECASE),
                            ).first
                            for _ in range(cantidad - 1):
                                plus_btn.click()
                                page.wait_for_timeout(500)
                        except Exception:
                            logs.append(
                                f"[HEB] No se pudo ajustar cantidad para '{termino}', se dejó en 1 unidad."
                            )

                # 5) Tomate (puedes ajustar cantidad después si quieres)
                buscar_y_agregar("tomate", cantidad=1)

                # 6) Coca Cola
                buscar_y_agregar("COCA COLA", cantidad=1)

                # Si solo queremos dejar productos en el carrito y NO comprar
                if only_add_to_cart:
                    logs.append("[HEB] Solo modo carrito, sin completar checkout.")
                    try:
                        # Cualquier indicador de carrito / finalizar compra
                        page.get_by_role(
                            "button",
                            name=re.compile(
                                r"Finalizar compra|Carrito|Ver carrito", re.IGNORECASE
                            ),
                        ).first.wait_for(timeout=5000)
                    except Exception:
                        logs.append(
                            "[HEB] No se pudo verificar visualmente el botón de carrito tras agregar productos."
                        )

                    reason = "OK HEB — productos agregados al carrito sin completar compra"

                else:
                    # 7) Abrir carrito y pasar a checkout
                    cart_btn = page.get_by_role(
                        "button",
                        name=re.compile(r"Finalizar compra|Carrito|Ver carrito", re.IGNORECASE),
                    ).first
                    cart_btn.click()
                    page.wait_for_url(
                        lambda url: "checkout" in url or "cart" in url,
                        timeout=60000,
                    )
                    page.wait_for_timeout(3000)

                    # 8) Proceder desde carrito
                    try:
                        page.get_by_role(
                            "button",
                            name=re.compile(r"Proceder a la compra|Continuar", re.IGNORECASE),
                        ).first.click()
                    except Exception:
                        logs.append("[HEB] No se encontró botón 'Proceder a la compra' en el carrito.")

                    # 9) Shipping / tienda
                    page.wait_for_timeout(4000)

                    # Modal de tienda (si aparece)
                    try:
                        page.get_by_text("HEB Gonzalitos").first.click()
                        page.get_by_role(
                            "button",
                            name=re.compile(r"Confirmar|Guardar", re.IGNORECASE),
                        ).click()
                        page.wait_for_timeout(2000)
                    except Exception:
                        logs.append("[HEB] No apareció modal de tienda, se asume tienda ya configurada.")

                    # 10) Confirmar "Recoger en la tienda" y continuar a pago
                    try:
                        if "shipping" in page.url or "envio" in page.url:
                            page.mouse.wheel(0, 800)
                            page.wait_for_timeout(1500)
                            try:
                                page.get_by_text("Recoger en la tienda").first.click()
                            except Exception:
                                logs.append("[HEB] No se encontró opción 'Recoger en la tienda'.")
                            try:
                                page.get_by_role(
                                    "button",
                                    name=re.compile(
                                        r"Proceder a la compra|Continuar", re.IGNORECASE
                                    ),
                                ).first.click()
                            except Exception:
                                logs.append(
                                    "[HEB] No se encontró botón para continuar desde shipping."
                                )
                    except Exception:
                        logs.append("[HEB] No se pudo validar pantalla de shipping explícitamente.")

                    # 11) Pago
                    page.wait_for_url(
                        lambda url: "payment" in url or "pago" in url,
                        timeout=60000,
                    )
                    page.wait_for_timeout(3000)

                    # Pago al recibir
                    try:
                        page.get_by_text("Pago al recibir").first.click()
                    except Exception:
                        logs.append("[HEB] No se encontró opción 'Pago al recibir'.")

                    # Comentarios: PRUEBA
                    try:
                        page.locator("textarea").first.fill("PRUEBA")
                    except Exception:
                        logs.append("[HEB] No se encontró textarea para comentarios.")

                    # Comprar ahora
                    try:
                        page.get_by_role(
                            "button",
                            name=re.compile(r"Comprar ahora|Realizar pedido", re.IGNORECASE),
                        ).click()
                    except Exception:
                        raise AssertionError("No se encontró botón para confirmar la compra.")

                    # 12) Confirmación
                    page.wait_for_timeout(5000)
                    try:
                        banner = page.get_by_text(
                            re.compile(
                                r"Tu pedido está siendo procesado|pedido ha sido recibido",
                                re.IGNORECASE,
                            )
                        ).first
                        if not banner.is_visible():
                            raise AssertionError(
                                "No se encontró mensaje de confirmación de pedido."
                            )
                    except Exception as e:
                        raise AssertionError(f"No se pudo confirmar el pedido: {e}")

                    reason = "OK HEB E2E"

            except PlaywrightTimeoutError as e:
                outcome = "fail"
                reason = f"Timeout en flujo HEB: {type(e).__name__}: {e}"
                logs.append(reason)

            except (AssertionError, ValueError) as e:
                outcome = "fail"
                reason = f"Fallo de validación en HEB: {type(e).__name__}: {e}"
                logs.append(reason)

            except PlaywrightError as e:
                outcome = "fail"
                had_error = True
                reason = f"Playwright error en HEB: {type(e).__name__}: {e}"
                logs.append(reason)

            except Exception as e:
                outcome = "fail"
                had_error = True
                reason = f"Error inesperado en HEB: {type(e).__name__}: {e}"
                logs.append(reason)

            finally:
                if page is not None and screenshot_b64 is None:
                    try:
                        shot, shot_logs = take_screenshot_robust(page)
                        logs.extend(shot_logs)
                        screenshot_b64 = shot
                    except Exception as e:
                        logs.append(f"Final screenshot HEB failed: {type(e).__name__}: {e}")

                try:
                    context.close()
                except Exception:
                    pass
                try:
                    browser.close()
                except Exception:
                    pass

    except Exception as e:
        outcome = "fail"
        had_error = True
        if reason is None:
            reason = f"Runner HEB crashed: {type(e).__name__}: {e}"
        logs.append(reason)

    duration_ms = int((time.time() - t0) * 1000)

    status = _final_status(expected_norm, outcome, had_error)
    if reason is None:
        reason = "OK" if status == "passed" else "Fallo en flujo HEB"

    ok = status == "passed"

    return {
        "ok": ok,
        "status": status,
        "expected": expected_norm,
        "outcome": outcome,
        "reason": reason,
        "evidence_id": evidence_id,
        "steps": [],  # este runner no expone steps detallados
        "logs": logs,
        "screenshot_b64": screenshot_b64,
        "duration_ms": duration_ms,
        "meta": {
            "headless": headless,
            "steps_count": 0,
            "base_url": base_url,
            "timeout_ms": timeout_ms_global,
            "viewport": {"width": vw, "height": vh},
        },
    }


# ============================================================
# Runner GENÉRICO POR STEPS 
# ============================================================
def execute_test(
    steps: List[Dict[str, Any]],
    base_url: Optional[str] = None,
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
    timeout_s: Optional[int] = None,  # ✅ timeout global (NO afecta screenshot_robust)
    expected: Optional[str] = None,   # ✅ "pass" | "fail" (si no viene, se infiere del payload/steps)
) -> Dict[str, Any]:
    """
    Ejecuta steps Playwright.

    Acciones soportadas:
      goto, fill, click, press,
      assert_visible, assert_text_contains,
      assert_not_visible, assert_url_contains,
      wait_ms
    Compatibilidad:
      wait_for, wait_for_selector -> assert_visible

    Semántica QA:
      - expected="pass": una falla => status "failed"
      - expected="fail": una falla => status "passed" (caso negativo exitoso)
      - expected="fail": si NO falla => status "failed" (debió fallar)

    Retorna (nivel producto):
      {
        ok: bool,
        status: "passed" | "failed" | "error",
        expected: "pass" | "fail",
        outcome: "pass" | "fail",
        reason: str | None,
        evidence_id: str,
        steps: [...],
        logs: [...],
        screenshot_b64: str | None,
        duration_ms: int,
        meta: { headless, steps_count, base_url, timeout_ms, viewport }
      }
    """
    t0 = time.time()

    screenshot_b64: Optional[str] = None
    report_steps: List[Dict[str, Any]] = []
    logs: List[str] = []
    evidence_id = f"EV-{uuid.uuid4().hex[:10]}"

    if not isinstance(steps, list) or not steps:
        return {
            "ok": False,
            "status": "failed",
            "expected": "pass",
            "outcome": "fail",
            "reason": "steps vacío o inválido",
            "evidence_id": evidence_id,
            "steps": [],
            "logs": ["Runner error: steps vacío o inválido"],
            "screenshot_b64": None,
            "duration_ms": int((time.time() - t0) * 1000),
            "meta": {"headless": headless, "steps_count": 0, "base_url": base_url, "timeout_ms": None},
        }

    # Defaults
    default_step_timeout_ms = 15000
    timeout_ms_global: Optional[int] = None
    if timeout_s is not None:
        timeout_ms_global = max(1000, int(timeout_s) * 1000)

    # Viewport defaults
    if not isinstance(viewport, dict):
        viewport = {"width": 1366, "height": 768}
    vw = _as_int(viewport.get("width"), 1366)
    vh = _as_int(viewport.get("height"), 768)

    # ✅ Expected: primero parámetro, luego payload en steps[0], luego default pass
    inferred_expected = expected
    if inferred_expected is None:
        inferred_expected = steps[0].get("expected") or steps[0].get("expect") or steps[0].get("expected_outcome")
    expected_norm = _norm_expected(inferred_expected)

    # Outcome real del run (pass si termina sin falla; fail si ocurre una falla de assertions/timeout/value)
    outcome = "pass"
    had_error = False
    reason: Optional[str] = None

    def _record_step(
        i: int,
        step: Dict[str, Any],
        st: str,
        err: Optional[str] = None,
        extra: Optional[Dict[str, Any]] = None,
    ):
        payload: Dict[str, Any] = {
            "index": i,
            "action": _normalize_action(step),
            "raw_action": step.get("action"),
            "selector": step.get("selector"),
            "url": step.get("url"),
            "value": step.get("value"),
            "text": step.get("text"),
            "status": st,     # passed/failed/error por step
            "error": err,
            "ts_ms": _now_ms(),
        }
        if extra:
            payload.update(extra)
        report_steps.append(payload)

    # Execute in Playwright
    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=headless)
            context = browser.new_context(viewport={"width": vw, "height": vh})

            # ✅ Timeout global: aplica a toda interacción/espera,
            #    pero NO tocamos screenshot_robust (por decisión).
            if timeout_ms_global is not None:
                context.set_default_timeout(timeout_ms_global)
                context.set_default_navigation_timeout(timeout_ms_global)
                logs.append(f"Global timeout applied: {timeout_ms_global}ms")

            page = context.new_page()

            # Base URL: si no viene, la intentamos inferir de un goto inicial
            inferred_base_url = base_url
            for st in steps:
                if _normalize_action(st) == "goto":
                    u = _url_from_step(st, base_url)
                    if u:
                        inferred_base_url = base_url or u
                    break

            # Ejecutar steps
            for i, step in enumerate(steps):
                action = _normalize_action(step)
                timeout_ms = _pick_timeout_ms(step, default_step_timeout_ms)

                try:
                    if action == "goto":
                        url = _url_from_step(step, inferred_base_url)
                        if not url:
                            raise ValueError("goto requiere url/base_url")

                        page.goto(url, wait_until="domcontentloaded", timeout=timeout_ms)

                        # ✅ Estabilidad: intenta networkidle con fallback (sin colgarse)
                        try:
                            page.wait_for_load_state("networkidle", timeout=min(6000, timeout_ms))
                        except Exception:
                            try:
                                page.wait_for_timeout(350)
                            except Exception:
                                pass

                        _record_step(i, step, "passed", extra={"resolved_url": url})
                        continue

                    if action == "wait_ms":
                        ms = _as_int(step.get("ms"), 500)
                        page.wait_for_timeout(ms)
                        _record_step(i, step, "passed", extra={"ms": ms})
                        continue

                    # Para acciones con selector
                    sel = _selector_from_step(step)
                    if not sel and action not in ("assert_text_contains", "assert_url_contains"):
                        raise ValueError(f"{action} requiere selector")

                    if action == "fill":
                        val = _safe_str(step.get("value"))
                        page.locator(sel).wait_for(state="visible", timeout=timeout_ms)
                        page.fill(sel, val, timeout=timeout_ms)
                        _record_step(i, step, "passed")
                        continue

                    if action == "click":
                        page.locator(sel).wait_for(state="visible", timeout=timeout_ms)
                        page.click(sel, timeout=timeout_ms)
                        _record_step(i, step, "passed")
                        continue

                    if action == "press":
                        key = _safe_str(step.get("key") or "Enter")
                        page.locator(sel).wait_for(state="visible", timeout=timeout_ms)
                        page.press(sel, key, timeout=timeout_ms)
                        _record_step(i, step, "passed", extra={"key": key})
                        continue

                    if action == "assert_visible":
                        page.locator(sel).wait_for(state="visible", timeout=timeout_ms)
                        _record_step(i, step, "passed")
                        continue

                    if action == "assert_not_visible":
                        loc = page.locator(sel)
                        # is_visible con timeout corto: si se llega a ver, falla
                        if loc.is_visible(timeout=1500):
                            raise AssertionError(f"assert_not_visible falló: se mostró {sel}")
                        _record_step(i, step, "passed")
                        continue

                    if action == "assert_url_contains":
                        needle = _safe_str(step.get("value") or step.get("text") or step.get("contains") or "").strip()
                        if not needle:
                            raise ValueError("assert_url_contains requiere value")
                        current = page.url or ""
                        if needle not in current:
                            raise AssertionError(f"assert_url_contains falló: '{needle}' no está en '{current}'")
                        _record_step(i, step, "passed", extra={"current_url": current})
                        continue

                    if action == "assert_text_contains":
                        expected_text = _safe_str(step.get("expected") or step.get("text") or "").strip()
                        if not expected_text:
                            raise ValueError("assert_text_contains requiere expected/text")
                        target_sel = _selector_from_step(step) or "body"
                        loc = page.locator(target_sel)
                        loc.wait_for(state="visible", timeout=timeout_ms)
                        content = (loc.inner_text(timeout=timeout_ms) or "").strip()
                        if expected_text not in content:
                            raise AssertionError(f"Texto no encontrado. Expected contiene: '{expected_text}'")
                        _record_step(i, step, "passed", extra={"target": target_sel})
                        continue

                    raise ValueError(f"Acción no soportada: {action}")

                except (PlaywrightTimeoutError,) as e:
                    outcome = "fail"
                    reason = f"Timeout en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(reason)
                    _record_step(i, step, "failed", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except (AssertionError, ValueError) as e:
                    outcome = "fail"
                    reason = f"Fallo en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(reason)
                    _record_step(i, step, "failed", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except PlaywrightError as e:
                    outcome = "fail"
                    had_error = True
                    reason = f"Playwright error en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(reason)
                    _record_step(i, step, "error", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except Exception as e:
                    outcome = "fail"
                    had_error = True
                    reason = f"Error inesperado en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(reason)
                    _record_step(i, step, "error", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

            # Screenshot final (si aún no hay)
            if screenshot_b64 is None:
                try:
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                except Exception as e:
                    logs.append(f"Final screenshot failed: {type(e).__name__}: {e}")

            try:
                context.close()
            except Exception:
                pass
            try:
                browser.close()
            except Exception:
                pass

    except Exception as e:
        outcome = "fail"
        had_error = True
        reason = f"Runner crashed: {type(e).__name__}: {e}"
        logs.append(reason)

    duration_ms = int((time.time() - t0) * 1000)

    # ✅ status final con semántica QA
    status = _final_status(expected_norm, outcome, had_error)

    # ✅ reason producto: si fue negativo exitoso, no lo llames error
    if status == "passed" and expected_norm == "fail" and outcome == "fail":
        reason = "Falló como se esperaba (caso negativo)."
    elif reason is None:
        # corrida limpia
        reason = "OK"

    ok = status == "passed"

    return {
        "ok": ok,
        "status": status,            # "passed" | "failed" | "error"
        "expected": expected_norm,   # "pass" | "fail"
        "outcome": outcome,          # "pass" | "fail"
        "reason": reason,
        "evidence_id": evidence_id,
        "steps": report_steps,
        "logs": logs,
        "screenshot_b64": screenshot_b64,
        "duration_ms": duration_ms,
        "meta": {
            "headless": headless,
            "steps_count": len(steps),
            "base_url": base_url,
            "timeout_ms": timeout_ms_global,
            "viewport": {"width": vw, "height": vh},
        },
    }
