# runner.py
# ============================================================
# Playwright Runner (Retail / E2E) - PRODUCT READY
# - Ejecuta steps Playwright
# - Devuelve screenshot_b64 para UI
# - Soporta timeout global (timeout_s) SIN afectar screenshot robust
# - ✅ Semántica QA: expected(pass/fail) + outcome(pass/fail) => status final passed/failed
# ============================================================

from __future__ import annotations

import base64
import os
import re
import time
import uuid
from typing import Any, Dict, List, Optional, Tuple

from dotenv import load_dotenv
from playwright.sync_api import Error as PlaywrightError
from playwright.sync_api import TimeoutError as PlaywrightTimeoutError
from playwright.sync_api import sync_playwright

# Load environment variables (OPENAI, SUPABASE, HEB_EMAIL, HEB_PASSWORD, etc.)
load_dotenv()

# ============================================================
# Helpers
# ============================================================


def _b64_png(img_bytes: bytes) -> str:
    """Encode bytes to base64 string."""
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
# Screenshot helper (JPG, optimized for memory)
# ============================================================


def take_screenshot_robust(page) -> Tuple[Optional[str], List[str]]:
    """
    Screenshot optimizado en JPG con timeouts/reintentos propios.
    No depende del timeout global del contexto.
    """
    logs: List[str] = []
    b64: Optional[str] = None

    NAV_STABILIZE_MS = 800
    SHOT_TIMEOUT_MS = 8000
    RETRIES = 2

    # Estabilizar un poco la página
    try:
        page.wait_for_timeout(NAV_STABILIZE_MS)
    except Exception:
        pass

    try:
        page.wait_for_load_state("domcontentloaded", timeout=3000)
    except Exception:
        pass

    last_err: Optional[Exception] = None
    for attempt in range(RETRIES + 1):
        try:
            jpg = page.screenshot(
                type="jpeg",
                quality=60,  # 0–100 (60 recomendado)
                full_page=False,  # menos RAM
                timeout=SHOT_TIMEOUT_MS,
            )
            b64 = _b64_png(jpg)
            logs.append(f"Screenshot JPG OK [attempt {attempt + 1}]")
            return b64, logs
        except Exception as e:
            last_err = e
            logs.append(
                f"Screenshot JPG failed [attempt {attempt + 1}]: {type(e).__name__}: {e}"
            )
            try:
                page.wait_for_timeout(500)
            except Exception:
                pass

    logs.append(
        f"Screenshot final failed: {type(last_err).__name__}: {last_err}"
        if last_err
        else "Screenshot final failed"
    )
    return None, logs


# ============================================================
# Runner ESPECIAL HEB (flujo carrito o compra completa, con screenshots por paso)
# ============================================================


def execute_heb_full_purchase(
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
    timeout_s: Optional[int] = None,
    expected: Optional[str] = None,
    only_add_to_cart: bool = False,  # True = solo dejar productos en carrito
) -> Dict[str, Any]:
    """
    Flujo HEB:

    - Login con HEB_EMAIL / HEB_PASSWORD (variables de entorno)
    - Buscar "tomate" y "Coca Cola"
    - Agregar al carrito
    - Si only_add_to_cart = False: completar checkout y validar confirmación
    - Si only_add_to_cart = True: dejar productos en carrito sin completar compra

    Devuelve el MISMO contrato que execute_test, para no afectar la UI.
    Además:
    - steps: lista de screenshots por paso {"name": str, "screenshot_b64": str}
    """
    t0 = time.time()
    evidence_id = f"EV-{uuid.uuid4().hex[:10]}"
    logs: List[str] = []
    screenshot_b64: Optional[str] = None
    steps: List[Dict[str, Any]] = []

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
            "steps": steps,
            "logs": logs,
            "screenshot_b64": screenshot_b64,
            "duration_ms": duration_ms,
            "meta": {
                "headless": headless,
                "steps_count": len(steps),
                "base_url": base_url,
                "timeout_ms": None,
                "viewport": viewport or {"width": 1366, "height": 768},
            },
        }

    # Viewport por default
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

            # ---------- helpers internos (usan steps + screenshot_b64) ----------
            def snap(step_name: str) -> None:
                nonlocal screenshot_b64
                if page is None:
                    return
                try:
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot or screenshot_b64
                    if shot:
                        steps.append(
                            {
                                "name": step_name,
                                "screenshot_b64": shot,
                            }
                        )
                        logs.append(f"[HEB] Screenshot capturado en paso: {step_name}")
                except Exception as e:
                    logs.append(f"[HEB] Screenshot failed at {step_name}: {type(e).__name__}: {e}")

            def _try_cerrar_modal_tienda(max_tries: int = 3) -> None:
                """
                Intenta cerrar el modal de selección de tienda (base-modal)
                hasta max_tries veces.
                """
                try:
                    modal = page.locator('[data-testid="base-modal"]')
                except Exception as e:
                    logs.append(f"[HEB] No se pudo localizar base-modal: {type(e).__name__}: {e}")
                    return

                for attempt in range(max_tries):
                    try:
                        if not modal.is_visible():
                            return

                        logs.append(f"[HEB] Modal de tienda visible, intento {attempt+1}/{max_tries} para cerrarlo.")
                        snap("modal_tienda")

                        # 1) Intentar seleccionar la tienda (Gonzalitos / Mi tienda / etc.)
                        try:
                            page.get_by_text(
                                re.compile(r"Gonzalitos|Mi tienda|Selecciona tu tienda", re.IGNORECASE)
                            ).first.click(timeout=4000)
                        except Exception:
                            pass

                        # 2) Intentar botón de confirmación
                        for label in (
                            "Confirmar",
                            "Guardar",
                            "Continuar",
                            "Aceptar",
                            "Ir a la tienda",
                            "Entrar a la tienda",
                        ):
                            try:
                                page.get_by_role(
                                    "button",
                                    name=re.compile(label, re.IGNORECASE),
                                ).first.click(timeout=4000)
                                page.wait_for_timeout(800)
                                if not modal.is_visible():
                                    logs.append(f"[HEB] Modal de tienda cerrado con botón '{label}'.")
                                    snap("modal_tienda_cerrado")
                                    return
                            except Exception:
                                continue

                        # 3) Fallback: botón de cerrar (X / Cerrar)
                        try:
                            page.get_by_role(
                                "button",
                                name=re.compile(r"Cerrar|✕|X", re.IGNORECASE),
                            ).first.click(timeout=4000)
                            page.wait_for_timeout(800)
                            if not modal.is_visible():
                                logs.append("[HEB] Modal de tienda cerrado con botón de cierre.")
                                snap("modal_tienda_cerrado")
                                return
                        except Exception:
                            pass

                        page.wait_for_timeout(800)
                    except Exception as e:
                        logs.append(f"[HEB] Error al manejar modal de tienda (intento {attempt+1}): {type(e).__name__}: {e}")
                        try:
                            page.wait_for_timeout(800)
                        except Exception:
                            pass

                logs.append("[HEB] Modal de tienda sigue visible después de varios intentos.")

            def buscar_y_agregar(termino: str, cantidad: int = 1, step_prefix: str = "") -> None:
                logs.append(f"[HEB] Buscando producto: {termino} (cantidad={cantidad})")

                # 1) Antes de usar el buscador, intentar cerrar el modal si está
                _try_cerrar_modal_tienda()

                sb = page.get_by_placeholder("Buscar productos")

                # Si el modal sigue visible, reintentar y, si no se puede, fallar pronto
                modal = page.locator('[data-testid="base-modal"]')
                if modal.is_visible():
                    logs.append("[HEB] Modal de tienda visible antes de click en buscador, reintentando cierre.")
                    _try_cerrar_modal_tienda()
                    if modal.is_visible():
                        raise AssertionError(
                            "Modal de tienda está bloqueando el buscador y no se pudo cerrar automáticamente."
                        )

                # 2) Click al buscador con timeout corto y manejo específico del Timeout
                try:
                    sb.click(timeout=10000)
                except PlaywrightTimeoutError as e:
                    logs.append(f"[HEB] Timeout al intentar click en buscador: {type(e).__name__}: {e}")
                    _try_cerrar_modal_tienda()
                    modal = page.locator('[data-testid="base-modal"]')
                    if modal.is_visible():
                        raise AssertionError(
                            "Timeout en click del buscador y el modal de tienda sigue visible."
                        )
                    # Segundo intento con timeout más corto
                    sb.click(timeout=5000)
                except Exception as e:
                    logs.append(f"[HEB] Error genérico al hacer click en buscador: {type(e).__name__}: {e}")
                    _try_cerrar_modal_tienda()
                    sb.click(timeout=5000)

                # 3) Escribir término y esperar resultados
                sb.fill(termino)
                sb.press("Enter")
                page.wait_for_timeout(5000)
                snap(f"{step_prefix}_resultados")

                # 4) Click en "Agregar" con estrategia normal + force=True si algo intercepta el click
                add_btn = page.get_by_role(
                    "button",
                    name=re.compile(r"Agregar", re.IGNORECASE),
                ).first

                try:
                    add_btn.click(timeout=8000)
                    logs.append(f"[HEB] Click normal en 'Agregar' para '{termino}'.")
                except PlaywrightTimeoutError as e:
                    logs.append(
                        f"[HEB] Timeout al hacer click en 'Agregar' para '{termino}': "
                        f"{type(e).__name__}: {e} — reintentando con force=True."
                    )
                    add_btn.click(timeout=5000, force=True)
                    logs.append(f"[HEB] Click forzado (force=True) en 'Agregar' para '{termino}'.")
                except PlaywrightError as e:
                    msg = str(e)
                    if "intercepts pointer events" in msg or "not clickable" in msg.lower():
                        logs.append(
                            "[HEB] Intercepción de eventos al hacer click en 'Agregar' "
                            f"para '{termino}', reintentando con force=True: {msg}"
                        )
                        add_btn.click(timeout=5000, force=True)
                        logs.append(f"[HEB] Click forzado (force=True) en 'Agregar' para '{termino}'.")
                    else:
                        # otros errores de Playwright se propagan
                        raise
                except Exception as e:
                    logs.append(
                        f"[HEB] Error inesperado al hacer click en 'Agregar' para '{termino}': "
                        f"{type(e).__name__}: {e}"
                    )
                    # último intento con force=True antes de rendirnos
                    try:
                        add_btn.click(timeout=5000, force=True)
                        logs.append(
                            f"[HEB] Click forzado final (force=True) en 'Agregar' para '{termino}' tras error genérico."
                        )
                    except Exception as e2:
                        logs.append(
                            f"[HEB] Incluso el click forzado en 'Agregar' falló para '{termino}': "
                            f"{type(e2).__name__}: {e2}"
                        )
                        raise

                page.wait_for_timeout(1500)
                snap(f"{step_prefix}_agregado")

                # 5) Ajustar cantidad (si aplica)
                if cantidad > 1:
                    try:
                        plus_btn = page.get_by_role(
                            "button",
                            name=re.compile(r"\+", re.IGNORECASE),
                        ).first
                        for _ in range(cantidad - 1):
                            plus_btn.click()
                            page.wait_for_timeout(500)
                        snap(f"{step_prefix}_cantidad_{cantidad}")
                    except Exception:
                        logs.append(
                            f"[HEB] No se pudo ajustar cantidad para '{termino}', se dejó en 1 unidad."
                        )


            # -------- helper NUEVO: localizar botón/enlace de carrito de forma robusta -----
            def _find_cart_button():
                """
                Intenta encontrar cualquier botón/enlace relacionado con el carrito o checkout,
                usando varias estrategias para evitar timeouts por cambios de layout.
                """
                # 1) Por texto típico
                label_patterns = [
                    r"Finalizar compra",
                    r"Ver carrito",
                    r"Carrito",
                    r"Mi carrito",
                    r"Checkout",
                ]
                for pattern in label_patterns:
                    try:
                        el = page.get_by_role(
                            "button",
                            name=re.compile(pattern, re.IGNORECASE),
                        ).first
                        if el.is_visible():
                            logs.append(f"[HEB] Botón de carrito encontrado por label '{pattern}'.")
                            return el
                    except Exception:
                        continue

                # 2) Por aria-label del botón (ícono de carrito)
                try:
                    el = page.locator('button[aria-label*="carrito" i]').first
                    if el.is_visible():
                        logs.append("[HEB] Botón de carrito encontrado por aria-label*='carrito'.")
                        return el
                except Exception:
                    pass

                # 3) Por enlaces al carrito/checkout en el header
                try:
                    el = page.locator('a[href*="cart" i], a[href*="checkout" i]').first
                    if el.is_visible():
                        logs.append("[HEB] Enlace de carrito encontrado por href a cart/checkout.")
                        return el
                except Exception:
                    pass

                logs.append("[HEB] No se encontró ningún botón/enlace de carrito visible.")
                return None

            # ------------------- flujo principal -------------------
            try:
                # 1) Home HEB — navegación robusta
                page.goto(
                    base_url,
                    wait_until="commit",  # evitamos quedarnos colgados por JS pesado
                    timeout=90000,
                )
                page.wait_for_timeout(4000)
                snap("home")

                # 1.1 Verificar que cargó algo razonable (header / login / cuenta)
                try:
                    page.get_by_text(
                        re.compile(r"Iniciar sesión|Mi cuenta|Identifícate", re.IGNORECASE)
                    ).first.wait_for(timeout=25000)
                except Exception:
                    raise AssertionError(
                        "No apareció ningún texto de login/cuenta en la home de HEB."
                    )
                snap("home_con_login_visible")

                # 1.2 Cerrar popups comunes (cookies, promos, etc.)
                try:
                    page.get_by_role(
                        "button",
                        name=re.compile(r"Aceptar|Aceptar todo|Entendido|Ok", re.IGNORECASE),
                    ).click(timeout=4000)
                    logs.append("[HEB] Cerrado banner de cookies.")
                    snap("cookies_cerradas")
                except Exception:
                    pass

                try:
                    page.get_by_role(
                        "button",
                        name=re.compile(r"Cerrar|✕|X", re.IGNORECASE),
                    ).first.click(timeout=4000)
                    logs.append("[HEB] Cerrado popup de promo.")
                    snap("promociones_cerradas")
                except Exception:
                    pass

                # 2) Localizar botón/enlace de login con múltiples estrategias
                login_candidates = [
                    lambda: page.get_by_role(
                        "link",
                        name=re.compile(
                            r"Iniciar sesión|Inicia sesión|Identifícate|Mi cuenta",
                            re.IGNORECASE,
                        ),
                    ),
                    lambda: page.get_by_role(
                        "button",
                        name=re.compile(
                            r"Iniciar sesión|Inicia sesión|Identifícate|Mi cuenta",
                            re.IGNORECASE,
                        ),
                    ),
                    lambda: page.get_by_text(
                        re.compile(
                            r"Iniciar sesión|Inicia sesión|Identifícate|Mi cuenta",
                            re.IGNORECASE,
                        )
                    ).first,
                ]

                login_el = None
                last_error = None

                for getter in login_candidates:
                    try:
                        el = getter()
                        el.wait_for(timeout=8000)
                        login_el = el
                        break
                    except Exception as e:
                        last_error = e
                        continue

                if login_el is None:
                    raise AssertionError(
                        "No se encontró ningún elemento de login (link/botón de 'Iniciar sesión' o 'Mi cuenta')."
                    )

                login_el.click(timeout=60000)
                logs.append("[HEB] Login abierto correctamente desde la home.")
                page.wait_for_timeout(2000)
                snap("login_form")

                # 3) Paso de correo
                try:
                    email_input = page.get_by_placeholder(
                        re.compile(r"Correo electrónico|Correo|Email", re.IGNORECASE)
                    )
                    email_input.wait_for(timeout=30000)
                except Exception:
                    email_input = page.get_by_role("textbox").first

                email_input.fill(email)
                page.wait_for_timeout(800)

                clicked_login_email = False
                login_email_errors: List[str] = []

                # 3.1 Preferido: botón "Continuar"
                try:
                    btn_email = page.locator("button:has-text('Continuar')")
                    btn_email.wait_for(timeout=15000)
                    btn_email.click(timeout=15000)
                    clicked_login_email = True
                    logs.append("[HEB] Paso de correo completado (botón 'Continuar').")
                except Exception as e:
                    login_email_errors.append(f"Botón 'Continuar' tras correo falló: {e}")

                # 3.2 Fallback: botón genérico
                if not clicked_login_email:
                    try:
                        btn_email_generic = page.get_by_role(
                            "button",
                            name=re.compile(
                                r"Continuar|Siguiente|Continuar con tu correo|Ingresar",
                                re.IGNORECASE,
                            ),
                        )
                        btn_email_generic.click(timeout=15000)
                        clicked_login_email = True
                        logs.append("[HEB] Paso de correo completado (botón genérico).")
                    except Exception as e:
                        login_email_errors.append(
                            f"Botón genérico tras correo falló: {e}"
                        )

                # 3.3 Fallback: Enter
                if not clicked_login_email:
                    try:
                        email_input.press("Enter")
                        clicked_login_email = True
                        logs.append("[HEB] Paso de correo completado (Enter).")
                    except Exception as e:
                        login_email_errors.append(f"Enter tras correo falló: {e}")

                if not clicked_login_email:
                    raise AssertionError(
                        "No se pudo avanzar después de capturar el correo en login HEB. "
                        + " | ".join(login_email_errors)
                    )

                # 4) Paso de contraseña
                try:
                    pwd_input = page.get_by_placeholder(
                        re.compile(r"Contraseña|Password", re.IGNORECASE)
                    )
                    pwd_input.wait_for(timeout=30000)
                except Exception:
                    pwd_input = page.get_by_role("textbox").nth(1)

                pwd_input.fill(password)
                page.wait_for_timeout(800)

                try:
                    btn_login = page.get_by_role(
                        "button",
                        name=re.compile(
                            r"Iniciar sesión|Acceder|Entrar|Continuar|Siguiente",
                            re.IGNORECASE,
                        ),
                    )
                except Exception:
                    btn_login = page.get_by_text(
                        re.compile(
                            r"Iniciar sesión|Acceder|Entrar|Continuar|Siguiente",
                            re.IGNORECASE,
                        )
                    ).first

                btn_login.click(timeout=60000)
                logs.append("[HEB] Contraseña enviada, esperando post-login.")

                page.wait_for_url(
                    lambda url: "heb.com.mx" in url and "login" not in url.lower(),
                    timeout=90000,
                )
                page.wait_for_timeout(4000)
                logs.append("[HEB] Login completado y home post-login cargada.")
                snap("home_logueado")

                try:
                    page.get_by_text("HEB Gonzalitos").first.click()
                    page.get_by_role("button", name=re.compile(r"Confirmar|Guardar", re.IGNORECASE)).click()
                    page.wait_for_timeout(1500)
                    snap("tienda_confirmada_pre_busqueda")
                except Exception:
                    logs.append("[HEB] Modal de tienda no apareció antes de la búsqueda.")

                # 5) Tomate
                buscar_y_agregar("tomate", cantidad=1, step_prefix="tomate")

                # 6) Coca Cola
                buscar_y_agregar("COCA COLA", cantidad=1, step_prefix="coca_cola")

                # 7) Solo carrito (VALIDACIÓN VISUAL, sin checkout)
                if only_add_to_cart:
                    logs.append("[HEB] Solo modo carrito, sin completar checkout.")
                    try:
                        cart_btn = _find_cart_button()
                        if cart_btn:
                            # no es necesario hacer click, solo asegurarnos que existe
                            cart_btn.wait_for(timeout=5000)
                            snap("carrito_listo")
                        else:
                            logs.append(
                                "[HEB] No se pudo localizar un botón/enlace de carrito para validación visual."
                            )
                    except Exception as e:
                        logs.append(
                            f"[HEB] Error al validar presencia de botón de carrito: {type(e).__name__}: {e}"
                        )

                    reason = "OK HEB — productos agregados al carrito sin completar compra"

                else:
                    # 7) Abrir carrito y pasar a checkout (usa helper robusto)
                    cart_btn = _find_cart_button()
                    if not cart_btn:
                        raise AssertionError(
                            "No se encontró ningún botón o enlace relacionado al carrito."
                        )

                    cart_btn.click(timeout=30000)
                    page.wait_for_url(
                        lambda url: "checkout" in url or "cart" in url,
                        timeout=60000,
                    )
                    page.wait_for_timeout(3000)
                    snap("carrito")

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
                    snap("shipping_inicio")

                    try:
                        page.get_by_text("HEB Gonzalitos").first.click()
                        page.get_by_role(
                            "button",
                            name=re.compile(r"Confirmar|Guardar", re.IGNORECASE),
                        ).click()
                        page.wait_for_timeout(2000)
                        snap("tienda_confirmada")
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
                            snap("shipping_continuado")
                    except Exception:
                        logs.append("[HEB] No se pudo validar pantalla de shipping explícitamente.")

                    # 11) Pago
                    page.wait_for_url(
                        lambda url: "payment" in url or "pago" in url,
                        timeout=60000,
                    )
                    page.wait_for_timeout(3000)
                    snap("payment_inicio")

                    try:
                        page.get_by_text("Pago al recibir").first.click()
                        snap("payment_pago_al_recibir")
                    except Exception:
                        logs.append("[HEB] No se encontró opción 'Pago al recibir'.")

                    try:
                        page.locator("textarea").first.fill("PRUEBA")
                        snap("payment_comentario")
                    except Exception:
                        logs.append("[HEB] No se encontró textarea para comentarios.")

                    try:
                        page.get_by_role(
                            "button",
                            name=re.compile(r"Comprar ahora|Realizar pedido", re.IGNORECASE),
                        ).click()
                    except Exception:
                        raise AssertionError("No se encontró botón para confirmar la compra.")

                    page.wait_for_timeout(5000)
                    snap("post_click_comprar")
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
                        snap("confirmacion_pedido")
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
                if screenshot_b64 is None and steps:
                    screenshot_b64 = steps[-1].get("screenshot_b64")

                if page is not None and screenshot_b64 is None:
                    try:
                        shot, shot_logs = take_screenshot_robust(page)
                        logs.extend(shot_logs)
                        screenshot_b64 = shot
                        if shot:
                            steps.append({"name": "final", "screenshot_b64": shot})
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
        "steps": steps,
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

# ============================================================
# Runner GENÉRICO POR STEPS
# ============================================================


def execute_test(
    steps: List[Dict[str, Any]],
    base_url: Optional[str] = None,
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
    timeout_s: Optional[int] = None,  # timeout global
    expected: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Ejecuta steps Playwright.

    Acciones soportadas:
      goto, fill, click, press,
      assert_visible, assert_text_contains,
      assert_not_visible, assert_url_contains,
      wait_ms

    Semántica QA:
      - expected="pass": una falla => status "failed"
      - expected="fail": una falla => status "passed" (caso negativo exitoso)
      - expected="fail": si NO falla => status "failed" (debió fallar)
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
            "meta": {
                "headless": headless,
                "steps_count": 0,
                "base_url": base_url,
                "timeout_ms": None,
            },
        }

    default_step_timeout_ms = 15000
    timeout_ms_global: Optional[int] = None
    if timeout_s is not None:
        timeout_ms_global = max(1000, int(timeout_s) * 1000)

    if not isinstance(viewport, dict):
        viewport = {"width": 1366, "height": 768}
    vw = _as_int(viewport.get("width"), 1366)
    vh = _as_int(viewport.get("height"), 768)

    inferred_expected = expected
    if inferred_expected is None:
        inferred_expected = (
            steps[0].get("expected")
            or steps[0].get("expect")
            or steps[0].get("expected_outcome")
        )
    expected_norm = _norm_expected(inferred_expected)

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
            "status": st,
            "error": err,
            "ts_ms": _now_ms(),
        }
        if extra:
            payload.update(extra)
        report_steps.append(payload)

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=headless)
            context = browser.new_context(viewport={"width": vw, "height": vh})

            if timeout_ms_global is not None:
                context.set_default_timeout(timeout_ms_global)
                context.set_default_navigation_timeout(timeout_ms_global)
                logs.append(f"Global timeout applied: {timeout_ms_global}ms")

            page = context.new_page()

            inferred_base_url = base_url
            for st in steps:
                if _normalize_action(st) == "goto":
                    u = _url_from_step(st, base_url)
                    if u:
                        inferred_base_url = base_url or u
                    break

            for i, step in enumerate(steps):
                action = _normalize_action(step)
                timeout_ms = _pick_timeout_ms(step, default_step_timeout_ms)

                try:
                    if action == "goto":
                        url = _url_from_step(step, inferred_base_url)
                        if not url:
                            raise ValueError("goto requiere url/base_url")

                        page.goto(
                            url,
                            wait_until="domcontentloaded",
                            timeout=timeout_ms,
                        )
                        try:
                            page.wait_for_load_state(
                                "networkidle", timeout=min(6000, timeout_ms)
                            )
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

                    sel = _selector_from_step(step)
                    if not sel and action not in (
                        "assert_text_contains",
                        "assert_url_contains",
                    ):
                        raise ValueError(f"{action} requiere selector")

                    if action == "fill":
                        val = _safe_str(step.get("value"))
                        page.locator(sel).wait_for(
                            state="visible", timeout=timeout_ms
                        )
                        page.fill(sel, val, timeout=timeout_ms)
                        _record_step(i, step, "passed")
                        continue

                    if action == "click":
                        page.locator(sel).wait_for(
                            state="visible", timeout=timeout_ms
                        )
                        page.click(sel, timeout=timeout_ms)
                        _record_step(i, step, "passed")
                        continue

                    if action == "press":
                        key = _safe_str(step.get("key") or "Enter")
                        page.locator(sel).wait_for(
                            state="visible", timeout=timeout_ms
                        )
                        page.press(sel, key, timeout=timeout_ms)
                        _record_step(
                            i, step, "passed", extra={"key": key}
                        )
                        continue

                    if action == "assert_visible":
                        page.locator(sel).wait_for(
                            state="visible", timeout=timeout_ms
                        )
                        _record_step(i, step, "passed")
                        continue

                    if action == "assert_not_visible":
                        loc = page.locator(sel)
                        if loc.is_visible():
                            raise AssertionError(
                                f"assert_not_visible falló: se mostró {sel}"
                            )
                        _record_step(i, step, "passed")
                        continue

                    if action == "assert_url_contains":
                        needle = _safe_str(
                            step.get("value")
                            or step.get("text")
                            or step.get("contains")
                            or ""
                        ).strip()
                        if not needle:
                            raise ValueError(
                                "assert_url_contains requiere value"
                            )
                        current = page.url or ""
                        if needle not in current:
                            raise AssertionError(
                                f"assert_url_contains falló: '{needle}' "
                                f"no está en '{current}'"
                            )
                        _record_step(
                            i,
                            step,
                            "passed",
                            extra={"current_url": current},
                        )
                        continue

                    if action == "assert_text_contains":
                        expected_text = _safe_str(
                            step.get("expected") or step.get("text") or ""
                        ).strip()
                        if not expected_text:
                            raise ValueError(
                                "assert_text_contains requiere expected/text"
                            )
                        target_sel = _selector_from_step(step) or "body"
                        loc = page.locator(target_sel)
                        loc.wait_for(
                            state="visible", timeout=timeout_ms
                        )
                        content = (
                            loc.inner_text(timeout=timeout_ms) or ""
                        ).strip()
                        if expected_text not in content:
                            raise AssertionError(
                                "Texto no encontrado. Expected contiene: "
                                f"'{expected_text}'"
                            )
                        _record_step(
                            i,
                            step,
                            "passed",
                            extra={"target": target_sel},
                        )
                        continue

                    raise ValueError(f"Acción no soportada: {action}")

                except PlaywrightTimeoutError as e:
                    outcome = "fail"
                    reason = (
                        f"Timeout en step {i + 1}: {action} — "
                        f"{type(e).__name__}: {e}"
                    )
                    logs.append(reason)
                    _record_step(i, step, "failed", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except (AssertionError, ValueError) as e:
                    outcome = "fail"
                    reason = (
                        f"Fallo en step {i + 1}: {action} — "
                        f"{type(e).__name__}: {e}"
                    )
                    logs.append(reason)
                    _record_step(i, step, "failed", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except PlaywrightError as e:
                    outcome = "fail"
                    had_error = True
                    reason = (
                        f"Playwright error en step {i + 1}: {action} — "
                        f"{type(e).__name__}: {e}"
                    )
                    logs.append(reason)
                    _record_step(i, step, "error", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except Exception as e:
                    outcome = "fail"
                    had_error = True
                    reason = (
                        f"Error inesperado en step {i + 1}: {action} — "
                        f"{type(e).__name__}: {e}"
                    )
                    logs.append(reason)
                    _record_step(i, step, "error", err=reason)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

            if screenshot_b64 is None:
                try:
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                except Exception as e:
                    logs.append(
                        f"Final screenshot failed: {type(e).__name__}: {e}"
                    )

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

    status = _final_status(expected_norm, outcome, had_error)

    if status == "passed" and expected_norm == "fail" and outcome == "fail":
        reason = "Falló como se esperaba (caso negativo)."
    elif reason is None:
        reason = "OK"

    ok = status == "passed"

    return {
        "ok": ok,
        "status": status,
        "expected": expected_norm,
        "outcome": outcome,
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
