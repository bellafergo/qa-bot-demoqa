# runners/heb_checkout.py
from __future__ import annotations

import os
import re
import time
import uuid
from typing import Any, Dict, List, Optional, Tuple

from dotenv import load_dotenv
from playwright.sync_api import Error as PlaywrightError
from playwright.sync_api import TimeoutError as PlaywrightTimeoutError
from playwright.sync_api import sync_playwright

from runners.common import _as_int, _final_status, _norm_expected
from runners.screenshot import take_screenshot_robust

load_dotenv()

# ============================================================
# HEB Checkout Runner (Demo-ready)
# - mode="cart": agrega productos y termina
# - mode="checkout": llega hasta payment (sin ordenar)
# - mode="purchase": compra completa (Pick&Go + Pago al recibir + Comprar ahora)
#
# IMPORTANT:
# - mode="purchase" puede generar órdenes reales.
# ============================================================


def execute_heb_full_purchase(
    products: Optional[List[str]] = None,
    mode: str = "checkout",  # "cart" | "checkout" | "purchase"
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
    timeout_s: Optional[int] = None,
    expected: Optional[str] = None,
    pickup_mode: str = "recoger_en_tienda",  # "recoger_en_tienda" (Pick&Go)
    payment_mode: str = "pago_al_recibir",   # "pago_al_recibir" (según tus pantallas)
) -> Dict[str, Any]:
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

    # ---------- Parse products ----------
    # None/[] => no agrega nada, usa lo que ya esté en carrito.
    raw_products = products or []
    parsed_products: List[Tuple[str, int]] = []

    def _parse_product_entry(x: str) -> Tuple[str, int]:
        s = (x or "").strip()
        if not s:
            return ("", 0)
        m = re.search(r"(.*?)(?:\s*x\s*(\d+)|:(\d+)|\((\d+)\))\s*$", s, re.IGNORECASE)
        if m:
            term = (m.group(1) or "").strip()
            qty = m.group(2) or m.group(3) or m.group(4) or "1"
            try:
                q = max(1, int(qty))
            except Exception:
                q = 1
            return (term, q)
        return (s, 1)

    for x in raw_products:
        if isinstance(x, str) and x.strip():
            term, qty = _parse_product_entry(x)
            if term and qty > 0:
                parsed_products.append((term, qty))

    # ---------- Credentials ----------
    email = (os.getenv("HEB_EMAIL") or "").strip()
    password = (os.getenv("HEB_PASSWORD") or "").strip()

    if not email or not password:
        reason = "Faltan variables de entorno HEB_EMAIL y/o HEB_PASSWORD"
        logs.append(reason)
        outcome = "fail"
        had_error = True
        duration_ms = int((time.time() - t0) * 1000)
        status = _final_status(expected_norm, outcome, had_error)
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
                "timeout_ms": None,
                "viewport": viewport or {"width": 1920, "height": 1080},
                "mode": mode,
            },
        }

    # ---------- Viewport ----------
    if not isinstance(viewport, dict):
        viewport = {"width": 1920, "height": 1080}
    vw = _as_int(viewport.get("width"), 1920)
    vh = _as_int(viewport.get("height"), 1080)

    # ---------- Global timeout ----------
    timeout_ms_global: Optional[int] = None
    if timeout_s is not None:
        timeout_ms_global = max(15000, int(timeout_s) * 1000)

    page = None
    browser = None
    context = None

    order_number: Optional[str] = None

    # Opcionales para hacer el flujo más determinístico
    heb_city = (os.getenv("HEB_CITY") or "Monterrey").strip()
    heb_store_name = (os.getenv("HEB_STORE_NAME") or "").strip()  # opcional: "HEB Contry"
    pickup_responsible = (os.getenv("HEB_PICKUP_RESPONSIBLE") or "QA Bot").strip()

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=headless)
            context = browser.new_context(viewport={"width": vw, "height": vh})

            if timeout_ms_global is not None:
                context.set_default_timeout(timeout_ms_global)
                context.set_default_navigation_timeout(timeout_ms_global)
                logs.append(f"[HEB] Global timeout applied: {timeout_ms_global}ms")

            page = context.new_page()

            # ------------------- helpers -------------------
            def snap(step_name: str) -> None:
                nonlocal screenshot_b64
                if page is None:
                    return
                try:
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    if shot:
                        screenshot_b64 = shot
                        steps.append({"name": step_name, "screenshot_b64": shot})
                        logs.append(f"[HEB] Screenshot OK: {step_name}")
                except Exception as e:
                    logs.append(f"[HEB] Screenshot failed at {step_name}: {type(e).__name__}: {e}")

            def _dismiss_overlays_quick() -> None:
                if page is None:
                    return
                try:
                    page.keyboard.press("Escape")
                except Exception:
                    pass
                for pat in (r"Cerrar", r"✕", r"\bX\b", r"Entendido", r"Aceptar", r"Aceptar todo"):
                    try:
                        page.get_by_role("button", name=re.compile(pat, re.IGNORECASE)).first.click(timeout=1200)
                        page.wait_for_timeout(200)
                    except Exception:
                        pass
                try:
                    page.mouse.click(10, 10)
                except Exception:
                    pass

            def _close_heb_suggested_popup() -> None:
                """
                Cierra el popup grande de "¿Olvidas algo?" (puede salir antes o después del CTA).
                """
                if page is None:
                    return
                try:
                    title = page.get_by_text(re.compile(r"¿Olvidas algo\?", re.IGNORECASE)).first
                    if title.is_visible(timeout=900):
                        snap("popup_olvidas_algo_visible")

                        for loc in [
                            page.locator("button.icon-remove").first,  # visto en DOM
                            page.get_by_role("button", name=re.compile(r"Cerrar|✕|X", re.IGNORECASE)).first,
                            page.locator("div[role='dialog'] button").filter(
                                has_text=re.compile(r"^x$|^×$|cerrar", re.IGNORECASE)
                            ).first,
                            page.locator("div[role='dialog'] button").first,
                        ]:
                            try:
                                if loc.is_visible(timeout=600):
                                    loc.click(timeout=2500)
                                    page.wait_for_timeout(350)
                                    logs.append("[HEB] Cerrado popup '¿Olvidas algo?'.")
                                    snap("popup_olvidas_algo_closed")
                                    return
                            except Exception:
                                pass

                        try:
                            page.keyboard.press("Escape")
                            page.wait_for_timeout(300)
                            logs.append("[HEB] Intento cierre popup '¿Olvidas algo?' con ESC.")
                            snap("popup_olvidas_algo_closed_esc")
                        except Exception:
                            pass
                except Exception:
                    pass

            def _close_feedback_modal() -> None:
                """
                Cierra el modal de encuesta: 'Ayúdanos a seguir mejorando'
                """
                if page is None:
                    return
                try:
                    header = page.get_by_text(re.compile(r"Ayúdanos a seguir mejorando", re.IGNORECASE)).first
                    if header.is_visible(timeout=900):
                        snap("feedback_modal_visible")

                        for loc in [
                            page.get_by_role("button", name=re.compile(r"Cerrar|✕|X", re.IGNORECASE)).first,
                            page.locator("div[role='dialog'] button").filter(
                                has_text=re.compile(r"^x$|^×$|cerrar", re.IGNORECASE)
                            ).first,
                            page.locator("div[role='dialog'] button").first,
                        ]:
                            try:
                                if loc.is_visible(timeout=600):
                                    loc.click(timeout=2500)
                                    page.wait_for_timeout(300)
                                    logs.append("[HEB] Modal de encuesta cerrado.")
                                    snap("feedback_modal_closed")
                                    return
                            except Exception:
                                pass

                        try:
                            page.keyboard.press("Escape")
                            page.wait_for_timeout(300)
                            logs.append("[HEB] Intento cierre modal encuesta con ESC.")
                            snap("feedback_modal_closed_esc")
                        except Exception:
                            pass
                except Exception:
                    pass

            def _detect_out_of_stock_toast() -> bool:
                if page is None:
                    return False
                try:
                    toast = page.get_by_text(re.compile(r"ya no se encuentra disponible", re.IGNORECASE)).first
                    toast.wait_for(timeout=1500)
                    logs.append("[HEB] Toast OOS detectado (producto no disponible).")
                    snap("toast_oos_detected")
                    return True
                except Exception:
                    return False

            def _try_cerrar_modal_tienda(max_tries: int = 4) -> None:
                if page is None:
                    return
                try:
                    modal = page.locator('[data-testid="base-modal"]')
                except Exception:
                    return
                for _ in range(max_tries):
                    try:
                        if not modal.is_visible():
                            return
                        snap("modal_tienda_visible")
                        try:
                            page.keyboard.press("Escape")
                            page.wait_for_timeout(300)
                            if not modal.is_visible():
                                snap("modal_tienda_closed")
                                return
                        except Exception:
                            pass
                        for label in ("Confirmar", "Guardar", "Continuar", "Aceptar"):
                            try:
                                page.get_by_role("button", name=re.compile(label, re.IGNORECASE)).first.click(timeout=2500)
                                page.wait_for_timeout(450)
                                if not modal.is_visible():
                                    snap("modal_tienda_closed")
                                    return
                            except Exception:
                                continue
                        try:
                            page.get_by_role("button", name=re.compile(r"Cerrar|✕|X", re.IGNORECASE)).first.click(timeout=2500)
                            page.wait_for_timeout(450)
                            if not modal.is_visible():
                                snap("modal_tienda_closed")
                                return
                        except Exception:
                            pass
                    except Exception:
                        pass

            # ---------- robust click ----------
            def _js_scroll_bottom() -> None:
                if page is None:
                    return
                try:
                    page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
                except Exception:
                    pass

            def _js_scroll_into_view(locator) -> None:
                if page is None:
                    return
                try:
                    page.evaluate("(el)=>el && el.scrollIntoView({block:'center', inline:'center'})", locator)
                except Exception:
                    pass

            def _click_robust(loc, name: str, tries: int = 6) -> None:
                """
                Click robusto para VTEX:
                - Cierra overlays
                - Scroll bottom / center
                - Reintenta normal -> force -> JS click
                """
                if page is None:
                    raise AssertionError("page is None in _click_robust")

                last_err = None
                for i in range(1, tries + 1):
                    try:
                        _close_heb_suggested_popup()
                        _dismiss_overlays_quick()
                        _try_cerrar_modal_tienda()

                        # Scroll
                        try:
                            page.keyboard.press("End")
                            page.wait_for_timeout(350)
                        except Exception:
                            pass
                        _js_scroll_bottom()
                        page.wait_for_timeout(250)

                        try:
                            loc.wait_for(state="attached", timeout=8000)
                        except Exception:
                            pass

                        try:
                            loc.scroll_into_view_if_needed(timeout=5000)
                        except Exception:
                            pass

                        # a veces el contenedor con scroll no es window
                        try:
                            _js_scroll_into_view(loc)
                        except Exception:
                            pass

                        # intento normal
                        try:
                            loc.click(timeout=6000)
                            logs.append(f"[HEB] Click OK: {name} (try {i})")
                            return
                        except Exception as e1:
                            last_err = e1

                        # force click
                        try:
                            loc.click(timeout=4000, force=True)
                            logs.append(f"[HEB] Force click OK: {name} (try {i})")
                            return
                        except Exception as e2:
                            last_err = e2

                        # JS click
                        try:
                            page.evaluate("(el)=>el && el.click()", loc)
                            logs.append(f"[HEB] JS click OK: {name} (try {i})")
                            return
                        except Exception as e3:
                            last_err = e3

                    except Exception as e:
                        last_err = e

                    # si falló, espera un poco y reintenta
                    try:
                        page.wait_for_timeout(600 + (i * 150))
                        snap(f"click_retry_{name}_{i}")
                    except Exception:
                        pass

                raise AssertionError(f"No pude clickear {name}. last_err={last_err}")

            # ---------- login helpers ----------
            def _get_password_input():
                if page is None:
                    raise AssertionError("page is None in _get_password_input")
                candidates = [
                    lambda: page.get_by_placeholder(re.compile(r"Contraseña|Password", re.IGNORECASE)).first,
                    lambda: page.get_by_label(re.compile(r"Contraseña|Password", re.IGNORECASE)).first,
                    lambda: page.locator('input[type="password"]').first,
                ]
                last_err = None
                for getter in candidates:
                    try:
                        el = getter()
                        el.wait_for(timeout=15000)
                        if el.is_visible():
                            return el
                    except Exception as e:
                        last_err = e
                snap("login_no_password_input")
                raise AssertionError(f"No se encontró campo contraseña. last_err={last_err}")

            def _login() -> None:
                if page is None:
                    raise AssertionError("page is None in _login")
                page.goto(base_url, wait_until="commit", timeout=90000)
                page.wait_for_timeout(2000)
                snap("home")

                # Cookies/promos
                try:
                    page.get_by_role("button", name=re.compile(r"Aceptar|Aceptar todo|Entendido|Ok", re.IGNORECASE)).click(timeout=2500)
                    snap("cookies_accepted")
                except Exception:
                    pass
                try:
                    page.get_by_role("button", name=re.compile(r"Cerrar|✕|X", re.IGNORECASE)).first.click(timeout=2000)
                    snap("promo_closed")
                except Exception:
                    pass

                _try_cerrar_modal_tienda()
                _dismiss_overlays_quick()

                # Abrir login
                login_candidates = [
                    page.get_by_role("link", name=re.compile(r"Iniciar sesión|Inicia sesión|Identifícate|Mi cuenta", re.IGNORECASE)).first,
                    page.get_by_role("button", name=re.compile(r"Iniciar sesión|Inicia sesión|Identifícate|Mi cuenta", re.IGNORECASE)).first,
                    page.get_by_text(re.compile(r"Iniciar sesión|Inicia sesión|Identifícate|Mi cuenta", re.IGNORECASE)).first,
                ]
                last_err = None
                login_el = None
                for el in login_candidates:
                    try:
                        el.wait_for(timeout=8000)
                        login_el = el
                        break
                    except Exception as e:
                        last_err = e
                if login_el is None:
                    raise AssertionError(f"No se encontró login en home. last_err={last_err}")

                _click_robust(login_el, "open_login", tries=3)
                page.wait_for_timeout(1200)
                snap("login_open")

                # Correo
                try:
                    email_input = page.get_by_placeholder(re.compile(r"Correo electrónico|Correo|Email", re.IGNORECASE)).first
                    email_input.wait_for(timeout=20000)
                except Exception:
                    email_input = page.get_by_role("textbox").first

                email_input.fill(email)
                page.wait_for_timeout(300)

                advanced = False
                try:
                    btn = page.locator("button:has-text('Continuar')").first
                    btn.wait_for(timeout=9000)
                    btn.click(timeout=9000)
                    advanced = True
                except Exception:
                    pass
                if not advanced:
                    try:
                        email_input.press("Enter")
                    except Exception:
                        pass

                # Password
                pwd = _get_password_input()
                pwd.fill(password)
                page.wait_for_timeout(250)

                # CTA login
                try:
                    _click_robust(
                        page.get_by_role("button", name=re.compile(r"Iniciar sesión|Acceder|Entrar|Continuar|Siguiente", re.IGNORECASE)).first,
                        "submit_login",
                        tries=4,
                    )
                except Exception:
                    _click_robust(
                        page.get_by_text(re.compile(r"Iniciar sesión|Acceder|Entrar|Continuar|Siguiente", re.IGNORECASE)).first,
                        "submit_login_text",
                        tries=3,
                    )

                page.wait_for_timeout(2500)
                snap("post_login")
                logs.append("[HEB] Login completado (best-effort).")

            # ---------- search/add ----------
            def buscar_y_agregar(termino: str, cantidad: int = 1, step_prefix: str = "") -> bool:
                if page is None:
                    raise AssertionError("page is None in buscar_y_agregar")

                logs.append(f"[HEB] Buscar/agregar: {termino} (qty={cantidad})")
                _try_cerrar_modal_tienda()
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()

                sb = page.get_by_placeholder("Buscar productos")
                sb.click(timeout=8000)
                sb.fill(termino)
                sb.press("Enter")

                page.wait_for_timeout(4500)
                snap(f"{step_prefix}_results")

                add_btn = None
                try:
                    add_btn = page.get_by_role("button", name=re.compile(r"Agregar", re.IGNORECASE)).first
                    add_btn.wait_for(timeout=10000)
                except Exception:
                    try:
                        add_btn = page.locator('button:has-text("Agregar")').first
                        add_btn.wait_for(timeout=10000)
                    except Exception:
                        add_btn = None

                if add_btn is None:
                    logs.append(f"[HEB] No encontré 'Agregar' para '{termino}'.")
                    snap(f"{step_prefix}_no_add")
                    return False

                try:
                    _click_robust(add_btn, f"add_{termino}", tries=4)
                except Exception as e:
                    logs.append(f"[HEB] Falló click Agregar '{termino}': {type(e).__name__}: {e}")
                    snap(f"{step_prefix}_add_click_failed")
                    return False

                page.wait_for_timeout(900)
                snap(f"{step_prefix}_added")

                if _detect_out_of_stock_toast():
                    logs.append(f"[HEB] Omitiendo '{termino}' por OOS.")
                    return False

                # cantidad>1 (best-effort)
                if cantidad > 1:
                    try:
                        plus = page.get_by_role("button", name=re.compile(r"\+", re.IGNORECASE)).first
                        for _ in range(cantidad - 1):
                            plus.click(timeout=3000)
                            page.wait_for_timeout(250)
                        snap(f"{step_prefix}_qty_{cantidad}")
                    except Exception:
                        logs.append(f"[HEB] No pude ajustar cantidad para '{termino}' (best-effort).")

                return True

            # ---------- checkout navigation ----------
            def _minicart_finalizar_compra() -> None:
                """
                Maneja:
                - 'Finalizar compra'
                - Paso intermedio 'Continuar' (a veces sale)
                - Luego /checkout/#/cart
                """
                if page is None:
                    raise AssertionError("page is None in _minicart_finalizar_compra")

                _close_heb_suggested_popup()
                _dismiss_overlays_quick()

                candidates = [
                    page.get_by_role("button", name=re.compile(r"Finalizar compra", re.IGNORECASE)).first,
                    page.locator("button.vtex-button--see-cart").first,
                    page.locator('button:has-text("Finalizar compra")').first,
                ]
                last_err = None
                for loc in candidates:
                    try:
                        loc.wait_for(state="attached", timeout=9000)
                        _click_robust(loc, "minicart_finalizar_compra", tries=5)
                        snap("minicart_finalizar_compra_clicked")

                        # A veces sale un "Continuar"
                        try:
                            cont_btn = page.get_by_role("button", name=re.compile(r"Continuar", re.IGNORECASE)).first
                            if cont_btn.is_visible(timeout=1200):
                                _click_robust(cont_btn, "minicart_continuar", tries=3)
                                snap("minicart_continuar_clicked")
                        except Exception:
                            pass

                        # Llegar a cart
                        try:
                            page.wait_for_url(re.compile(r"/checkout/#/cart"), timeout=30000)
                        except Exception:
                            # fallback: si el URL no cambia rápido, forzamos
                            page.goto(f"{base_url}/checkout/#/cart", wait_until="commit", timeout=60000)

                        snap("checkout_cart_loaded")
                        return
                    except Exception as e:
                        last_err = e

                raise AssertionError(f"No pude clickear 'Finalizar compra' mini-cart. last_err={last_err}")

            def _cart_proceder_al_pago() -> None:
                """
                FIX PRINCIPAL:
                - El CTA existe pero puede estar hidden/cubierto por popup
                - Reintenta con click robusto y valida navegación a /shipping
                """
                if page is None:
                    raise AssertionError("page is None in _cart_proceder_al_pago")

                _close_heb_suggested_popup()
                _dismiss_overlays_quick()
                _try_cerrar_modal_tienda()

                snap("cart_before_proceder")

                # El CTA real en tus logs:
                # a#cart-to-orderform  ... "Proceder al pago"
                candidates = [
                    page.locator("a#cart-to-orderform").first,
                    page.locator("a.btn-place-order").first,
                    page.locator("a:has-text('Proceder al pago')").first,
                    page.get_by_role("link", name=re.compile(r"Proceder al pago", re.IGNORECASE)).first,
                ]

                last_err = None
                for idx, loc in enumerate(candidates, start=1):
                    try:
                        loc.wait_for(state="attached", timeout=12000)
                        _click_robust(loc, f"cart_proceder_al_pago_candidate_{idx}", tries=6)

                        # Si al click aparece popup, ciérralo y vuelve a validar url
                        _close_heb_suggested_popup()
                        _dismiss_overlays_quick()

                        try:
                            page.wait_for_url(re.compile(r"/checkout/#/shipping"), timeout=35000)
                            snap("checkout_shipping_loaded")
                            return
                        except Exception as e:
                            last_err = e
                            snap(f"cart_after_click_no_shipping_{idx}")
                            # reintenta con siguiente candidato
                    except Exception as e:
                        last_err = e
                        snap(f"cart_proceder_click_failed_{idx}")

                raise AssertionError(f"No pude avanzar a Shipping (Proceder al pago). last_err={last_err}")

            def _shipping_pickup_recoger_en_tienda() -> None:
                """
                Alineado a lo que describiste:
                - Pantalla: 1 email 2 identificación 3 envío
                - Elegir: envío a domicilio vs Pick & Go (Recoger)
                - Si Pick&Go: seleccionar tienda:
                    - dropdown ciudad -> Monterrey -> lista sucursales -> click sucursal -> Confirmar
                - Seleccionar fecha: 'Elige una fecha de entrega'
                - Responsable al recoger: 'Cambiar' -> input -> texto
                - Continuar a método de pago
                """
                if page is None:
                    raise AssertionError("page is None in _shipping_pickup_recoger_en_tienda")

                _close_heb_suggested_popup()
                _dismiss_overlays_quick()

                snap("shipping_start")

                # 1) Elegir Pick&Go / Recoger
                if pickup_mode == "recoger_en_tienda":
                    try:
                        pick = page.get_by_text(re.compile(r"Pick\s*&\s*Go|Recoger|Recoger en tienda", re.IGNORECASE)).first
                        if pick.is_visible(timeout=2500):
                            _click_robust(pick, "shipping_pickup_pickgo", tries=4)
                            page.wait_for_timeout(700)
                            snap("shipping_pickup_selected")
                    except Exception:
                        logs.append("[HEB] No pude seleccionar Pick&Go/Recoger (puede ya estar default).")

                    # 2) Seleccionar tienda (ciudad -> sucursal -> confirmar)
                    #    Esto es clave para que el CTA de continuar no esté bloqueado.
                    try:
                        _close_heb_suggested_popup()
                        _dismiss_overlays_quick()

                        # Abrir dropdown ciudad (best-effort)
                        city_openers = [
                            page.get_by_role("button", name=re.compile(r"Ciudad|Selecciona.*ciudad|Buscar.*ciudad", re.IGNORECASE)).first,
                            page.get_by_text(re.compile(r"Selecciona.*ciudad|Ciudad", re.IGNORECASE)).first,
                            page.locator("div:has-text('Ciudad')").locator("button").first,
                            page.locator("select").first,
                        ]

                        opened = False
                        for op in city_openers:
                            try:
                                if op.is_visible(timeout=1200):
                                    _click_robust(op, "shipping_city_open", tries=3)
                                    opened = True
                                    break
                            except Exception:
                                continue

                        # Buscar input de ciudad (si existe)
                        try:
                            city_input = page.locator("input").filter(has_text=re.compile(r"", re.IGNORECASE)).first
                            # mejor: intenta placeholder
                            city_input = page.get_by_placeholder(re.compile(r"Buscar|Ciudad|Escribe", re.IGNORECASE)).first
                        except Exception:
                            city_input = None

                        if city_input is not None:
                            try:
                                if city_input.is_visible(timeout=1200):
                                    city_input.fill(heb_city)
                                    page.wait_for_timeout(450)
                                    snap("shipping_city_typed")
                            except Exception:
                                pass

                        # Seleccionar Monterrey (o primera opción)
                        try:
                            opt_city = page.get_by_text(re.compile(rf"\b{re.escape(heb_city)}\b", re.IGNORECASE)).first
                            if opt_city.is_visible(timeout=2500):
                                _click_robust(opt_city, "shipping_city_select", tries=3)
                                page.wait_for_timeout(700)
                                snap("shipping_city_selected")
                        except Exception:
                            # fallback: primera opción de lista
                            try:
                                first_opt = page.locator("li").first
                                if first_opt.is_visible(timeout=1200):
                                    _click_robust(first_opt, "shipping_city_first_option", tries=2)
                                    page.wait_for_timeout(600)
                            except Exception:
                                pass

                        # Seleccionar sucursal
                        store_clicked = False
                        try:
                            if heb_store_name:
                                store_loc = page.get_by_text(re.compile(re.escape(heb_store_name), re.IGNORECASE)).first
                                if store_loc.is_visible(timeout=2500):
                                    _click_robust(store_loc, "shipping_store_named", tries=4)
                                    store_clicked = True
                            if not store_clicked:
                                # Cualquier item tipo "HEB Contry / HEB Cumbres / ..."
                                store_loc = page.get_by_text(re.compile(r"\bHEB\b", re.IGNORECASE)).first
                                if store_loc.is_visible(timeout=2500):
                                    _click_robust(store_loc, "shipping_store_first", tries=4)
                                    store_clicked = True
                        except Exception:
                            store_clicked = False

                        if store_clicked:
                            page.wait_for_timeout(500)
                            snap("shipping_store_selected")
                            # Confirmar
                            try:
                                conf = page.get_by_role("button", name=re.compile(r"Confirmar", re.IGNORECASE)).first
                                if conf.is_visible(timeout=2500):
                                    _click_robust(conf, "shipping_store_confirm", tries=4)
                                    page.wait_for_timeout(700)
                                    snap("shipping_store_confirmed")
                            except Exception:
                                pass

                    except Exception as e:
                        logs.append(f"[HEB] No pude seleccionar tienda Pick&Go (best-effort): {type(e).__name__}: {e}")

                # 3) Fecha de entrega
                try:
                    btn_fecha = page.get_by_role("button", name=re.compile(r"Elige una fecha de entrega", re.IGNORECASE)).first
                    if btn_fecha.is_visible(timeout=8000):
                        _click_robust(btn_fecha, "shipping_open_calendar", tries=4)
                        page.wait_for_timeout(800)
                        snap("shipping_calendar_open")

                        # Seleccionar primer día disponible (best-effort)
                        day_btn = page.locator("button:not([disabled])").filter(has_text=re.compile(r"^\d{1,2}$")).first
                        try:
                            _click_robust(day_btn, "shipping_select_first_day", tries=3)
                            page.wait_for_timeout(600)
                            snap("shipping_date_selected")
                        except Exception:
                            logs.append("[HEB] No pude seleccionar día del calendario (best-effort).")
                except Exception as e:
                    logs.append(f"[HEB] No pude abrir selector de fecha (best-effort): {type(e).__name__}: {e}")

                # 4) Responsable al recoger (Cambiar -> input -> texto)
                try:
                    _close_heb_suggested_popup()
                    _dismiss_overlays_quick()

                    cambiar = page.get_by_text(re.compile(r"^Cambiar$", re.IGNORECASE)).first
                    if cambiar.is_visible(timeout=2000):
                        _click_robust(cambiar, "pickup_responsible_change", tries=3)
                        page.wait_for_timeout(400)
                        snap("pickup_responsible_edit_enabled")

                    # Input (best-effort)
                    try:
                        inp = page.locator("input").filter(has_text=re.compile(r"", re.IGNORECASE)).first
                        inp = page.get_by_placeholder(re.compile(r"Responsable|Nombre", re.IGNORECASE)).first
                    except Exception:
                        inp = page.locator("input[type='text']").first

                    if inp.is_visible(timeout=2000):
                        inp.fill(pickup_responsible)
                        page.wait_for_timeout(250)
                        snap("pickup_responsible_filled")

                        # Guardar/Confirmar si aparece
                        for lab in ("Guardar", "Confirmar", "Aceptar"):
                            try:
                                b = page.get_by_role("button", name=re.compile(lab, re.IGNORECASE)).first
                                if b.is_visible(timeout=800):
                                    _click_robust(b, f"pickup_responsible_{lab.lower()}", tries=2)
                                    page.wait_for_timeout(250)
                                    break
                            except Exception:
                                continue
                except Exception:
                    logs.append("[HEB] No pude setear responsable al recoger (best-effort).")

                # 5) Continuar a pago
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()

                candidates = [
                    page.get_by_role("button", name=re.compile(r"Continuar a método de pago", re.IGNORECASE)).first,
                    page.get_by_role("button", name=re.compile(r"Continuar", re.IGNORECASE)).first,
                    page.locator('button:has-text("Continuar a método de pago")').first,
                    page.locator('button:has-text("Continuar")').first,
                ]
                last_err = None
                for loc in candidates:
                    try:
                        if loc.is_visible(timeout=2500):
                            _click_robust(loc, "shipping_continue_to_payment", tries=5)
                            snap("shipping_continue_to_payment_clicked")
                            page.wait_for_url(re.compile(r"/checkout/#/payment"), timeout=45000)
                            snap("checkout_payment_loaded")
                            return
                    except Exception as e:
                        last_err = e
                        continue

                raise AssertionError(f"No pude avanzar a Payment desde Shipping. last_err={last_err}")

            # ------------------- PAYMENT -------------------
            def _payment_select_pago_al_recibir() -> None:
                """
                Selecciona 'Pago al recibir' (como en tus pantallas).
                Acepta también que el chat mande "pagar_en_tienda" (solo meta), pero aquí buscamos por texto.
                """
                if page is None:
                    raise AssertionError("page is None in _payment_select_pago_al_recibir")
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()
                snap("payment_start")

                patterns = [
                    r"Pago al recibir",
                    r"Pago en tienda",
                    r"Pago al contado",
                    r"Efectivo",
                ]

                last_err = None
                for pat in patterns:
                    try:
                        opt = page.get_by_text(re.compile(pat, re.IGNORECASE)).first
                        if opt.is_visible(timeout=3500):
                            _click_robust(opt, f"payment_select_{pat}", tries=4)
                            page.wait_for_timeout(700)
                            snap("payment_pago_al_recibir_selected")
                            logs.append(f"[HEB] Método de pago seleccionado: {pat}")
                            return
                    except Exception as e:
                        last_err = e

                logs.append(f"[HEB] No pude seleccionar método por texto (best-effort). last_err={last_err}")

            def _click_place_order() -> None:
                """
                Click final: 'Comprar ahora' (como en tus pantallas).
                """
                if page is None:
                    raise AssertionError("page is None in _click_place_order")

                _close_heb_suggested_popup()
                _dismiss_overlays_quick()
                snap("payment_pre_place_order")

                try:
                    page.keyboard.press("End")
                    page.wait_for_timeout(800)
                except Exception:
                    pass
                _js_scroll_bottom()
                page.wait_for_timeout(350)

                candidates = [
                    page.get_by_role("button", name=re.compile(r"Comprar ahora", re.IGNORECASE)).first,
                    page.locator('button:has-text("Comprar ahora")').first,
                    page.get_by_role("button", name=re.compile(r"Realizar pedido|Confirmar|Finalizar", re.IGNORECASE)).first,
                    page.locator('button[type="submit"]').first,
                ]

                last_err = None
                for i, loc in enumerate(candidates, start=1):
                    try:
                        if loc.is_visible(timeout=2500):
                            _click_robust(loc, f"payment_place_order_candidate_{i}", tries=6)
                            page.wait_for_timeout(1200)
                            snap(f"payment_place_order_clicked_{i}")
                            logs.append(f"[HEB] Click CTA final (candidate #{i}).")
                            return
                    except Exception as e:
                        last_err = e

                snap("payment_place_order_not_found")
                raise AssertionError(f"No se encontró CTA final ('Comprar ahora'). last_err={last_err}")

            def _extract_order_number() -> Optional[str]:
                """
                Extrae el número en pantalla final:
                'Orden #1602916184814-01'
                """
                if page is None:
                    return None
                snap("orderplaced_page")

                try:
                    loc = page.get_by_text(re.compile(r"Orden\s*#\s*\d{6,}-\d{1,3}", re.IGNORECASE)).first
                    loc.wait_for(timeout=15000)
                    txt = (loc.inner_text(timeout=2000) or "").strip()
                    m = re.search(r"Orden\s*#\s*([0-9]{6,}-[0-9]{1,3})", txt, re.IGNORECASE)
                    if m:
                        return m.group(1).strip()
                except Exception:
                    pass

                try:
                    body_text = page.locator("body").inner_text(timeout=5000)
                    m = re.search(r"Orden\s*#\s*([0-9]{6,}-[0-9]{1,3})", body_text, re.IGNORECASE)
                    if m:
                        return m.group(1).strip()
                except Exception:
                    pass

                return None

            # ------------------- FLOW -------------------
            try:
                if mode not in ("cart", "checkout", "purchase"):
                    raise AssertionError("mode inválido. Usa: cart | checkout | purchase")

                _login()

                # Agregar productos si mandan lista (si no, usamos carrito existente)
                added_any = False
                if parsed_products:
                    for idx, (term, qty) in enumerate(parsed_products, start=1):
                        ok_add = buscar_y_agregar(term, cantidad=qty, step_prefix=f"prod_{idx}")
                        if ok_add:
                            added_any = True
                else:
                    logs.append("[HEB] products vacío/None: no se agrega nada, se usa carrito existente.")
                    snap("no_products_requested")

                if parsed_products and not added_any:
                    raise AssertionError("No se pudo agregar ningún producto (todos OOS o sin Agregar).")

                if mode == "cart":
                    reason = "OK HEB — modo carrito (sin checkout)"
                    snap("cart_mode_end")

                else:
                    # mini-cart -> /checkout/#/cart -> proceder -> shipping -> payment
                    try:
                        _minicart_finalizar_compra()
                    except Exception as e:
                        logs.append(f"[HEB] Mini-cart no disponible: {e}. Fallback /checkout/#/cart")
                        page.goto(f"{base_url}/checkout/#/cart", wait_until="commit", timeout=60000)
                        snap("fallback_checkout_cart")

                    _cart_proceder_al_pago()
                    _shipping_pickup_recoger_en_tienda()

                    if mode == "checkout":
                        reason = "OK HEB — llegó a Payment (sin colocar orden)"
                        snap("checkout_mode_end")

                    else:
                        # mode == "purchase"
                        _payment_select_pago_al_recibir()
                        _click_place_order()

                        try:
                            page.wait_for_url(re.compile(r"/checkout/orderPlaced", re.IGNORECASE), timeout=60000)
                            snap("orderplaced_loaded")
                        except Exception as e:
                            logs.append(f"[HEB] No vi /orderPlaced en URL (best-effort): {type(e).__name__}: {e}")

                        _close_feedback_modal()

                        order_number = _extract_order_number()
                        if order_number:
                            logs.append(f"[HEB] Orden detectada: {order_number}")
                            reason = f"OK HEB — orden creada: {order_number}"
                        else:
                            reason = "OK HEB — orden creada (no pude extraer número en pantalla)"
                            snap("order_number_not_found")

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
            "mode": mode,
            "pickup_mode": pickup_mode,
            "payment_mode": payment_mode,
            "order_number": order_number,
            "heb_city": heb_city,
            "heb_store_name": heb_store_name,
        },
    }
