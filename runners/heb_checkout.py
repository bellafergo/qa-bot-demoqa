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
# - mode="purchase": compra completa (Recoger en tienda + Pago al recibir + Comprar ahora)
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
                try:
                    page.keyboard.press("Escape")
                except Exception:
                    pass
                for pat in (r"Cerrar", r"✕", r"\bX\b"):
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
                Cierra el popup grande de "¿Olvidas algo?" que bloquea el flujo.
                En tus pantallas se ve como modal con X y contenido 'Antojos de último momento'.
                """
                try:
                    title = page.get_by_text(re.compile(r"¿Olvidas algo\?", re.IGNORECASE)).first
                    if title.is_visible(timeout=1200):
                        snap("popup_olvidas_algo_visible")

                        # Intentos: selector conocido + X por role/text
                        for loc in [
                            page.locator("button.icon-remove").first,  # ✅ visto en tu DOM
                            page.get_by_role("button", name=re.compile(r"Cerrar|✕|X", re.IGNORECASE)).first,
                            page.locator("div[role='dialog'] button").filter(
                                has_text=re.compile(r"^x$|^×$|cerrar", re.IGNORECASE)
                            ).first,
                            page.locator("div[role='dialog'] button").first,
                        ]:
                            try:
                                if loc.is_visible(timeout=800):
                                    loc.click(timeout=3000)
                                    page.wait_for_timeout(400)
                                    logs.append("[HEB] Cerrado popup '¿Olvidas algo?'.")
                                    snap("popup_olvidas_algo_closed")
                                    return
                            except Exception:
                                pass

                        # fallback: Escape
                        try:
                            page.keyboard.press("Escape")
                            page.wait_for_timeout(400)
                            logs.append("[HEB] Intento cierre popup '¿Olvidas algo?' con ESC.")
                            snap("popup_olvidas_algo_closed_esc")
                        except Exception:
                            pass
                except Exception:
                    pass

            def _close_feedback_modal() -> None:
                """
                Cierra el modal de encuesta: 'Ayúdanos a seguir mejorando'
                que aparece en /checkout/orderPlaced (tu última pantalla).
                """
                try:
                    header = page.get_by_text(re.compile(r"Ayúdanos a seguir mejorando", re.IGNORECASE)).first
                    if header.is_visible(timeout=1200):
                        snap("feedback_modal_visible")

                        for loc in [
                            page.get_by_role("button", name=re.compile(r"Cerrar|✕|X", re.IGNORECASE)).first,
                            page.locator("div[role='dialog'] button").filter(
                                has_text=re.compile(r"^x$|^×$|cerrar", re.IGNORECASE)
                            ).first,
                            page.locator("div[role='dialog'] button").first,
                        ]:
                            try:
                                if loc.is_visible(timeout=800):
                                    loc.click(timeout=3000)
                                    page.wait_for_timeout(350)
                                    logs.append("[HEB] Modal de encuesta cerrado.")
                                    snap("feedback_modal_closed")
                                    return
                            except Exception:
                                pass

                        # fallback: ESC
                        try:
                            page.keyboard.press("Escape")
                            page.wait_for_timeout(350)
                            logs.append("[HEB] Intento cierre modal encuesta con ESC.")
                            snap("feedback_modal_closed_esc")
                        except Exception:
                            pass
                except Exception:
                    pass

            def _detect_out_of_stock_toast() -> bool:
                try:
                    toast = page.get_by_text(re.compile(r"ya no se encuentra disponible", re.IGNORECASE)).first
                    toast.wait_for(timeout=1500)
                    logs.append("[HEB] Toast OOS detectado (producto no disponible).")
                    snap("toast_oos_detected")
                    return True
                except Exception:
                    return False

            def _try_cerrar_modal_tienda(max_tries: int = 4) -> None:
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

            def _get_password_input():
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

                login_el.click(timeout=60000)
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
                    btn.wait_for(timeout=12000)
                    btn.click(timeout=12000)
                    advanced = True
                except Exception:
                    pass
                if not advanced:
                    email_input.press("Enter")

                # Password
                pwd = _get_password_input()
                pwd.fill(password)
                page.wait_for_timeout(250)

                try:
                    page.get_by_role("button", name=re.compile(r"Iniciar sesión|Acceder|Entrar|Continuar|Siguiente", re.IGNORECASE)).first.click(timeout=60000)
                except Exception:
                    page.get_by_text(re.compile(r"Iniciar sesión|Acceder|Entrar|Continuar|Siguiente", re.IGNORECASE)).first.click(timeout=60000)

                page.wait_for_timeout(2500)
                snap("post_login")
                logs.append("[HEB] Login completado (best-effort).")

            def buscar_y_agregar(termino: str, cantidad: int = 1, step_prefix: str = "") -> bool:
                logs.append(f"[HEB] Buscar/agregar: {termino} (qty={cantidad})")
                _try_cerrar_modal_tienda()
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
                    add_btn.scroll_into_view_if_needed(timeout=5000)
                except Exception:
                    pass

                try:
                    add_btn.click(timeout=12000)
                except Exception:
                    try:
                        _dismiss_overlays_quick()
                        add_btn.click(timeout=8000, force=True)
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

            # ---------- checkout navigation (confirmado por tus pantallas) ----------
            def _minicart_finalizar_compra() -> None:
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
                        loc.wait_for(state="visible", timeout=12000)
                        loc.click(timeout=15000)
                        snap("minicart_finalizar_compra_clicked")
                        page.wait_for_url(re.compile(r"/checkout/#/cart"), timeout=25000)
                        snap("checkout_cart_loaded")
                        return
                    except Exception as e:
                        last_err = e

                raise AssertionError(f"No pude clickear 'Finalizar compra' mini-cart. last_err={last_err}")

            def _cart_proceder_al_pago() -> None:
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()
                link = page.locator("a#cart-to-orderform").first
                link.wait_for(state="visible", timeout=15000)
                link.click(timeout=15000)
                snap("cart_proceder_al_pago_clicked")
                page.wait_for_url(re.compile(r"/checkout/#/shipping"), timeout=30000)
                snap("checkout_shipping_loaded")

            def _shipping_pickup_recoger_en_tienda() -> None:
                """
                Alineado a tus pantallas:
                - Popup '¿Olvidas algo?' puede salir aquí
                - Debe existir 'Elige una fecha de entrega'
                - Botón 'Continuar a método de pago'
                """
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()

                snap("shipping_start")

                # Pick and Go / Recoger (best-effort por texto)
                if pickup_mode == "recoger_en_tienda":
                    try:
                        page.get_by_text(re.compile(r"Pick\s*&\s*Go|Recoger|Recoger en tienda", re.IGNORECASE)).first.click(timeout=10000)
                        page.wait_for_timeout(800)
                        snap("shipping_pickup_selected")
                    except Exception:
                        logs.append("[HEB] No pude seleccionar Pick&Go/Recoger (puede ya estar default).")

                # Fecha
                try:
                    btn_fecha = page.get_by_role("button", name=re.compile(r"Elige una fecha de entrega", re.IGNORECASE)).first
                    btn_fecha.click(timeout=20000)
                    page.wait_for_timeout(800)
                    snap("shipping_calendar_open")

                    # Seleccionar primer día disponible (best-effort)
                    day_btn = page.locator("button:not([disabled])").filter(has_text=re.compile(r"^\d{1,2}$")).first
                    try:
                        day_btn.click(timeout=15000)
                        page.wait_for_timeout(600)
                        snap("shipping_date_selected")
                    except Exception:
                        logs.append("[HEB] No pude seleccionar día del calendario (best-effort).")
                except Exception as e:
                    logs.append(f"[HEB] No pude abrir selector de fecha: {type(e).__name__}: {e}")

                # Continuar a pago
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()
                candidates = [
                    page.get_by_role("button", name=re.compile(r"Continuar a método de pago", re.IGNORECASE)).first,
                    page.get_by_role("button", name=re.compile(r"Continuar", re.IGNORECASE)).first,
                ]
                last_err = None
                for loc in candidates:
                    try:
                        loc.wait_for(state="visible", timeout=20000)
                        loc.click(timeout=30000)
                        snap("shipping_continue_to_payment_clicked")
                        page.wait_for_url(re.compile(r"/checkout/#/payment"), timeout=30000)
                        snap("checkout_payment_loaded")
                        return
                    except Exception as e:
                        last_err = e
                raise AssertionError(f"No pude avanzar a Payment desde Shipping. last_err={last_err}")

            # ------------------- PAYMENT (según tus pantallas) -------------------
            def _payment_select_pago_al_recibir() -> None:
                """
                Selecciona 'Pago al recibir' (como en tus pantallas).
                NOTA: el parámetro payment_mode se conserva en meta; hoy forzamos 'pago al recibir' para demo sin tarjeta.
                """
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()
                snap("payment_start")

                patterns = [
                    r"Pago al recibir",
                    r"Pago al contado",
                    r"Efectivo",
                ]

                last_err = None
                for pat in patterns:
                    try:
                        opt = page.get_by_text(re.compile(pat, re.IGNORECASE)).first
                        opt.wait_for(timeout=12000)
                        opt.click(timeout=12000)
                        page.wait_for_timeout(700)
                        snap("payment_pago_al_recibir_selected")
                        logs.append(f"[HEB] Método de pago seleccionado: {pat}")
                        return
                    except Exception as e:
                        last_err = e

                logs.append(f"[HEB] No pude seleccionar 'Pago al recibir' por texto (best-effort). last_err={last_err}")

            def _click_place_order() -> None:
                """
                Click final: 'Comprar ahora' (como en tus pantallas).
                """
                _close_heb_suggested_popup()
                _dismiss_overlays_quick()
                snap("payment_pre_place_order")

                # Scroll al final
                try:
                    page.keyboard.press("End")
                    page.wait_for_timeout(800)
                except Exception:
                    pass

                candidates = [
                    page.get_by_role("button", name=re.compile(r"Comprar ahora", re.IGNORECASE)).first,  # ✅ visto en tu pantalla
                    page.locator('button:has-text("Comprar ahora")').first,
                    page.get_by_role("button", name=re.compile(r"Realizar pedido|Confirmar|Finalizar", re.IGNORECASE)).first,
                    page.locator('button[type="submit"]').first,
                ]

                last_err = None
                for i, loc in enumerate(candidates, start=1):
                    try:
                        loc.wait_for(state="visible", timeout=15000)
                        loc.click(timeout=20000)
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
                Extrae el número en pantalla final (tu screenshot):
                'Orden #1602916184814-01'
                """
                snap("orderplaced_page")

                # 1) Directo por texto visible
                try:
                    loc = page.get_by_text(re.compile(r"Orden\s*#\s*\d{6,}-\d{1,3}", re.IGNORECASE)).first
                    loc.wait_for(timeout=15000)
                    txt = (loc.inner_text(timeout=2000) or "").strip()
                    m = re.search(r"Orden\s*#\s*([0-9]{6,}-[0-9]{1,3})", txt, re.IGNORECASE)
                    if m:
                        return m.group(1).strip()
                except Exception:
                    pass

                # 2) Fallback: body text
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
                    # Checkout path confirmado por tus pantallas:
                    # mini-cart -> /checkout/#/cart -> a#cart-to-orderform -> shipping -> payment
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

                        # Esperar redirección a orderPlaced (como tu pantalla)
                        try:
                            page.wait_for_url(re.compile(r"/checkout/orderPlaced", re.IGNORECASE), timeout=60000)
                            snap("orderplaced_loaded")
                        except Exception as e:
                            logs.append(f"[HEB] No vi /orderPlaced en URL (best-effort): {type(e).__name__}: {e}")

                        # Cerrar encuesta
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
        },
    }
