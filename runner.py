# runner.py
# ============================================================
# Playwright Runner (Retail / E2E)
# - Ejecuta steps Playwright
# - Devuelve screenshot_b64 para UI
# - Soporta timeout global (timeout_s) SIN afectar screenshot robust
# ============================================================

from __future__ import annotations

import base64
import time
import uuid
from typing import Any, Dict, List, Optional, Tuple

from playwright.sync_api import Error as PlaywrightError
from playwright.sync_api import TimeoutError as PlaywrightTimeoutError
from playwright.sync_api import sync_playwright


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
    # per-step override
    return _as_int(step.get("timeout_ms"), default_ms)


def _step_label(i: int, step: Dict[str, Any]) -> str:
    a = _normalize_action(step)
    tgt = step.get("selector") or step.get("url") or step.get("text") or ""
    return f"{i+1:02d}. {a} {tgt}".strip()


def _selector_from_step(step: Dict[str, Any]) -> str:
    # prefer selector; fallbacks used by generator sometimes
    sel = step.get("selector") or step.get("target") or step.get("loc") or ""
    return _safe_str(sel).strip()


def _url_from_step(step: Dict[str, Any], base_url: Optional[str]) -> str:
    url = (step.get("url") or step.get("href") or "").strip()
    if url:
        return url
    # compat: sometimes generator sends {"action":"goto","path":"/login"}
    path = (step.get("path") or "").strip()
    if path and base_url:
        if base_url.endswith("/") and path.startswith("/"):
            return base_url[:-1] + path
        if (not base_url.endswith("/")) and (not path.startswith("/")):
            return base_url + "/" + path
        return base_url + path
    return base_url or ""


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
    NAV_STABILIZE_MS = 1500
    SHOT_TIMEOUT_MS = 15000
    RETRIES = 2

    try:
        page.wait_for_timeout(NAV_STABILIZE_MS)
    except Exception:
        pass

    last_err = None
    for attempt in range(RETRIES + 1):
        try:
            # full_page=True puede fallar en sitios pesados; intentamos primero full y luego normal
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

    logs.append(f"Screenshot: failed окончательно: {type(last_err).__name__}: {last_err}" if last_err else "Screenshot: failed")
    return None, logs


# ============================================================
# Runner
# ============================================================
def execute_test(
    steps: List[Dict[str, Any]],
    base_url: Optional[str] = None,
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
    timeout_s: Optional[int] = None,  # ✅ timeout global (NO afecta screenshot_robust)
) -> Dict[str, Any]:
    """
    Ejecuta steps Playwright.

    Acciones soportadas:
      goto, fill, click, press, assert_visible, assert_text_contains, wait_ms
    Compatibilidad:
      wait_for, wait_for_selector -> assert_visible

    Retorna:
      {
        ok: bool,
        status: "passed" | "fail" | "error",
        error: str | None,
        evidence_id: str,
        steps: [...],
        logs: [...],
        screenshot_b64: str | None,
        duration_ms: int,
        meta: { headless, steps_count, base_url, timeout_ms }
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
            "status": "fail",
            "error": "steps vacío o inválido",
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

    status = "passed"
    ok = True
    error_msg: Optional[str] = None

    def _record_step(i: int, step: Dict[str, Any], st: str, err: Optional[str] = None, extra: Optional[Dict[str, Any]] = None):
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

    # Execute in Playwright
    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=headless)
            context = browser.new_context(viewport={"width": vw, "height": vh})

            # ✅ Timeout global (A): aplica a toda interacción/espera,
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
                        # OJO: dejamos el goto con timeout por step si viene; si no, usa default/global
                        page.goto(url, wait_until="domcontentloaded", timeout=timeout_ms)
                        _record_step(i, step, "passed", extra={"resolved_url": url})
                        continue

                    if action == "wait_ms":
                        ms = _as_int(step.get("ms"), 500)
                        page.wait_for_timeout(ms)
                        _record_step(i, step, "passed", extra={"ms": ms})
                        continue

                    # Para acciones con selector
                    sel = _selector_from_step(step)
                    if not sel and action not in ("assert_text_contains",):
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

                    if action == "assert_text_contains":
                        # puede venir selector + expected o solo expected (en ese caso usa body)
                        expected = _safe_str(step.get("expected") or step.get("text") or "").strip()
                        if not expected:
                            raise ValueError("assert_text_contains requiere expected/text")
                        target_sel = _selector_from_step(step) or "body"
                        loc = page.locator(target_sel)
                        loc.wait_for(state="visible", timeout=timeout_ms)
                        content = (loc.inner_text(timeout=timeout_ms) or "").strip()
                        if expected not in content:
                            raise AssertionError(f"Texto no encontrado. Expected contiene: '{expected}'")
                        _record_step(i, step, "passed", extra={"target": target_sel})
                        continue

                    raise ValueError(f"Acción no soportada: {action}")

                except (PlaywrightTimeoutError,) as e:
                    ok = False
                    status = "fail"
                    error_msg = f"Timeout en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(error_msg)
                    _record_step(i, step, "failed", err=error_msg)
                    # screenshot en fallo (NO depende de timeout global)
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except (AssertionError, ValueError) as e:
                    ok = False
                    status = "fail"
                    error_msg = f"Fallo en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(error_msg)
                    _record_step(i, step, "failed", err=error_msg)
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except PlaywrightError as e:
                    ok = False
                    status = "error"
                    error_msg = f"Playwright error en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(error_msg)
                    _record_step(i, step, "error", err=error_msg)
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except Exception as e:
                    ok = False
                    status = "error"
                    error_msg = f"Error inesperado en step {i+1}: {action} — {type(e).__name__}: {e}"
                    logs.append(error_msg)
                    _record_step(i, step, "error", err=error_msg)
                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

            # Screenshot final (si pasó y aún no hay)
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
        ok = False
        status = "error"
        error_msg = f"Runner crashed: {type(e).__name__}: {e}"
        logs.append(error_msg)

    duration_ms = int((time.time() - t0) * 1000)

    return {
        "ok": ok,
        "status": status,
        "error": error_msg,
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