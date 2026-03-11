# runners/generic_steps.py
from __future__ import annotations

import logging
import time
import uuid
from typing import Any, Dict, List, Optional, Tuple

from dotenv import load_dotenv
from playwright.sync_api import Error as PlaywrightError
from playwright.sync_api import TimeoutError as PlaywrightTimeoutError
from playwright.sync_api import sync_playwright

from runners.common import (
    _as_int,
    _domain_from_url,
    _final_status,
    _normalize_action,
    _now_ms,
    _norm_expected,
    _pick_timeout_ms,
    _safe_str,
    _selector_from_step,
    _url_from_step,
)
from runners.page_context import build_failure_context, capture_element_context, capture_page_context
from runners.screenshot import take_screenshot_robust

from services.selector_healer import resolve_locator
from services.failure_classifier import classify_failure
from services.memory_store import save_memory, load_memory  # ✅ fix real

from core.dom_analyzer import extract_dom_inventory
from core.selector_resolver import (
    resolve_intent,
    build_playwright_target,
    build_well_known_form_target,
    is_intent_only,
)

load_dotenv()

logger = logging.getLogger("vanya.runner.generic")


_SUBMIT_KEYWORDS = frozenset({
    "submit", "login", "sign", "signin", "entrar", "ingresar", "acceder",
})


def _is_submit_click(sel: str, intent: str) -> bool:
    """
    Returns True when the clicked target looks like a submit / login button.
    Used to trigger safe post-click navigation wait.
    """
    hint = (str(sel or "") + " " + str(intent or "")).lower()
    return any(kw in hint for kw in _SUBMIT_KEYWORDS)


def _normalize_ws(s: str) -> str:
    """
    Normalize whitespace for text comparison.
    Collapses tabs, newlines, and runs of spaces into a single space, then strips.
    Preserves case (case-sensitive by default).
    """
    return " ".join(s.split())


def execute_test(
    steps: List[Dict[str, Any]],
    base_url: Optional[str] = None,
    headless: bool = True,
    viewport: Optional[Dict[str, int]] = None,
    timeout_s: Optional[int] = None,
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

    Selector Healing:
      - Si step trae target como dict, se usa tal cual.
      - Si step NO trae target dict, se construye target mínimo con:
        {"intent": "...", "primary": "<selector>"}
      - Si el healer usa fallback != primary, se guarda en memoria.
    """
    t0 = time.time()

    screenshot_b64: Optional[str] = None
    report_steps: List[Dict[str, Any]] = []
    logs: List[str] = []
    evidence_id = f"EV-{uuid.uuid4().hex[:10]}"
    resolution_log: List[Dict[str, Any]] = []
    healing_log: List[Dict[str, Any]] = []       # structured selector healing events
    failure_context: Optional[Dict[str, Any]] = None
    last_page_context: Optional[Dict[str, Any]] = None
    dom_inventory: Optional[Dict[str, Any]] = None  # captured after first goto

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
            "resolution_log": [],
            "failure_context": None,
            "page_context": None,
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
            "target": step.get("target"),
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

    def _intent_from_step(step: Dict[str, Any], fallback: str) -> str:
        t = step.get("target")
        if isinstance(t, dict):
            return _safe_str(t.get("intent") or fallback) or fallback
        return fallback

    def _domain_from_page(page, inferred_base_url: Optional[str]) -> str:
        try:
            if page.url:
                return _domain_from_url(page.url)
        except Exception:
            pass
        if inferred_base_url:
            return _domain_from_url(inferred_base_url)
        if base_url:
            return _domain_from_url(base_url)
        return "unknown"

    def _build_target(step: Dict[str, Any], selector: str) -> Dict[str, Any]:
        action = _normalize_action(step)
        fallback_intent = f"{action}:{selector}" if selector else action
        intent = _intent_from_step(step, fallback=fallback_intent)

        raw_target = step.get("target")
        if isinstance(raw_target, dict):
            if not raw_target.get("intent"):
                raw_target["intent"] = intent
            if not raw_target.get("primary") and selector:
                raw_target["primary"] = selector
            return raw_target

        if isinstance(raw_target, str) and raw_target.strip():
            primary = raw_target.strip()
        else:
            primary = selector

        return {"intent": intent, "primary": primary}

    def _resolve(step: Dict[str, Any], page, selector: str, inferred_base_url: Optional[str], step_index: int = -1):
        domain = _domain_from_page(page, inferred_base_url)
        target = _build_target(step, selector)
        intent = _safe_str(target.get("intent") or "unknown") or "unknown"

        # ── DOM-aware intent resolution ──────────────────────────────────
        # When the selector is a plain-language intent (no CSS syntax) and
        # the step has no rich target with fallbacks, try to resolve it
        # against the captured DOM inventory before falling back to the healer.
        action = _normalize_action(step)
        if (
            dom_inventory
            and selector
            and is_intent_only(selector)
            and not target.get("fallbacks")
            and action in ("fill", "click", "press", "assert_visible", "assert_not_visible")
        ):
            resolution = resolve_intent(dom_inventory, action, selector)
            if resolution:
                resolved_target = build_playwright_target(resolution)
                resolved_target["intent"] = intent
                target = resolved_target
                logs.append(
                    f"[INTENT] step={step_index} action={action} intent={selector!r} "
                    f"→ strategy={resolution['strategy']} value={resolution['value']!r} "
                    f"score={resolution['score']}"
                )
            else:
                logs.append(
                    f"[INTENT] step={step_index} action={action} intent={selector!r} "
                    f"→ no DOM match (score below threshold), falling back to healer"
                )
        # ────────────────────────────────────────────────────────────────

        # ── Well-known form selector fallbacks ───────────────────────────
        # When DOM resolution produced nothing (or DOM inventory was absent),
        # try static fallback lists for common form fields before giving up.
        if (
            selector
            and is_intent_only(selector)
            and not target.get("fallbacks")
            and action in ("fill", "click", "press")
        ):
            wk = build_well_known_form_target(action, selector)
            if wk:
                wk["intent"] = intent
                target = wk
                logs.append(
                    f"[FORM] step={step_index} action={action} intent={selector!r} "
                    f"→ well-known form target ({len(wk.get('fallbacks', []))} fallbacks)"
                )
        # ────────────────────────────────────────────────────────────────

        n_fallbacks = len(target.get("fallbacks") or [])
        locator, used, resolved_selector, res_meta = resolve_locator(
            page,
            target,
            domain=domain,
            timeout_ms=target.get("timeout_ms"),
        )

        logger.debug(
            "[RESOLVE] action=%s primary=%s fallbacks=%d used=%s resolved=%s fb_index=%s domain=%s",
            step.get("action"),
            target.get("primary"),
            n_fallbacks,
            used,
            resolved_selector,
            res_meta.get("fallback_index"),
            domain,
        )

        # Lightweight resolution metadata for observability
        page_ctx = capture_page_context(page)
        elem_ctx = capture_element_context(locator)
        resolution_log.append({
            "step_index": step_index,
            "action": step.get("action"),
            "primary": target.get("primary"),
            "original_selector": step.get("selector"),
            "intent_resolved": bool(target.get("resolved_by") == "selector_resolver"),
            "intent_strategy": target.get("resolved_by"),
            "n_fallbacks": n_fallbacks,
            "used": used,
            "resolved": resolved_selector,
            "fallback_used": used != "primary",
            "fallback_index": res_meta.get("fallback_index"),
            "fallback_type": res_meta.get("fallback_type"),
            "attempts": res_meta.get("attempts"),
            "url": page_ctx.get("url"),
            "element": elem_ctx,
        })

        # ✅ guardar solo si NO fue primary
        if used != "primary":
            # Structured healing event — stored in run payload for observability
            healing_log.append({
                "selector_healed":  True,
                "original_selector": step.get("selector") or target.get("primary", ""),
                "healed_selector":  resolved_selector,
                "healing_strategy": used,
                "fallback_index":   res_meta.get("fallback_index"),
                "domain":           domain,
                "timestamp":        _now_ms(),
                "step_index":       step_index,
                "action":           step.get("action"),
                "intent":           intent,
            })

            try:
                current = load_memory(domain) or {}
                healed = current.get("healed_selectors") or {}
                if not isinstance(healed, dict):
                    healed = {}
                healed[intent] = resolved_selector
                save_memory(domain, {"healed_selectors": healed})

                logs.append(
                    f"[HEAL] saved intent='{intent}' used='{used}' domain='{domain}' selector='{resolved_selector}'"
                )
            except Exception as e:
                logs.append(f"[HEAL] save_memory failed (best-effort): {type(e).__name__}: {e}")

        return locator, used, domain, intent

    def _raise_classified(e: Exception, page, inferred_base_url: Optional[str], action: str, selector: str, step: Dict[str, Any]):
        domain = _domain_from_page(page, inferred_base_url)
        signals = {
            "action": action,
            "selector": selector,
            "domain": domain,
            "step": {
                "action": step.get("action"),
                "type": step.get("type"),
                "target": step.get("target"),
                "selector": step.get("selector"),
            },
        }
        classification = classify_failure(e, signals=signals)
        logger.debug(
            "[RESOLVE_FAIL] action=%s selector=%s domain=%s classification=%s error=%s",
            action, selector, domain, classification, e,
        )
        raise Exception({"error": str(e), "classification": classification})

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

                        page.goto(url, wait_until="domcontentloaded", timeout=timeout_ms)
                        try:
                            page.wait_for_load_state("networkidle", timeout=min(6000, timeout_ms))
                        except Exception:
                            try:
                                page.wait_for_timeout(350)
                            except Exception:
                                pass

                        _record_step(i, step, "passed", extra={"resolved_url": url, "domain": _domain_from_url(url)})
                        last_page_context = capture_page_context(page)
                        # Capture DOM inventory for intent-based selector resolution
                        if dom_inventory is None:
                            dom_inventory = extract_dom_inventory(page)
                            if dom_inventory.get("inputs") or dom_inventory.get("buttons"):
                                logs.append(
                                    f"[DOM] inventory captured: "
                                    f"{len(dom_inventory.get('inputs', []))} inputs, "
                                    f"{len(dom_inventory.get('buttons', []))} buttons"
                                )
                        continue

                    if action == "wait_ms":
                        ms = _as_int(step.get("ms"), 500)
                        page.wait_for_timeout(ms)
                        _record_step(i, step, "passed", extra={"ms": ms})
                        continue

                    sel = _selector_from_step(step)

                    if not sel and action not in ("assert_text_contains", "assert_url_contains"):
                        raise ValueError(f"{action} requiere selector/target/loc")

                    if action == "fill":
                        val = _safe_str(step.get("value"))
                        try:
                            locator, used, domain, intent = _resolve(step, page, sel, inferred_base_url, step_index=i)
                            locator.wait_for(state="visible", timeout=timeout_ms)
                            locator.fill(val, timeout=timeout_ms)
                            _record_step(i, step, "passed", extra={"locator_used": used, "intent": intent, "domain": domain})
                            continue
                        except Exception as e:
                            _raise_classified(e, page, inferred_base_url, action, sel, step)

                    if action == "click":
                        try:
                            locator, used, domain, intent = _resolve(step, page, sel, inferred_base_url, step_index=i)
                            # Log the resolved selector from the last resolution_log entry
                            _res = resolution_log[-1] if resolution_log else {}
                            _resolved_sel = _res.get("resolved") or _res.get("primary") or sel
                            logs.append(
                                f"[CLICK] step={i} intent={sel!r} "
                                f"used={used!r} resolved={_resolved_sel!r}"
                            )
                            locator.wait_for(state="visible", timeout=timeout_ms)
                            logs.append(f"[CLICK] locator visible — calling click()")
                            locator.click(timeout=timeout_ms)
                            logs.append(f"[CLICK] click() returned — checking navigation")
                            # Navigation wait for submit/login buttons.
                            # Use load (fires once DOM+resources ready) rather than
                            # networkidle (requires 500ms of zero network) — more
                            # reliable on slower deployments and CDN-heavy pages.
                            if _is_submit_click(sel, intent):
                                _url_before = page.url
                                try:
                                    page.wait_for_load_state("load", timeout=8000)
                                except Exception:
                                    pass
                                _url_after = page.url
                                logs.append(
                                    f"[NAV] url_before={_url_before!r} "
                                    f"url_after={_url_after!r} "
                                    f"navigated={_url_before != _url_after}"
                                )
                            _record_step(i, step, "passed", extra={"locator_used": used, "intent": intent, "domain": domain})
                            continue
                        except Exception as e:
                            logs.append(f"[CLICK_FAIL] step={i} intent={sel!r} error={type(e).__name__}: {e}")
                            _raise_classified(e, page, inferred_base_url, action, sel, step)

                    if action == "press":
                        key = _safe_str(step.get("key") or "Enter")
                        try:
                            locator, used, domain, intent = _resolve(step, page, sel, inferred_base_url, step_index=i)
                            locator.wait_for(state="visible", timeout=timeout_ms)
                            locator.press(key, timeout=timeout_ms)
                            _record_step(i, step, "passed", extra={"key": key, "locator_used": used, "intent": intent, "domain": domain})
                            continue
                        except Exception as e:
                            _raise_classified(e, page, inferred_base_url, action, sel, step)

                    if action == "assert_visible":
                        try:
                            locator, used, domain, intent = _resolve(step, page, sel, inferred_base_url, step_index=i)
                            locator.wait_for(state="visible", timeout=timeout_ms)
                            _record_step(i, step, "passed", extra={"locator_used": used, "intent": intent, "domain": domain})
                            continue
                        except Exception as e:
                            _raise_classified(e, page, inferred_base_url, action, sel, step)

                    if action == "assert_not_visible":
                        loc = page.locator(sel)
                        if loc.is_visible():
                            raise AssertionError(f"assert_not_visible falló: se mostró {sel}")
                        _record_step(i, step, "passed")
                        continue

                    if action == "assert_url_contains":
                        needle = _safe_str(step.get("value") or step.get("text") or step.get("contains") or "").strip()
                        if not needle:
                            raise ValueError("assert_url_contains requiere value/text/contains")
                        current = page.url or ""
                        if needle not in current:
                            raise AssertionError(f"assert_url_contains falló: '{needle}' no está en '{current}'")
                        _record_step(i, step, "passed", extra={"current_url": current})
                        continue

                    if action == "assert_text_contains":
                        expected_text = _safe_str(step.get("expected") or step.get("text") or "").strip()
                        if not expected_text:
                            raise ValueError("assert_text_contains requiere expected/text")

                        # Normalize whitespace in expected for comparison (preserves case)
                        expected_norm = _normalize_ws(expected_text)

                        target_sel = _selector_from_step(step) or "body"
                        loc = page.locator(target_sel)
                        loc.wait_for(state="visible", timeout=timeout_ms)
                        content = ""
                        try:
                            content = (loc.inner_text(timeout=timeout_ms) or "").strip()
                        except Exception:
                            content = ""

                        if not content:
                            try:
                                content = (loc.text_content(timeout=timeout_ms) or "").strip()
                            except Exception:
                                content = ""

                        # Compare with normalized whitespace (handles \t, \n, double-spaces)
                        def _found(raw: str) -> bool:
                            return expected_norm in _normalize_ws(raw)

                        # Fallback 1: full body via JS — catches SPA-rendered / lazy text
                        if not _found(content):
                            try:
                                body_text = str(page.evaluate("() => document.body.innerText") or "")
                                if _found(body_text):
                                    content = body_text
                            except Exception:
                                pass

                        # Fallback 2: ARIA labels — catches accessibility-only text
                        if not _found(content):
                            try:
                                aria_text = str(page.evaluate(
                                    "() => Array.from(document.querySelectorAll('[aria-label]'))"
                                    ".map(el => el.getAttribute('aria-label') || '').join(' ')"
                                ) or "")
                                if _found(aria_text):
                                    content = aria_text
                            except Exception:
                                pass

                        if not _found(content):
                            raise AssertionError(f"Texto no encontrado. Expected contiene: '{expected_text}'")

                        _record_step(i, step, "passed", extra={"target": target_sel})
                        continue

                    raise ValueError(f"Acción no soportada: {action}")

                except PlaywrightTimeoutError as e:
                    outcome = "fail"
                    reason = f"Timeout en step {i + 1}: {action} — {type(e).__name__}: {e}"
                    logs.append(reason)
                    _record_step(i, step, "failed", err=reason)
                    last_page_context = capture_page_context(page)
                    failure_context = build_failure_context(step_index=i, action=action, step=step, error_str=reason, page_ctx=last_page_context)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except (AssertionError, ValueError) as e:
                    outcome = "fail"
                    reason = f"Fallo en step {i + 1}: {action} — {type(e).__name__}: {e}"
                    logs.append(reason)
                    _record_step(i, step, "failed", err=reason)
                    last_page_context = capture_page_context(page)
                    failure_context = build_failure_context(step_index=i, action=action, step=step, error_str=reason, page_ctx=last_page_context)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except PlaywrightError as e:
                    outcome = "fail"
                    had_error = True
                    reason = f"Playwright error en step {i + 1}: {action} — {type(e).__name__}: {e}"
                    logs.append(reason)
                    _record_step(i, step, "error", err=reason)
                    last_page_context = capture_page_context(page)
                    failure_context = build_failure_context(step_index=i, action=action, step=step, error_str=reason, page_ctx=last_page_context)

                    shot, shot_logs = take_screenshot_robust(page)
                    logs.extend(shot_logs)
                    screenshot_b64 = shot
                    break

                except Exception as e:
                    outcome = "fail"
                    had_error = True

                    if e.args and isinstance(e.args[0], dict):
                        payload = e.args[0]
                        reason = (
                            f"Error en step {i + 1}: {action} — {payload.get('error')} "
                            f"(classification={payload.get('classification')})"
                        )
                    else:
                        reason = f"Error inesperado en step {i + 1}: {action} — {type(e).__name__}: {e}"

                    logs.append(reason)
                    _record_step(i, step, "error", err=reason)
                    last_page_context = capture_page_context(page)
                    failure_context = build_failure_context(step_index=i, action=action, step=step, error_str=reason, page_ctx=last_page_context)

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
        # Observability extensions — all optional, never break existing consumers
        "resolution_log": resolution_log,
        "healing_log":    healing_log,       # structured selector healing events
        "failure_context": failure_context,
        "page_context": last_page_context,
    }
