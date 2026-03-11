from __future__ import annotations

from typing import Any, Dict, List, Optional, Tuple
from playwright.sync_api import Page, Locator, TimeoutError as PwTimeout


# ============================================================
# Supported fallback types (ordered by typical reliability)
# ============================================================
_SUPPORTED_TYPES = frozenset({
    "css", "testid", "name", "role", "label", "text", "placeholder",
})


def _as_int(v: Any, default: int) -> int:
    try:
        return int(v)
    except Exception:
        return default


# ============================================================
# Failure classification
# ============================================================

def _classify_resolution_failure(
    errors: List[str],
    primary: str,
    fallbacks: List[Any],
    unsupported: int,
) -> str:
    """
    Returns a stable string classification for why all resolution attempts failed.

    Categories:
      invalid_target          - no primary and no fallbacks provided
      transient               - timeout / visibility issue (likely intermittent)
      primary_not_found       - primary failed, no supported fallbacks present
      all_fallbacks_failed    - primary + at least one fallback all failed
      unsupported_fallback_type - primary failed, only unsupported fallback types present
    """
    # Transient: any timeout in the error trail
    combined = " ".join(errors).lower()
    if "timeout" in combined or "timed out" in combined:
        return "transient"

    has_primary_err = any(e.startswith("primary") for e in errors)
    has_fallback_err = any(not e.startswith("primary") for e in errors)
    supported_fallback_count = len(fallbacks) - unsupported

    if not primary and not fallbacks:
        return "invalid_target"

    if has_primary_err and has_fallback_err:
        return "all_fallbacks_failed"

    if has_primary_err and not has_fallback_err:
        if supported_fallback_count == 0 and unsupported > 0:
            return "unsupported_fallback_type"
        return "primary_not_found"

    if not primary and has_fallback_err:
        return "all_fallbacks_failed"

    return "primary_not_found"


# ============================================================
# Per-type fallback executor
# ============================================================

def _try_fallback(page: Page, fb_type: str, fb_val: Any, timeout_ms: int) -> Tuple[Locator, str]:
    """
    Attempt a single fallback strategy.
    Returns (locator, resolved_selector_str) on success.
    Raises on failure or unsupported type.
    """
    if fb_type == "css":
        sel = str(fb_val or "").strip()
        if not sel:
            raise ValueError("css fallback: empty value")
        loc = page.locator(sel)
        loc.wait_for(state="visible", timeout=timeout_ms)
        return loc, sel

    if fb_type == "testid":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("testid fallback: empty value")
        loc = page.get_by_test_id(val)
        loc.first.wait_for(state="visible", timeout=timeout_ms)
        return loc.first, f"testid={val}"

    if fb_type == "name":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("name fallback: empty value")
        sel = f"[name='{val}']"
        loc = page.locator(sel)
        loc.wait_for(state="visible", timeout=timeout_ms)
        return loc, sel

    if fb_type == "role":
        rv = fb_val if isinstance(fb_val, dict) else {}
        role = (rv.get("role") or "").strip()
        if not role:
            raise ValueError("role fallback: missing role key")
        name = rv.get("name")
        loc = page.get_by_role(role, name=name)
        loc.first.wait_for(state="visible", timeout=timeout_ms)
        return loc.first, f"role={role} name={name}"

    if fb_type == "label":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("label fallback: empty value")
        loc = page.get_by_label(val)
        loc.first.wait_for(state="visible", timeout=timeout_ms)
        return loc.first, f"label={val}"

    if fb_type == "text":
        txt = str(fb_val or "").strip()
        if not txt:
            raise ValueError("text fallback: empty value")
        loc = page.get_by_text(txt, exact=False)
        loc.first.wait_for(state="visible", timeout=timeout_ms)
        return loc.first, f"text={txt}"

    if fb_type == "placeholder":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("placeholder fallback: empty value")
        loc = page.get_by_placeholder(val)
        loc.first.wait_for(state="visible", timeout=timeout_ms)
        return loc.first, f"placeholder={val}"

    raise ValueError(f"unsupported fallback type: {fb_type}")


# ============================================================
# Public API
# ============================================================

def resolve_locator(
    page: Page,
    target: Dict[str, Any],
    *,
    domain: Optional[str] = None,
    timeout_ms: Optional[int] = None,
) -> Tuple[Locator, str, str, Dict[str, Any]]:
    """
    Resolves a locator using target.primary first, then target.fallbacks in order.

    Supported fallback types:
      css, testid, name, role, label, text, placeholder

    Returns:
      (locator, used, resolved_selector, meta)

      used              "primary" | fallback type string
      resolved_selector CSS selector or descriptive string (e.g. "testid=login-btn")
      meta = {
          "fallback_index": None | int,   # None = primary succeeded
          "fallback_type":  None | str,   # None = primary succeeded
          "attempts":       int,          # total attempts made
          "errors":         List[str],    # per-attempt error messages before success
      }

    Raises Exception({"reason":"locator_not_found","classification":str,...}) on total failure.

    target schema:
    {
      "primary": "<css selector>",
      "fallbacks": [
        {"type": "css",         "value": ".alt-selector"},
        {"type": "testid",      "value": "login-btn"},
        {"type": "name",        "value": "username"},
        {"type": "role",        "value": {"role": "button", "name": "Sign in"}},
        {"type": "label",       "value": "Email address"},
        {"type": "text",        "value": "Sign in"},
        {"type": "placeholder", "value": "Enter email"},
      ]
    }
    """
    if not isinstance(target, dict):
        raise ValueError("target debe ser dict")

    primary: str = (target.get("primary") or "").strip()
    fallbacks: List[Dict[str, Any]] = list(target.get("fallbacks") or [])

    if not primary and not fallbacks:
        raise ValueError("target debe traer primary y/o fallbacks")

    t = _as_int(timeout_ms, 3000)
    errors: List[str] = []
    attempts = 0
    unsupported_count = 0

    # ── 1) Primary (treated as CSS / Playwright selector string) ──────────
    if primary:
        attempts += 1
        try:
            loc = page.locator(primary)
            loc.wait_for(state="visible", timeout=t)
            return loc, "primary", primary, {
                "fallback_index": None,
                "fallback_type": None,
                "attempts": attempts,
                "errors": [],
            }
        except Exception as e:
            errors.append(f"primary failed: {type(e).__name__}: {str(e)[:200]}")

    # ── 2) Fallbacks in order ─────────────────────────────────────────────
    for idx, fb in enumerate(fallbacks):
        fb_type = (fb.get("type") or "").strip().lower()

        if fb_type not in _SUPPORTED_TYPES:
            unsupported_count += 1
            continue

        attempts += 1
        try:
            loc, resolved = _try_fallback(page, fb_type, fb.get("value"), t)
            return loc, fb_type, resolved, {
                "fallback_index": idx,
                "fallback_type": fb_type,
                "attempts": attempts,
                "errors": errors,
            }
        except Exception as e:
            errors.append(f"{fb_type}[{idx}] failed: {type(e).__name__}: {str(e)[:200]}")

    # ── 3) Total failure ──────────────────────────────────────────────────
    classification = _classify_resolution_failure(errors, primary, fallbacks, unsupported_count)

    raise Exception({
        "reason": "locator_not_found",
        "classification": classification,
        "errors": errors,
        "target": target,
        "domain": domain,
    })
