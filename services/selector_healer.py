from __future__ import annotations

import logging
import re
from typing import Any, Dict, List, Optional, Tuple

from playwright.sync_api import Page, Locator, TimeoutError as PwTimeout

logger = logging.getLogger("vanya.selector_healer")

# Auto-heal after explicit fallbacks: only apply when confidence >= threshold (no guessing).
AUTO_HEAL_MIN_CONFIDENCE = 0.85


# ============================================================
# Supported fallback types (ordered by typical reliability)
# ============================================================
_SUPPORTED_TYPES = frozenset({
    "css", "testid", "name", "role", "label", "text", "placeholder",
    # Block 16 additions
    "partial_text",    # page.get_by_text(val, exact=False) — alias more explicit than "text"
    "aria_heuristic",  # try [aria-label*=val], [title*=val], [alt*=val] in sequence
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

def _try_fallback(
    page: Page,
    fb_type: str,
    fb_val: Any,
    timeout_ms: int,
    element_state: str = "visible",
) -> Tuple[Locator, str]:
    """
    Attempt a single fallback strategy.
    Returns (locator, resolved_selector_str) on success.
    Raises on failure or unsupported type.

    element_state: Playwright wait_for state — use ``attached`` when the caller
    will assert non-visibility (element may legitimately be hidden).
    """
    ws = element_state if element_state in ("visible", "attached", "hidden", "detached") else "visible"
    if fb_type == "css":
        sel = str(fb_val or "").strip()
        if not sel:
            raise ValueError("css fallback: empty value")
        loc = page.locator(sel)
        loc.wait_for(state=ws, timeout=timeout_ms)
        return loc, sel

    if fb_type == "testid":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("testid fallback: empty value")
        loc = page.get_by_test_id(val)
        loc.first.wait_for(state=ws, timeout=timeout_ms)
        return loc.first, f"testid={val}"

    if fb_type == "name":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("name fallback: empty value")
        sel = f"[name='{val}']"
        loc = page.locator(sel)
        loc.wait_for(state=ws, timeout=timeout_ms)
        return loc, sel

    if fb_type == "role":
        rv = fb_val if isinstance(fb_val, dict) else {}
        role = (rv.get("role") or "").strip()
        if not role:
            raise ValueError("role fallback: missing role key")
        name = rv.get("name")
        loc = page.get_by_role(role, name=name)
        loc.first.wait_for(state=ws, timeout=timeout_ms)
        return loc.first, f"role={role} name={name}"

    if fb_type == "label":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("label fallback: empty value")
        loc = page.get_by_label(val)
        loc.first.wait_for(state=ws, timeout=timeout_ms)
        return loc.first, f"label={val}"

    if fb_type == "text":
        txt = str(fb_val or "").strip()
        if not txt:
            raise ValueError("text fallback: empty value")
        loc = page.get_by_text(txt, exact=False)
        loc.first.wait_for(state=ws, timeout=timeout_ms)
        return loc.first, f"text={txt}"

    if fb_type == "placeholder":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("placeholder fallback: empty value")
        loc = page.get_by_placeholder(val)
        loc.first.wait_for(state=ws, timeout=timeout_ms)
        return loc.first, f"placeholder={val}"

    if fb_type == "partial_text":
        txt = str(fb_val or "").strip()
        if not txt:
            raise ValueError("partial_text fallback: empty value")
        loc = page.get_by_text(txt, exact=False)
        loc.first.wait_for(state=ws, timeout=timeout_ms)
        return loc.first, f"partial_text={txt}"

    if fb_type == "aria_heuristic":
        val = str(fb_val or "").strip()
        if not val:
            raise ValueError("aria_heuristic fallback: empty value")
        # Try aria-label, then title, then alt with partial (contains) match
        for attr in ("aria-label", "title", "alt"):
            sel = f"[{attr}*='{val}']"
            try:
                loc = page.locator(sel)
                loc.first.wait_for(state=ws, timeout=timeout_ms)
                return loc.first, sel
            except Exception:
                pass
        raise ValueError(f"aria_heuristic fallback: no element found for value '{val}'")

    raise ValueError(f"unsupported fallback type: {fb_type}")


# ============================================================
# Conservative auto-heal (primary + declared fallbacks failed)
# ============================================================

def _selector_too_complex_for_auto_heal(primary: str) -> bool:
    p = (primary or "").strip()
    if not p:
        return True
    if len(p) > 220:
        return True
    if p.count(" ") > 10:
        return True
    if ">>" in p or ":has(" in p or ":scope" in p:
        return True
    return False


def _extract_data_testid_values(css: str) -> List[str]:
    out: List[str] = []
    for m in re.finditer(
        r"data-testid\s*=\s*['\"]([^'\"<>{}\]]+)['\"]",
        css,
        re.I,
    ):
        v = (m.group(1) or "").strip()
        if v:
            out.append(v)
    return out


def _hash_id_candidates(css: str) -> List[str]:
    """#id tokens (rightmost first) as candidate data-testid values."""
    ids: List[str] = []
    for m in re.finditer(r"#([A-Za-z][A-Za-z0-9_-]{0,63})\b", css):
        ids.append(m.group(1))
    seen: set = set()
    out: List[str] = []
    for i in reversed(ids):
        if i not in seen:
            seen.add(i)
            out.append(i)
    return out


def _extract_name_attr_values(css: str) -> List[str]:
    out: List[str] = []
    for m in re.finditer(r"""name\s*=\s*['\"]([^'\"]+)['\"]""", css, re.I):
        v = (m.group(1) or "").strip()
        if v:
            out.append(v)
    return out


def _extract_placeholder_attr_values(css: str) -> List[str]:
    out: List[str] = []
    for m in re.finditer(r"""placeholder\s*=\s*['\"]([^'\"]+)['\"]""", css, re.I):
        v = (m.group(1) or "").strip()
        if v:
            out.append(v)
    return out


def _extract_aria_label_values(css: str) -> List[str]:
    out: List[str] = []
    for m in re.finditer(r"""aria-label\s*=\s*['\"]([^'\"]+)['\"]""", css, re.I):
        v = (m.group(1) or "").strip()
        if v:
            out.append(v)
    return out


def _intent_label_for_heal(step: Optional[Dict[str, Any]]) -> str:
    if not step:
        return ""
    t = step.get("target")
    if isinstance(t, dict):
        return str(t.get("intent") or "").strip()
    if isinstance(t, str):
        return t.strip()
    return ""


def _step_placeholder_hint(step: Optional[Dict[str, Any]]) -> str:
    if not step:
        return ""
    for k in ("placeholder", "value"):
        v = step.get(k)
        if isinstance(v, str) and v.strip() and len(v.strip()) < 120:
            return v.strip()
    return ""


def _css_attr_equals(attr: str, val: str) -> Optional[str]:
    """Build a simple [attr="value"] selector; skip ambiguous quoting."""
    v = (val or "").strip()
    if not v or len(v) > 200:
        return None
    if any(ch in v for ch in ("\n", "\r", "\t", "<", ">")):
        return None
    if '"' in v and "'" in v:
        return None
    if '"' not in v:
        return f'[{attr}="{v}"]'
    return f"[{attr}='{v}']"


def _name_attribute_selector(name: str) -> Optional[str]:
    return _css_attr_equals("name", name)


def _extract_role_attribute_values(css: str) -> List[str]:
    out: List[str] = []
    for m in re.finditer(r"""role\s*=\s*['\"]([a-z][a-z0-9_-]*)['\"]""", css, re.I):
        r = (m.group(1) or "").strip().lower()
        if r:
            out.append(r)
    seen: set = set()
    uniq: List[str] = []
    for x in out:
        if x not in seen:
            seen.add(x)
            uniq.append(x)
    return uniq


_AUTO_HEAL_ACTIONS = frozenset({
    "click", "fill", "hover", "press",
    "assert_visible", "assert_not_visible",
    "select", "check", "uncheck",
})


def _auto_heal_allowed_for_step(step: Optional[Dict[str, Any]]) -> bool:
    if not step:
        return True
    a = str(step.get("action") or "").strip().lower().replace("-", "_")
    return a in _AUTO_HEAL_ACTIONS


def _try_conservative_auto_heal(
    page: Page,
    primary: str,
    timeout_ms: int,
    errors: List[str],
    step: Optional[Dict[str, Any]],
    wait_state: str = "visible",
) -> Optional[Tuple[Locator, str, str, Dict[str, Any]]]:
    """
    Last-resort healing after primary + declared fallbacks failed.

    Chain (high confidence only, exactly one DOM match, then wait_state gate):
      data-testid (attr) → data-testid (#hash) → aria-label= → name= →
      placeholder= → step placeholder/value → role= → exact visible text (#hash) →
      get_by_label(intent) → fuzzy partial (logged only, never auto-applied).

    Returns (locator.first, strategy_id, resolved_label, meta) or None.
    """
    logger.info(
        "[HEAL_AUTO] chain start primary=%r step_keys=%s",
        (primary or "")[:200],
        list((step or {}).keys())[:12] if step else [],
    )
    if not primary or not str(primary).strip():
        logger.info("[HEAL_AUTO] skip: empty primary")
        return None
    if _selector_too_complex_for_auto_heal(primary):
        logger.info("[HEAL_AUTO] skip: selector too complex for auto chain")
        return None

    t = timeout_ms
    ws = wait_state if wait_state in ("visible", "attached", "hidden", "detached") else "visible"

    def _single_match(loc: Locator, resolved: str, conf: float, strat: str) -> Optional[Tuple[Locator, str, str, Dict[str, Any]]]:
        try:
            c = loc.count()
        except Exception as ex:
            errors.append(f"auto:{strat}: count error {ex}")
            return None
        if c != 1:
            errors.append(f"auto:{strat}: need exactly 1 match, got {c} for {resolved!r}")
            return None
        if conf < AUTO_HEAL_MIN_CONFIDENCE:
            logger.info(
                "[HEAL_AUTO] skip strategy=%s conf=%.2f < min %.2f (would not auto-apply)",
                strat,
                conf,
                AUTO_HEAL_MIN_CONFIDENCE,
            )
            return None
        try:
            loc.first.wait_for(state=ws, timeout=t)
        except Exception as ex:
            errors.append(f"auto:{strat}: visibility {type(ex).__name__}: {ex}")
            return None
        meta = {
            "fallback_index": None,
            "fallback_type": strat,
            "attempts": None,
            "errors": list(errors),
            "confidence": round(conf, 3),
            "healing_auto_applied": True,
            "healing_reason": f"{strat}: single match state={ws} (conf={conf:.2f})",
        }
        logger.info(
            "[HEAL_AUTO] success strategy=%s confidence=%.3f resolved=%r",
            strat,
            conf,
            resolved[:160],
        )
        return loc.first, strat, resolved, meta

    # 1–2) data-testid (explicit in selector, then hash-id convention)
    for v in _extract_data_testid_values(primary):
        r = _single_match(page.get_by_test_id(v), f"testid={v}", 0.96, "auto_data_testid_attr")
        if r:
            return r
    for hid in _hash_id_candidates(primary):
        r = _single_match(page.get_by_test_id(hid), f"testid={hid}", 0.9, "auto_data_testid_from_hash_id")
        if r:
            return r

    # 3) aria-label= in selector (exact attribute → locator)
    for v in _extract_aria_label_values(primary):
        sel = _css_attr_equals("aria-label", v)
        if not sel:
            errors.append(f"auto:aria_label_attr: skip unsafe quoting for {v!r}")
            continue
        r = _single_match(page.locator(sel), sel, 0.93, "auto_aria_label_attr")
        if r:
            return r

    # 4) name=
    for v in _extract_name_attr_values(primary):
        sel = _name_attribute_selector(v)
        if not sel:
            errors.append(f"auto:name_attr: skip unsafe name quoting for {v!r}")
            continue
        r = _single_match(page.locator(sel), sel, 0.92, "auto_name_attr")
        if r:
            return r

    # 5) placeholder= in selector
    for v in _extract_placeholder_attr_values(primary):
        r = _single_match(
            page.get_by_placeholder(v),
            f"placeholder={v}",
            0.9,
            "auto_placeholder_attr",
        )
        if r:
            return r

    # 6) step-level placeholder / value hint (same family as placeholder)
    ph = _step_placeholder_hint(step)
    if ph:
        r = _single_match(
            page.get_by_placeholder(ph),
            f"placeholder(step)={ph}",
            0.88,
            "auto_step_placeholder",
        )
        if r:
            return r

    # 7) role= in selector (accessible role, no name — single visible match only)
    for role in _extract_role_attribute_values(primary):
        r = _single_match(page.get_by_role(role), f"role={role}", 0.91, "auto_role_attr")
        if r:
            return r

    # 8) visible text (exact): readable token from #hash id
    for tok in _hash_id_candidates(primary):
        readable = re.sub(r"[-_]+", " ", tok).strip()
        if not (3 <= len(readable) <= 48):
            continue
        if not re.match(r"^[A-Za-z][A-Za-z\s]+$", readable):
            continue
        r = _single_match(
            page.get_by_text(readable, exact=True),
            f"text(exact)={readable}",
            0.86,
            "auto_visible_text_exact",
        )
        if r:
            return r

    # 9) label ↔ control (intent string as accessible name)
    intent = _intent_label_for_heal(step)
    if intent and len(intent) < 100 and not re.search(r"[#.\[\]=|><]", intent):
        r = _single_match(
            page.get_by_label(intent),
            f"label={intent}",
            0.87,
            "auto_label_intent",
        )
        if r:
            return r

    # 10) Bounded fuzzy / partial text: medium confidence — never auto-apply
    if intent and 4 <= len(intent) <= 80:
        try:
            loc_f = page.get_by_text(intent, exact=False)
            fc = loc_f.count()
        except Exception as ex:
            fc = -1
            logger.info("[HEAL_AUTO] fuzzy probe error: %s", ex)
        if fc == 1:
            logger.info(
                "[HEAL_AUTO] skipped due to low confidence: partial text would be medium; "
                "not auto-applying (hint=%r)",
                intent[:80],
            )
        elif fc > 1:
            logger.info(
                "[HEAL_AUTO] skipped fuzzy: multiple partial matches (n=%s) hint=%r",
                fc,
                intent[:80],
            )

    logger.info("[HEAL_AUTO] chain exhausted without high-confidence match")
    return None


def resolve_locator(
    page: Page,
    target: Dict[str, Any],
    *,
    timeout_ms: Any = None,
    domain: Any = None,
    step: Optional[Dict[str, Any]] = None,
    element_state: str = "visible",
) -> Tuple[Locator, str, str, Dict[str, Any]]:
    """
    Resolves a locator using target.primary first, then target.fallbacks in order.

    Supported fallback types:
      css, testid, name, role, label, text, placeholder, partial_text, aria_heuristic

    If primary and all declared fallbacks fail, runs a conservative deterministic auto-heal
    chain (confidence-gated). Pass ``step`` for action allow-listing and placeholder/label hints.
    Use ``element_state="attached"`` when resolving for ``assert_not_visible`` (element may be hidden).

    Returns:
      (locator, used, resolved_selector, meta)

      used              "primary" | fallback type string | auto_* strategy id
      resolved_selector CSS selector or descriptive string (e.g. "testid=login-btn")
      meta = {
          "fallback_index": None | int,
          "fallback_type":  None | str,
          "attempts":       int,
          "errors":         List[str],
          "confidence":     optional float (auto-heal),
          "healing_auto_applied": optional bool,
          "healing_reason": optional str,
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
    es = element_state if element_state in ("visible", "attached", "hidden", "detached") else "visible"
    errors: List[str] = []
    attempts = 0
    unsupported_count = 0

    # ── 1) Primary (treated as CSS / Playwright selector string) ──────────
    if primary:
        attempts += 1
        try:
            loc = page.locator(primary)
            loc.wait_for(state=es, timeout=t)
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
            loc, resolved = _try_fallback(page, fb_type, fb.get("value"), t, es)
            return loc, fb_type, resolved, {
                "fallback_index": idx,
                "fallback_type": fb_type,
                "attempts": attempts,
                "errors": errors,
            }
        except Exception as e:
            errors.append(f"{fb_type}[{idx}] failed: {type(e).__name__}: {str(e)[:200]}")

    # ── 3) Conservative auto-heal (after primary + declared fallbacks) ────
    if primary and _auto_heal_allowed_for_step(step):
        logger.info(
            "[HEAL_AUTO] resolve_locator: primary+fallbacks exhausted, attempting conservative chain "
            "(domain=%r)",
            domain,
        )
        healed = _try_conservative_auto_heal(page, primary, t, errors, step, wait_state=es)
        if healed:
            attempts += 1
            loc, used_s, resolved, hmeta = healed
            hmeta["attempts"] = attempts
            return loc, used_s, resolved, hmeta
        logger.info("[HEAL_AUTO] failure: no high-confidence recovery after exhausted fallbacks")

    # ── 4) Total failure ──────────────────────────────────────────────────
    # Auto-heal attempts log as "auto:..." — exclude from classification so stable
    # categories (e.g. primary_not_found) stay correct when no declared fallback ran.
    classification_errors = [e for e in errors if not e.startswith("auto:")]
    classification = _classify_resolution_failure(
        classification_errors, primary, fallbacks, unsupported_count
    )

    raise Exception({
        "reason": "locator_not_found",
        "classification": classification,
        "errors": errors,
        "target": target,
        "domain": domain,
    })
