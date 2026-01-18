from __future__ import annotations

from typing import Any, Dict, Tuple, Optional, List
from playwright.sync_api import Page, Locator, TimeoutError as PwTimeout

def _as_int(v: Any, default: int) -> int:
    try:
        return int(v)
    except Exception:
        return default

def resolve_locator(
    page: Page,
    target: Dict[str, Any],
    *,
    domain: Optional[str] = None,
    timeout_ms: Optional[int] = None,
) -> Tuple[Locator, str, str]:
    """
    Resuelve un locator con fallbacks.
    Returns:
      (locator, used, resolved_selector)

    target schema esperado:
    {
      "primary": "<css selector>",
      "fallbacks": [
        {"type":"css","value":"..."},
        {"type":"text","value":"..."},
        {"type":"role","value":{"role":"button","name":"Continuar"}}
      ]
    }
    """
    if not isinstance(target, dict):
        raise ValueError("target debe ser dict")

    primary = (target.get("primary") or "").strip()
    fallbacks = target.get("fallbacks") or []
    if not primary and not fallbacks:
        raise ValueError("target debe traer primary y/o fallbacks")

    t = _as_int(timeout_ms, 3000)

    errors: List[str] = []

    # 1) Primary (CSS)
    if primary:
        try:
            loc = page.locator(primary)
            loc.wait_for(state="visible", timeout=t)
            return loc, "primary", primary
        except Exception as e:
            errors.append(f"primary failed: {type(e).__name__}: {e}")

    # 2) Fallbacks
    for fb in fallbacks:
        try:
            fb_type = (fb.get("type") or "").strip().lower()
            fb_val = fb.get("value")

            if fb_type == "css":
                sel = str(fb_val or "").strip()
                if not sel:
                    continue
                loc = page.locator(sel)
                loc.wait_for(state="visible", timeout=t)
                return loc, "css", sel

            if fb_type == "text":
                txt = str(fb_val or "").strip()
                if not txt:
                    continue
                loc = page.get_by_text(txt, exact=False)
                loc.first.wait_for(state="visible", timeout=t)
                return loc.first, "text", f'text={txt}'

            if fb_type == "role":
                rv = fb_val or {}
                role = (rv.get("role") or "").strip()
                name = rv.get("name")
                if not role:
                    continue
                loc = page.get_by_role(role, name=name)
                loc.first.wait_for(state="visible", timeout=t)
                # resolved_selector aquí es “descriptivo” (no CSS real) pero sirve para memoria/logs
                return loc.first, "role", f'role={role} name={name}'

            # otros tipos: ignorar
            continue

        except Exception as e:
            errors.append(f"{fb.get('type')} failed: {type(e).__name__}: {e}")

    raise Exception(
        {
            "reason": "locator_not_found",
            "errors": errors,
            "target": target,
            "domain": domain,
        }
    )
