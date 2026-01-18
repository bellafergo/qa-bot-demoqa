from typing import Dict, Any, Optional
from playwright.sync_api import Page, TimeoutError as PwTimeout

def resolve_locator(page: Page, target: Dict[str, Any]):
    errors = []

    # 1. Primary
    try:
        loc = page.locator(target["primary"])
        loc.wait_for(state="visible", timeout=3000)
        return loc, "primary"
    except Exception as e:
        errors.append(str(e))

    # 2. Fallbacks
    for fb in target.get("fallbacks", []):
        try:
            if fb["type"] == "css":
                loc = page.locator(fb["value"])
            elif fb["type"] == "text":
                loc = page.get_by_text(fb["value"], exact=False)
            elif fb["type"] == "role":
                loc = page.get_by_role(
                    fb["value"]["role"],
                    name=fb["value"]["name"]
                )
            else:
                continue

            loc.wait_for(state="visible", timeout=3000)
            return loc, fb["type"]
        except Exception as e:
            errors.append(str(e))

    raise Exception({
        "reason": "locator_not_found",
        "errors": errors,
        "target": target
    })
