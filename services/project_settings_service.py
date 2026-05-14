# services/project_settings_service.py
"""
Project settings: login_profile + variables (secrets).

Storage is JSON in SQLite (plain text at rest — no vault in this iteration).
API responses use masked shapes so PASSWORD and similar values are not exposed.
"""
from __future__ import annotations

import json
import re
from typing import Any, Dict, Optional


def parse_settings_json(raw: Optional[str]) -> Dict[str, Any]:
    if not raw or not str(raw).strip():
        return {}
    try:
        o = json.loads(raw)
        return o if isinstance(o, dict) else {}
    except Exception:
        return {}


def dump_settings_json(settings: Optional[Dict[str, Any]]) -> str:
    if not settings:
        return "{}"
    try:
        return json.dumps(settings, ensure_ascii=False, separators=(",", ":"))
    except Exception:
        return "{}"


_SENSITIVE_KEY = re.compile(
    r"(?i)(password|passwd|pwd|secret|token|api_?key|bearer|auth|credential)$|"
    r"^(password|passwd|secret|token|apikey|api_key)",
)


def is_sensitive_variable_key(key: str) -> bool:
    k = (key or "").strip().upper().replace("-", "_")
    if not k:
        return False
    if _SENSITIVE_KEY.search(k):
        return True
    return any(
        x in k
        for x in (
            "PASSWORD",
            "SECRET",
            "TOKEN",
            "API_KEY",
            "BEARER",
            "CREDENTIAL",
        )
    )


def _preview_non_secret(value: str, max_len: int = 48) -> str:
    s = (value or "").strip()
    if not s:
        return ""
    if "@" in s and "." in s:
        local, _, rest = s.partition("@")
        if len(local) <= 2:
            return f"{local[0]}***@{rest}" if local else s[:max_len]
        return f"{local[:2]}***@{rest}"[:max_len]
    if len(s) <= 8:
        return "***"
    return s[:3] + "***" + s[-2:]


def mask_settings_for_api(settings: Optional[Dict[str, Any]]) -> Optional[Dict[str, Any]]:
    """
    Safe JSON for GET /projects. Sensitive values are never echoed; only flags/previews.
    """
    if not settings:
        return None
    lp = settings.get("login_profile")
    lp_out = dict(lp) if isinstance(lp, dict) else {}
    vars_in = settings.get("variables") if isinstance(settings.get("variables"), dict) else {}
    vars_out: Dict[str, Any] = {}
    for k, v in vars_in.items():
        sk = str(k)
        present = bool(v is not None and str(v).strip())
        if is_sensitive_variable_key(sk):
            vars_out[sk] = {"sensitive": True, "present": present}
        else:
            vars_out[sk] = {
                "sensitive": False,
                "present": present,
                "preview": _preview_non_secret(str(v or "")) if present else "",
            }
    gh_in = settings.get("github") if isinstance(settings.get("github"), dict) else {}
    gh_out: Dict[str, Any] = {}
    if gh_in:
        gh_out["enabled"] = bool(gh_in.get("enabled"))
        for key in ("repo_url", "owner", "repo", "default_branch", "installation_id", "api_base"):
            v = gh_in.get(key)
            if v is not None and str(v).strip():
                gh_out[key] = str(v).strip()
        tok = gh_in.get("github_token")
        tok_present = bool(tok is not None and str(tok).strip())
        gh_out["github_token"] = {"sensitive": True, "present": tok_present}

    out: Dict[str, Any] = {
        "login_profile": lp_out,
        "variables": vars_out,
        "_security_note": "Sensitive variable values are stored for execution only and are not returned by the API.",
    }
    if gh_out:
        out["github"] = gh_out
    return out


def merge_settings(existing: Optional[Dict[str, Any]], patch: Optional[Dict[str, Any]]) -> Dict[str, Any]:
    """Deep-merge settings for PATCH. None in patch skips; variables null removes key."""
    base: Dict[str, Any] = dict(existing) if existing else {}
    if not patch:
        return base
    out = dict(base)

    if "login_profile" in patch and patch["login_profile"] is not None:
        old_lp = dict(out.get("login_profile") or {}) if isinstance(out.get("login_profile"), dict) else {}
        new_lp = patch["login_profile"]
        if isinstance(new_lp, dict):
            for k, v in new_lp.items():
                if v is None:
                    old_lp.pop(str(k), None)
                else:
                    old_lp[str(k)] = v
            out["login_profile"] = old_lp

    if "variables" in patch and patch["variables"] is not None:
        old_v = dict(out.get("variables") or {}) if isinstance(out.get("variables"), dict) else {}
        new_v = patch["variables"]
        if isinstance(new_v, dict):
            for k, v in new_v.items():
                key = str(k)
                if v is None:
                    old_v.pop(key, None)
                else:
                    old_v[key] = v
            out["variables"] = old_v

    if "github" in patch and patch["github"] is not None:
        old_g = dict(out.get("github") or {}) if isinstance(out.get("github"), dict) else {}
        new_g = patch["github"]
        if isinstance(new_g, dict):
            for k, v in new_g.items():
                key = str(k)
                if v is None:
                    old_g.pop(key, None)
                else:
                    old_g[key] = v
            out["github"] = old_g

    return out
