# core/redaction.py
"""
Secret redaction for prompts, steps, logs, and PDFs.
Replaces credential-like values with ***REDACTED*** to avoid exposure.
"""
from __future__ import annotations

import re
from typing import Any, Dict, List

_SECRET_PATTERN = re.compile(
    r"(password|passwd|token|api[_-]?key|secret|credential)"
    r"([\"'\s:=]+)"
    r"([\"']?)([^\s\"'&,\]}\)]{1,200})",
    re.IGNORECASE,
)


def redact_secrets(text: str) -> str:
    """
    Replace secret values with ***REDACTED***.
    Safe for prompts, steps (as JSON str), logs, and PDF content.
    """
    if not text or not isinstance(text, str):
        return text
    return _SECRET_PATTERN.sub(r"\1\2\3***REDACTED***", text)


def redact_steps(steps: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """
    Return a copy of steps with sensitive values redacted (e.g. password fields).
    Use before storing steps in meta, logs, or PDFs.
    """
    if not steps:
        return steps
    out: List[Dict[str, Any]] = []
    for s in steps:
        if not isinstance(s, dict):
            out.append(s)
            continue
        c = dict(s)
        val = c.get("value")
        if val is not None and str(val).strip():
            sel = str(c.get("selector") or "").lower()
            target = c.get("target")
            if isinstance(target, dict):
                sel += " " + str(target.get("primary") or "").lower()
            if "password" in sel or "passwd" in sel or "secret" in sel:
                c["value"] = "***REDACTED***"
            else:
                c["value"] = redact_secrets(str(val))
        out.append(c)
    return out
