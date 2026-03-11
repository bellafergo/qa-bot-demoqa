# core/exec_env.py
"""
Lightweight environment configuration for suite execution.
Client-agnostic: no customer-specific logic, no hardcoded URLs.
"""
from __future__ import annotations

from typing import Any, Dict, Optional

from pydantic import BaseModel


class ExecEnv(BaseModel):
    """
    Execution environment descriptor.
    Provides base_url, browser settings, and optional credential references.
    All fields are optional — TestCase-level values take precedence.
    """
    name:        str                        = "default"
    base_url:    Optional[str]             = None
    headless:    bool                      = True
    timeout_s:   int                       = 30
    viewport:    Optional[Dict[str, int]]  = None    # {"width": 1366, "height": 768}
    credentials: Optional[Dict[str, str]] = None    # plain-text for test envs only
    browser:     str                       = "chromium"   # reserved for multi-browser
    extra:       Dict[str, Any]            = {}


DEFAULT_ENV = ExecEnv()
