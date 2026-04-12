# services/api_runner.py
"""
Backward-compatible entry point for the HTTP API test runner.

Implementation: ``runners.api_runner`` (httpx, step-based + optional legacy assertions).
"""
from __future__ import annotations

from runners.api_runner import run_api_test

__all__ = ["run_api_test"]
