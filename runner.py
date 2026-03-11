# runner.py
"""
Compat wrapper — keeps the stable public export:
  from runner import execute_test

runners/heb_checkout.py is preserved as an optional isolated module
but is no longer part of the core import path.
"""

from runners.generic_steps import execute_test

__all__ = ["execute_test"]
