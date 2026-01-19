# runner.py
"""
Compat wrapper.

Antes runner.py tenía toda la lógica (1636 líneas).
Ahora la lógica vive en runners/* para mantenerlo limpio y modular.

Mantén estos exports para no romper imports existentes:
  - from runner import execute_test
  - from runner import execute_heb_full_purchase
"""

from runners.generic_steps import execute_test
from runners.heb_checkout import execute_heb_full_purchase

__all__ = ["execute_test", "execute_heb_full_purchase"]
