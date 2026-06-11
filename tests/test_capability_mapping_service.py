# tests/test_capability_mapping_service.py
"""Shared capability mapping (ROI-01C / QMETRY-01B)."""
from __future__ import annotations

from services.capability_mapping_service import (
    known_capabilities,
    map_module_to_capability,
    map_text_to_capability,
    match_module_keyword,
)


class TestSharedCapabilityMapping:
    def test_map_text_payments(self):
        assert map_text_to_capability("payments_api") == "Revenue Collection"

    def test_map_text_checkout(self):
        assert map_text_to_capability("Checkout journey") == "Customer Purchase Flow"

    def test_map_text_authentication(self):
        assert map_text_to_capability("Authentication service") == "Customer Access"

    def test_map_module_checkout(self):
        assert map_module_to_capability("Checkout happy path") == "Customer Purchase Flow"

    def test_map_module_payment(self):
        assert map_module_to_capability("Payment gateway timeout") == "Revenue Collection"

    def test_map_module_login(self):
        assert map_module_to_capability("Login with MFA") == "Customer Access"

    def test_match_module_keyword_returns_module_label(self):
        hit = match_module_keyword("Orders export flow")
        assert hit is not None
        assert hit[0] == "Orders"
        assert hit[1] == "Order Processing"

    def test_known_capabilities_stable(self):
        caps = known_capabilities()
        assert "Revenue Collection" in caps
        assert "Recruiting Operations" in caps
