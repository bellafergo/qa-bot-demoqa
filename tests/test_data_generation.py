# tests/test_data_generation.py
"""
Tests for the Synthetic Test Data Generation service.

No browser, no network, no external services required.
Uses the existing temporary SQLite setup via conftest.py.
"""
from __future__ import annotations

import pytest

from models.test_data_models import TestDataRequest, TestDataResponse
from services.test_data_service import (
    TestDataService,
    preprocess_data_steps,
    substitute_data_context,
    SUPPORTED_ENTITY_TYPES,
)


# ── Helpers ───────────────────────────────────────────────────────────────────

def _svc() -> TestDataService:
    return TestDataService()


def _req(entity_type: str, **kwargs) -> TestDataRequest:
    return TestDataRequest(entity_type=entity_type, **kwargs)


# ── Supported entity types ────────────────────────────────────────────────────

class TestSupportedTypes:
    def test_user_is_supported(self):
        assert "user" in SUPPORTED_ENTITY_TYPES

    def test_product_is_supported(self):
        assert "product" in SUPPORTED_ENTITY_TYPES

    def test_order_is_supported(self):
        assert "order" in SUPPORTED_ENTITY_TYPES

    def test_payment_is_supported(self):
        assert "payment" in SUPPORTED_ENTITY_TYPES

    def test_address_is_supported(self):
        assert "address" in SUPPORTED_ENTITY_TYPES

    def test_unsupported_type_raises(self):
        svc = _svc()
        with pytest.raises(ValueError, match="Unsupported entity type"):
            svc.generate(_req("dragon"))

    def test_supported_types_list(self):
        types = _svc().supported_types()
        assert set(types) == {"user", "product", "order", "payment", "address"}


# ── User generation ───────────────────────────────────────────────────────────

class TestUserGeneration:
    def test_generates_requested_count(self):
        result = _svc().generate(_req("user", count=3))
        assert result.count == 3
        assert len(result.data) == 3

    def test_user_has_required_fields(self):
        result = _svc().generate(_req("user", count=1))
        user = result.data[0]
        assert "id" in user
        assert "email" in user
        assert "password" in user
        assert "name" in user

    def test_user_email_is_valid_format(self):
        result = _svc().generate(_req("user", count=5))
        for user in result.data:
            assert "@" in user["email"]
            assert "." in user["email"].split("@")[1]

    def test_user_password_is_non_empty(self):
        result = _svc().generate(_req("user", count=3))
        for user in result.data:
            assert len(user["password"]) >= 6

    def test_user_id_is_non_empty(self):
        result = _svc().generate(_req("user", count=1))
        assert result.data[0]["id"]

    def test_user_email_domain_constraint(self):
        result = _svc().generate(_req("user", count=5, constraints={"email_domain": "myco.com"}))
        for user in result.data:
            assert user["email"].endswith("@myco.com")

    def test_user_name_prefix_constraint(self):
        result = _svc().generate(_req("user", count=3, constraints={"name_prefix": "QA-"}))
        for user in result.data:
            assert user["name"].startswith("QA-")

    def test_user_password_prefix_constraint(self):
        result = _svc().generate(_req("user", count=3, constraints={"password_prefix": "SecurePW"}))
        for user in result.data:
            assert user["password"].startswith("SecurePW")


# ── Product generation ────────────────────────────────────────────────────────

class TestProductGeneration:
    def test_generates_requested_count(self):
        result = _svc().generate(_req("product", count=4))
        assert result.count == 4
        assert len(result.data) == 4

    def test_product_has_required_fields(self):
        result = _svc().generate(_req("product", count=1))
        p = result.data[0]
        assert "id" in p
        assert "name" in p
        assert "price" in p
        assert "category" in p

    def test_product_id_has_prod_prefix(self):
        result = _svc().generate(_req("product", count=3))
        for p in result.data:
            assert p["id"].startswith("PROD-")

    def test_product_price_is_positive(self):
        result = _svc().generate(_req("product", count=5))
        for p in result.data:
            assert p["price"] > 0

    def test_product_category_constraint(self):
        result = _svc().generate(_req("product", count=5, constraints={"category": "Electronics"}))
        for p in result.data:
            assert p["category"] == "Electronics"

    def test_product_price_range_constraint(self):
        result = _svc().generate(_req("product", count=10, constraints={"min_price": 10.0, "max_price": 20.0}))
        for p in result.data:
            assert 10.0 <= p["price"] <= 20.0


# ── Order generation ──────────────────────────────────────────────────────────

class TestOrderGeneration:
    def test_generates_requested_count(self):
        result = _svc().generate(_req("order", count=2))
        assert result.count == 2

    def test_order_has_required_fields(self):
        result = _svc().generate(_req("order", count=1))
        o = result.data[0]
        assert "id" in o
        assert "user_id" in o
        assert "product_ids" in o
        assert "total_amount" in o

    def test_order_id_has_ord_prefix(self):
        result = _svc().generate(_req("order", count=3))
        for o in result.data:
            assert o["id"].startswith("ORD-")

    def test_order_has_at_least_one_product(self):
        result = _svc().generate(_req("order", count=5))
        for o in result.data:
            assert len(o["product_ids"]) >= 1

    def test_order_total_amount_is_positive(self):
        result = _svc().generate(_req("order", count=3))
        for o in result.data:
            assert o["total_amount"] > 0

    def test_order_user_id_constraint(self):
        uid = "my-user-123"
        result = _svc().generate(_req("order", count=3, constraints={"user_id": uid}))
        for o in result.data:
            assert o["user_id"] == uid


# ── Payment generation ────────────────────────────────────────────────────────

class TestPaymentGeneration:
    def test_generates_requested_count(self):
        result = _svc().generate(_req("payment", count=3))
        assert result.count == 3

    def test_payment_has_required_fields(self):
        result = _svc().generate(_req("payment", count=1))
        p = result.data[0]
        assert "id" in p
        assert "order_id" in p
        assert "amount" in p
        assert "status" in p

    def test_payment_id_has_pay_prefix(self):
        result = _svc().generate(_req("payment", count=3))
        for p in result.data:
            assert p["id"].startswith("PAY-")

    def test_payment_status_is_valid(self):
        valid = {"pending", "completed", "failed", "refunded"}
        result = _svc().generate(_req("payment", count=10))
        for p in result.data:
            assert p["status"] in valid

    def test_payment_status_constraint(self):
        result = _svc().generate(_req("payment", count=5, constraints={"status": "completed"}))
        for p in result.data:
            assert p["status"] == "completed"

    def test_payment_amount_constraint(self):
        result = _svc().generate(_req("payment", count=3, constraints={"amount": 99.99}))
        for p in result.data:
            assert p["amount"] == 99.99


# ── Address generation ────────────────────────────────────────────────────────

class TestAddressGeneration:
    def test_generates_requested_count(self):
        result = _svc().generate(_req("address", count=3))
        assert result.count == 3

    def test_address_has_required_fields(self):
        result = _svc().generate(_req("address", count=1))
        a = result.data[0]
        assert "street" in a
        assert "city" in a
        assert "country" in a
        assert "postal_code" in a

    def test_address_city_constraint(self):
        result = _svc().generate(_req("address", count=3, constraints={"city": "Berlin"}))
        for a in result.data:
            assert a["city"] == "Berlin"

    def test_address_country_constraint(self):
        result = _svc().generate(_req("address", count=3, constraints={"country": "Germany"}))
        for a in result.data:
            assert a["country"] == "Germany"

    def test_address_postal_code_is_numeric_string(self):
        result = _svc().generate(_req("address", count=5))
        for a in result.data:
            assert a["postal_code"].isdigit()


# ── Deterministic generation with seed ───────────────────────────────────────

class TestDeterministicGeneration:
    def test_same_seed_produces_same_user(self):
        r1 = _svc().generate(_req("user", count=1, seed=42))
        r2 = _svc().generate(_req("user", count=1, seed=42))
        assert r1.data == r2.data

    def test_same_seed_produces_same_product(self):
        r1 = _svc().generate(_req("product", count=3, seed=99))
        r2 = _svc().generate(_req("product", count=3, seed=99))
        assert r1.data == r2.data

    def test_same_seed_produces_same_order(self):
        r1 = _svc().generate(_req("order", count=2, seed=7))
        r2 = _svc().generate(_req("order", count=2, seed=7))
        assert r1.data == r2.data

    def test_different_seeds_produce_different_users(self):
        r1 = _svc().generate(_req("user", count=1, seed=1))
        r2 = _svc().generate(_req("user", count=1, seed=2))
        assert r1.data != r2.data

    def test_seed_is_recorded_in_response(self):
        result = _svc().generate(_req("user", count=1, seed=123))
        assert result.seed_used == 123

    def test_no_seed_produces_random_data(self):
        r1 = _svc().generate(_req("user", count=1))
        r2 = _svc().generate(_req("user", count=1))
        # Very unlikely to collide without a seed
        assert r1.seed_used is None
        assert r2.seed_used is None


# ── Response structure ────────────────────────────────────────────────────────

class TestResponseStructure:
    def test_response_is_test_data_response(self):
        result = _svc().generate(_req("user", count=1))
        assert isinstance(result, TestDataResponse)

    def test_response_entity_type_matches_request(self):
        result = _svc().generate(_req("product", count=1))
        assert result.entity_type == "product"

    def test_response_count_matches_data_length(self):
        result = _svc().generate(_req("user", count=5))
        assert result.count == len(result.data)

    def test_count_capped_at_100(self):
        result = _svc().generate(_req("user", count=200))
        assert result.count == 100
        assert len(result.data) == 100
        assert len(result.notes) >= 1

    def test_count_one_is_minimum(self):
        result = _svc().generate(_req("user", count=0))
        assert result.count >= 1


# ── Template substitution ─────────────────────────────────────────────────────

class TestTemplateSubstitution:
    def test_substitutes_user_email_placeholder(self):
        ctx = {"user": {"email": "alice@test.com", "password": "Pass123!"}}
        steps = [{"action": "fill", "target": "email field", "value": "{{user.email}}"}]
        result = substitute_data_context(steps, ctx)
        assert result[0]["value"] == "alice@test.com"

    def test_substitutes_user_password_placeholder(self):
        ctx = {"user": {"email": "alice@test.com", "password": "Pass123!"}}
        steps = [{"action": "fill", "target": "password field", "value": "{{user.password}}"}]
        result = substitute_data_context(steps, ctx)
        assert result[0]["value"] == "Pass123!"

    def test_substitutes_product_name_placeholder(self):
        ctx = {"product": {"name": "Widget Pro", "price": 9.99}}
        steps = [{"action": "fill", "target": "search", "value": "{{product.name}}"}]
        result = substitute_data_context(steps, ctx)
        assert result[0]["value"] == "Widget Pro"

    def test_unknown_placeholder_left_unchanged(self):
        ctx = {"user": {"email": "test@test.com"}}
        steps = [{"action": "fill", "value": "{{unknown.field}}"}]
        result = substitute_data_context(steps, ctx)
        assert result[0]["value"] == "{{unknown.field}}"

    def test_multiple_placeholders_in_one_step(self):
        ctx = {"user": {"email": "x@y.com", "name": "Alice"}}
        steps = [{"action": "fill", "value": "{{user.email}} ({{user.name}})"}]
        result = substitute_data_context(steps, ctx)
        assert result[0]["value"] == "x@y.com (Alice)"

    def test_no_placeholders_steps_unchanged(self):
        ctx = {"user": {"email": "x@y.com"}}
        steps = [{"action": "goto", "value": "/login"}]
        result = substitute_data_context(steps, ctx)
        assert result == steps

    def test_empty_context_steps_unchanged(self):
        steps = [{"action": "fill", "value": "{{user.email}}"}]
        result = substitute_data_context(steps, {})
        assert result[0]["value"] == "{{user.email}}"


# ── Runner integration: preprocess_data_steps ────────────────────────────────

class TestPreprocessDataSteps:
    def test_generate_test_data_step_is_removed(self):
        raw = [
            {"action": "generate_test_data", "entity_type": "user"},
            {"action": "goto", "value": "/login"},
        ]
        processed, ctx = preprocess_data_steps(raw)
        actions = [s["action"] for s in processed]
        assert "generate_test_data" not in actions
        assert "goto" in actions

    def test_generated_data_is_in_context(self):
        raw = [{"action": "generate_test_data", "entity_type": "user"}]
        _, ctx = preprocess_data_steps(raw)
        assert "user" in ctx
        assert "email" in ctx["user"]

    def test_placeholders_are_substituted(self):
        raw = [
            {"action": "generate_test_data", "entity_type": "user", "seed": 42},
            {"action": "fill", "target": "email", "value": "{{user.email}}"},
        ]
        processed, ctx = preprocess_data_steps(raw)
        # The fill step's value should now be the actual email
        fill_step = next(s for s in processed if s.get("action") == "fill")
        assert fill_step["value"] == ctx["user"]["email"]
        assert "@" in fill_step["value"]

    def test_multiple_entity_types(self):
        raw = [
            {"action": "generate_test_data", "entity_type": "user",    "seed": 1},
            {"action": "generate_test_data", "entity_type": "product", "seed": 2},
            {"action": "fill", "value": "{{user.email}}"},
            {"action": "fill", "value": "{{product.name}}"},
        ]
        processed, ctx = preprocess_data_steps(raw)
        assert "user"    in ctx
        assert "product" in ctx
        values = [s["value"] for s in processed]
        assert ctx["user"]["email"]      in values
        assert ctx["product"]["name"]    in values

    def test_empty_steps_returns_empty(self):
        processed, ctx = preprocess_data_steps([])
        assert processed == []
        assert ctx == {}

    def test_no_data_steps_passes_through_unchanged(self):
        raw = [
            {"action": "goto",  "value": "/login"},
            {"action": "click", "target": "submit"},
        ]
        processed, ctx = preprocess_data_steps(raw)
        assert processed == raw
        assert ctx == {}

    def test_seed_makes_substitution_deterministic(self):
        raw1 = [
            {"action": "generate_test_data", "entity_type": "user", "seed": 7},
            {"action": "fill", "value": "{{user.email}}"},
        ]
        raw2 = [
            {"action": "generate_test_data", "entity_type": "user", "seed": 7},
            {"action": "fill", "value": "{{user.email}}"},
        ]
        p1, _ = preprocess_data_steps(raw1)
        p2, _ = preprocess_data_steps(raw2)
        assert p1[0]["value"] == p2[0]["value"]

    def test_unsupported_entity_type_does_not_crash(self):
        raw = [
            {"action": "generate_test_data", "entity_type": "unicorn"},
            {"action": "goto", "value": "/"},
        ]
        # Should not raise — unsupported type is logged and skipped
        processed, ctx = preprocess_data_steps(raw)
        assert any(s["action"] == "goto" for s in processed)
        assert "unicorn" not in ctx
