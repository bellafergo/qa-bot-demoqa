# services/test_data_service.py
"""
Synthetic Test Data Service
============================

Generates realistic, deterministic test data for common entity types
without requiring external services or a database.

Supported entity types:
  user, product, order, payment, address

Determinism:
  Pass a seed to get reproducible output across runs.
  Same seed + same count + same constraints = identical data.

Constraints (optional, per entity type):
  user:    email_domain, name_prefix, password_prefix
  product: category, min_price, max_price
  order:   user_id, min_items, max_items
  payment: status, amount
  address: country, city

Runner integration:
  Steps can use  {"action": "generate_test_data", "entity_type": "user"}
  to inject generated data at test execution time. The test_catalog_service
  preprocessing layer expands these steps and substitutes {{user.email}},
  {{user.password}}, etc. in subsequent step values.
"""
from __future__ import annotations

import logging
import random
import re
import uuid
from typing import Any, Dict, List, Optional, Tuple

from models.test_data_models import TestDataRequest, TestDataResponse

logger = logging.getLogger("vanya.test_data")


# ── Word lists (no external dependencies) ─────────────────────────────────────

_FIRST_NAMES = [
    "Alice", "Bob", "Carol", "David", "Emma", "Frank", "Grace", "Henry",
    "Isabel", "James", "Karen", "Luis", "Maria", "Nathan", "Olivia",
    "Paul", "Quinn", "Rachel", "Samuel", "Tina", "Uma", "Victor",
    "Wendy", "Xander", "Yara", "Zoe",
]

_LAST_NAMES = [
    "Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller",
    "Davis", "Martinez", "Anderson", "Taylor", "Thomas", "Hernandez",
    "Moore", "Martin", "Jackson", "Thompson", "White", "Lopez", "Lee",
    "Gonzalez", "Harris", "Clark", "Lewis", "Robinson", "Walker",
]

_PRODUCT_ADJECTIVES = [
    "Premium", "Deluxe", "Standard", "Professional", "Ultra",
    "Advanced", "Classic", "Elite", "Pro", "Compact",
]

_PRODUCT_CATEGORIES = [
    "Electronics", "Clothing", "Books", "Home", "Sports",
    "Beauty", "Toys", "Food", "Tools", "Office",
]

_PRODUCT_NOUNS = [
    "Widget", "Gadget", "Kit", "Bundle", "Set",
    "Pack", "Module", "Unit", "Accessory", "Device",
]

_CITIES = [
    "New York", "Los Angeles", "Chicago", "Houston", "Phoenix",
    "Philadelphia", "San Antonio", "San Diego", "Dallas", "Austin",
    "London", "Paris", "Berlin", "Madrid", "Rome",
    "Toronto", "Sydney", "Tokyo", "Seoul", "São Paulo",
]

_COUNTRIES = [
    "United States", "United Kingdom", "Canada", "Australia",
    "Germany", "France", "Spain", "Italy", "Japan", "Brazil",
]

_STREET_NAMES = [
    "Main St", "Oak Ave", "Maple Dr", "Cedar Blvd", "Pine Rd",
    "Elm St", "Walnut Way", "Park Ave", "Lake Rd", "Hill St",
]

_PAYMENT_STATUSES = ["pending", "completed", "failed", "refunded"]

_EMAIL_DOMAINS = ["example.com", "test.org", "demo.net", "qa.io", "testmail.dev"]


# ── ID generators ─────────────────────────────────────────────────────────────

def _short_id(rng: random.Random) -> str:
    """Generate a short hex ID."""
    return rng.randbytes(4).hex().upper()


# ── Entity generators ─────────────────────────────────────────────────────────

def _generate_user(rng: random.Random, constraints: Dict[str, Any]) -> Dict[str, Any]:
    first  = rng.choice(_FIRST_NAMES)
    last   = rng.choice(_LAST_NAMES)
    domain = constraints.get("email_domain") or rng.choice(_EMAIL_DOMAINS)
    prefix = constraints.get("name_prefix") or ""
    pw_pfx = constraints.get("password_prefix") or "Test"

    email = f"{first.lower()}.{last.lower()}{rng.randint(1, 999)}@{domain}"
    name  = f"{prefix}{first} {last}".strip()
    pw    = f"{pw_pfx}{rng.randint(1000, 9999)}!"

    return {
        "id":       str(uuid.UUID(int=rng.getrandbits(128))),
        "email":    email,
        "password": pw,
        "name":     name,
    }


def _generate_product(rng: random.Random, constraints: Dict[str, Any]) -> Dict[str, Any]:
    category = constraints.get("category") or rng.choice(_PRODUCT_CATEGORIES)
    min_p    = float(constraints.get("min_price") or 1.99)
    max_p    = float(constraints.get("max_price") or 499.99)
    price    = round(rng.uniform(min_p, max_p), 2)
    name     = f"{rng.choice(_PRODUCT_ADJECTIVES)} {category} {rng.choice(_PRODUCT_NOUNS)}"

    return {
        "id":       f"PROD-{_short_id(rng)}",
        "name":     name,
        "price":    price,
        "category": category,
    }


def _generate_address(rng: random.Random, constraints: Dict[str, Any]) -> Dict[str, Any]:
    city    = constraints.get("city")    or rng.choice(_CITIES)
    country = constraints.get("country") or rng.choice(_COUNTRIES)
    street  = f"{rng.randint(1, 9999)} {rng.choice(_STREET_NAMES)}"
    postal  = f"{rng.randint(10000, 99999)}"

    return {
        "street":      street,
        "city":        city,
        "country":     country,
        "postal_code": postal,
    }


def _generate_order(rng: random.Random, constraints: Dict[str, Any]) -> Dict[str, Any]:
    user_id    = constraints.get("user_id") or str(uuid.UUID(int=rng.getrandbits(128)))
    min_items  = int(constraints.get("min_items") or 1)
    max_items  = int(constraints.get("max_items") or 5)
    n_products = rng.randint(min_items, max_items)

    product_ids = [f"PROD-{_short_id(rng)}" for _ in range(n_products)]
    total       = round(rng.uniform(9.99, 499.99), 2)

    return {
        "id":           f"ORD-{_short_id(rng)}",
        "user_id":      user_id,
        "product_ids":  product_ids,
        "total_amount": total,
    }


def _generate_payment(rng: random.Random, constraints: Dict[str, Any]) -> Dict[str, Any]:
    order_id = constraints.get("order_id") or f"ORD-{_short_id(rng)}"
    status   = constraints.get("status")   or rng.choice(_PAYMENT_STATUSES)
    amount   = float(constraints.get("amount") or round(rng.uniform(9.99, 499.99), 2))

    return {
        "id":       f"PAY-{_short_id(rng)}",
        "order_id": order_id,
        "amount":   amount,
        "status":   status,
    }


# ── Dispatcher ────────────────────────────────────────────────────────────────

_GENERATORS = {
    "user":    _generate_user,
    "product": _generate_product,
    "order":   _generate_order,
    "payment": _generate_payment,
    "address": _generate_address,
}

SUPPORTED_ENTITY_TYPES = sorted(_GENERATORS.keys())


# ── Template substitution (runner integration) ────────────────────────────────

def substitute_data_context(
    steps: List[Dict[str, Any]],
    data_context: Dict[str, Dict[str, Any]],
) -> List[Dict[str, Any]]:
    """
    Replace ``{{entity_type.field}}`` placeholders in step string values.

    Example:
      data_context = {"user": {"email": "alice@example.com", "password": "Test1234!"}}
      step value "{{user.email}}" → "alice@example.com"
    """
    _PLACEHOLDER = re.compile(r"\{\{(\w+)\.(\w+)\}\}")

    def _sub_str(val: str) -> str:
        def _replace(m: re.Match) -> str:
            entity = m.group(1)
            field  = m.group(2)
            entity_data = data_context.get(entity) or {}
            return str(entity_data.get(field, m.group(0)))
        return _PLACEHOLDER.sub(_replace, val)

    def _sub_val(val: Any) -> Any:
        if isinstance(val, str):
            return _sub_str(val)
        if isinstance(val, dict):
            return {k: _sub_val(v) for k, v in val.items()}
        if isinstance(val, list):
            return [_sub_val(item) for item in val]
        return val

    return [_sub_val(step) for step in steps]  # type: ignore[return-value]


def preprocess_data_steps(
    raw_steps: List[Dict[str, Any]],
) -> Tuple[List[Dict[str, Any]], Dict[str, Dict[str, Any]]]:
    """
    Scan *raw_steps* for ``{"action": "generate_test_data", ...}`` entries.

    For each such step:
      - Generate the requested entity.
      - Store the first result in *data_context[entity_type]*.
      - Remove the step from the returned list.

    Then substitute ``{{entity_type.field}}`` placeholders in the
    remaining steps.

    Returns ``(processed_steps, data_context)``.
    """
    data_context: Dict[str, Dict[str, Any]] = {}
    cleaned: List[Dict[str, Any]] = []

    for step in raw_steps:
        action = (step.get("action") or "").strip().lower()
        if action == "generate_test_data":
            entity_type = (step.get("entity_type") or step.get("type") or "user").lower()
            try:
                svc = TestDataService()
                result = svc.generate(TestDataRequest(
                    entity_type  = entity_type,
                    count        = int(step.get("count") or 1),
                    seed         = step.get("seed"),
                    constraints  = dict(step.get("constraints") or {}),
                ))
                if result.data:
                    data_context[entity_type] = result.data[0]
                    logger.info(
                        "test_data: generated %s '%s' via step",
                        entity_type, list(result.data[0].keys()),
                    )
            except Exception as e:
                logger.warning(
                    "test_data: generate_test_data step failed for '%s': %s",
                    entity_type, e,
                )
            continue  # never pass this step to the Playwright runner

        cleaned.append(step)

    if data_context:
        cleaned = substitute_data_context(cleaned, data_context)

    return cleaned, data_context


# ── Service class ─────────────────────────────────────────────────────────────

class TestDataService:

    def generate(self, req: TestDataRequest) -> TestDataResponse:
        """Generate *req.count* entities of *req.entity_type*."""
        entity_type = req.entity_type.lower().strip()
        notes: List[str] = []

        generator = _GENERATORS.get(entity_type)
        if generator is None:
            supported = ", ".join(SUPPORTED_ENTITY_TYPES)
            raise ValueError(
                f"Unsupported entity type '{entity_type}'. "
                f"Supported: {supported}"
            )

        count = max(1, min(req.count, 100))  # cap at 100 to avoid runaway generation
        if count != req.count:
            notes.append(f"count capped at {count} (requested {req.count})")

        # Seed handling: None = random; int = deterministic
        rng = random.Random(req.seed)

        data = [generator(rng, req.constraints) for _ in range(count)]

        logger.info(
            "test_data: generated %d %s(s) seed=%s constraints=%s",
            count, entity_type, req.seed, req.constraints or None,
        )

        return TestDataResponse(
            entity_type = entity_type,
            data        = data,
            count       = count,
            seed_used   = req.seed,
            notes       = notes,
        )

    @staticmethod
    def supported_types() -> List[str]:
        return SUPPORTED_ENTITY_TYPES


# ── Module-level singleton ────────────────────────────────────────────────────

test_data_service = TestDataService()
