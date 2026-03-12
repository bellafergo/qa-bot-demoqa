# tests/test_api_testing.py
"""
Tests for the OpenAPI / Swagger API Testing block.

No external network required.
All API runner tests use mocked httpx responses.
All catalog operations use the isolated SQLite test DB.
"""
from __future__ import annotations

import json
from typing import Any, Dict, List
from unittest.mock import MagicMock, patch

import pytest

from models.api_testing_models import (
    APITestExecutionRequest,
    APITestGenerationRequest,
    ApproveAPITestsRequest,
    DraftAPITest,
    EndpointInfo,
    OpenAPISpecRequest,
)
from models.test_case import TestCaseCreate
from services.api_test_generation_service import (
    APITestGenerationService,
    _infer_module,
    _build_example_body,
    _get_required_fields,
    _path_example,
)
from services.openapi_parser_service import (
    OpenAPIParserService,
    _load_spec,
    _spec_version,
)
from services.test_catalog_service import TestCatalogService, _reset_for_testing
from services.catalog_orchestrator import _reset_for_testing as _orch_reset


# ── Helpers ────────────────────────────────────────────────────────────────────

def _reset():
    _reset_for_testing()
    _orch_reset()


def _svc() -> APITestGenerationService:
    return APITestGenerationService()


def _parser() -> OpenAPIParserService:
    return OpenAPIParserService()


# ── Minimal spec fixtures ─────────────────────────────────────────────────────

_OPENAPI3_JSON = json.dumps({
    "openapi": "3.0.0",
    "info": {"title": "Test API", "version": "1.0.0"},
    "paths": {
        "/users": {
            "get": {
                "operationId": "listUsers",
                "summary": "List users",
                "tags": ["users"],
                "parameters": [
                    {"name": "limit", "in": "query", "schema": {"type": "integer"}}
                ],
                "responses": {
                    "200": {
                        "content": {
                            "application/json": {
                                "schema": {"type": "array", "items": {"type": "object"}}
                            }
                        }
                    }
                }
            },
            "post": {
                "operationId": "createUser",
                "summary": "Create user",
                "tags": ["users"],
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "type": "object",
                                "required": ["email", "name"],
                                "properties": {
                                    "email": {"type": "string"},
                                    "name":  {"type": "string"},
                                    "age":   {"type": "integer"},
                                }
                            }
                        }
                    }
                },
                "security": [{"bearerAuth": []}],
                "responses": {"201": {"description": "Created"}},
            }
        },
        "/users/{id}": {
            "get": {
                "operationId": "getUser",
                "summary": "Get user by ID",
                "tags": ["users"],
                "parameters": [
                    {"name": "id", "in": "path", "required": True, "schema": {"type": "string"}}
                ],
                "security": [{"bearerAuth": []}],
                "responses": {"200": {"description": "OK"}},
            }
        }
    }
})

_SWAGGER2_JSON = json.dumps({
    "swagger": "2.0",
    "info": {"title": "Swagger API", "version": "1.0"},
    "basePath": "/api/v1",
    "paths": {
        "/products": {
            "get": {
                "operationId": "listProducts",
                "summary": "List products",
                "tags": ["catalog"],
                "parameters": [
                    {"name": "page", "in": "query", "type": "integer"}
                ],
                "responses": {"200": {"description": "OK"}}
            },
            "post": {
                "operationId": "createProduct",
                "summary": "Create product",
                "tags": ["catalog"],
                "parameters": [
                    {
                        "name": "body", "in": "body",
                        "schema": {
                            "type": "object",
                            "required": ["name", "price"],
                            "properties": {
                                "name":  {"type": "string"},
                                "price": {"type": "number"},
                            }
                        }
                    }
                ],
                "security": [{"ApiKeyAuth": []}],
                "responses": {"201": {"description": "Created"}}
            }
        }
    },
    "securityDefinitions": {
        "ApiKeyAuth": {"type": "apiKey", "in": "header", "name": "X-API-Key"}
    }
})

_OPENAPI3_YAML = """
openapi: "3.0.0"
info:
  title: YAML API
  version: "1.0.0"
paths:
  /items:
    get:
      operationId: listItems
      summary: List items
      tags:
        - catalog
      responses:
        "200":
          description: OK
          content:
            application/json:
              schema:
                type: array
                items:
                  type: object
"""

_AUTH_SPEC = json.dumps({
    "openapi": "3.0.0",
    "info": {"title": "Auth API", "version": "1.0.0"},
    "paths": {
        "/auth/login": {
            "post": {
                "operationId": "login",
                "summary": "Login",
                "tags": ["auth"],
                "requestBody": {
                    "content": {
                        "application/json": {
                            "schema": {
                                "type": "object",
                                "required": ["username", "password"],
                                "properties": {
                                    "username": {"type": "string"},
                                    "password": {"type": "string"},
                                }
                            }
                        }
                    }
                },
                "responses": {"200": {"description": "OK"}},
            }
        },
        "/auth/me": {
            "get": {
                "operationId": "me",
                "summary": "Current user",
                "tags": ["auth"],
                "security": [{"bearerAuth": []}],
                "responses": {"200": {"description": "OK"}},
            }
        }
    }
})


# ── 1. OpenAPI JSON parsing ───────────────────────────────────────────────────

class TestOpenAPIJSONParsing:
    def test_parse_openapi3_json_returns_endpoints(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        assert len(endpoints) >= 3  # GET /users, POST /users, GET /users/{id}

    def test_endpoints_have_correct_paths(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        paths = {e.path for e in endpoints}
        assert "/users" in paths
        assert "/users/{id}" in paths

    def test_endpoints_have_correct_methods(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        method_path = {(e.method, e.path) for e in endpoints}
        assert ("GET", "/users") in method_path
        assert ("POST", "/users") in method_path
        assert ("GET", "/users/{id}") in method_path

    def test_endpoint_has_summary(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        ep = next(e for e in endpoints if e.method == "GET" and e.path == "/users")
        assert ep.summary == "List users"

    def test_endpoint_has_tags(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        ep = next(e for e in endpoints if e.path == "/users" and e.method == "GET")
        assert "users" in ep.tags

    def test_endpoint_has_operation_id(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        ep = next(e for e in endpoints if e.method == "GET" and e.path == "/users")
        assert ep.operation_id == "listUsers"

    def test_load_spec_json(self):
        data = _load_spec(_OPENAPI3_JSON)
        assert isinstance(data, dict)
        assert "openapi" in data

    def test_spec_version_openapi3(self):
        data = _load_spec(_OPENAPI3_JSON)
        assert _spec_version(data) == "3"


# ── 2. OpenAPI YAML parsing ───────────────────────────────────────────────────

class TestOpenAPIYAMLParsing:
    def test_parse_yaml_spec(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_YAML, format_hint="yaml")
        endpoints = _parser().parse(req)
        assert len(endpoints) >= 1

    def test_yaml_endpoint_path_correct(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_YAML, format_hint="yaml")
        endpoints = _parser().parse(req)
        assert any(e.path == "/items" for e in endpoints)

    def test_yaml_endpoint_method_correct(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_YAML, format_hint="yaml")
        endpoints = _parser().parse(req)
        ep = next(e for e in endpoints if e.path == "/items")
        assert ep.method == "GET"

    def test_load_spec_yaml_string(self):
        data = _load_spec(_OPENAPI3_YAML, format_hint="yaml")
        assert isinstance(data, dict)
        assert "openapi" in data


# ── 3. Swagger 2.0 parsing ────────────────────────────────────────────────────

class TestSwagger2Parsing:
    def test_swagger2_endpoints_discovered(self):
        req = OpenAPISpecRequest(spec_text=_SWAGGER2_JSON)
        endpoints = _parser().parse(req)
        assert len(endpoints) >= 2

    def test_swagger2_spec_version(self):
        data = _load_spec(_SWAGGER2_JSON)
        assert _spec_version(data) == "2"

    def test_swagger2_request_schema_extracted(self):
        req = OpenAPISpecRequest(spec_text=_SWAGGER2_JSON)
        endpoints = _parser().parse(req)
        post_ep = next(e for e in endpoints if e.method == "POST" and e.path == "/products")
        assert post_ep.request_body_schema is not None

    def test_swagger2_requires_auth(self):
        req = OpenAPISpecRequest(spec_text=_SWAGGER2_JSON)
        endpoints = _parser().parse(req)
        post_ep = next(e for e in endpoints if e.method == "POST" and e.path == "/products")
        assert post_ep.requires_auth is True

    def test_swagger2_body_params_excluded_from_parameters(self):
        req = OpenAPISpecRequest(spec_text=_SWAGGER2_JSON)
        endpoints = _parser().parse(req)
        post_ep = next(e for e in endpoints if e.method == "POST" and e.path == "/products")
        # Body parameter should NOT appear in parameters list
        assert not any(p.get("in") == "body" for p in post_ep.parameters)


# ── 4. Endpoint discovery ─────────────────────────────────────────────────────

class TestEndpointDiscovery:
    def test_parameters_extracted(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        get_users = next(e for e in endpoints if e.method == "GET" and e.path == "/users")
        assert any(p.get("name") == "limit" for p in get_users.parameters)

    def test_request_body_schema_extracted(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        post_users = next(e for e in endpoints if e.method == "POST" and e.path == "/users")
        assert post_users.request_body_schema is not None
        assert "properties" in post_users.request_body_schema

    def test_requires_auth_true_for_secured(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        post_users = next(e for e in endpoints if e.method == "POST" and e.path == "/users")
        assert post_users.requires_auth is True

    def test_requires_auth_false_for_unsecured(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        get_users = next(e for e in endpoints if e.method == "GET" and e.path == "/users")
        # No security on GET /users
        assert get_users.requires_auth is False

    def test_security_schemes_populated(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        post_users = next(e for e in endpoints if e.method == "POST" and e.path == "/users")
        assert "bearerAuth" in post_users.security_schemes

    def test_response_schema_extracted(self):
        req = OpenAPISpecRequest(spec_text=_OPENAPI3_JSON)
        endpoints = _parser().parse(req)
        get_users = next(e for e in endpoints if e.method == "GET" and e.path == "/users")
        assert get_users.response_schema is not None

    def test_invalid_spec_raises(self):
        with pytest.raises(Exception):
            _load_spec("not json or yaml at all !!!@#$%")

    def test_missing_source_raises(self):
        with pytest.raises(ValueError):
            OpenAPISpecRequest()  # no spec_url or spec_text


# ── 5. Module inference ───────────────────────────────────────────────────────

class TestModuleInference:
    def _ep(self, path="", tags=None, summary=""):
        return EndpointInfo(path=path, method="GET", tags=tags or [], summary=summary)

    def test_auth_tag_infers_auth(self):
        ep = self._ep(tags=["auth"])
        assert _infer_module(ep) == "auth"

    def test_login_path_infers_auth(self):
        ep = self._ep(path="/auth/login")
        assert _infer_module(ep) == "auth"

    def test_checkout_path_infers_checkout(self):
        ep = self._ep(path="/checkout/complete")
        assert _infer_module(ep) == "checkout"

    def test_catalog_tag_infers_catalog(self):
        ep = self._ep(tags=["catalog"])
        assert _infer_module(ep) == "catalog"

    def test_user_path_infers_account(self):
        ep = self._ep(path="/users/profile")
        assert _infer_module(ep) == "account"

    def test_unknown_path_returns_api(self):
        ep = self._ep(path="/xyz/foo/bar")
        assert _infer_module(ep) == "api"

    def test_override_takes_precedence(self):
        ep = self._ep(tags=["auth"])
        assert _infer_module(ep, override="custom-module") == "custom-module"

    def test_product_path_infers_catalog(self):
        ep = self._ep(path="/products")
        assert _infer_module(ep) == "catalog"

    def test_payment_path_infers_checkout(self):
        ep = self._ep(path="/payments/process")
        assert _infer_module(ep) == "checkout"

    def test_summary_keywords_used(self):
        ep = self._ep(path="/v1/foo", summary="Login endpoint for users")
        assert _infer_module(ep) == "auth"


# ── 6. Happy path draft generation ───────────────────────────────────────────

class TestHappyPathGeneration:
    def setup_method(self):
        _reset()

    def test_generate_returns_response(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON)
        result = _svc().generate(req)
        assert result.total_drafts > 0

    def test_happy_path_draft_generated_for_each_endpoint(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=False)
        result = _svc().generate(req)
        # 3 endpoints → at least 3 drafts (one happy path each)
        assert result.total_drafts >= 3

    def test_draft_has_api_request_step(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=False)
        result = _svc().generate(req)
        for draft in result.drafts:
            assert any(s.get("action") == "api_request" for s in draft.steps)

    def test_happy_path_type_is_smoke(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=False)
        result = _svc().generate(req)
        smoke_drafts = [d for d in result.drafts if d.type == "smoke"]
        assert len(smoke_drafts) > 0

    def test_draft_has_status_assertion(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=False)
        result = _svc().generate(req)
        for draft in result.drafts:
            assert any("status_code" in a.get("type", "") for a in draft.assertions)

    def test_draft_name_is_non_empty(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=False)
        result = _svc().generate(req)
        for draft in result.drafts:
            assert isinstance(draft.name, str) and draft.name

    def test_draft_has_endpoint(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=False)
        result = _svc().generate(req)
        for draft in result.drafts:
            assert draft.endpoint

    def test_draft_is_draft_api_test_instance(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=False)
        result = _svc().generate(req)
        for d in result.drafts:
            assert isinstance(d, DraftAPITest)

    def test_path_params_replaced_in_step(self):
        ep = EndpointInfo(path="/users/{id}", method="GET", requires_auth=False)
        req = APITestGenerationRequest(endpoints=[ep], include_negative_tests=False)
        result = _svc().generate(req)
        step = result.drafts[0].steps[0]
        assert "{id}" not in step.get("endpoint", "")


# ── 7. Missing required field generation ─────────────────────────────────────

class TestMissingFieldGeneration:
    def setup_method(self):
        _reset()

    def test_missing_field_draft_generated_for_post(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=True)
        result = _svc().generate(req)
        negative = [d for d in result.drafts if d.type == "negative" and "Missing" in d.name]
        assert len(negative) >= 1

    def test_missing_field_draft_has_400_assertion(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=True)
        result = _svc().generate(req)
        for draft in result.drafts:
            if "Missing Field" in draft.name:
                assert any(str(a.get("value", "")) == "400" for a in draft.assertions)

    def test_missing_field_draft_type_is_negative(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_negative_tests=True)
        result = _svc().generate(req)
        for draft in result.drafts:
            if "Missing Field" in draft.name:
                assert draft.type == "negative"

    def test_build_example_body_with_schema(self):
        schema = {
            "type": "object",
            "required": ["email"],
            "properties": {
                "email": {"type": "string"},
                "age": {"type": "integer"},
            }
        }
        body = _build_example_body(schema)
        assert "email" in body

    def test_get_required_fields_returns_list(self):
        schema = {"required": ["email", "name"]}
        assert _get_required_fields(schema) == ["email", "name"]

    def test_get_required_fields_empty_schema(self):
        assert _get_required_fields(None) == []
        assert _get_required_fields({}) == []


# ── 8. Auth-related test generation ─────────────────────────────────────────

class TestAuthTestGeneration:
    def setup_method(self):
        _reset()

    def test_auth_draft_generated_for_secured_endpoint(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_auth_tests=True)
        result = _svc().generate(req)
        auth_drafts = [d for d in result.drafts if "Unauthorized" in d.name]
        assert len(auth_drafts) >= 1

    def test_auth_draft_not_generated_when_disabled(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_auth_tests=False)
        result = _svc().generate(req)
        auth_drafts = [d for d in result.drafts if "Unauthorized" in d.name]
        assert len(auth_drafts) == 0

    def test_auth_draft_assertion_expects_401_or_403(self):
        req = APITestGenerationRequest(spec_text=_AUTH_SPEC, include_auth_tests=True)
        result = _svc().generate(req)
        auth_drafts = [d for d in result.drafts if "Unauthorized" in d.name]
        assert len(auth_drafts) >= 1
        for d in auth_drafts:
            assert any("401" in str(a.get("value", "")) or "403" in str(a.get("value", ""))
                       for a in d.assertions)

    def test_auth_draft_type_is_negative(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, include_auth_tests=True)
        result = _svc().generate(req)
        for d in result.drafts:
            if "Unauthorized" in d.name:
                assert d.type == "negative"

    def test_auth_module_inferred_from_auth_spec(self):
        req = APITestGenerationRequest(spec_text=_AUTH_SPEC, include_negative_tests=False)
        result = _svc().generate(req)
        modules = {d.module for d in result.drafts}
        assert "auth" in modules


# ── 9. Max drafts limit ───────────────────────────────────────────────────────

class TestMaxDraftsLimit:
    def setup_method(self):
        _reset()

    def test_max_drafts_respected(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, max_drafts=2)
        result = _svc().generate(req)
        assert result.total_drafts == 2
        assert len(result.drafts) == 2

    def test_cap_note_added_when_limited(self):
        req = APITestGenerationRequest(spec_text=_OPENAPI3_JSON, max_drafts=1)
        result = _svc().generate(req)
        assert any("cap" in n.lower() for n in result.generation_notes)


# ── 10. Approval into catalog ─────────────────────────────────────────────────

class TestApprovalIntoCatalog:
    def setup_method(self):
        _reset()

    def _make_draft(self, name="Test Draft", module="api") -> DraftAPITest:
        return DraftAPITest(
            name      = name,
            module    = module,
            endpoint  = "/test",
            method    = "GET",
            type      = "smoke",
            priority  = "medium",
            rationale = "test",
            steps     = [{"action": "api_request", "method": "GET", "endpoint": "/test"}],
            assertions = [{"type": "status_code_equals", "value": "200"}],
        )

    def test_approve_creates_catalog_entry(self):
        draft = self._make_draft()
        result = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        assert result.total_created == 1
        assert len(result.created_test_case_ids) == 1

    def test_approved_id_is_tc_api_format(self):
        draft = self._make_draft()
        result = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        tc_id = result.created_test_case_ids[0]
        assert tc_id.startswith("TC-API-")

    def test_multiple_approvals_get_sequential_ids(self):
        drafts = [self._make_draft(f"Draft {i}") for i in range(3)]
        result = _svc().approve(ApproveAPITestsRequest(drafts=drafts))
        assert result.total_created == 3
        nums = [int(tc_id.split("-")[-1]) for tc_id in result.created_test_case_ids]
        assert nums == sorted(nums)
        assert len(set(nums)) == 3  # all unique

    def test_catalog_stores_test_type_api(self):
        from services.db.catalog_repository import catalog_repo
        draft = self._make_draft()
        result = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        tc = catalog_repo.get_test_case(result.created_test_case_ids[0])
        assert tc is not None
        assert tc.test_type == "api"

    def test_activate_false_saves_as_inactive(self):
        from services.db.catalog_repository import catalog_repo
        draft = self._make_draft()
        result = _svc().approve(ApproveAPITestsRequest(drafts=[draft], activate=False))
        tc = catalog_repo.get_test_case(result.created_test_case_ids[0])
        assert tc.status == "inactive"

    def test_activate_true_saves_as_active(self):
        from services.db.catalog_repository import catalog_repo
        draft = self._make_draft()
        result = _svc().approve(ApproveAPITestsRequest(drafts=[draft], activate=True))
        tc = catalog_repo.get_test_case(result.created_test_case_ids[0])
        assert tc.status == "active"

    def test_approved_test_has_api_generated_tag(self):
        from services.db.catalog_repository import catalog_repo
        draft = self._make_draft()
        result = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        tc = catalog_repo.get_test_case(result.created_test_case_ids[0])
        assert "api-generated" in tc.tags

    def test_duplicate_approval_counted_as_skipped(self):
        # First approval succeeds, second triggers duplicate ID error
        draft = self._make_draft()
        r1 = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        assert r1.total_created == 1
        # Patch allocate to return the same ID to force duplicate
        tc_id = r1.created_test_case_ids[0]
        with patch("services.api_test_generation_service._allocate_api_ids", return_value=[tc_id]):
            r2 = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        assert r2.total_skipped == 1
        assert r2.total_created == 0

    def test_approved_steps_preserved(self):
        from services.db.catalog_repository import catalog_repo
        step = {"action": "api_request", "method": "POST", "endpoint": "/checkout", "body": {"item": "x"}}
        draft = DraftAPITest(
            name="Checkout test", module="checkout", endpoint="/checkout",
            method="POST", steps=[step], assertions=[],
        )
        result = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        tc = catalog_repo.get_test_case(result.created_test_case_ids[0])
        first_step = tc.steps[0]
        assert first_step.action == "api_request"


# ── 11. test_type field in catalog ────────────────────────────────────────────

class TestTestTypeField:
    def setup_method(self):
        _reset()

    def test_default_test_type_is_ui(self):
        from services.db.catalog_repository import catalog_repo
        from services.test_catalog_service import catalog_service
        tc = catalog_service.create_test_case(TestCaseCreate(
            test_case_id = "TC-UI-TYPE-001",
            name         = "UI test",
            module       = "auth",
            type         = "smoke",
            priority     = "medium",
            steps        = [{"action": "goto", "value": "https://example.com"}],
        ))
        stored = catalog_repo.get_test_case("TC-UI-TYPE-001")
        assert stored.test_type == "ui"

    def test_api_test_type_persisted(self):
        from services.db.catalog_repository import catalog_repo
        from services.test_catalog_service import catalog_service
        tc = catalog_service.create_test_case(TestCaseCreate(
            test_case_id = "TC-API-TYPE-001",
            name         = "API test",
            module       = "api",
            type         = "smoke",
            priority     = "medium",
            test_type    = "api",
            steps        = [{"action": "api_request", "method": "GET", "endpoint": "/health"}],
        ))
        stored = catalog_repo.get_test_case("TC-API-TYPE-001")
        assert stored.test_type == "api"

    def test_count_by_test_type_returns_dict(self):
        from services.db.catalog_repository import catalog_repo
        from services.test_catalog_service import catalog_service
        catalog_service.create_test_case(TestCaseCreate(
            test_case_id="TC-CTT-001", name="UI", module="m", type="smoke",
            priority="medium", test_type="ui",
            steps=[{"action": "goto", "value": "https://x.com"}],
        ))
        catalog_service.create_test_case(TestCaseCreate(
            test_case_id="TC-CTT-002", name="API", module="m", type="smoke",
            priority="medium", test_type="api",
            steps=[{"action": "api_request", "method": "GET", "endpoint": "/x"}],
        ))
        counts = catalog_repo.count_by_test_type()
        assert counts.get("ui", 0) >= 1
        assert counts.get("api", 0) >= 1

    def test_list_test_cases_filter_by_test_type(self):
        from services.db.catalog_repository import catalog_repo
        from services.test_catalog_service import catalog_service
        catalog_service.create_test_case(TestCaseCreate(
            test_case_id="TC-FTT-001", name="UI", module="m", type="smoke",
            priority="medium", test_type="ui",
            steps=[{"action": "goto", "value": "https://x.com"}],
        ))
        catalog_service.create_test_case(TestCaseCreate(
            test_case_id="TC-FTT-002", name="API", module="m", type="smoke",
            priority="medium", test_type="api",
            steps=[{"action": "api_request", "method": "GET", "endpoint": "/x"}],
        ))
        api_tests = catalog_repo.list_test_cases(test_type="api")
        ids = [t.test_case_id for t in api_tests]
        assert "TC-FTT-002" in ids
        assert "TC-FTT-001" not in ids


# ── 12. API runner (mocked httpx) ─────────────────────────────────────────────

def _mock_response(status_code: int, json_body: Any = None, headers: Dict = None) -> MagicMock:
    """Create a mock httpx.Response."""
    resp = MagicMock()
    resp.status_code = status_code
    resp.is_success = (200 <= status_code < 300)
    resp.text = json.dumps(json_body or {})
    resp.headers = headers or {"Content-Type": "application/json"}
    if json_body is not None:
        resp.json.return_value = json_body
    else:
        resp.json.side_effect = Exception("no json")
    return resp


class TestAPIRunner:
    def _run(self, steps, assertions=None, mock_response=None, status_code=200, json_body=None):
        from services.api_runner import run_api_test

        resp = mock_response or _mock_response(status_code, json_body or {"ok": True})

        with patch("httpx.Client") as mock_client_cls:
            mock_client = MagicMock()
            mock_client_cls.return_value.__enter__.return_value = mock_client
            mock_client.request.return_value = resp

            return run_api_test(
                steps=steps,
                base_url="https://api.example.com",
                assertions=assertions or [],
            )

    def test_success_returns_pass(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/users"}]
        result = self._run(steps, status_code=200)
        assert result["status"] == "pass"
        assert result["ok"] is True

    def test_failure_response_returns_fail(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/users"}]
        result = self._run(steps, status_code=500)
        assert result["ok"] is False

    def test_status_code_equals_assertion_passes(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/users"}]
        assertions = [{"type": "status_code_equals", "value": "200"}]
        result = self._run(steps, assertions=assertions, status_code=200)
        assert result["ok"] is True

    def test_status_code_equals_assertion_fails(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/users"}]
        assertions = [{"type": "status_code_equals", "value": "201"}]
        result = self._run(steps, assertions=assertions, status_code=200)
        assert result["ok"] is False

    def test_status_code_in_assertion(self):
        steps = [{"action": "api_request", "method": "POST", "endpoint": "/users"}]
        assertions = [{"type": "status_code_in", "value": [200, 201]}]
        result = self._run(steps, assertions=assertions, status_code=201)
        assert result["ok"] is True

    def test_response_contains_assertion_passes(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/user/1"}]
        assertions = [{"type": "response_contains", "target": "email", "value": "test@example.com"}]
        result = self._run(
            steps, assertions=assertions, status_code=200,
            json_body={"email": "test@example.com"}
        )
        assert result["ok"] is True

    def test_response_contains_assertion_fails(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/user/1"}]
        assertions = [{"type": "response_contains", "target": "email", "value": "wrong@example.com"}]
        result = self._run(
            steps, assertions=assertions, status_code=200,
            json_body={"email": "test@example.com"}
        )
        assert result["ok"] is False

    def test_response_schema_match_assertion(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/items"}]
        assertions = [{"type": "response_schema_match"}]
        result = self._run(steps, assertions=assertions, status_code=200, json_body=[{"id": 1}])
        assert result["ok"] is True

    def test_header_exists_assertion(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/items"}]
        assertions = [{"type": "header_exists", "value": "Content-Type"}]
        resp = _mock_response(200, {"ok": True}, headers={"Content-Type": "application/json"})
        result = self._run(steps, assertions=assertions, mock_response=resp)
        assert result["ok"] is True

    def test_result_has_duration_ms(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/health"}]
        result = self._run(steps)
        assert isinstance(result.get("duration_ms"), int)

    def test_result_has_logs(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/health"}]
        result = self._run(steps)
        assert isinstance(result.get("logs"), list)

    def test_result_has_steps(self):
        steps = [{"action": "api_request", "method": "GET", "endpoint": "/health"}]
        result = self._run(steps)
        assert isinstance(result.get("steps"), list)

    def test_non_api_request_action_skipped(self):
        steps = [
            {"action": "goto", "url": "https://example.com"},
            {"action": "api_request", "method": "GET", "endpoint": "/health"},
        ]
        result = self._run(steps, status_code=200)
        # Should still pass (goto skipped, api_request succeeds)
        assert result["status"] == "pass"

    def test_no_steps_returns_error(self):
        from services.api_runner import run_api_test
        result = run_api_test(steps=[], base_url="https://api.example.com")
        assert result["status"] == "error"


# ── 13. Auth handling ────────────────────────────────────────────────────────

class TestAuthHandling:
    def _run_with_auth(self, auth_config, capture_headers=None):
        from services.api_runner import run_api_test

        captured = {}
        resp = _mock_response(200, {"ok": True})

        with patch("httpx.Client") as mock_client_cls:
            mock_client = MagicMock()
            mock_client_cls.return_value.__enter__.return_value = mock_client

            def capture_request(**kwargs):
                captured["headers"] = kwargs.get("headers", {})
                return resp

            mock_client.request.side_effect = capture_request

            run_api_test(
                steps=[{"action": "api_request", "method": "GET", "endpoint": "/me"}],
                base_url="https://api.example.com",
                auth_config=auth_config,
            )

        return captured.get("headers", {})

    def test_bearer_auth_adds_authorization_header(self):
        headers = self._run_with_auth({"type": "bearer", "token": "abc123"})
        assert headers.get("Authorization") == "Bearer abc123"

    def test_api_key_auth_adds_custom_header(self):
        headers = self._run_with_auth({"type": "api_key", "header": "X-API-Key", "token": "key456"})
        assert headers.get("X-API-Key") == "key456"

    def test_basic_auth_adds_authorization_header(self):
        import base64
        headers = self._run_with_auth({"type": "basic", "username": "user", "password": "pass"})
        expected = "Basic " + base64.b64encode(b"user:pass").decode()
        assert headers.get("Authorization") == expected

    def test_no_auth_config_sends_no_auth_headers(self):
        headers = self._run_with_auth(None)
        assert "Authorization" not in headers


# ── 14. Dashboard test type counts ───────────────────────────────────────────

class TestDashboardTestTypeCounts:
    def setup_method(self):
        _reset()

    def test_summary_has_total_ui_tests(self):
        from services.dashboard_service import dashboard_service
        summary = dashboard_service.get_summary()
        assert hasattr(summary, "total_ui_tests")

    def test_summary_has_total_api_tests(self):
        from services.dashboard_service import dashboard_service
        summary = dashboard_service.get_summary()
        assert hasattr(summary, "total_api_tests")

    def test_api_tests_counted_correctly(self):
        from services.dashboard_service import dashboard_service
        from services.test_catalog_service import catalog_service
        catalog_service.create_test_case(TestCaseCreate(
            test_case_id="TC-DASH-API-001", name="API test", module="api",
            type="smoke", priority="medium", test_type="api",
            steps=[{"action": "api_request", "method": "GET", "endpoint": "/x"}],
        ))
        summary = dashboard_service.get_summary()
        assert summary.total_api_tests >= 1

    def test_ui_tests_counted_correctly(self):
        from services.dashboard_service import dashboard_service
        from services.test_catalog_service import catalog_service
        catalog_service.create_test_case(TestCaseCreate(
            test_case_id="TC-DASH-UI-001", name="UI test", module="auth",
            type="smoke", priority="medium", test_type="ui",
            steps=[{"action": "goto", "value": "https://x.com"}],
        ))
        summary = dashboard_service.get_summary()
        assert summary.total_ui_tests >= 1


# ── 15. Orchestrator compatibility ───────────────────────────────────────────

class TestOrchestratorCompatibility:
    def setup_method(self):
        _reset()

    def test_approved_api_test_can_be_enqueued(self):
        """Approved API tests appear in catalog and can be enqueued via orchestrator."""
        from services.catalog_orchestrator import orchestrator_service

        draft = DraftAPITest(
            name="API smoke", module="api", endpoint="/health",
            method="GET", steps=[{"action": "api_request", "method": "GET", "endpoint": "/health"}],
            assertions=[{"type": "status_code_equals", "value": "200"}],
        )
        approval = _svc().approve(ApproveAPITestsRequest(drafts=[draft]))
        assert approval.total_created == 1
        tc_id = approval.created_test_case_ids[0]

        job = orchestrator_service.enqueue_suite(test_case_ids=[tc_id])
        assert job.job_id is not None
        assert job.status in ("queued", "running", "completed", "failed")

    def test_approved_api_test_is_active_in_catalog(self):
        from services.db.catalog_repository import catalog_repo
        draft = DraftAPITest(
            name="Active API", module="api", endpoint="/status",
            method="GET", steps=[{"action": "api_request", "method": "GET", "endpoint": "/status"}],
            assertions=[],
        )
        approval = _svc().approve(ApproveAPITestsRequest(drafts=[draft], activate=True))
        tc = catalog_repo.get_test_case(approval.created_test_case_ids[0])
        assert tc.status == "active"
        assert tc.test_type == "api"


# ── 16. Path example helper ───────────────────────────────────────────────────

class TestPathExample:
    def test_path_param_replaced(self):
        result = _path_example("/users/{id}")
        assert "{id}" not in result
        assert "test-id-001" in result

    def test_multiple_params_replaced(self):
        result = _path_example("/orgs/{org_id}/repos/{repo_id}")
        assert "{org_id}" not in result
        assert "{repo_id}" not in result

    def test_no_params_unchanged(self):
        assert _path_example("/users") == "/users"
