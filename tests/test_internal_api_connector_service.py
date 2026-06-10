# tests/test_internal_api_connector_service.py
"""INT-03C — Internal API connectors (read-only foundation)."""
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import patch

import pytest
from fastapi import HTTPException

from models.internal_api_connector_models import (
    ApiEndpointInput,
    ApiValidationCreateRequest,
    InternalApiConnectorRegistrationRequest,
)
from models.local_agent_models import LocalAgentFoundationRegistrationRequest, LocalAgentMockHeartbeatRequest
from services.internal_api_connector_service import (
    build_api_connector_report,
    build_connector_id,
    build_endpoint_id,
    build_validation_request_id,
    create_validation_request,
    list_internal_api_connectors,
    list_validation_requests,
    register_internal_api_connector,
)
from services.local_agent_service import mock_foundation_heartbeat, register_foundation_agent


class _Req:
    headers: dict = {}


def _register_agent(project_id: str, name: str):
    reg = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(
            project_id=project_id,
            name=name,
            environment="staging",
            version="1.0.0",
            capabilities=["contract_validation", "intranet_access"],
        ),
        _Req(),
    )
    mock_foundation_heartbeat(
        LocalAgentMockHeartbeatRequest(
            agent_id=reg.agent_id,
            version="1.0.0",
            capabilities=["contract_validation", "intranet_access"],
            timestamp=datetime.now(timezone.utc).isoformat(),
        ),
        _Req(),
    )
    return reg


def _register_connector(agent_id: str, name: str = "Payments API"):
    return register_internal_api_connector(
        InternalApiConnectorRegistrationRequest(
            agent_id=agent_id,
            name=name,
            api_type="REST",
            environment="STAGING",
            base_url_label="https://payments.internal",
            endpoints=[
                ApiEndpointInput(method="GET", path_label="/payments", category="payments"),
                ApiEndpointInput(method="GET", path_label="/payments/{id}", category="payments"),
                ApiEndpointInput(method="POST", path_label="/payments", category="payments"),
            ],
        ),
        _Req(),
    )


def test_empty_report():
    report = build_api_connector_report(project_id="empty-api-proj")
    assert report.connectors == []
    assert report.endpoints == []
    assert report.validations == []
    assert "No internal API connectors" in report.summary


def test_connector_registration():
    reg = _register_agent("api-reg-proj", "Api-Agent")
    connector = _register_connector(reg.agent_id)
    assert connector.connector_id == build_connector_id(reg.agent_id, "Payments API")
    assert connector.api_type == "REST"
    assert connector.environment == "STAGING"
    assert connector.endpoint_count == 3
    assert connector.read_only is True


def test_endpoint_registration():
    reg = _register_agent("api-endpoint-proj", "Endpoint-Agent")
    connector = _register_connector(reg.agent_id)
    report = build_api_connector_report(project_id="api-endpoint-proj")
    assert len(report.endpoints) == 3
    get_endpoints = [e for e in report.endpoints if e.method == "GET"]
    post_endpoints = [e for e in report.endpoints if e.method == "POST"]
    assert len(get_endpoints) == 2
    assert len(post_endpoints) == 1
    assert all(e.read_only for e in get_endpoints)
    assert post_endpoints[0].read_only is False
    assert post_endpoints[0].endpoint_id == build_endpoint_id(
        connector.connector_id,
        "POST",
        "/payments",
    )


def test_get_endpoint_allowed_validation():
    reg = _register_agent("api-get-proj", "Get-Agent")
    connector = _register_connector(reg.agent_id)
    report = build_api_connector_report(project_id="api-get-proj")
    get_endpoint = next(e for e in report.endpoints if e.method == "GET" and e.path_label == "/payments")
    validation = create_validation_request(
        ApiValidationCreateRequest(
            connector_id=connector.connector_id,
            endpoint_id=get_endpoint.endpoint_id,
            validation_type="contract_validation",
        ),
        _Req(),
    )
    assert validation.status == "PENDING"
    assert validation.requires_user_approval is True


def test_post_endpoint_blocked_validation():
    reg = _register_agent("api-post-proj", "Post-Agent")
    connector = _register_connector(reg.agent_id)
    report = build_api_connector_report(project_id="api-post-proj")
    post_endpoint = next(e for e in report.endpoints if e.method == "POST")
    validation = create_validation_request(
        ApiValidationCreateRequest(
            connector_id=connector.connector_id,
            endpoint_id=post_endpoint.endpoint_id,
            validation_type="availability_check",
        ),
        _Req(),
    )
    assert validation.status == "BLOCKED"
    assert validation.requires_user_approval is True


def test_validation_creation_listing():
    reg = _register_agent("api-valid-proj", "Valid-Agent")
    connector = _register_connector(reg.agent_id)
    report = build_api_connector_report(project_id="api-valid-proj")
    endpoint = report.endpoints[0]
    create_validation_request(
        ApiValidationCreateRequest(
            connector_id=connector.connector_id,
            endpoint_id=endpoint.endpoint_id,
            validation_type="schema_validation",
        ),
        _Req(),
    )
    validations = list_validation_requests(connector_id=connector.connector_id, limit=50, request=_Req())
    assert len(validations) == 1
    assert validations[0].validation_type == "schema_validation"


def test_approval_requirement_on_get():
    reg = _register_agent("api-approval-proj", "Approval-Agent")
    connector = _register_connector(reg.agent_id)
    endpoint = build_api_connector_report(project_id="api-approval-proj").endpoints[0]
    validation = create_validation_request(
        ApiValidationCreateRequest(
            connector_id=connector.connector_id,
            endpoint_id=endpoint.endpoint_id,
            validation_type="response_structure_validation",
        ),
        _Req(),
    )
    assert validation.requires_user_approval is True


def test_deterministic_ids():
    reg = _register_agent("api-det-proj", "Det-Agent")
    connector = _register_connector(reg.agent_id)
    endpoint_id = build_endpoint_id(connector.connector_id, "GET", "/payments")
    request_id = build_validation_request_id(connector.connector_id, endpoint_id, "availability_check")
    assert connector.connector_id.startswith("apiconn:")
    assert endpoint_id.startswith("apiendpoint:")
    assert request_id.startswith("apivalid:")
    again = build_validation_request_id(connector.connector_id, endpoint_id, "availability_check")
    assert again == request_id


def test_no_discovery_without_agent_registration():
    report = build_api_connector_report(project_id="no-scan-unregistered-project")
    assert report.connectors == []
    assert report.endpoints == []


def test_no_external_calls():
    reg = _register_agent("api-ext-proj", "Ext-Agent")
    _register_connector(reg.agent_id)
    with patch("urllib.request.urlopen", side_effect=AssertionError("external HTTP")):
        with patch("requests.request", side_effect=AssertionError("external HTTP")):
            connectors = list_internal_api_connectors(agent_id=reg.agent_id, limit=50, request=_Req())
    assert len(connectors) == 1


def test_registration_requires_endpoints():
    reg = _register_agent("api-no-endpoints", "No-End-Agent")
    with pytest.raises(HTTPException) as exc:
        register_internal_api_connector(
            InternalApiConnectorRegistrationRequest(
                agent_id=reg.agent_id,
                name="Empty API",
                api_type="REST",
                environment="STAGING",
                base_url_label="https://empty.internal",
                endpoints=[],
            ),
            _Req(),
        )
    assert exc.value.status_code == 400
