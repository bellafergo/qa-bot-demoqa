# tests/test_enterprise_system_connector_service.py
"""INT-03D — Enterprise systems connectors (read-only foundation)."""
from __future__ import annotations

from datetime import datetime, timezone
from unittest.mock import patch

import pytest
from fastapi import HTTPException

from models.enterprise_system_connector_models import (
    EnterpriseSystemModuleInput,
    EnterpriseSystemRegistrationRequest,
    EnterpriseSystemValidationCreateRequest,
)
from models.local_agent_models import LocalAgentFoundationRegistrationRequest, LocalAgentMockHeartbeatRequest
from services.enterprise_system_connector_service import (
    build_connector_id,
    build_enterprise_system_report,
    build_module_id,
    build_validation_request_id,
    create_enterprise_validation,
    list_enterprise_systems,
    register_enterprise_system,
)
from services.local_agent_service import mock_foundation_heartbeat, register_foundation_agent


class _Req:
    headers: dict = {}


def _register_agent(project_id: str, name: str):
    reg = register_foundation_agent(
        LocalAgentFoundationRegistrationRequest(
            project_id=project_id,
            name=name,
            environment="qa",
            version="1.0.0",
            capabilities=["contract_validation", "database_validation"],
        ),
        _Req(),
    )
    mock_foundation_heartbeat(
        LocalAgentMockHeartbeatRequest(
            agent_id=reg.agent_id,
            version="1.0.0",
            capabilities=["contract_validation", "database_validation"],
            timestamp=datetime.now(timezone.utc).isoformat(),
        ),
        _Req(),
    )
    return reg


def _register_sap(agent_id: str):
    return register_enterprise_system(
        EnterpriseSystemRegistrationRequest(
            agent_id=agent_id,
            system_name="SAP ECC",
            system_type="SAP_ECC",
            environment="QA",
            modules=[
                EnterpriseSystemModuleInput(module_name="MM", category="logistics", criticality="HIGH"),
                EnterpriseSystemModuleInput(module_name="SD", category="sales", criticality="HIGH"),
                EnterpriseSystemModuleInput(module_name="FI", category="finance", criticality="CRITICAL"),
            ],
        ),
        _Req(),
    )


def _register_oracle(agent_id: str):
    return register_enterprise_system(
        EnterpriseSystemRegistrationRequest(
            agent_id=agent_id,
            system_name="Oracle EBS",
            system_type="ORACLE_EBS",
            environment="STAGING",
            modules=[
                EnterpriseSystemModuleInput(module_name="Procurement", category="procurement"),
                EnterpriseSystemModuleInput(module_name="Finance", category="finance"),
                EnterpriseSystemModuleInput(module_name="Inventory", category="inventory"),
            ],
        ),
        _Req(),
    )


def test_empty_report():
    report = build_enterprise_system_report(project_id="empty-erp-proj")
    assert report.connectors == []
    assert report.modules == []
    assert report.validations == []
    assert "No enterprise systems registered" in report.summary


def test_sap_registration():
    reg = _register_agent("sap-proj", "Sap-Agent")
    connector = _register_sap(reg.agent_id)
    assert connector.connector_id == build_connector_id(reg.agent_id, "SAP ECC")
    assert connector.system_type == "SAP_ECC"
    assert connector.environment == "QA"
    assert connector.read_only is True


def test_oracle_registration():
    reg = _register_agent("oracle-proj", "Oracle-Agent")
    connector = _register_oracle(reg.agent_id)
    assert connector.system_type == "ORACLE_EBS"
    report = build_enterprise_system_report(project_id="oracle-proj")
    assert len(report.connectors) == 1
    assert {m.module_name for m in report.modules} == {"Procurement", "Finance", "Inventory"}


def test_module_registration():
    reg = _register_agent("module-proj", "Module-Agent")
    connector = _register_sap(reg.agent_id)
    report = build_enterprise_system_report(project_id="module-proj")
    assert len(report.modules) == 3
    mm = next(m for m in report.modules if m.module_name == "MM")
    assert mm.connector_id == connector.connector_id
    assert mm.module_id == build_module_id(connector.connector_id, "MM")


def test_validation_creation():
    reg = _register_agent("valid-proj", "Valid-Agent")
    connector = _register_sap(reg.agent_id)
    report = build_enterprise_system_report(project_id="valid-proj")
    module = next(m for m in report.modules if m.module_name == "SD")
    validation = create_enterprise_validation(
        EnterpriseSystemValidationCreateRequest(
            connector_id=connector.connector_id,
            module_id=module.module_id,
            validation_type="interface_validation",
        ),
        _Req(),
    )
    assert validation.status == "PENDING"
    assert validation.validation_type == "interface_validation"


def test_approval_requirement():
    reg = _register_agent("approval-proj", "Approval-Agent")
    connector = _register_oracle(reg.agent_id)
    module = build_enterprise_system_report(project_id="approval-proj").modules[0]
    validation = create_enterprise_validation(
        EnterpriseSystemValidationCreateRequest(
            connector_id=connector.connector_id,
            module_id=module.module_id,
            validation_type="data_consistency_validation",
        ),
        _Req(),
    )
    assert validation.requires_user_approval is True
    assert validation.status == "PENDING"


def test_deterministic_ids():
    reg = _register_agent("det-proj", "Det-Agent")
    connector = _register_sap(reg.agent_id)
    module_id = build_module_id(connector.connector_id, "FI")
    request_id = build_validation_request_id(connector.connector_id, module_id, "integration_validation")
    assert connector.connector_id.startswith("erpconn:")
    assert module_id.startswith("erpmodule:")
    assert request_id.startswith("erpvalid:")
    assert build_validation_request_id(connector.connector_id, module_id, "integration_validation") == request_id


def test_no_discovery_without_registration():
    report = build_enterprise_system_report(project_id="no-discovery-project")
    assert report.connectors == []


def test_no_erp_execution():
    reg = _register_agent("no-exec-proj", "No-Exec-Agent")
    connector = _register_sap(reg.agent_id)
    module = build_enterprise_system_report(project_id="no-exec-proj").modules[0]
    validation = create_enterprise_validation(
        EnterpriseSystemValidationCreateRequest(
            connector_id=connector.connector_id,
            module_id=module.module_id,
            validation_type="contract_validation",
        ),
        _Req(),
    )
    assert validation.status == "PENDING"
    assert validation.status != "COMPLETED"


def test_no_external_calls():
    reg = _register_agent("ext-proj", "Ext-Agent")
    _register_sap(reg.agent_id)
    with patch("urllib.request.urlopen", side_effect=AssertionError("external HTTP")):
        with patch("requests.request", side_effect=AssertionError("external HTTP")):
            systems = list_enterprise_systems(agent_id=reg.agent_id, limit=50, request=_Req())
    assert len(systems) == 1


def test_registration_requires_modules():
    reg = _register_agent("no-modules", "No-Mod-Agent")
    with pytest.raises(HTTPException) as exc:
        register_enterprise_system(
            EnterpriseSystemRegistrationRequest(
                agent_id=reg.agent_id,
                system_name="Empty ERP",
                system_type="LEGACY",
                environment="QA",
                modules=[],
            ),
            _Req(),
        )
    assert exc.value.status_code == 400
