# tests/test_change_impact_ecommerce_matching.py
"""E-commerce / auth path → catalog module matching (PR Intelligence)."""
from __future__ import annotations

from unittest.mock import patch

import pytest

from models.pr_analysis_models import ProjectPRAnalysisRequest
from models.project_knowledge_models import (
    KnowledgeModule,
    KnowledgeRelatedTest,
    ProjectKnowledge,
)
from models.risk_engine_models import ModuleRisk, RecommendedTest
from services.change_impact_service import map_changed_files, map_file_to_modules, resolve_impacted_modules
from services.pr_analysis_service import pr_analysis_service


def _demo_catalog_modules() -> list[str]:
    return [
        "auth",
        "cart",
        "checkout",
        "navigation",
        "orders",
        "product-listing",
        "smoke",
    ]


def _demo_knowledge() -> ProjectKnowledge:
    return ProjectKnowledge(
        project_id="demo-enterprise-qa",
        project_name="Demo Enterprise QA",
        modules=[
            KnowledgeModule(name="auth", test_count=3),
            KnowledgeModule(name="cart", test_count=2),
            KnowledgeModule(name="checkout", test_count=3),
            KnowledgeModule(name="product-listing", test_count=1),
            KnowledgeModule(name="navigation", test_count=1),
            KnowledgeModule(name="orders", test_count=1),
            KnowledgeModule(name="smoke", test_count=1),
        ],
        related_tests=[
            KnowledgeRelatedTest(
                test_case_id="demo_enterprise_qa__auth__login_valid_credentials_submission",
                name="Login - Valid Credentials Submission",
                module="auth",
            ),
            KnowledgeRelatedTest(
                test_case_id="demo_enterprise_qa__auth__login_error_missing_username_validation",
                name="Login - Error: Missing Username Validation",
                module="auth",
            ),
            KnowledgeRelatedTest(
                test_case_id="demo_enterprise_qa__auth__login_error_missing_password_validation",
                name="Login - Error: Missing Password Validation",
                module="auth",
            ),
        ],
        module_risks=[
            ModuleRisk(module="auth", module_risk_score=40.0, module_risk_level="MEDIUM"),
        ],
        recommended_tests=[
            RecommendedTest(
                test_case_id="demo_enterprise_qa__auth__login_valid_credentials_submission",
                name="Login - Valid Credentials Submission",
                module="auth",
                reason="auth regression coverage",
            ),
            RecommendedTest(
                test_case_id="demo_enterprise_qa__auth__login_error_missing_username_validation",
                name="Login - Error: Missing Username Validation",
                module="auth",
                reason="validation coverage",
            ),
        ],
    )


@pytest.mark.parametrize(
    "file_path,expected_module",
    [
        ("pages/LoginPage.ts", "auth"),
        ("components/LoginForm.tsx", "auth"),
        ("src/auth/session.ts", "auth"),
        ("pages/CheckoutPage.ts", "checkout"),
        ("pages/CartPage.ts", "cart"),
        ("pages/InventoryPage.ts", "product-listing"),
    ],
)
def test_ecommerce_paths_map_to_catalog_modules(file_path, expected_module):
    knowledge = _demo_knowledge()
    matches = map_file_to_modules(
        file_path,
        known_modules=_demo_catalog_modules(),
        knowledge=knowledge,
    )
    assert matches, f"expected match for {file_path!r}"
    assert matches[0][0] == expected_module
    assert matches[0][1] >= 0.65


def test_readme_does_not_map_to_product_module():
    knowledge = _demo_knowledge()
    matches = map_file_to_modules(
        "README.md",
        known_modules=_demo_catalog_modules(),
        knowledge=knowledge,
    )
    assert matches == []


def test_test_only_spec_maps_but_policy_suppresses_recommendations():
    knowledge = _demo_knowledge()
    mappings = map_changed_files(
        ["tests/ecommerce.spec.ts"],
        knowledge=knowledge,
        catalog_modules=_demo_catalog_modules(),
    )
    # Path may or may not map depending on tokens; policy handles test_only regardless.
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = [
                    (f"tc_{m}", m) for m in _demo_catalog_modules()
                ]
                report = pr_analysis_service.analyze_for_project(
                    "demo-enterprise-qa",
                    ProjectPRAnalysisRequest(changed_files=["tests/ecommerce.spec.ts"]),
                )
    assert report.memory_available is True
    assert report.recommended_tests == []


def test_login_page_pr_recommends_auth_tests():
    knowledge = _demo_knowledge()
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Demo Enterprise QA"):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = [
                    (f"tc_{m}", m) for m in _demo_catalog_modules()
                ]
                report = pr_analysis_service.analyze_for_project(
                    "demo-enterprise-qa",
                    ProjectPRAnalysisRequest(changed_files=["pages/LoginPage.ts"]),
                )

    assert report.memory_available is True
    assert any(m.module == "auth" for m in report.impacted_modules)
    rec_ids = {t.test_case_id for t in report.recommended_tests}
    assert "demo_enterprise_qa__auth__login_valid_credentials_submission" in rec_ids
    assert "demo_enterprise_qa__auth__login_error_missing_username_validation" in rec_ids


def test_readme_functional_change_low_recommendations():
    knowledge = _demo_knowledge()
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Demo"):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = [
                    (f"tc_{m}", m) for m in _demo_catalog_modules()
                ]
                report = pr_analysis_service.analyze_for_project(
                    "demo-enterprise-qa",
                    ProjectPRAnalysisRequest(changed_files=["README.md"]),
                )
    assert report.memory_available is True
    assert report.impacted_modules == []
    assert report.recommended_tests == []
