# tests/test_pr_risk_composer.py
"""PR Risk Composer + Test Recommendation Policy — acceptance scenarios."""
from __future__ import annotations

from unittest.mock import patch

from models.pr_analysis_models import ProjectPRAnalysisRequest
from models.project_knowledge_models import KnowledgeModule, ProjectKnowledge
from models.risk_engine_models import ModuleRisk, RecommendedTest
from services.pr_analysis_service import pr_analysis_service
from services.pr_risk_composer_service import dominant_change_class, risk_signals_total
from services.change_classification_service import classify_diff


def _talent_knowledge(**overrides) -> ProjectKnowledge:
    base = dict(
        project_id="zuperio-talent-os",
        project_name="Zuperio Talent OS",
        modules=[
            KnowledgeModule(name="Candidatos", test_count=5),
            KnowledgeModule(name="Auth", test_count=4),
        ],
        risk_score=82.0,
        risk_level="HIGH",
        module_risks=[
            ModuleRisk(module="Candidatos", module_risk_score=40.0, module_risk_level="MEDIUM"),
            ModuleRisk(module="Auth", module_risk_score=88.0, module_risk_level="HIGH", regression_count=2),
        ],
        recommended_tests=[
            RecommendedTest(test_case_id="TC-SMOKE-C", name="Smoke candidatos", module="Candidatos", reason="smoke"),
            RecommendedTest(test_case_id="TC-REG-C", name="Regression candidatos", module="Candidatos", reason="regression"),
            RecommendedTest(test_case_id="TC-CRIT-A1", name="Auth login critical", module="Auth", reason="critical auth"),
            RecommendedTest(test_case_id="TC-CRIT-A2", name="Auth session", module="Auth", reason="critical session"),
            RecommendedTest(test_case_id="TC-CRIT-A3", name="Auth permissions", module="Auth", reason="critical permissions"),
            RecommendedTest(test_case_id="TC-CRIT-A4", name="Auth logout", module="Auth", reason="critical regression"),
            RecommendedTest(test_case_id="TC-CRIT-A5", name="Auth MFA", module="Auth", reason="critical security"),
        ],
    )
    base.update(overrides)
    return ProjectKnowledge(**base)


def _analyze(changed_files, file_patches=None, knowledge=None):
    knowledge = knowledge or _talent_knowledge()
    catalog = [
        ("TC-SMOKE-C", "Candidatos"),
        ("TC-REG-C", "Candidatos"),
        ("TC-CRIT-A1", "Auth"),
        ("TC-CRIT-A2", "Auth"),
        ("TC-CRIT-A3", "Auth"),
        ("TC-CRIT-A4", "Auth"),
        ("TC-CRIT-A5", "Auth"),
    ]
    req = ProjectPRAnalysisRequest(
        changed_files=changed_files,
        file_patches=file_patches or {},
    )
    with patch("services.project_knowledge_service.get_project_knowledge", return_value=knowledge):
        with patch("services.project_knowledge_service._resolve_project_name", return_value="Zuperio Talent OS"):
            with patch("services.db.catalog_repository.catalog_repo") as cat:
                cat.all_modules_for_project.return_value = catalog
                return pr_analysis_service.analyze_for_project("zuperio-talent-os", req)


PATCH_COMMENT = """@@ -1,3 +1,4 @@
 export async function listCandidates() {
+  // solo comentario de validación
   return [];
 }
"""

PATCH_IMPORT = """@@ -1,3 +1,4 @@
 import { a } from 'alpha';
+import { mapRow } from './row-mapper';
 export const x = 1;
"""

PATCH_SCHEMA_COMMENT = """@@ -1,3 +1,4 @@
 model User {
+  /// CCE validation note
   id String @id
 }
"""

PATCH_AUTH_COMMENT = """@@ -1,3 +1,4 @@
 export function checkAccess() {
+  // TODO: document access policy
   return true;
 }
"""

PATCH_FUNCTIONAL = """@@ -1,3 +1,4 @@
 export async function listCandidates() {
+  return fetch('/api/candidates/v2');
   return [];
 }
"""


def _assert_risk_signals_sum(report):
    assert report.risk_signals
    assert risk_signals_total(report.risk_signals) == report.pr_risk_score


def test_candidates_comment_only_low_pr_risk_and_few_tests():
    report = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT},
    )
    assert report.file_classifications[0].primary_class == "comments"
    assert any(m.module == "Candidatos" for m in report.impacted_modules)
    assert report.project_risk_score == 82.0
    assert report.pr_risk_score <= 8.0
    assert report.pr_risk_score < report.project_risk_score
    assert len(report.recommended_tests) <= 1
    assert len(report.recommended_tests_raw) >= 2
    assert report.engine_version == "pr-v1.3"
    _assert_risk_signals_sum(report)


def test_candidates_imports_only_moderate_pr_risk():
    report = _analyze(
        ["lib/candidates/mappers.ts"],
        {"lib/candidates/mappers.ts": PATCH_IMPORT},
    )
    assert report.file_classifications[0].primary_class == "imports"
    assert 5.0 <= report.pr_risk_score <= 20.0
    assert len(report.recommended_tests) <= 2


def test_schema_comment_elevated_pr_risk():
    report = _analyze(
        ["prisma/schema.prisma"],
        {"prisma/schema.prisma": PATCH_SCHEMA_COMMENT},
    )
    assert report.file_classifications[0].primary_class == "schema"
    assert report.pr_risk_score >= 30.0
    assert len(report.recommended_tests) >= 1
    assert len(report.recommended_tests_raw) >= 1


def test_auth_comment_only_intermediate_pr_risk_and_reduced_tests():
    report = _analyze(
        ["lib/auth/access.ts"],
        {"lib/auth/access.ts": PATCH_AUTH_COMMENT},
    )
    assert report.file_classifications[0].primary_class == "comments"
    assert any(m.module == "Auth" for m in report.impacted_modules)
    candidates_report = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT},
    )
    assert report.pr_risk_score > candidates_report.pr_risk_score
    assert report.pr_risk_score <= 30.0
    assert len(report.recommended_tests) <= 2
    assert len(report.recommended_tests_raw) >= 5


def test_functional_change_higher_than_comments():
    comment_report = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT},
    )
    functional_report = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_FUNCTIONAL},
    )
    assert functional_report.pr_risk_score > comment_report.pr_risk_score
    assert len(functional_report.recommended_tests) >= len(comment_report.recommended_tests)


def test_high_project_risk_does_not_dominate_comment_only_normal_module():
    knowledge = _talent_knowledge(risk_score=95.0, risk_level="CRITICAL")
    report = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT},
        knowledge=knowledge,
    )
    assert report.project_risk_score == 95.0
    assert report.project_risk_level == "CRITICAL"
    assert report.pr_risk_score <= 8.0
    assert report.pr_risk_level in ("LOW", "MEDIUM")


def test_compose_pr_risk_unit_dominant_class():
    c1 = classify_diff("lib/candidates/queries.ts", PATCH_COMMENT)
    c2 = classify_diff("prisma/schema.prisma", None)
    assert dominant_change_class([c1, c2]) == "schema"


def test_schema_pr_risk_above_candidates_comments():
    comments = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT},
    )
    schema = _analyze(["prisma/schema.prisma"], {"prisma/schema.prisma": PATCH_SCHEMA_COMMENT})
    assert schema.pr_risk_score > comments.pr_risk_score


def test_candidates_comment_generates_expected_risk_signals():
    report = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT},
    )
    _assert_risk_signals_sum(report)
    titles = [s.title for s in report.risk_signals]
    assert any("Comment-only" in t for t in titles)
    assert any("Candidatos" in t or "module" in t.lower() for t in titles)
    assert any(s.category == "history" for s in report.risk_signals)
    assert report.pr_risk_score <= 8.0


def test_auth_comment_generates_critical_module_signal():
    report = _analyze(
        ["lib/auth/access.ts"],
        {"lib/auth/access.ts": PATCH_AUTH_COMMENT},
    )
    _assert_risk_signals_sum(report)
    assert any("Critical module" in s.title for s in report.risk_signals)
    assert any("Comment-only" in s.title for s in report.risk_signals)
    assert 15.0 <= report.pr_risk_score <= 30.0


def test_schema_generates_schema_change_signal():
    report = _analyze(
        ["prisma/schema.prisma"],
        {"prisma/schema.prisma": PATCH_SCHEMA_COMMENT},
    )
    _assert_risk_signals_sum(report)
    assert any("Schema-related" in s.title for s in report.risk_signals)
    assert report.pr_risk_score >= 30.0


def test_project_risk_generates_history_signal():
    report = _analyze(
        ["lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT},
    )
    history = [s for s in report.risk_signals if s.category == "history"]
    assert history
    assert history[0].impact > 0
    assert history[0].impact <= 15.0
