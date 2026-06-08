# tests/test_change_classification_service.py
"""Change Classification Engine — deterministic diff/path tests."""
from __future__ import annotations

from services.change_classification_service import classify_changed_files, classify_diff

PATCH_COMMENT_ONLY = """@@ -10,6 +10,7 @@ export async function listCandidates() {
   const db = getDb();
+  // comentario sobre candidatos
   return db.query('SELECT * FROM candidates');
 }
"""

PATCH_IMPORTS_ONLY = """@@ -1,3 +1,4 @@
 import { a } from 'alpha';
+import { c } from 'charlie';
 export const x = 1;
"""

PATCH_FORMATTING = """@@ -1,3 +1,3 @@
-const x=1;
+const x = 1;
"""


def test_queries_ts_comment_only():
    result = classify_diff("lib/candidates/queries.ts", PATCH_COMMENT_ONLY)
    assert result.primary_class == "comments"
    assert result.confidence >= 0.85
    assert any("comment" in s.lower() for s in result.signals)


def test_readme_md_docs():
    result = classify_diff("README.md", None)
    assert result.primary_class == "docs"
    assert result.confidence >= 0.9


def test_schema_prisma_schema():
    result = classify_diff("prisma/schema.prisma", None)
    assert result.primary_class == "schema"
    assert result.confidence >= 0.9


def test_candidates_spec_test_only():
    result = classify_diff("tests/candidates.spec.ts", None)
    assert result.primary_class == "test_only"
    assert result.confidence >= 0.9


def test_package_json_config():
    result = classify_diff("package.json", None)
    assert result.primary_class == "config"
    assert result.confidence >= 0.9


def test_imports_only_diff():
    result = classify_diff("src/lib/utils.ts", PATCH_IMPORTS_ONLY)
    assert result.primary_class == "imports"
    assert result.confidence >= 0.85
    assert any("import" in s.lower() for s in result.signals)


def test_formatting_whitespace_diff():
    result = classify_diff("src/lib/utils.ts", PATCH_FORMATTING)
    assert result.primary_class == "formatting"
    assert result.confidence >= 0.85


def test_classify_changed_files_batch():
    results = classify_changed_files(
        ["README.md", "lib/candidates/queries.ts"],
        {"lib/candidates/queries.ts": PATCH_COMMENT_ONLY},
    )
    assert len(results) == 2
    by_path = {r.file_path: r for r in results}
    assert by_path["README.md"].primary_class == "docs"
    assert by_path["lib/candidates/queries.ts"].primary_class == "comments"
