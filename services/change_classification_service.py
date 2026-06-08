# services/change_classification_service.py
"""
Change Classification Engine (CCE) — deterministic diff/path classification.

No LLM, no AST, no embeddings. Uses unified diff patches from GitHub (or callers)
plus path heuristics when patch is unavailable.
"""
from __future__ import annotations

import re
from typing import Dict, List, Literal, Optional, Tuple

from models.pr_analysis_models import FileChangeClassification

ChangeClass = Literal[
    "comments",
    "docs",
    "formatting",
    "imports",
    "test_only",
    "config",
    "schema",
]

# ── Path heuristics (checked before diff content) ─────────────────────────────

_DOCS_PATH_RE = re.compile(
    r"(?:^|/)(?:docs?/|documentation/)|(?:^|/)readme(?:\..*)?$|\.md$|\.rst$|\.adoc$",
    re.IGNORECASE,
)

_TEST_PATH_RE = re.compile(
    r"(?:^|/)(?:tests?|__tests__|spec)/|(?:^|/)[^/]*\.(?:spec|test)\.[a-z0-9]+$",
    re.IGNORECASE,
)

_CONFIG_PATH_RE = re.compile(
    r"(?:^|/)?\.env(?:\.|$)|(?:^|/)(?:config|settings)(?:/|\.|$)"
    r"|(?:^|/)(?:vite|webpack|eslint|tsconfig|jest|babel|prettier)\."
    r"|(?:^|/)package\.json$|(?:^|/)package-lock\.json$|(?:^|/)pnpm-lock\.yaml$",
    re.IGNORECASE,
)

_SCHEMA_PATH_RE = re.compile(
    r"migration|(?:^|/)schema\.(?:prisma|sql)$|\.sql$|(?:^|/)alembic/",
    re.IGNORECASE,
)

# ── Diff line patterns ───────────────────────────────────────────────────────

_IMPORT_RE = re.compile(
    r"^\s*(?:import|export)\s+.*(?:from\s+['\"]|require\s*\(|import\s*\()",
    re.IGNORECASE,
)

_PY_IMPORT_RE = re.compile(r"^\s*(?:import|from)\s+\S+", re.IGNORECASE)

_GO_IMPORT_RE = re.compile(r'^\s*import\s+(?:\(|")', re.IGNORECASE)


def _normalize_path(file_path: str) -> str:
    return (file_path or "").replace("\\", "/").strip()


def _path_classification(file_path: str) -> Optional[Tuple[ChangeClass, float, List[str]]]:
    p = _normalize_path(file_path)
    if not p:
        return None
    if _DOCS_PATH_RE.search(p):
        return "docs", 0.95, ["path matches documentation file pattern"]
    if _TEST_PATH_RE.search(p):
        return "test_only", 0.95, ["path matches test file pattern"]
    if _SCHEMA_PATH_RE.search(p):
        return "schema", 0.92, ["path matches schema/migration pattern"]
    if _CONFIG_PATH_RE.search(p):
        return "config", 0.9, ["path matches configuration file pattern"]
    return None


def _parse_diff_hunks(patch: str) -> Tuple[List[str], List[str]]:
    """Return (added_lines, removed_lines) content without +/- prefix."""
    adds: List[str] = []
    dels: List[str] = []
    for raw in (patch or "").splitlines():
        if not raw:
            continue
        if raw.startswith("+++") or raw.startswith("---") or raw.startswith("@@"):
            continue
        if raw.startswith("+"):
            adds.append(raw[1:])
        elif raw.startswith("-"):
            dels.append(raw[1:])
    return adds, dels


def _ext(file_path: str) -> str:
    p = _normalize_path(file_path).lower()
    if "." not in p:
        return ""
    return p.rsplit(".", 1)[-1]


def _is_comment_line(line: str, file_path: str) -> bool:
    s = line.strip()
    if not s:
        return True
    ext = _ext(file_path)
    # Block comment inner lines
    if s.startswith("*") and not s.startswith("*/"):
        return True
    if s.startswith("/*") or s.endswith("*/") or s == "*/":
        return True
    # Line comments by language family
    if ext in ("py", "sh", "bash", "yaml", "yml", "toml", "ini", "cfg", "sql", "rb", "pl"):
        return s.startswith("#")
    if ext in ("js", "ts", "tsx", "jsx", "java", "kt", "cs", "cpp", "c", "h", "go", "rs", "swift"):
        return s.startswith("//")
    if ext in ("html", "xml", "svg"):
        return s.startswith("<!--") or s.endswith("-->") or "<!--" in s
    # Fallback: common comment starters
    return s.startswith("//") or s.startswith("#") or s.startswith("--")


def _is_import_line(line: str, file_path: str) -> bool:
    s = line.strip()
    if not s:
        return True
    ext = _ext(file_path)
    if ext in ("py",):
        return bool(_PY_IMPORT_RE.match(s))
    if ext in ("go",):
        return bool(_GO_IMPORT_RE.match(s))
    return bool(_IMPORT_RE.match(s))


def _code_skeleton(line: str) -> str:
    """Strip all whitespace for formatting-only comparison."""
    return re.sub(r"\s+", "", line or "")


def _is_formatting_only_pair(added: str, removed: str) -> bool:
    """True when the only delta is whitespace or spacing around tokens."""
    if added == removed:
        return False
    return _code_skeleton(added) == _code_skeleton(removed)


def _classify_diff_content(
    file_path: str,
    patch: str,
) -> Optional[Tuple[ChangeClass, float, List[str]]]:
    adds, dels = _parse_diff_hunks(patch)
    all_lines = adds + dels
    non_empty = [ln for ln in all_lines if ln.strip()]

    if not non_empty:
        return "formatting", 0.85, ["diff contains only blank-line or whitespace changes"]

    # Comment-only changes
    if all(_is_comment_line(ln, file_path) for ln in non_empty):
        return "comments", 0.9, ["all changed lines are comments"]

    # Import-only changes
    if all(_is_import_line(ln, file_path) for ln in non_empty):
        return "imports", 0.88, ["all changed lines are import/export statements"]

    # Whitespace-only: every line has a counterpart with same stripped content
    if adds and dels and len(adds) == len(dels):
        if all(_is_formatting_only_pair(a, d) for a, d in zip(adds, dels)):
            return "formatting", 0.9, ["all changed lines differ only in whitespace"]

    # Single-sided whitespace/formatting (e.g. trailing space fix on one line)
    if len(non_empty) == 1:
        lone = non_empty[0]
        if adds and not dels:
            # added blank line
            if not lone.strip():
                return "formatting", 0.85, ["blank line added"]
        if dels and not adds:
            if not lone.strip():
                return "formatting", 0.85, ["blank line removed"]

    # Pairs where each +/- line is formatting-only relative to paired lines
    if adds and dels and len(adds) == len(dels):
        if all(_is_formatting_only_pair(a, d) for a, d in zip(adds, dels)):
            return "formatting", 0.9, ["all changed lines differ only in whitespace"]

    if len(adds) == 1 and len(dels) == 1:
        if _is_formatting_only_pair(adds[0], dels[0]):
            return "formatting", 0.9, ["single-line whitespace-only change"]

    return None


def classify_diff(file_path: str, patch: Optional[str] = None) -> FileChangeClassification:
    """
    Classify a single changed file using path heuristics and optional unified diff patch.

    Diff content (comments, formatting, imports) takes precedence over path when patch
    is present and yields a confident content signal. Strong path signals (docs, tests,
    schema, config) apply when no patch is available or content is inconclusive.
    """
    fp = _normalize_path(file_path)
    path_hit = _path_classification(fp)

    if patch and patch.strip():
        content_hit = _classify_diff_content(fp, patch)
        if content_hit:
            cls, conf, signals = content_hit
            # Strong path types win when patch is ambiguous (e.g. test file with comment)
            if path_hit and path_hit[0] in ("docs", "test_only", "schema", "config"):
                p_cls, p_conf, p_signals = path_hit
                if p_cls != cls:
                    return FileChangeClassification(
                        file_path=fp,
                        primary_class=p_cls,
                        confidence=p_conf,
                        signals=p_signals + [f"patch suggested {cls}; path rule applied"],
                    )
            return FileChangeClassification(
                file_path=fp,
                primary_class=cls,
                confidence=conf,
                signals=signals,
            )

    if path_hit:
        cls, conf, signals = path_hit
        return FileChangeClassification(
            file_path=fp,
            primary_class=cls,
            confidence=conf,
            signals=signals + (["no patch available"] if not patch else ["patch inconclusive"]),
        )

    return FileChangeClassification(
        file_path=fp,
        primary_class="formatting",
        confidence=0.35,
        signals=["no deterministic class matched; functional change assumed pending composer"],
    )


def classify_changed_files(
    changed_files: List[str],
    file_patches: Optional[Dict[str, str]] = None,
) -> List[FileChangeClassification]:
    """Classify each changed file path; ``file_patches`` maps path → unified diff patch."""
    patches = file_patches or {}
    out: List[FileChangeClassification] = []
    for raw in changed_files or []:
        fp = _normalize_path(raw)
        if not fp:
            continue
        out.append(classify_diff(fp, patches.get(fp) or patches.get(raw)))
    return out
