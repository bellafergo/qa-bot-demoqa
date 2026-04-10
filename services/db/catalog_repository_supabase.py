# services/db/catalog_repository_supabase.py
"""
Persist catalog test_cases in Supabase Postgres (PostgREST).

Used when SUPABASE_STRICT=1 in production (Render) so the catalog survives redeploys.
Requires SUPABASE_URL + SUPABASE_SERVICE_ROLE_KEY and public.test_cases (see supabase/migration.sql).
"""
from __future__ import annotations

import json
import logging
import time
from collections import Counter
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional, TypeVar

from services.db.catalog_repository_sqlite import test_case_to_storage_dict
from services.supabase_store import supabase_client

logger = logging.getLogger("vanya.db.catalog.supabase")

_TABLE = "test_cases"
_MAX_ATTEMPTS = 3
_T_BASE_SLEEP = 0.15

T = TypeVar("T")


def _utc_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _is_transient_error(exc: BaseException) -> bool:
    msg = (str(exc) or "").lower()
    blob = f"{type(exc).__name__} {msg}"
    return any(
        x in blob
        for x in (
            "timeout",
            "timed out",
            "connection reset",
            "connection refused",
            "econnrefused",
            "temporarily unavailable",
            "502",
            "503",
            "504",
            "bad gateway",
            "service unavailable",
            "broken pipe",
            "network",
        )
    )


def _with_retry(op: Callable[[], T], *, what: str) -> T:
    last: Optional[BaseException] = None
    for attempt in range(1, _MAX_ATTEMPTS + 1):
        try:
            return op()
        except Exception as e:
            last = e
            if attempt >= _MAX_ATTEMPTS or not _is_transient_error(e):
                logger.exception(
                    "catalog_repo.supabase: %s failed (attempt %s/%s)",
                    what,
                    attempt,
                    _MAX_ATTEMPTS,
                )
                raise
            sleep_s = _T_BASE_SLEEP * (2 ** (attempt - 1))
            logger.warning(
                "catalog_repo.supabase: transient error on %s (%s) — retry in %.2fs",
                what,
                type(e).__name__,
                sleep_s,
            )
            time.sleep(sleep_s)
    assert last is not None
    raise last


def _parse_ts(raw: Any) -> datetime:
    if raw is None:
        return datetime.now(timezone.utc)
    if isinstance(raw, datetime):
        return raw
    s = str(raw).strip()
    if not s:
        return datetime.now(timezone.utc)
    try:
        return datetime.fromisoformat(s.replace("Z", "+00:00"))
    except Exception:
        return datetime.now(timezone.utc)


def _dict_to_test_case(d: Dict[str, Any]):
    from models.test_case import TestCase, TestStep, TestAssertion

    steps_data      = json.loads(d.get("steps_json") or "[]")
    assertions_data = json.loads(d.get("assertions_json") or "[]")
    tags            = json.loads(d.get("tags_json") or "[]")

    return TestCase(
        id           = str(d.get("id") or ""),
        test_case_id = str(d.get("test_case_id") or ""),
        name         = str(d.get("name") or ""),
        module       = str(d.get("module") or ""),
        type         = d.get("type"),
        priority     = d.get("priority"),
        status       = d.get("status") or "active",
        test_type    = (d.get("test_type") or "ui") or "ui",
        project_id   = str(d.get("project_id") or "default") or "default",
        version      = int(d.get("version") or 1),
        tags         = tags,
        base_url     = d.get("base_url"),
        steps        = [TestStep(**s) for s in steps_data],
        assertions   = [TestAssertion(**a) for a in assertions_data],
        created_at   = _parse_ts(d.get("created_at")),
        updated_at   = _parse_ts(d.get("updated_at")),
        created_from = d.get("created_from"),
        source_run_id = d.get("source_run_id"),
    )


class CatalogRepositorySupabase:

    def _sb(self):
        sb = supabase_client()
        if sb is None:
            raise RuntimeError(
                "Supabase client unavailable — set SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY "
                "(SUPABASE_STRICT=1 requires a working catalog backend)."
            )
        return sb

    def is_empty(self) -> bool:
        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("id").limit(1).execute()
            rows = res.data if isinstance(res.data, list) else []
            return len(rows) == 0

        return bool(_with_retry(_op, what="is_empty"))

    def get_test_case(self, test_case_id: str):
        tid = (test_case_id or "").strip()
        if not tid:
            return None

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("*").eq("test_case_id", tid).limit(1).execute()
            rows = res.data if isinstance(res.data, list) else []
            if not rows:
                return None
            return _dict_to_test_case(rows[0]) if isinstance(rows[0], dict) else None

        return _with_retry(_op, what=f"get_test_case({tid})")

    def get_test_case_by_source_run_id(self, source_run_id: str):
        sid = (source_run_id or "").strip()
        if not sid:
            return None

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("*").eq("source_run_id", sid).limit(1).execute()
            rows = res.data if isinstance(res.data, list) else []
            if not rows:
                return None
            return _dict_to_test_case(rows[0]) if isinstance(rows[0], dict) else None

        return _with_retry(_op, what="get_test_case_by_source_run_id")

    def list_test_cases(
        self,
        *,
        module: Optional[str] = None,
        type_: Optional[str] = None,
        priority: Optional[str] = None,
        status: Optional[str] = "active",
        test_type: Optional[str] = None,
        search: Optional[str] = None,
        tags: Optional[List[str]] = None,
        project_id: Optional[str] = None,
        limit: int = 200,
    ):
        def _op():
            sb = self._sb()
            q = sb.table(_TABLE).select("*")
            if project_id is not None and str(project_id).strip():
                q = q.eq("project_id", str(project_id).strip())
            if module:
                q = q.ilike("module", f"%{module}%")
            if type_:
                q = q.eq("type", type_)
            if priority:
                q = q.eq("priority", priority)
            if status:
                q = q.eq("status", status)
            if test_type:
                q = q.eq("test_type", test_type)
            q = q.order("created_at", desc=True)
            res = q.limit(5000).execute()
            rows = res.data if isinstance(res.data, list) else []
            models = [_dict_to_test_case(r) for r in rows if isinstance(r, dict)]

            if search and search.strip():
                term = search.strip().lower()

                def _matches(m) -> bool:
                    return (
                        term in (m.test_case_id or "").lower()
                        or term in (m.name or "").lower()
                        or term in (m.module or "").lower()
                        or term in str(m.type or "").lower()
                        or term in (m.test_type or "").lower()
                    )

                models = [m for m in models if _matches(m)]

            if tags:
                tag_set = {t.lower() for t in tags}
                models = [m for m in models if tag_set.issubset({t.lower() for t in m.tags})]

            return models[:limit]

        return _with_retry(_op, what="list_test_cases")

    def create_test_case(self, tc):
        payload = test_case_to_storage_dict(tc)

        def _op():
            sb = self._sb()
            sb.table(_TABLE).insert(payload).execute()
            logger.info("catalog_repo.supabase: inserted %s", tc.test_case_id)
            return tc

        return _with_retry(_op, what=f"create_test_case({tc.test_case_id})")

    def update_test_case(self, test_case_id: str, data: dict):
        tid = (test_case_id or "").strip()
        if not tid:
            return None

        def _op():
            current = self.get_test_case(tid)
            if current is None:
                return None
            row = test_case_to_storage_dict(current)
            if "name" in data and data["name"] is not None:
                row["name"] = data["name"]
            if "module" in data and data["module"] is not None:
                row["module"] = data["module"]
            if "type" in data and data["type"] is not None:
                row["type"] = data["type"]
            if "priority" in data and data["priority"] is not None:
                row["priority"] = data["priority"]
            if "status" in data and data["status"] is not None:
                row["status"] = data["status"]
            if "test_type" in data and data["test_type"] is not None:
                row["test_type"] = data["test_type"]
            if "project_id" in data and data["project_id"] is not None:
                row["project_id"] = str(data["project_id"]).strip() or "default"
            if "base_url" in data:
                row["base_url"] = data["base_url"]
            if "version" in data and data["version"] is not None:
                row["version"] = data["version"]
            if "steps" in data and data["steps"] is not None:
                row["steps_json"] = json.dumps(data["steps"])
            if "assertions" in data and data["assertions"] is not None:
                row["assertions_json"] = json.dumps(data["assertions"])
            if "tags" in data and data["tags"] is not None:
                row["tags_json"] = json.dumps(data["tags"])
            row["updated_at"] = _utc_iso()

            sb = self._sb()
            sb.table(_TABLE).update(
                {k: v for k, v in row.items() if k != "id" and k != "test_case_id"},
            ).eq("test_case_id", tid).execute()
            got = self.get_test_case(tid)
            return got

        return _with_retry(_op, what=f"update_test_case({tid})")

    def delete_test_case(self, test_case_id: str) -> bool:
        tid = (test_case_id or "").strip()
        if not tid:
            return False

        def _op():
            if self.get_test_case(tid) is None:
                return False
            sb = self._sb()
            sb.table(_TABLE).delete().eq("test_case_id", tid).execute()
            logger.info("catalog_repo.supabase: deleted %s", tid)
            return True

        return _with_retry(_op, what=f"delete_test_case({tid})")

    def count_by_status(self) -> dict:
        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("status").execute()
            rows = res.data if isinstance(res.data, list) else []
            return dict(Counter(str(r.get("status") or "unknown") for r in rows if isinstance(r, dict)))

        return _with_retry(_op, what="count_by_status")

    def all_modules(self) -> list:
        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("test_case_id,module").execute()
            rows = res.data if isinstance(res.data, list) else []
            return [
                (str(r.get("test_case_id") or ""), str(r.get("module") or ""))
                for r in rows
                if isinstance(r, dict)
            ]

        return _with_retry(_op, what="all_modules")

    def count_tests_for_project(self, project_id: str) -> int:
        pid = (project_id or "").strip() or "default"

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("id").eq("project_id", pid).execute()
            rows = res.data if isinstance(res.data, list) else []
            return len(rows)

        return _with_retry(_op, what="count_tests_for_project")

    def count_by_test_type(self) -> dict:
        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("test_type").execute()
            rows = res.data if isinstance(res.data, list) else []
            return dict(Counter(str(r.get("test_type") or "ui") for r in rows if isinstance(r, dict)))

        return _with_retry(_op, what="count_by_test_type")

    def count_test_cases_by_module(self) -> dict:
        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("module").execute()
            rows = res.data if isinstance(res.data, list) else []
            return dict(Counter(str(r.get("module") or "") for r in rows if isinstance(r, dict)))

        return _with_retry(_op, what="count_test_cases_by_module")

    def list_test_case_ids_for_project(self, project_id: str) -> List[str]:
        pid = (project_id or "").strip()
        if not pid:
            return []

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("test_case_id").eq("project_id", pid).execute()
            rows = res.data if isinstance(res.data, list) else []
            return [str(r.get("test_case_id") or "") for r in rows if isinstance(r, dict)]

        return _with_retry(_op, what="list_test_case_ids_for_project")

    def count_by_status_for_project(self, project_id: str) -> dict:
        pid = (project_id or "").strip()
        if not pid:
            return {}

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("status").eq("project_id", pid).execute()
            rows = res.data if isinstance(res.data, list) else []
            return dict(Counter(str(r.get("status") or "unknown") for r in rows if isinstance(r, dict)))

        return _with_retry(_op, what="count_by_status_for_project")

    def count_by_test_type_for_project(self, project_id: str) -> dict:
        pid = (project_id or "").strip()
        if not pid:
            return {}

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("test_type").eq("project_id", pid).execute()
            rows = res.data if isinstance(res.data, list) else []
            return dict(Counter(str(r.get("test_type") or "ui") for r in rows if isinstance(r, dict)))

        return _with_retry(_op, what="count_by_test_type_for_project")

    def count_test_cases_by_module_for_project(self, project_id: str) -> dict:
        pid = (project_id or "").strip()
        if not pid:
            return {}

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("module").eq("project_id", pid).execute()
            rows = res.data if isinstance(res.data, list) else []
            return dict(Counter(str(r.get("module") or "") for r in rows if isinstance(r, dict)))

        return _with_retry(_op, what="count_test_cases_by_module_for_project")

    def all_modules_for_project(self, project_id: str) -> list:
        pid = (project_id or "").strip()
        if not pid:
            return []

        def _op():
            sb = self._sb()
            res = sb.table(_TABLE).select("test_case_id,module").eq("project_id", pid).execute()
            rows = res.data if isinstance(res.data, list) else []
            return [
                (str(r.get("test_case_id") or ""), str(r.get("module") or ""))
                for r in rows
                if isinstance(r, dict)
            ]

        return _with_retry(_op, what="all_modules_for_project")

    def clear_all(self) -> None:
        def _op():
            sb = self._sb()
            # Every row has non-empty test_case_id (matches SQLite DELETE FROM test_cases).
            sb.table(_TABLE).delete().neq("test_case_id", "").execute()
            logger.warning("catalog_repo.supabase: clear_all — all test_cases removed")

        _with_retry(_op, what="clear_all")
