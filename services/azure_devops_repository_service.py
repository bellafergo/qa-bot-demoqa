# services/azure_devops_repository_service.py
"""
Low-level Azure DevOps REST API client.

Never logs tokens. Azure DevOps does not expose GitHub-style unified patch strings on PR
iterations — see ``patches_limited`` in integration responses.
"""
from __future__ import annotations

import logging
from typing import Any, Dict, List, Optional, Tuple

import httpx

from core.settings import settings

logger = logging.getLogger("vanya.azure_devops_repository")

API_VERSION = "7.1"


class AzureDevOpsAPIError(Exception):
    """Safe Azure DevOps API failure — no credentials in message."""

    def __init__(self, message: str, *, status_code: int = 502, code: str = "azure_devops_error"):
        super().__init__(message)
        self.status_code = status_code
        self.code = code


def _headers(access_token: str) -> Dict[str, str]:
    return {
        "Authorization": f"Bearer {access_token}",
        "Accept": "application/json",
    }


def _api_base() -> str:
    return (settings.AZURE_DEVOPS_API_BASE or "https://dev.azure.com").strip().rstrip("/")


def _profile_base() -> str:
    return "https://app.vssps.visualstudio.com"


def _raise_for_status(res: httpx.Response, *, context: str) -> None:
    sc = res.status_code
    if sc == 401:
        raise AzureDevOpsAPIError(
            "Azure DevOps token is invalid or expired. Reconnect OAuth.",
            status_code=401,
            code="invalid_token",
        )
    if sc == 404:
        raise AzureDevOpsAPIError(
            f"Azure DevOps resource not found ({context}).",
            status_code=404,
            code="not_found",
        )
    if sc == 403:
        raise AzureDevOpsAPIError(
            "Azure DevOps API access forbidden. Check OAuth scopes.",
            status_code=403,
            code="forbidden",
        )
    if sc >= 500:
        raise AzureDevOpsAPIError(
            "Azure DevOps is temporarily unavailable. Retry later.",
            status_code=502,
            code="azure_unavailable",
        )
    if not res.is_success:
        raise AzureDevOpsAPIError(
            f"Azure DevOps API error ({sc}) for {context}.",
            status_code=502,
            code="azure_devops_error",
        )


class AzureDevOpsClient:
    """Project-bound Azure DevOps operations."""

    def __init__(
        self,
        access_token: str,
        *,
        organization: str,
        azure_project: str,
        repository_id: str,
        repository_name: str = "",
        timeout_s: int = 30,
    ):
        self._token = (access_token or "").strip()
        self.organization = (organization or "").strip()
        self.azure_project = (azure_project or "").strip()
        self.repository_id = (repository_id or "").strip()
        self.repository_name = (repository_name or "").strip()
        self.timeout_s = max(5, min(60, int(timeout_s or 30)))
        if not self._token:
            raise ValueError("access_token is required")
        if not self.organization:
            raise ValueError("organization is required")

    @property
    def full_name(self) -> str:
        if self.repository_name:
            return f"{self.organization}/{self.azure_project}/{self.repository_name}"
        return f"{self.organization}/{self.azure_project}"

    def _get(self, url: str, *, params: Optional[Dict[str, Any]] = None) -> httpx.Response:
        try:
            with httpx.Client(timeout=self.timeout_s) as client:
                return client.get(url, headers=_headers(self._token), params=params or {})
        except httpx.RequestError as exc:
            logger.warning("azure devops GET failed: %s", type(exc).__name__)
            raise AzureDevOpsAPIError(
                "Could not reach Azure DevOps API.",
                status_code=502,
                code="network_error",
            ) from exc

    def _org_url(self, path: str) -> str:
        return f"{_api_base()}/{self.organization}{path}"

    def _project_url(self, path: str) -> str:
        return f"{_api_base()}/{self.organization}/{self.azure_project}{path}"

    @staticmethod
    def list_organizations(access_token: str) -> List[Dict[str, Any]]:
        """Accounts the signed-in user can access."""
        token = (access_token or "").strip()
        if not token:
            raise ValueError("access_token is required")

        profile_url = f"{_profile_base()}/_apis/profile/profiles/me?api-version={API_VERSION}"
        try:
            with httpx.Client(timeout=30) as client:
                prof_res = client.get(profile_url, headers=_headers(token))
        except httpx.RequestError as exc:
            raise AzureDevOpsAPIError("Could not reach Azure DevOps profile API.", status_code=502) from exc
        _raise_for_status(prof_res, context="profile")

        profile = prof_res.json()
        member_id = str((profile or {}).get("id") or "").strip()
        if not member_id:
            return []

        accounts_url = f"{_profile_base()}/_apis/accounts?memberId={member_id}&api-version={API_VERSION}"
        with httpx.Client(timeout=30) as client:
            acc_res = client.get(accounts_url, headers=_headers(token))
        _raise_for_status(acc_res, context="accounts")
        payload = acc_res.json()
        values = payload.get("value") if isinstance(payload, dict) else []
        return [v for v in (values or []) if isinstance(v, dict)]

    def list_projects(self) -> List[Dict[str, Any]]:
        url = self._org_url(f"/_apis/projects?api-version={API_VERSION}&$top=100")
        res = self._get(url)
        _raise_for_status(res, context="projects")
        payload = res.json()
        values = payload.get("value") if isinstance(payload, dict) else []
        return [v for v in (values or []) if isinstance(v, dict)]

    def list_repositories(self) -> List[Dict[str, Any]]:
        if not self.azure_project:
            raise ValueError("azure_project is required to list repositories")
        url = self._project_url(f"/_apis/git/repositories?api-version={API_VERSION}")
        res = self._get(url)
        _raise_for_status(res, context="repositories")
        payload = res.json()
        values = payload.get("value") if isinstance(payload, dict) else []
        return [v for v in (values or []) if isinstance(v, dict)]

    def validate_connection(self) -> Tuple[Dict[str, Any], Dict[str, Any]]:
        if not self.repository_id:
            raise ValueError("repository_id is required")
        url = self._project_url(
            f"/_apis/git/repositories/{self.repository_id}?api-version={API_VERSION}"
        )
        res = self._get(url)
        _raise_for_status(res, context="repository")
        data = res.json()
        meta = data if isinstance(data, dict) else {}
        return meta, {}

    def list_open_pull_requests(self, *, limit: int = 20) -> List[Dict[str, Any]]:
        if not self.repository_id:
            raise ValueError("repository_id is required")
        url = self._project_url(f"/_apis/git/pullrequests?api-version={API_VERSION}")
        params = {
            "searchCriteria.status": "active",
            "searchCriteria.repositoryId": self.repository_id,
            "$top": max(1, min(50, int(limit))),
        }
        res = self._get(url, params=params)
        _raise_for_status(res, context="pullrequests")
        payload = res.json()
        values = payload.get("value") if isinstance(payload, dict) else []
        return [v for v in (values or []) if isinstance(v, dict)]

    def get_pull_request(self, pull_request_id: int) -> Dict[str, Any]:
        url = self._project_url(
            f"/_apis/git/repositories/{self.repository_id}/pullrequests/{int(pull_request_id)}"
            f"?api-version={API_VERSION}"
        )
        res = self._get(url)
        _raise_for_status(res, context=f"pullrequest/{pull_request_id}")
        data = res.json()
        return data if isinstance(data, dict) else {}

    def _latest_iteration_id(self, pull_request_id: int) -> Optional[int]:
        url = self._project_url(
            f"/_apis/git/repositories/{self.repository_id}/pullrequests/{int(pull_request_id)}"
            f"/iterations?api-version={API_VERSION}"
        )
        res = self._get(url)
        _raise_for_status(res, context=f"pr/{pull_request_id}/iterations")
        payload = res.json()
        values = payload.get("value") if isinstance(payload, dict) else []
        if not values:
            return None
        latest = max(values, key=lambda x: int((x or {}).get("id") or 0))
        iid = int(latest.get("id") or 0)
        return iid if iid > 0 else None

    def list_pull_request_changes(self, pull_request_id: int) -> List[Dict[str, Any]]:
        iteration_id = self._latest_iteration_id(pull_request_id)
        if not iteration_id:
            return []
        url = self._project_url(
            f"/_apis/git/repositories/{self.repository_id}/pullrequests/{int(pull_request_id)}"
            f"/iterations/{iteration_id}/changes?api-version={API_VERSION}"
        )
        res = self._get(url)
        _raise_for_status(res, context=f"pr/{pull_request_id}/changes")
        payload = res.json()
        changes = payload.get("changeEntries") if isinstance(payload, dict) else None
        if changes is None:
            changes = payload.get("changes") if isinstance(payload, dict) else []
        return [c for c in (changes or []) if isinstance(c, dict)]

    @staticmethod
    def _ref_name(ref: str) -> str:
        s = (ref or "").strip()
        if s.startswith("refs/heads/"):
            return s[len("refs/heads/"):]
        return s

    @staticmethod
    def _change_path(entry: Dict[str, Any]) -> str:
        item = entry.get("item") if isinstance(entry.get("item"), dict) else {}
        path = str(item.get("path") or entry.get("path") or "").strip()
        if path.startswith("/"):
            path = path[1:]
        return path

    @staticmethod
    def _change_status(entry: Dict[str, Any]) -> str:
        ct = entry.get("changeType")
        if isinstance(ct, str):
            return ct.lower()
        if isinstance(ct, int):
            # Azure enum: 1=add, 2=edit, 4=delete, 8=rename, etc.
            mapping = {1: "add", 2: "edit", 4: "delete", 8: "rename"}
            return mapping.get(ct, "edit")
        return "edit"

    def pull_request_file_entries(self, pull_request_id: int) -> Tuple[List[Dict[str, Any]], bool]:
        """
        Return file entries and patches_limited flag.

        Azure DevOps iteration changes do not include unified diff text like GitHub patches.
        """
        pr = self.get_pull_request(pull_request_id)
        changes = self.list_pull_request_changes(pull_request_id)
        files: List[Dict[str, Any]] = []
        seen: set[str] = set()

        for entry in changes:
            fn = self._change_path(entry)
            if not fn or fn in seen:
                continue
            seen.add(fn)
            files.append({
                "filename": fn,
                "status": self._change_status(entry),
                "additions": 0,
                "deletions": 0,
                "patch": None,
            })

        branch = self._ref_name(str((pr.get("sourceRefName") or "")))
        return files, True

    def pull_request_summary_from_raw(self, pr: Dict[str, Any]) -> Dict[str, Any]:
        created_by = pr.get("createdBy") if isinstance(pr.get("createdBy"), dict) else {}
        return {
            "pull_request_id": int(pr.get("pullRequestId") or 0),
            "title": str(pr.get("title") or ""),
            "state": str(pr.get("status") or ""),
            "branch": self._ref_name(str(pr.get("sourceRefName") or "")),
            "base_branch": self._ref_name(str(pr.get("targetRefName") or "")),
            "author": str(created_by.get("displayName") or created_by.get("uniqueName") or ""),
            "html_url": str(pr.get("url") or ""),
            "updated_at": str(pr.get("creationDate") or ""),
            "draft": bool(pr.get("isDraft")),
        }
