"""HTTP client for Vanya Cloud agent-api (Phase 4B)."""
from __future__ import annotations

import base64
import json
import logging
import time
from typing import Any, Dict, List, Optional

import httpx

logger = logging.getLogger("vanya.local_agent.client")


class AgentClientError(Exception):
    """Recoverable or fatal error talking to Vanya Cloud."""


class AgentAuthError(AgentClientError):
    """401/403 from cloud."""


def build_dry_run_result_ref() -> str:
    """Compact JSON for succeeded dry-run (fits cloud result_ref cap)."""
    payload = {
        "kind": "vanya_local_agent_dry_run",
        "message": "local agent dry-run result",
        "artifacts": [],
    }
    raw = json.dumps(payload, separators=(",", ":"))
    if len(raw) > 512:
        raw = raw[:512]
    return raw


class VanyaAgentClient:
    """Sync httpx client for heartbeat / poll / job result."""

    def __init__(
        self,
        base_url: str,
        agent_id: str,
        agent_token: str,
        *,
        timeout_s: float = 30.0,
        max_retries: int = 2,
        client: Optional[httpx.Client] = None,
    ) -> None:
        self._base = base_url.rstrip("/")
        self._agent_id = agent_id.strip()
        self._token = agent_token.strip()
        self._timeout = timeout_s
        self._max_retries = max(0, int(max_retries))
        self._owns_client = client is None
        self._http = client or httpx.Client(timeout=httpx.Timeout(timeout_s))

    def close(self) -> None:
        if self._owns_client:
            self._http.close()

    def __enter__(self) -> "VanyaAgentClient":
        return self

    def __exit__(self, *args: Any) -> None:
        self.close()

    def _headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self._token}",
            "Content-Type": "application/json",
            "Accept": "application/json",
        }

    def _url(self, path: str) -> str:
        p = path if path.startswith("/") else f"/{path}"
        return f"{self._base}{p}"

    def _request(self, method: str, path: str, *, json_body: Optional[dict] = None) -> httpx.Response:
        url = self._url(path)
        last_exc: Optional[Exception] = None
        for attempt in range(self._max_retries + 1):
            try:
                r = self._http.request(
                    method,
                    url,
                    headers=self._headers(),
                    json=json_body,
                )
                self._auth_errors_from_response(r)
                if r.status_code >= 500 and attempt < self._max_retries:
                    time.sleep(0.4 * (attempt + 1))
                    continue
                r.raise_for_status()
                return r
            except AgentAuthError:
                raise
            except httpx.HTTPStatusError as e:
                if e.response is not None and e.response.status_code >= 500 and attempt < self._max_retries:
                    time.sleep(0.4 * (attempt + 1))
                    last_exc = e
                    continue
                raise AgentClientError(f"HTTP error {e.response.status_code if e.response else '?'}: {e}") from e
            except (httpx.ConnectError, httpx.ReadTimeout, httpx.WriteTimeout) as e:
                last_exc = e
                if attempt < self._max_retries:
                    time.sleep(0.4 * (attempt + 1))
                    continue
                raise AgentClientError(f"cloud unreachable after retries: {e}") from e
        raise AgentClientError(f"request failed: {last_exc}")

    def _auth_errors_from_response(self, r: httpx.Response) -> None:
        if r.status_code == 401:
            raise AgentAuthError(
                "authentication failed (401): invalid or revoked agent token — check VANYA_AGENT_TOKEN"
            )
        if r.status_code == 403:
            detail = ""
            try:
                detail = str((r.json() or {}).get("detail") or "")
            except Exception:
                detail = (r.text or "")[:200]
            raise AgentAuthError(
                "forbidden (403): agent disabled or job not allowed — "
                f"cloud detail: {detail or 'no detail'}"
            )

    def _post_multipart(
        self,
        path: str,
        *,
        params: Optional[Dict[str, str]] = None,
        files: Dict[str, Any],
    ) -> httpx.Response:
        url = self._url(path)
        headers = {
            "Authorization": f"Bearer {self._token}",
            "Accept": "application/json",
        }
        last_exc: Optional[Exception] = None
        for attempt in range(self._max_retries + 1):
            try:
                r = self._http.post(url, params=params or {}, files=files, headers=headers, timeout=self._timeout)
                self._auth_errors_from_response(r)
                if r.status_code >= 500 and attempt < self._max_retries:
                    time.sleep(0.4 * (attempt + 1))
                    continue
                r.raise_for_status()
                return r
            except AgentAuthError:
                raise
            except httpx.HTTPStatusError as e:
                if e.response is not None and e.response.status_code >= 500 and attempt < self._max_retries:
                    time.sleep(0.4 * (attempt + 1))
                    last_exc = e
                    continue
                raise AgentClientError(f"HTTP error {e.response.status_code if e.response else '?'}: {e}") from e
            except (httpx.ConnectError, httpx.ReadTimeout, httpx.WriteTimeout) as e:
                last_exc = e
                if attempt < self._max_retries:
                    time.sleep(0.4 * (attempt + 1))
                    continue
                raise AgentClientError(f"cloud unreachable after retries: {e}") from e
        raise AgentClientError(f"request failed: {last_exc}")

    def heartbeat(self, *, agent_version: Optional[str] = None, notes: Optional[str] = None) -> Dict[str, Any]:
        path = f"/agent-api/{self._agent_id}/heartbeat"
        body: Dict[str, Any] = {}
        if agent_version:
            body["agent_version"] = agent_version
        if notes:
            body["notes"] = notes
        r = self._request("POST", path, json_body=body or {})
        return r.json()

    def poll(self, *, limit: int = 10) -> Dict[str, Any]:
        path = f"/agent-api/{self._agent_id}/poll"
        r = self._request("POST", path, json_body={"limit": limit})
        return r.json()

    def submit_job_result(
        self,
        job_id: str,
        *,
        status: str = "succeeded",
        result_ref: Optional[str] = None,
        error: Optional[str] = None,
    ) -> Dict[str, Any]:
        path = f"/agent-api/{self._agent_id}/jobs/{job_id}/result"
        body: Dict[str, Any] = {"status": status}
        if result_ref is not None:
            body["result_ref"] = result_ref
        if error is not None:
            body["error"] = error
        r = self._request("POST", path, json_body=body)
        return r.json()

    def upload_browser_inspection_artifact(
        self,
        inspection_id: str,
        data: bytes,
        *,
        filename: str = "screenshot.png",
        content_type: str = "image/png",
    ) -> Dict[str, Any]:
        path = f"/agent-api/{self._agent_id}/artifacts/upload"
        files = {"file": (filename, data, content_type)}
        r = self._post_multipart(path, params={"inspection_id": inspection_id}, files=files)
        return r.json()

    def persist_browser_inspection_to_cloud(
        self,
        *,
        inspection: Any,
        job_id: Optional[str] = None,
        watch_id: Optional[str] = None,
        project_id: Optional[str] = None,
        artifact_sha256: Optional[str] = None,
    ) -> Dict[str, Any]:
        path = f"/agent-api/{self._agent_id}/browser-inspections/persist"
        body: Dict[str, Any] = {
            "job_id": job_id,
            "watch_id": watch_id,
            "project_id": project_id,
            "artifact_sha256": artifact_sha256,
            "inspection": inspection.model_dump(mode="json"),
        }
        r = self._request("POST", path, json_body=body)
        return r.json()

    def process_jobs(
        self,
        jobs: List[Dict[str, Any]],
        *,
        cfg: Any,
        agent_capabilities: Optional[List[str]] = None,
    ) -> int:
        """
        Heartbeat/poll caller dispatches here. Supports ``browser_inspection`` (Phase 4C)
        and rejects unknown job types with a controlled failed result.
        """
        from local_agent.browser_job import execute_browser_inspection_job

        submitted = 0
        caps = list(agent_capabilities or [])
        for job in jobs:
            jid = str(job.get("job_id") or "").strip()
            jt = str(job.get("job_type") or "").strip().lower()
            logger.info("job received job_id=%s job_type=%s", jid, jt)
            if not jid:
                logger.warning("skip job with empty job_id")
                continue
            if cfg.dry_run:
                logger.info("dry-run: skip result POST job_id=%s job_type=%s", jid, jt)
                continue
            if jt == "browser_inspection":
                st, ref, err = execute_browser_inspection_job(
                    job,
                    cfg,
                    agent_capabilities=caps,
                    client=self if not cfg.dry_run else None,
                )
                self.submit_job_result(jid, status=st, result_ref=ref, error=err)
                submitted += 1
                continue
            self.submit_job_result(
                jid,
                status="failed",
                error=f"unsupported job_type: {jt!r}"[:500],
            )
            submitted += 1
        return submitted
