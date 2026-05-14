# services/local_agent_evidence_service.py
"""Phase 4D — Local Agent screenshot upload + browser inspection cloud persistence."""
from __future__ import annotations

import hashlib
import logging
import re
from typing import Optional

from fastapi import HTTPException, UploadFile

from models.local_agent_models import (
    LOCAL_AGENT_MAX_ARTIFACT_BYTES,
    LocalAgentArtifactUploadResponse,
    LocalAgentBrowserInspectionPersistRequest,
    LocalAgentBrowserInspectionPersistResponse,
)

logger = logging.getLogger("vanya.local_agent_evidence")

_INSPECTION_ID_RE = re.compile(r"^[a-zA-Z0-9_-]{8,128}$")

_ALLOWED_CT = frozenset({"image/png", "image/jpeg", "image/jpg", "image/webp"})


def sniff_image_content_type(data: bytes) -> Optional[str]:
    if not data or len(data) < 12:
        return None
    if data[:8] == b"\x89PNG\r\n\x1a\n":
        return "image/png"
    if len(data) >= 3 and data[:3] == b"\xff\xd8\xff":
        return "image/jpeg"
    if data[:4] == b"RIFF" and data[8:12] == b"WEBP":
        return "image/webp"
    return None


def validate_artifact_filename(name: Optional[str]) -> None:
    """Reject path traversal / odd names (filename is not trusted for storage paths)."""
    if not name:
        return
    n = str(name).strip()
    if not n:
        return
    if ".." in n or "/" in n or "\\" in n or "\x00" in n:
        raise ValueError("unsafe filename")


def _normalize_declared_content_type(raw: Optional[str]) -> Optional[str]:
    if not raw:
        return None
    s = str(raw).split(";", 1)[0].strip().lower()
    if s == "image/jpg":
        return "image/jpeg"
    return s or None


def reconcile_image_types(declared: Optional[str], data: bytes) -> str:
    sniffed = sniff_image_content_type(data)
    if not sniffed:
        raise ValueError("unsupported or corrupt image (allowed: PNG, JPEG, WEBP)")
    dec = _normalize_declared_content_type(declared)
    if dec and dec not in _ALLOWED_CT:
        raise ValueError("declared Content-Type not allowed")
    if dec and dec != sniffed:
        # Do not trust MIME alone — magic bytes win; declared must match sniff.
        raise ValueError("Content-Type does not match file contents")
    if not dec:
        # If client omitted type, still allow when sniff is unambiguous.
        pass
    return sniffed


async def read_upload_bytes_capped(upload: UploadFile, *, max_bytes: int) -> bytes:
    """Read ``UploadFile`` with a hard cap (413 if exceeded)."""
    chunks: list[bytes] = []
    total = 0
    while True:
        chunk = await upload.read(1024 * 1024)
        if not chunk:
            break
        total += len(chunk)
        if total > max_bytes:
            raise HTTPException(status_code=413, detail="artifact too large")
        chunks.append(chunk)
    return b"".join(chunks)


def _validate_inspection_id(inspection_id: str) -> str:
    iid = (inspection_id or "").strip()
    if not _INSPECTION_ID_RE.match(iid):
        raise HTTPException(status_code=400, detail="invalid inspection_id")
    return iid


async def upload_browser_inspection_artifact(
    *,
    agent_id: str,
    inspection_id: str,
    upload: UploadFile,
) -> LocalAgentArtifactUploadResponse:
    """
    Validate bytes + types, upload to Cloudinary with a deterministic public_id.

    Does not use client-supplied paths — only ``agent_id`` + ``inspection_id``.
    """
    iid = _validate_inspection_id(inspection_id)
    try:
        validate_artifact_filename(upload.filename)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from e

    data = await read_upload_bytes_capped(upload, max_bytes=LOCAL_AGENT_MAX_ARTIFACT_BYTES)
    if not data:
        raise HTTPException(status_code=400, detail="empty upload")

    try:
        ct = reconcile_image_types(upload.content_type, data)
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e)) from e

    sha = hashlib.sha256(data).hexdigest()
    safe_agent = re.sub(r"[^a-zA-Z0-9_-]", "", (agent_id or "").strip())[:128] or "agent"
    public_id = f"{safe_agent}/{iid}"

    try:
        from services.cloudinary_service import upload_image_bytes

        res = upload_image_bytes(
            data,
            public_id=public_id,
            folder="vanya/local_agent",
            tags=["vanya", "local_agent", "browser_inspection"],
        )
        url = (res.get("secure_url") or res.get("url") or "").strip()
        if not url:
            raise RuntimeError("missing secure_url")
    except HTTPException:
        raise
    except Exception as exc:
        logger.warning("local_agent artifact upload failed agent=%s inspection=%s — %s", agent_id, iid, exc)
        raise HTTPException(status_code=503, detail="artifact storage unavailable") from exc

    return LocalAgentArtifactUploadResponse(evidence_url=url, sha256=sha, content_type=ct)


def persist_browser_inspection_from_local_agent(
    *,
    agent_id: str,
    agent_project_id: Optional[str],
    body: LocalAgentBrowserInspectionPersistRequest,
) -> LocalAgentBrowserInspectionPersistResponse:
    """Persist inspection via ``persist_light_browser_inspection`` with local_agent metadata."""
    from services.browser_inspection_persistence import persist_light_browser_inspection

    proj = (body.project_id or agent_project_id or "").strip() or None
    ins = body.inspection

    rid, ok, warn = persist_light_browser_inspection(
        ins,
        inv={},
        extras={},
        project_id=proj,
        app_map=None,
        meta_source="local_agent",
        execution_mode="local_agent",
        local_agent_id=(agent_id or "").strip() or None,
        watch_id=(body.watch_id or "").strip() or None,
        job_id=(body.job_id or "").strip() or None,
        artifact_sha256=(body.artifact_sha256 or "").strip() or None,
    )
    return LocalAgentBrowserInspectionPersistResponse(
        persisted_run_id=rid,
        persisted=ok,
        persistence_warning=warn,
        evidence_url=ins.screenshot_url,
    )
