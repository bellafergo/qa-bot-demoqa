# services/evidence_service.py
from __future__ import annotations

import base64
import logging
import os
import uuid
from typing import Any, Dict, Optional

from core.settings import settings

logger = logging.getLogger("vanya.evidence")

# Cloudinary (opcional)
try:
    import cloudinary
    import cloudinary.uploader
except Exception:
    cloudinary = None
    cloudinary_uploader = None


def _cloudinary_ready() -> bool:
    if not settings.HAS_CLOUDINARY:
        return False
    if cloudinary is None:
        return False
    return True


def _ensure_cloudinary_config():
    """
    Configura Cloudinary una sola vez por proceso.
    """
    if not _cloudinary_ready():
        return

    # Si ya usas CLOUDINARY_URL, Cloudinary lo toma por env.
    # Aun así, seteamos explícito por si estás usando keys sueltas.
    try:
        cloudinary.config(
            cloud_name=settings.CLOUDINARY_CLOUD_NAME,
            api_key=settings.CLOUDINARY_API_KEY,
            api_secret=settings.CLOUDINARY_API_SECRET,
            secure=True,
        )
    except Exception:
        logger.warning("Cloudinary config failed", exc_info=True)


def upload_evidence_png_bytes(
    png_bytes: bytes,
    *,
    public_id: Optional[str] = None,
    folder: str = "vanya/evidence",
) -> Dict[str, Any]:
    """
    Sube PNG bytes a Cloudinary y regresa metadata.
    Si Cloudinary no está configurado, lanza RuntimeError.
    """
    if not _cloudinary_ready():
        raise RuntimeError("Cloudinary no está configurado (CLOUDINARY_* faltante o lib no instalada)")

    _ensure_cloudinary_config()

    public_id = (public_id or f"EV-{uuid.uuid4().hex[:10]}").strip()

    uploaded = cloudinary.uploader.upload(  # type: ignore
        png_bytes,
        public_id=public_id,
        folder=folder,
        resource_type="image",
        overwrite=True,
    )

    # url segura
    url = uploaded.get("secure_url") or uploaded.get("url")

    return {
        "ok": True,
        "provider": "cloudinary",
        "public_id": uploaded.get("public_id") or public_id,
        "url": url,
        "bytes": uploaded.get("bytes"),
        "format": uploaded.get("format"),
        "width": uploaded.get("width"),
        "height": uploaded.get("height"),
    }


def upload_screenshot_b64(
    screenshot_b64: str,
    *,
    evidence_id: Optional[str] = None,
    folder: str = "vanya/evidence",
) -> Dict[str, Any]:
    """
    Convierte screenshot base64 a PNG bytes y lo sube a Cloudinary.
    """
    if not screenshot_b64:
        raise ValueError("screenshot_b64 vacío")

    # tolera "data:image/png;base64,...."
    raw = screenshot_b64.strip()
    if "," in raw and raw.lower().startswith("data:"):
        raw = raw.split(",", 1)[1].strip()

    png_bytes = base64.b64decode(raw)
    return upload_evidence_png_bytes(png_bytes, public_id=evidence_id, folder=folder)