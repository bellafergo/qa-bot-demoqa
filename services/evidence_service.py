# services/evidence_service.py
from __future__ import annotations

import base64
import logging
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


def _cloudinary_ready() -> bool:
    return bool(getattr(settings, "HAS_CLOUDINARY", False)) and cloudinary is not None


def _ensure_cloudinary_config() -> None:
    """
    Configura Cloudinary una sola vez por proceso.
    """
    if not _cloudinary_ready():
        return

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
    Sube PNG bytes a Cloudinary y regresa metadata consistente:
    - secure_url (versionado)
    - image_url (sin version, pero directo)
    - url (alias para compat)
    """
    if not _cloudinary_ready():
        raise RuntimeError(
            "Cloudinary no está configurado (CLOUDINARY_* faltante o lib no instalada)"
        )

    _ensure_cloudinary_config()

    public_id = (public_id or f"EV-{uuid.uuid4().hex[:10]}").strip()

    uploaded = cloudinary.uploader.upload(  # type: ignore
        png_bytes,
        public_id=public_id,
        folder=folder,
        resource_type="image",
        overwrite=True,
        format="png",  # fuerza png
    )

    secure_url = uploaded.get("secure_url") or ""
    url = uploaded.get("url") or secure_url or ""
    public_id_full = uploaded.get("public_id") or f"{folder}/{public_id}"

    # Imagen “directa” (sin version). Útil para UI.
    # Si secure_url existe, la derivamos.
    image_url = ""
    try:
        if isinstance(secure_url, str) and "/upload/" in secure_url:
            # https://res.cloudinary.com/<cloud>/image/upload/v123/folder/id.png
            # -> https://res.cloudinary.com/<cloud>/image/upload/folder/id.png
            image_url = secure_url.replace("/upload/v", "/upload/", 1)
            # quita solo el número de version "v123/"
            # (si queda raro, igual secure_url es la verdad)
            parts = image_url.split("/upload/", 1)
            if len(parts) == 2 and parts[1].startswith("v"):
                # v1766....../...
                tail = parts[1]
                slash = tail.find("/")
                if slash != -1:
                    image_url = parts[0] + "/upload/" + tail[slash + 1 :]
    except Exception:
        image_url = ""

    return {
        "ok": True,
        "provider": "cloudinary",
        "public_id": public_id_full,
        "secure_url": secure_url or url,
        "image_url": image_url or (secure_url or url),
        "url": secure_url or url,  # compat viejo
        "bytes": uploaded.get("bytes"),
        "format": uploaded.get("format") or "png",
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
    Acepta:
      - "data:image/png;base64,...."
      - "...." (raw b64)
    """
    if not screenshot_b64:
        raise ValueError("screenshot_b64 vacío")

    raw = screenshot_b64.strip()
    if raw.lower().startswith("data:") and "," in raw:
        raw = raw.split(",", 1)[1].strip()

    png_bytes = base64.b64decode(raw)
    return upload_evidence_png_bytes(png_bytes, public_id=evidence_id, folder=folder)