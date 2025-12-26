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

    try:
        cloudinary.config(
            cloud_name=settings.CLOUDINARY_CLOUD_NAME,
            api_key=settings.CLOUDINARY_API_KEY,
            api_secret=settings.CLOUDINARY_API_SECRET,
            secure=True,
        )
    except Exception:
        logger.warning("Cloudinary config failed", exc_info=True)


def _direct_image_url_from_public_id(public_id: str, *, fmt: str = "png") -> Optional[str]:
    """
    Construye un URL DIRECTO de imagen (ideal para <img src="...">):
      https://res.cloudinary.com/<cloud_name>/image/upload/<public_id>.<fmt>

    Esto evita URLs raras/redirects y asegura que el frontend siempre pueda renderizar inline.
    """
    cloud_name = (settings.CLOUDINARY_CLOUD_NAME or "").strip()
    if not cloud_name or not public_id:
        return None
    # public_id ya puede venir con folder: vanya/evidence/EV-xxxx
    return f"https://res.cloudinary.com/{cloud_name}/image/upload/{public_id}.{fmt}"


def upload_evidence_png_bytes(
    png_bytes: bytes,
    *,
    public_id: Optional[str] = None,
    folder: str = "vanya/evidence",
) -> Dict[str, Any]:
    """
    Sube PNG bytes a Cloudinary y regresa metadata.
    Si Cloudinary no está configurado, lanza RuntimeError.

    IMPORTANT:
    - Forzamos format="png" para obtener URL con extensión.
    - Regresamos "image_url" (directo) además de secure_url.
    """
    if not _cloudinary_ready():
        raise RuntimeError(
            "Cloudinary no está configurado (CLOUDINARY_* faltante o lib no instalada)"
        )

    _ensure_cloudinary_config()

    # Genera un evidence id estable si no viene uno
    public_id = (public_id or f"EV-{uuid.uuid4().hex[:10]}").strip()

    try:
        uploaded = cloudinary.uploader.upload(  # type: ignore
            png_bytes,
            public_id=public_id,
            folder=folder,
            resource_type="image",
            overwrite=True,
            format="png",  # ✅ fuerza extensión y tipo
            use_filename=True,
            unique_filename=False,
        )
    except Exception:
        logger.exception("Cloudinary upload failed")
        raise

    # Cloudinary regresa public_id con folder incluido cuando usas folder=...
    uploaded_public_id = uploaded.get("public_id") or f"{folder}/{public_id}".strip("/")

    secure_url = uploaded.get("secure_url") or uploaded.get("url")

    # ✅ URL directo para IMG (lo más importante para tu UI)
    direct_image_url = _direct_image_url_from_public_id(uploaded_public_id, fmt="png")

    return {
        "ok": True,
        "provider": "cloudinary",
        "public_id": uploaded_public_id,
        "secure_url": secure_url,
        "image_url": direct_image_url or secure_url,  # fallback
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
    """
    if not screenshot_b64:
        raise ValueError("screenshot_b64 vacío")

    # tolera "data:image/png;base64,...."
    raw = screenshot_b64.strip()
    if "," in raw and raw.lower().startswith("data:"):
        raw = raw.split(",", 1)[1].strip()

    png_bytes = base64.b64decode(raw)
    return upload_evidence_png_bytes(png_bytes, public_id=evidence_id, folder=folder)