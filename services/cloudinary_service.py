# services/cloudinary_service.py
from __future__ import annotations

import cloudinary
import cloudinary.uploader
from typing import Any, Dict


def upload_screenshot_b64(
    data_url: str,
    *,
    evidence_id: str,
    folder: str = "vanya/evidence",
) -> Dict[str, Any]:
    """
    Sube un screenshot (data:image/png;base64,...) a Cloudinary.
    """
    if not data_url or len(data_url) < 500:
        raise ValueError("Screenshot inválido o muy pequeño")

    public_id = f"{folder}/{evidence_id}".replace("//", "/")

    return cloudinary.uploader.upload(
        data_url,
        public_id=public_id,
        overwrite=True,
        resource_type="image",
    )


def upload_pdf_bytes(
    pdf_bytes: bytes,
    *,
    evidence_id: str,
    folder: str = "vanya/reports",
) -> Dict[str, Any]:
    """
    Sube un PDF a Cloudinary como archivo RAW.
    """
    if not pdf_bytes or len(pdf_bytes) < 500:
        raise ValueError("PDF vacío o inválido")

    public_id = f"{folder}/{evidence_id}".replace("//", "/")

    return cloudinary.uploader.upload(
        pdf_bytes,
        public_id=public_id,
        resource_type="raw",  # 👈 CLAVE
        overwrite=True,
    )