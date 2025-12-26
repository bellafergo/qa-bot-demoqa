# services/cloudinary_service.py
from __future__ import annotations

import os
import base64
from typing import Any, Dict, Optional

import cloudinary
import cloudinary.uploader


def _configure_cloudinary() -> None:
    """
    Cloudinary se puede configurar con:
      - CLOUDINARY_URL (recomendado)  ej: cloudinary://API_KEY:API_SECRET@CLOUD_NAME
    o con variables separadas:
      - CLOUDINARY_CLOUD_NAME, CLOUDINARY_API_KEY, CLOUDINARY_API_SECRET
    """
    # Si ya viene CLOUDINARY_URL, Cloudinary lo toma solo.
    # Aun así, permitimos fallback por variables separadas.
    cloud_name = os.getenv("CLOUDINARY_CLOUD_NAME", "").strip()
    api_key = os.getenv("CLOUDINARY_API_KEY", "").strip()
    api_secret = os.getenv("CLOUDINARY_API_SECRET", "").strip()

    if cloud_name and api_key and api_secret:
        cloudinary.config(
            cloud_name=cloud_name,
            api_key=api_key,
            api_secret=api_secret,
            secure=True,
        )


def upload_screenshot_b64(
    data_url: str,
    *,
    evidence_id: str,
    folder: str = "vanya/evidence",
    tags: Optional[list[str]] = None,
) -> Dict[str, Any]:
    """
    Sube un screenshot (data:image/png;base64,...) a Cloudinary.
    Retorna dict con secure_url, public_id, etc.
    """
    _configure_cloudinary()

    if not data_url or len(str(data_url)) < 500:
        raise ValueError("Screenshot inválido o muy pequeño")

    public_id = f"{folder}/{evidence_id}".replace("//", "/")

    return cloudinary.uploader.upload(
        str(data_url).strip(),
        public_id=public_id,
        overwrite=True,
        resource_type="image",
        tags=tags or ["vanya", "evidence"],
    )


def upload_pdf_bytes(
    pdf_bytes: bytes,
    *,
    evidence_id: str,
    folder: str = "vanya/reports",
    tags: Optional[list[str]] = None,
) -> Dict[str, Any]:
    """
    Sube un PDF a Cloudinary como RAW.
    Cloudinary funciona más estable si lo subimos como data-url:
      data:application/pdf;base64,....
    """
    _configure_cloudinary()

    if not pdf_bytes or len(pdf_bytes) < 800:
        raise ValueError("PDF vacío o inválido")

    public_id = f"{folder}/{evidence_id}".replace("//", "/")

    b64 = base64.b64encode(pdf_bytes).decode("utf-8")
    data_url = f"data:application/pdf;base64,{b64}"

    return cloudinary.uploader.upload(
        data_url,
        public_id=public_id,
        resource_type="raw",  # 👈 CLAVE: RAW para PDFs
        overwrite=True,
        tags=tags or ["vanya", "report"],
    )