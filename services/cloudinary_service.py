# services/cloudinary_service.py
from __future__ import annotations

import base64
import os
from typing import Any, Dict, Optional, Sequence

import cloudinary
import cloudinary.uploader


def _configure_cloudinary() -> None:
    """
    Configura Cloudinary.
    Preferido:
      - CLOUDINARY_URL = cloudinary://API_KEY:API_SECRET@CLOUD_NAME

    Fallback:
      - CLOUDINARY_CLOUD_NAME
      - CLOUDINARY_API_KEY
      - CLOUDINARY_API_SECRET
    """
    # Si existe CLOUDINARY_URL, cloudinary normalmente lo toma automáticamente.
    # Pero llamar config con secure=True no hace daño y ayuda a consistencia.
    cloud_url = os.getenv("CLOUDINARY_URL", "").strip()
    if cloud_url:
        cloudinary.config(secure=True)
        return

    cloud_name = os.getenv("CLOUDINARY_CLOUD_NAME", "").strip()
    api_key = os.getenv("CLOUDINARY_API_KEY", "").strip()
    api_secret = os.getenv("CLOUDINARY_API_SECRET", "").strip()

    if not (cloud_name and api_key and api_secret):
        raise RuntimeError(
            "Cloudinary no configurado. Define CLOUDINARY_URL o "
            "(CLOUDINARY_CLOUD_NAME, CLOUDINARY_API_KEY, CLOUDINARY_API_SECRET)."
        )

    cloudinary.config(
        cloud_name=cloud_name,
        api_key=api_key,
        api_secret=api_secret,
        secure=True,
    )


def _to_data_url_png(s: str) -> str:
    """
    Acepta:
      - data:image/png;base64,....
      - base64 puro (sin prefijo)
    Retorna SIEMPRE data-url válido para Cloudinary.
    """
    s = (s or "").strip()
    if not s:
        raise ValueError("Screenshot vacío")

    if s.startswith("data:image"):
        return s

    # base64 puro -> data-url
    return f"data:image/png;base64,{s}"


def upload_screenshot_b64(
    screenshot_b64_or_data_url: str,
    *,
    evidence_id: str,
    folder: str = "vanya/evidence",
    tags: Optional[Sequence[str]] = None,
) -> Dict[str, Any]:
    """
    Sube un screenshot a Cloudinary.
    - Acepta base64 puro (runner) o data-url.
    - Retorna el dict de Cloudinary (incluye secure_url).
    """
    _configure_cloudinary()

    if not evidence_id or not str(evidence_id).strip():
        raise ValueError("evidence_id requerido")

    data_url = _to_data_url_png(str(screenshot_b64_or_data_url))

    # umbral razonable: un PNG real en base64 suele superar esto
    if len(data_url) < 800:
        raise ValueError("Screenshot inválido o muy pequeño")

    public_id = f"{folder}/{evidence_id}".replace("//", "/")

    return cloudinary.uploader.upload(
        data_url,
        public_id=public_id,
        overwrite=True,
        resource_type="image",
        tags=list(tags) if tags else ["vanya", "evidence"],
    )


def upload_screenshot_b64_url(
    screenshot_b64_or_data_url: str,
    *,
    evidence_id: str,
    folder: str = "vanya/evidence",
    tags: Optional[Sequence[str]] = None,
) -> str:
    """
    Helper: sube screenshot y devuelve SOLO la URL segura.
    """
    res = upload_screenshot_b64(
        screenshot_b64_or_data_url,
        evidence_id=evidence_id,
        folder=folder,
        tags=tags,
    )
    url = (res.get("secure_url") or res.get("url") or "").strip()
    if not url:
        raise RuntimeError("Cloudinary no devolvió secure_url/url")
    return url


def upload_pdf_bytes(
    pdf_bytes: bytes,
    *,
    evidence_id: str,
    folder: str = "vanya/reports",
    tags: Optional[Sequence[str]] = None,
) -> Dict[str, Any]:
    """
    Sube un PDF a Cloudinary como RAW.
    Recomendado como data-url:
      data:application/pdf;base64,....
    """
    _configure_cloudinary()

    if not evidence_id or not str(evidence_id).strip():
        raise ValueError("evidence_id requerido")

    if not pdf_bytes or len(pdf_bytes) < 800:
        raise ValueError("PDF vacío o inválido")

    public_id = f"{folder}/{evidence_id}".replace("//", "/")

    b64 = base64.b64encode(pdf_bytes).decode("utf-8")
    data_url = f"data:application/pdf;base64,{b64}"

    return cloudinary.uploader.upload(
        data_url,
        public_id=public_id,
        resource_type="raw",
        overwrite=True,
        tags=list(tags) if tags else ["vanya", "report"],
    )


def upload_pdf_bytes_url(
    pdf_bytes: bytes,
    *,
    evidence_id: str,
    folder: str = "vanya/reports",
    tags: Optional[Sequence[str]] = None,
) -> str:
    """
    Helper: sube PDF y devuelve SOLO la URL segura.
    """
    res = upload_pdf_bytes(
        pdf_bytes,
        evidence_id=evidence_id,
        folder=folder,
        tags=tags,
    )
    url = (res.get("secure_url") or res.get("url") or "").strip()
    if not url:
        raise RuntimeError("Cloudinary no devolvió secure_url/url")
    return url
