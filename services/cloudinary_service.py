# services/cloudinary_service.py
from __future__ import annotations

import base64
import io
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


def upload_image_bytes(
    image_bytes: bytes,
    *,
    public_id: str,
    folder: str = "vanya/local_agent",
    tags: Optional[Sequence[str]] = None,
) -> Dict[str, Any]:
    """
    Upload raw image bytes (PNG/JPEG/WEBP) to Cloudinary as ``resource_type=image``.

    ``public_id`` must be a server-controlled path segment (no user filesystem paths).
    """
    _configure_cloudinary()

    pid = (public_id or "").strip().replace("//", "/").lstrip("/")
    if not pid or ".." in pid:
        raise ValueError("public_id inválido")

    if not image_bytes or len(image_bytes) < 32:
        raise ValueError("Imagen vacía o demasiado pequeña")

    base = folder.strip("/").replace("//", "/")
    full_public_id = f"{base}/{pid}".replace("//", "/")

    buf = io.BytesIO(image_bytes)
    return cloudinary.uploader.upload(
        buf,
        public_id=full_public_id,
        overwrite=True,
        resource_type="image",
        tags=list(tags) if tags else ["vanya", "local_agent", "evidence"],
    )


def upload_image_bytes_url(
    image_bytes: bytes,
    *,
    public_id: str,
    folder: str = "vanya/local_agent",
    tags: Optional[Sequence[str]] = None,
) -> str:
    """Sube bytes de imagen y devuelve la URL segura."""
    res = upload_image_bytes(image_bytes, public_id=public_id, folder=folder, tags=tags)
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

    # Include .pdf extension so Cloudinary serves the file with the correct
    # filename and Content-Type when downloaded (avoids extensionless files on macOS).
    public_id = f"{folder}/{evidence_id}.pdf".replace("//", "/")

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
