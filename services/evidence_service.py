# services/evidence_service.py
import base64
from typing import Any, Dict

def upload_screenshot_b64(evidence_id: str, screenshot_b64: str, upload_evidence_to_cloudinary) -> Dict[str, Any]:
    """
    Convierte screenshot_b64 (PNG base64) a bytes y sube a Cloudinary.
    `upload_evidence_to_cloudinary` se inyecta para evitar import circular.
    """
    png_bytes = base64.b64decode(screenshot_b64)
    uploaded = upload_evidence_to_cloudinary(png_bytes=png_bytes, public_id=evidence_id, folder="vanya/evidence")
    return {
        "id": evidence_id,
        "url": uploaded.get("url"),
        "provider": "cloudinary",
        "public_id": uploaded.get("public_id"),
        "bytes": uploaded.get("bytes"),
        "format": uploaded.get("format"),
        "width": uploaded.get("width"),
        "height": uploaded.get("height"),
    }