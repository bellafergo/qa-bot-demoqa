# services/visual_diff.py
from __future__ import annotations

import io
import os
import math
import logging
from dataclasses import dataclass
from typing import Any, Dict, Optional, Tuple

logger = logging.getLogger("vanya.visual")

VRT_ENABLED = (os.getenv("VRT_ENABLED", "1").strip() != "0")
VRT_DEFAULT_THRESHOLD = float(os.getenv("VRT_THRESHOLD", "0.02"))  # 2% diff default
CLOUDINARY_FOLDER_BASELINES = (os.getenv("CLOUDINARY_BASELINES_FOLDER", "vanya/baselines")).strip()
CLOUDINARY_FOLDER_DIFFS = (os.getenv("CLOUDINARY_DIFFS_FOLDER", "vanya/diffs")).strip()

# Cloudinary must already be configured in your project (CLOUDINARY_URL or cloudinary.config)
try:
    import cloudinary
    import cloudinary.uploader
except Exception:  # pragma: no cover
    cloudinary = None  # type: ignore

try:
    import requests
except Exception:  # pragma: no cover
    requests = None  # type: ignore

try:
    from PIL import Image, ImageChops
except Exception:  # pragma: no cover
    Image = None  # type: ignore
    ImageChops = None  # type: ignore


@dataclass
class VisualDiffResult:
    ok: bool
    score: float
    threshold: float
    baseline_url: Optional[str] = None
    current_url: Optional[str] = None
    diff_url: Optional[str] = None
    reason: Optional[str] = None


def _ensure_enabled() -> None:
    if not VRT_ENABLED:
        raise RuntimeError("VRT_DISABLED (set VRT_ENABLED=1 to enable)")


def _require_deps() -> None:
    if requests is None:
        raise RuntimeError("Missing dependency: requests")
    if Image is None or ImageChops is None:
        raise RuntimeError("Missing dependency: Pillow (PIL)")


def _norm_key(key: str) -> str:
    k = (key or "").strip().strip("/")
    if not k:
        raise ValueError("baseline_key vacío")
    # Cloudinary public_id friendly
    k = k.replace(" ", "_")
    return k


def _cloudinary_upload_png(png_bytes: bytes, *, folder: str, public_id: str) -> str:
    if cloudinary is None:
        raise RuntimeError("Cloudinary SDK not installed/configured")
    res = cloudinary.uploader.upload(
        io.BytesIO(png_bytes),
        resource_type="image",
        folder=folder,
        public_id=public_id,
        overwrite=True,
        format="png",
    )
    return str(res.get("secure_url") or res.get("url") or "")


def _download_image_bytes(url: str) -> bytes:
    _require_deps()
    r = requests.get(url, timeout=30)
    r.raise_for_status()
    return r.content


def _open_png(png_bytes: bytes):
    _require_deps()
    img = Image.open(io.BytesIO(png_bytes)).convert("RGBA")
    return img


def _png_bytes(img) -> bytes:
    buf = io.BytesIO()
    img.save(buf, format="PNG")
    return buf.getvalue()


def _diff_score_and_image(baseline_img, current_img) -> Tuple[float, Any]:
    """
    Returns:
      score: float 0..1 (approx)
      diff_img: RGBA image
    """
    # normalize sizes (MVP: force same size; if different, pad to max)
    bw, bh = baseline_img.size
    cw, ch = current_img.size
    if (bw, bh) != (cw, ch):
        W, H = max(bw, cw), max(bh, ch)
        base2 = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        cur2 = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        base2.paste(baseline_img, (0, 0))
        cur2.paste(current_img, (0, 0))
        baseline_img, current_img = base2, cur2

    diff = ImageChops.difference(baseline_img, current_img)

    # RMS of pixel differences
    hist = diff.histogram()
    # histogram is 4*256 length for RGBA
    sq = 0.0
    total = (diff.size[0] * diff.size[1]) * 4.0
    for i, v in enumerate(hist):
        val = (i % 256)
        sq += (val * val) * v

    rms = math.sqrt(sq / max(1.0, total))  # 0..255
    score = rms / 255.0  # normalize to 0..1

    # Make diff visible (increase contrast): MVP simple alpha boost
    diff_vis = diff.copy()
    return float(score), diff_vis


def ensure_baseline(
    baseline_key: str,
    *,
    screenshot_png_bytes: bytes,
) -> str:
    """
    If you want to "record" a baseline for a key, call this once.
    Returns baseline_url.
    """
    _ensure_enabled()
    k = _norm_key(baseline_key)
    baseline_url = _cloudinary_upload_png(
        screenshot_png_bytes,
        folder=CLOUDINARY_FOLDER_BASELINES,
        public_id=k,
    )
    logger.info("Baseline saved key=%s url=%s", k, baseline_url)
    return baseline_url


def compare_with_baseline(
    baseline_key: str,
    *,
    screenshot_png_bytes: bytes,
    threshold: Optional[float] = None,
    upload_current: bool = True,
    upload_diff: bool = True,
) -> VisualDiffResult:
    """
    Compares current screenshot vs baseline stored in Cloudinary.
    Returns VisualDiffResult with score + urls.
    """
    _ensure_enabled()
    _require_deps()

    k = _norm_key(baseline_key)
    thr = float(threshold if threshold is not None else VRT_DEFAULT_THRESHOLD)

    # Baseline URL convention: cloudinary public_id is folder + key.
    # We don't need to "get" from API; we can store baseline_url in your DB later.
    # MVP: rebuild URL by uploading baseline once; for compare we assume it's already uploaded.
    # We'll download baseline from Cloudinary using a derived URL:
    # If you don't have the URL, best practice is: store baseline_url in Supabase.
    # MVP fallback: try to build a predictable URL is not reliable.
    #
    # So: we re-upload baseline only when asked explicitly via ensure_baseline().
    # Here we need baseline_url from env or storage. MVP: use CLOUDINARY_BASELINE_URL_PREFIX if provided.
    prefix = (os.getenv("CLOUDINARY_BASELINE_URL_PREFIX") or "").strip().rstrip("/")
    if not prefix:
        return VisualDiffResult(
            ok=False,
            score=1.0,
            threshold=thr,
            reason="Missing CLOUDINARY_BASELINE_URL_PREFIX. Store baseline_url in DB or provide prefix.",
        )

    baseline_url = f"{prefix}/{CLOUDINARY_FOLDER_BASELINES}/{k}.png"

    try:
        baseline_bytes = _download_image_bytes(baseline_url)
    except Exception as e:
        return VisualDiffResult(
            ok=False,
            score=1.0,
            threshold=thr,
            baseline_url=baseline_url,
            reason=f"Baseline not found for key='{k}'. Record it first. ({type(e).__name__}: {e})",
        )

    baseline_img = _open_png(baseline_bytes)
    current_img = _open_png(screenshot_png_bytes)

    score, diff_img = _diff_score_and_image(baseline_img, current_img)
    ok = score <= thr

    current_url = None
    diff_url = None

    if upload_current:
        try:
            current_url = _cloudinary_upload_png(
                screenshot_png_bytes,
                folder=f"{CLOUDINARY_FOLDER_DIFFS}/current",
                public_id=k,
            )
        except Exception as e:
            logger.warning("upload_current failed: %s", e)

    if upload_diff:
        try:
            diff_url = _cloudinary_upload_png(
                _png_bytes(diff_img),
                folder=f"{CLOUDINARY_FOLDER_DIFFS}/diff",
                public_id=k,
            )
        except Exception as e:
            logger.warning("upload_diff failed: %s", e)

    reason = "OK" if ok else f"Visual regression detected (score={score:.4f} > thr={thr:.4f})"
    return VisualDiffResult(
        ok=ok,
        score=float(score),
        threshold=float(thr),
        baseline_url=baseline_url,
        current_url=current_url,
        diff_url=diff_url,
        reason=reason,
    )
