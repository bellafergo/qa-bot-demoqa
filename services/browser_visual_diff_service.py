# services/browser_visual_diff_service.py
"""
Lightweight visual diff between two screenshot URLs (Phase 3D).

- Bounded HTTPS fetch (host allowlist + max bytes + timeouts).
- 8×8 grayscale average hash (64 bits) via Pillow — no OpenCV, no embeddings.
- Degrades cleanly when URLs are missing, host disallowed, or download/decode fails.
"""
from __future__ import annotations

import io
import logging
import os
from typing import Callable, Optional, Tuple
from urllib.parse import urlparse

import requests
from PIL import Image

from models.browser_visual_diff_models import BrowserVisualDiffResult, VisualChangeLevel

logger = logging.getLogger("vanya.browser_visual_diff")

_MAX_BYTES_DEFAULT = 2_097_152  # 2 MiB
_CONNECT_TIMEOUT_S = 3.0
_READ_TIMEOUT_S = 12.0


def _max_download_bytes() -> int:
    raw = (os.getenv("VANYA_BROWSER_VISUAL_MAX_BYTES") or "").strip()
    if not raw:
        return _MAX_BYTES_DEFAULT
    try:
        return max(64_000, min(int(raw), _MAX_BYTES_DEFAULT * 2))
    except Exception:
        return _MAX_BYTES_DEFAULT


def _allowed_screenshot_host(hostname: str) -> bool:
    h = (hostname or "").lower().rstrip(".")
    if not h:
        return False
    custom = (os.getenv("VANYA_BROWSER_SCREENSHOT_FETCH_SUFFIXES") or "").strip()
    if custom:
        for suf in custom.split(","):
            s = suf.strip().lower()
            if not s:
                continue
            if h == s or h.endswith("." + s):
                return True
    # Default: Cloudinary delivery hosts (persisted screenshot_url in Vanya).
    return h == "res.cloudinary.com" or h.endswith(".cloudinary.com")


def _validate_https_image_url(url: str) -> Tuple[bool, list[str]]:
    w: list[str] = []
    u = (url or "").strip()
    if not u:
        w.append("visual_diff_skipped: missing_screenshot_url")
        return False, w
    try:
        p = urlparse(u)
    except Exception:
        w.append("visual_diff_skipped: invalid_screenshot_url")
        return False, w
    if (p.scheme or "").lower() != "https":
        w.append("visual_diff_skipped: screenshot_url_must_be_https")
        return False, w
    host = p.hostname or ""
    if not _allowed_screenshot_host(host):
        w.append("visual_diff_skipped: screenshot_url_host_not_allowed")
        return False, w
    return True, w


def fetch_screenshot_bytes_capped(
    url: str,
    *,
    session: Optional[requests.Session] = None,
) -> Tuple[Optional[bytes], list[str]]:
    """GET ``url`` with streaming read capped at ``_max_download_bytes()``."""
    warnings: list[str] = []
    ok, w = _validate_https_image_url(url)
    warnings.extend(w)
    if not ok:
        return None, warnings

    max_b = _max_download_bytes()
    sess = session or requests.Session()
    try:
        with sess.get(
            url.strip(),
            stream=True,
            timeout=(_CONNECT_TIMEOUT_S, _READ_TIMEOUT_S),
            headers={"User-Agent": "VanyaBrowserVisualDiff/1.0"},
        ) as resp:
            resp.raise_for_status()
            ctype = (resp.headers.get("Content-Type") or "").lower()
            if "image" not in ctype and "octet-stream" not in ctype:
                warnings.append("visual_diff_skipped: unexpected_content_type")
                return None, warnings
            chunks: list[bytes] = []
            total = 0
            for block in resp.iter_content(chunk_size=65536):
                if not block:
                    continue
                total += len(block)
                if total > max_b:
                    warnings.append("visual_diff_skipped: screenshot_exceeds_size_cap")
                    return None, warnings
                chunks.append(block)
            data = b"".join(chunks)
            if len(data) < 32:
                warnings.append("visual_diff_skipped: screenshot_too_small")
                return None, warnings
            return data, warnings
    except requests.RequestException as exc:
        logger.info("browser_visual_diff: fetch failed url=%s err=%s", url[:120], exc)
        warnings.append("visual_diff_skipped: download_failed")
        return None, warnings


def average_hash_hex(png_or_jpeg_bytes: bytes) -> Tuple[Optional[str], list[str]]:
    """
    8×8 average hash → 16 hex chars (64 bits).

    Returns None if decode/convert fails.
    """
    warnings: list[str] = []
    try:
        im = Image.open(io.BytesIO(png_or_jpeg_bytes))
        im = im.convert("L").resize((8, 8), Image.Resampling.LANCZOS)
        pixels = list(im.tobytes())
        if len(pixels) != 64:
            warnings.append("visual_diff_skipped: unexpected_hash_geometry")
            return None, warnings
        mean = sum(pixels) / 64.0
        bits = 0
        for i, px in enumerate(pixels):
            if px >= mean:
                bits |= 1 << i
        h16 = f"{bits & 0xFFFFFFFFFFFFFFFF:016x}"
        return h16, warnings
    except Exception as exc:
        logger.info("browser_visual_diff: hash failed: %s", exc)
        warnings.append("visual_diff_skipped: image_decode_failed")
        return None, warnings


def _hamming64(a_hex: str, b_hex: str) -> int:
    try:
        va = int(a_hex, 16)
        vb = int(b_hex, 16)
    except Exception:
        return 64
    x = va ^ vb
    return x.bit_count()


def _level_from_hamming(h: int) -> VisualChangeLevel:
    if h <= 0:
        return "none"
    if h <= 2:
        return "low"
    if h <= 8:
        return "medium"
    return "high"


def compare_visual_hashes(a_hex: str, b_hex: str) -> Tuple[VisualChangeLevel, float, bool]:
    """
    Returns (visual_change_level, similarity_score in [0,1], visual_hash_changed).
    """
    h = _hamming64(a_hex, b_hex)
    sim = max(0.0, min(1.0, 1.0 - h / 64.0))
    level = _level_from_hamming(h)
    changed = h > 0
    return level, sim, changed


def compare_browser_visual_pair(
    screenshot_url_a: Optional[str],
    screenshot_url_b: Optional[str],
    *,
    fetch_a: Optional[Callable[[str], Tuple[Optional[bytes], list[str]]]] = None,
    fetch_b: Optional[Callable[[str], Tuple[Optional[bytes], list[str]]]] = None,
) -> BrowserVisualDiffResult:
    """
    Compare two persisted ``screenshot_url`` values.

    ``fetch_*`` hooks default to :func:`fetch_screenshot_bytes_capped` (tests may inject fakes).
    """
    warnings: list[str] = []
    fa = fetch_a or fetch_screenshot_bytes_capped
    fb = fetch_b or fetch_screenshot_bytes_capped

    ua = (screenshot_url_a or "").strip()
    ub = (screenshot_url_b or "").strip()
    if not ua or not ub:
        if not ua and not ub:
            warnings.append("visual_diff_skipped: both_screenshot_urls_missing")
        else:
            warnings.append("visual_diff_skipped: one_screenshot_url_missing")
        return BrowserVisualDiffResult(warnings=warnings)

    ba, wa = fa(ua)
    warnings.extend(wa)
    bb, wb = fb(ub)
    warnings.extend(wb)
    if ba is None or bb is None:
        return BrowserVisualDiffResult(warnings=warnings)

    ha, wah = average_hash_hex(ba)
    warnings.extend(wah)
    hb, wbh = average_hash_hex(bb)
    warnings.extend(wbh)
    if ha is None or hb is None:
        return BrowserVisualDiffResult(warnings=warnings)

    level, sim, changed = compare_visual_hashes(ha, hb)
    detected = level != "none"
    return BrowserVisualDiffResult(
        visual_change_detected=detected,
        visual_change_level=level,
        visual_hash_changed=changed,
        visual_similarity_score=round(sim, 4),
        base_visual_hash=ha,
        target_visual_hash=hb,
        warnings=warnings,
    )
