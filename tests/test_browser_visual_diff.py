# tests/test_browser_visual_diff.py
from __future__ import annotations

import io
from unittest.mock import patch

from PIL import Image
from services.browser_visual_diff_service import (
    average_hash_hex,
    compare_browser_visual_pair,
    compare_visual_hashes,
    fetch_screenshot_bytes_capped,
)


def _png_bytes(fill: tuple[int, int, int]) -> bytes:
    im = Image.new("RGB", (32, 24), fill)
    buf = io.BytesIO()
    im.save(buf, format="PNG")
    return buf.getvalue()


def test_average_hash_identical_images_same_hash():
    b = _png_bytes((200, 100, 50))
    h1, w1 = average_hash_hex(b)
    h2, w2 = average_hash_hex(b)
    assert not w1 and not w2
    assert h1 == h2


def test_compare_visual_hashes_equal_none_level():
    h = "a" * 16
    level, sim, changed = compare_visual_hashes(h, h)
    assert level == "none"
    assert sim == 1.0
    assert changed is False


def test_compare_visual_hashes_different_detected():
    level, sim, changed = compare_visual_hashes("0000000000000000", "ffffffffffffffff")
    assert changed is True
    assert level != "none"
    assert sim < 1.0


def test_compare_pair_missing_urls_degrades():
    r = compare_browser_visual_pair(None, None)
    assert r.visual_change_detected is False
    assert r.visual_change_level is None
    assert any("missing" in w for w in r.warnings)


def test_compare_pair_same_bytes_via_fetch_hook():
    png = _png_bytes((128, 64, 32))

    def fetch(_url: str):
        return png, []

    r = compare_browser_visual_pair(
        "https://res.cloudinary.com/demo/x.png",
        "https://res.cloudinary.com/demo/y.png",
        fetch_a=fetch,
        fetch_b=fetch,
    )
    assert r.visual_change_level == "none"
    assert r.visual_change_detected is False
    assert r.visual_similarity_score == 1.0


def test_compare_pair_different_images_detected():
    import random

    def _noise_png(seed: int) -> bytes:
        r = random.Random(seed)
        im = Image.new("RGB", (96, 96))
        px = im.load()
        for y in range(96):
            for x in range(96):
                px[x, y] = (r.randint(0, 255), r.randint(0, 255), r.randint(0, 255))
        buf = io.BytesIO()
        im.save(buf, format="PNG")
        return buf.getvalue()

    a = _noise_png(1)
    b = _noise_png(42)

    r = compare_browser_visual_pair(
        "https://res.cloudinary.com/a/x.png",
        "https://res.cloudinary.com/b/y.png",
        fetch_a=lambda _u: (a, []),
        fetch_b=lambda _u: (b, []),
    )
    assert r.visual_change_detected is True
    assert r.visual_hash_changed is True


def test_fetch_rejects_non_https():
    data, w = fetch_screenshot_bytes_capped("http://res.cloudinary.com/x.png")
    assert data is None
    assert any("https" in x for x in w)


def test_fetch_rejects_unknown_host():
    data, w = fetch_screenshot_bytes_capped("https://evil.example.com/a.png")
    assert data is None
    assert any("host_not_allowed" in x for x in w)


@patch("services.browser_visual_diff_service.requests.Session.get")
def test_fetch_respects_size_cap(mock_get):
    class FakeResp:
        def __enter__(self):
            return self

        def __exit__(self, *a):
            return False

        def raise_for_status(self):
            return None

        headers = {"Content-Type": "image/png"}

        def iter_content(self, chunk_size=65536):
            yield b"x" * 100_000
            yield b"y" * 2_500_000

    mock_get.return_value = FakeResp()
    data, w = fetch_screenshot_bytes_capped("https://res.cloudinary.com/demo/x.png")
    assert data is None
    assert any("size_cap" in x for x in w)
