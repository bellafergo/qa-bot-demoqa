# tests/test_api_evidence.py
from __future__ import annotations

import unittest

from runners.api_evidence import redact_plain_text, sanitize_headers, truncate_text


class TestApiEvidence(unittest.TestCase):
    def test_redact_bearer(self):
        s = redact_plain_text('Authorization: Bearer secret-token-123')
        self.assertIn("[REDACTED]", s)
        self.assertNotIn("secret-token", s)

    def test_sanitize_headers_strips_auth(self):
        h = sanitize_headers({"Authorization": "Bearer x", "X-Custom": "ok"})
        self.assertEqual(h["Authorization"], "[REDACTED]")
        self.assertEqual(h["X-Custom"], "ok")

    def test_truncate_marks_meta(self):
        long = "a" * 50_000
        out, trunc, sz = truncate_text(long)
        self.assertTrue(trunc)
        self.assertEqual(sz, 50_000)
        self.assertTrue(out.endswith("[truncated]"))


if __name__ == "__main__":
    unittest.main()
