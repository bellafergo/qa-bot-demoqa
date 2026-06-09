# tests/test_api_evidence.py
from __future__ import annotations

import unittest

from runners.api_evidence import build_step_evidence, redact_plain_text, sanitize_headers, truncate_text


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

    def test_build_step_evidence_keyword_only(self):
        failure = {"type": "assertion_failed", "message": "expected 201"}
        ev = build_step_evidence(
            request={"method": "GET", "url": "https://api.example.com/x"},
            response={"status_code": 200},
            failure=failure,
        )
        self.assertEqual(ev["failure"], failure)
        self.assertIn("request", ev)
        self.assertIn("response", ev)


if __name__ == "__main__":
    unittest.main()
