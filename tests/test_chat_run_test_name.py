# tests/test_chat_run_test_name.py
"""Unit tests for chat-originated run display names (no live OpenAI calls)."""
from __future__ import annotations

import unittest

from services.chat_run_test_name import (
    fallback_test_name_from_prompt,
    sanitize_llm_test_name,
)


class TestSanitizeLlmTestName(unittest.TestCase):
    def test_strips_quotes_and_prefix(self):
        self.assertEqual(sanitize_llm_test_name('Test: "Verificar login"'), "Verificar login")
        self.assertEqual(sanitize_llm_test_name("Nombre: Mi flujo"), "Mi flujo")

    def test_truncates_long_line(self):
        long = "A" * 80
        out = sanitize_llm_test_name(long)
        self.assertIsNotNone(out)
        self.assertLessEqual(len(out), 60)

    def test_rejects_url_like_output(self):
        self.assertIsNone(sanitize_llm_test_name("https://evil.com/attack"))
        self.assertIsNone(sanitize_llm_test_name(None))
        self.assertIsNone(sanitize_llm_test_name(""))

    def test_first_line_only(self):
        self.assertEqual(
            sanitize_llm_test_name("Verificar login\n\nextra explanation"),
            "Verificar login",
        )


class TestFallbackTestName(unittest.TestCase):
    def test_berel_example(self):
        prompt = (
            "VE A https://berel.com/ Y VERIFICA QUE APAREZCA LA OPCION DE LOGIN"
        )
        out = fallback_test_name_from_prompt(prompt)
        self.assertNotIn("http", out.lower())
        self.assertNotIn("berel.com", out.lower())
        self.assertNotRegex(out, r"(?i)^ve\s+a\s")
        self.assertIn("Verifica", out)
        self.assertLessEqual(len(out), 60)

    def test_strip_intro_verbs(self):
        self.assertIn(
            "Login",
            fallback_test_name_from_prompt("IR A https://x.com Y VERIFICA LOGIN"),
        )
        self.assertNotRegex(
            fallback_test_name_from_prompt("ABRE https://a.com/ HOME"),
            r"(?i)^abre\s",
        )

    def test_empty_prompt(self):
        self.assertEqual(fallback_test_name_from_prompt(""), "Run desde chat")
        self.assertEqual(fallback_test_name_from_prompt("   "), "Run desde chat")

    def test_url_only_becomes_run_desde_chat(self):
        self.assertEqual(fallback_test_name_from_prompt("https://x.com/"), "Run desde chat")


if __name__ == "__main__":
    unittest.main()
