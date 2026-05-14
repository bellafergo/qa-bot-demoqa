# tests/test_project_github_settings.py
"""Project settings merge/mask for GitHub block (no network)."""
from __future__ import annotations

import unittest

from services.project_settings_service import mask_settings_for_api, merge_settings


class TestProjectGithubSettings(unittest.TestCase):
    def test_merge_preserves_github_when_patch_omits_it(self):
        existing = {
            "github": {"enabled": True, "owner": "acme", "repo": "app", "github_token": "secret"},
            "variables": {"EMAIL": "a@b.com"},
        }
        patch = {"variables": {"EMAIL": "x@y.com"}}
        out = merge_settings(existing, patch)
        self.assertEqual(out["github"]["owner"], "acme")
        self.assertEqual(out["github"]["github_token"], "secret")
        self.assertEqual(out["variables"]["EMAIL"], "x@y.com")

    def test_merge_github_partial_updates(self):
        existing = {"github": {"enabled": True, "owner": "acme", "repo": "app", "github_token": "t"}}
        patch = {"github": {"default_branch": "develop"}}
        out = merge_settings(existing, patch)
        self.assertEqual(out["github"]["default_branch"], "develop")
        self.assertEqual(out["github"]["owner"], "acme")
        self.assertEqual(out["github"]["github_token"], "t")

    def test_mask_github_never_exposes_token(self):
        raw = {
            "github": {
                "enabled": True,
                "owner": "acme",
                "repo": "app",
                "github_token": "supersecret",
            }
        }
        masked = mask_settings_for_api(raw)
        self.assertIsNotNone(masked)
        self.assertIn("github", masked)
        self.assertEqual(masked["github"]["owner"], "acme")
        tok = masked["github"].get("github_token")
        self.assertIsInstance(tok, dict)
        self.assertTrue(tok.get("present"))
        self.assertTrue(tok.get("sensitive"))


if __name__ == "__main__":
    unittest.main()
