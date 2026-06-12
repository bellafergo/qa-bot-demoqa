# tests/test_project_settings_service.py
"""Project settings merge/mask for environments (onboarding ENT-03A)."""
from __future__ import annotations

import unittest

from services.project_settings_service import (
    mask_settings_for_api,
    merge_settings,
    project_environments_metadata,
)


class TestProjectEnvironmentsSettings(unittest.TestCase):
    def test_merge_persists_environments(self):
        existing = {"variables": {"EMAIL": "a@b.com"}}
        patch = {
            "environments": [
                {"name": "QA", "type": "QA", "url": "https://qa.example.com"},
                {"name": "Staging", "type": "STAGING"},
            ]
        }
        out = merge_settings(existing, patch)
        self.assertEqual(len(out["environments"]), 2)
        self.assertEqual(out["environments"][0]["name"], "QA")
        self.assertEqual(out["environments"][0]["url"], "https://qa.example.com")
        self.assertEqual(out["variables"]["EMAIL"], "a@b.com")

    def test_merge_replaces_environments_list(self):
        existing = {
            "environments": [{"name": "Old", "type": "QA"}],
            "github": {"enabled": True, "owner": "acme"},
        }
        patch = {"environments": [{"name": "Prod", "type": "PRODUCTION", "url": "https://app.example.com"}]}
        out = merge_settings(existing, patch)
        self.assertEqual(len(out["environments"]), 1)
        self.assertEqual(out["environments"][0]["name"], "Prod")
        self.assertEqual(out["github"]["owner"], "acme")

    def test_merge_clears_environments_with_null(self):
        existing = {"environments": [{"name": "QA", "type": "QA"}]}
        out = merge_settings(existing, {"environments": None})
        self.assertNotIn("environments", out)

    def test_merge_skips_invalid_environment_rows(self):
        patch = {
            "environments": [
                {"name": "QA", "type": "QA"},
                {"name": "", "type": "STAGING"},
                {"name": "Prod"},
            ]
        }
        out = merge_settings({}, patch)
        self.assertEqual(len(out["environments"]), 1)
        self.assertEqual(out["environments"][0]["name"], "QA")

    def test_mask_returns_environments_without_secrets(self):
        raw = {
            "environments": [
                {"name": "QA", "type": "QA", "url": "https://qa.example.com"},
                {"name": "Prod", "type": "PRODUCTION"},
            ],
            "variables": {"PASSWORD": "secret"},
        }
        masked = mask_settings_for_api(raw)
        self.assertIsNotNone(masked)
        self.assertEqual(len(masked["environments"]), 2)
        self.assertEqual(masked["environments"][0]["url"], "https://qa.example.com")
        self.assertNotIn("secret", str(masked))

    def test_mask_omits_empty_environments(self):
        masked = mask_settings_for_api({"variables": {}})
        self.assertIsNotNone(masked)
        self.assertNotIn("environments", masked)

    def test_existing_github_settings_still_merge(self):
        existing = {"github": {"enabled": True, "owner": "acme", "repo": "app", "github_token": "secret"}}
        patch = {"environments": [{"name": "QA", "type": "QA"}]}
        out = merge_settings(existing, patch)
        self.assertEqual(out["github"]["owner"], "acme")
        masked = mask_settings_for_api(out)
        self.assertTrue(masked["github"]["github_token"]["sensitive"])

    def test_project_environments_metadata_prefers_settings_environments(self):
        project = type(
            "P",
            (),
            {
                "settings": {
                    "environments": [{"name": "QA", "type": "QA"}, {"name": "Staging", "type": "STAGING"}]
                }
            },
        )()
        meta = project_environments_metadata(project)
        self.assertIsNotNone(meta)
        self.assertEqual(len(meta["environments"]), 2)

    def test_project_environments_metadata_falls_back_to_metadata_environments(self):
        project = type(
            "P",
            (),
            {"settings": {"metadata": {"environments": [{"name": "QA", "type": "QA"}]}}},
        )()
        meta = project_environments_metadata(project)
        self.assertIsNotNone(meta)
        self.assertEqual(meta["environments"][0]["name"], "QA")


if __name__ == "__main__":
    unittest.main()
