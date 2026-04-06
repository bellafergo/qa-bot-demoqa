# tests/test_site_profiles.py
from __future__ import annotations

import unittest

from core.site_profiles import (
    PROFILE_SAUCEDEMO,
    resolve_site_profile,
    semantic_override_for_url,
    collect_intent_markers_from_profiles,
)


class TestSiteProfiles(unittest.TestCase):

    def test_resolve_saucedemo(self):
        self.assertIs(resolve_site_profile("https://www.saucedemo.com/"), PROFILE_SAUCEDEMO)
        self.assertIsNone(resolve_site_profile("https://client.example.com/app"))

    def test_semantic_override_username(self):
        spec = semantic_override_for_url("https://www.saucedemo.com", "input", "username")
        self.assertIsNotNone(spec)
        self.assertEqual(spec.get("primary"), "#user-name")

    def test_intent_markers_include_registered_profiles(self):
        m = collect_intent_markers_from_profiles()
        self.assertIn("saucedemo.com", m)


if __name__ == "__main__":
    unittest.main()
