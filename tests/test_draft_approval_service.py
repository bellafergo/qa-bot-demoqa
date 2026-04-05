# tests/test_draft_approval_service.py
"""
Tests for services/draft_approval_service.py  (pure functions only).

No database, no catalog service — all tests target:
    draft_to_catalog_test   (pure)
    approve_drafts          (pure)

Run: .venv/bin/python -m pytest tests/test_draft_approval_service.py -v
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.draft_approval_service import (
    draft_to_catalog_test,
    approve_drafts,
)


# ── Fixtures ───────────────────────────────────────────────────────────────────

_GOTO_STEP   = {"action": "goto",           "url":      "https://www.saucedemo.com"}
_FILL_STEP   = {"action": "fill",           "selector": "#user-name", "value": "<username>"}
_CLICK_STEP  = {"action": "click",          "selector": "#login-button"}
_ASSERT_STEP = {"action": "assert_visible", "selector": "body"}

_VALID_DRAFT = {
    "test_name": "login_form_valid_submission",
    "status":    "draft",
    "priority":  "high",
    "reason":    "form_detected",
    "steps":     [_GOTO_STEP, _FILL_STEP, _CLICK_STEP, _ASSERT_STEP],
}

_MISSING_FIELD_DRAFT = {
    "test_name": "login_form_missing_password",
    "status":    "draft",
    "priority":  "high",
    "reason":    "required_field_detected",
    "steps":     [_GOTO_STEP, _FILL_STEP, _CLICK_STEP,
                  {"action": "assert_visible", "selector": "#password"}],
}

_SEARCH_DRAFT = {
    "test_name": "search_empty_search",
    "status":    "draft",
    "priority":  "medium",
    "reason":    "search_button_detected",
    "steps":     [_GOTO_STEP, _CLICK_STEP, _ASSERT_STEP],
}

_NAV_DRAFT = {
    "test_name": "navigation_smoke",
    "status":    "draft",
    "priority":  "low",
    "reason":    "links_detected",
    "steps":     [_GOTO_STEP, _ASSERT_STEP],
}


# ══════════════════════════════════════════════════════════════
# 1. draft_to_catalog_test — output contract
# ══════════════════════════════════════════════════════════════

class TestDraftToCatalogTestContract:

    def test_returns_dict_for_valid_draft(self):
        assert isinstance(draft_to_catalog_test(_VALID_DRAFT), dict)

    def test_has_all_required_keys(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        for key in ("test_id", "name", "status", "priority", "type",
                    "module", "source", "reason", "steps", "meta"):
            assert key in ct, f"missing key: {key}"

    def test_test_id_equals_test_name(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["test_id"] == "login_form_valid_submission"

    def test_name_equals_test_name(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["name"] == "login_form_valid_submission"

    def test_status_is_active(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["status"] == "active"

    def test_module_is_discovered(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["module"] == "discovered"

    def test_module_override_from_draft(self):
        d = dict(_VALID_DRAFT)
        d["module"] = "checkout"
        ct = draft_to_catalog_test(d)
        assert ct["module"] == "checkout"

    def test_source_is_draft_generator(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["source"] == "draft_generator"


# ══════════════════════════════════════════════════════════════
# 2. draft_to_catalog_test — field preservation
# ══════════════════════════════════════════════════════════════

class TestFieldPreservation:

    def test_priority_high_preserved(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["priority"] == "high"

    def test_priority_medium_preserved(self):
        ct = draft_to_catalog_test(_SEARCH_DRAFT)
        assert ct["priority"] == "medium"

    def test_priority_low_preserved(self):
        ct = draft_to_catalog_test(_NAV_DRAFT)
        assert ct["priority"] == "low"

    def test_reason_preserved(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["reason"] == "form_detected"

    def test_reason_required_field_preserved(self):
        ct = draft_to_catalog_test(_MISSING_FIELD_DRAFT)
        assert ct["reason"] == "required_field_detected"

    def test_steps_preserved(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["steps"] == _VALID_DRAFT["steps"]

    def test_steps_is_a_copy(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        ct["steps"].clear()
        # original draft unchanged
        assert len(_VALID_DRAFT["steps"]) > 0

    def test_unknown_priority_defaults_to_medium(self):
        draft = {**_VALID_DRAFT, "priority": "critical_level"}
        ct = draft_to_catalog_test(draft)
        assert ct["priority"] == "medium"

    def test_missing_priority_defaults_to_medium(self):
        draft = {k: v for k, v in _VALID_DRAFT.items() if k != "priority"}
        ct = draft_to_catalog_test(draft)
        assert ct["priority"] == "medium"


# ══════════════════════════════════════════════════════════════
# 3. draft_to_catalog_test — type mapping
# ══════════════════════════════════════════════════════════════

class TestTypeMapping:

    def test_form_detected_maps_to_functional(self):
        ct = draft_to_catalog_test(_VALID_DRAFT)
        assert ct["type"] == "functional"

    def test_required_field_maps_to_negative(self):
        ct = draft_to_catalog_test(_MISSING_FIELD_DRAFT)
        assert ct["type"] == "negative"

    def test_search_button_maps_to_functional(self):
        ct = draft_to_catalog_test(_SEARCH_DRAFT)
        assert ct["type"] == "functional"

    def test_links_detected_maps_to_smoke(self):
        ct = draft_to_catalog_test(_NAV_DRAFT)
        assert ct["type"] == "smoke"

    def test_unknown_reason_defaults_to_smoke(self):
        draft = {**_VALID_DRAFT, "reason": "unknown_signal"}
        ct = draft_to_catalog_test(draft)
        assert ct["type"] == "smoke"

    def test_empty_reason_defaults_to_smoke(self):
        draft = {**_VALID_DRAFT, "reason": ""}
        ct = draft_to_catalog_test(draft)
        assert ct["type"] == "smoke"


# ══════════════════════════════════════════════════════════════
# 4. draft_to_catalog_test — meta field
# ══════════════════════════════════════════════════════════════

class TestMetaField:

    def test_meta_is_dict(self):
        assert isinstance(draft_to_catalog_test(_VALID_DRAFT)["meta"], dict)

    def test_meta_generated_from(self):
        meta = draft_to_catalog_test(_VALID_DRAFT)["meta"]
        assert meta["generated_from"] == "application_explorer"

    def test_meta_draft_true(self):
        meta = draft_to_catalog_test(_VALID_DRAFT)["meta"]
        assert meta["draft"] is True


# ══════════════════════════════════════════════════════════════
# 5. draft_to_catalog_test — invalid drafts
# ══════════════════════════════════════════════════════════════

class TestInvalidDrafts:

    def test_missing_test_name_returns_none(self):
        draft = {k: v for k, v in _VALID_DRAFT.items() if k != "test_name"}
        assert draft_to_catalog_test(draft) is None

    def test_empty_test_name_returns_none(self):
        draft = {**_VALID_DRAFT, "test_name": ""}
        assert draft_to_catalog_test(draft) is None

    def test_whitespace_test_name_returns_none(self):
        draft = {**_VALID_DRAFT, "test_name": "   "}
        assert draft_to_catalog_test(draft) is None

    def test_missing_steps_returns_none(self):
        draft = {k: v for k, v in _VALID_DRAFT.items() if k != "steps"}
        assert draft_to_catalog_test(draft) is None

    def test_empty_steps_list_returns_none(self):
        draft = {**_VALID_DRAFT, "steps": []}
        assert draft_to_catalog_test(draft) is None

    def test_non_list_steps_returns_none(self):
        draft = {**_VALID_DRAFT, "steps": "not a list"}
        assert draft_to_catalog_test(draft) is None

    def test_none_input_returns_none(self):
        assert draft_to_catalog_test(None) is None

    def test_non_dict_input_returns_none(self):
        assert draft_to_catalog_test("bad") is None
        assert draft_to_catalog_test(42)    is None


# ══════════════════════════════════════════════════════════════
# 6. approve_drafts — output contract
# ══════════════════════════════════════════════════════════════

class TestApproveDraftsContract:

    def test_returns_list(self):
        assert isinstance(approve_drafts([]), list)

    def test_empty_list_returns_empty(self):
        assert approve_drafts([]) == []

    def test_none_input_returns_empty(self):
        assert approve_drafts(None) == []

    def test_non_list_input_returns_empty(self):
        assert approve_drafts("bad") == []

    def test_valid_draft_approved(self):
        result = approve_drafts([_VALID_DRAFT])
        assert len(result) == 1
        assert result[0]["test_id"] == "login_form_valid_submission"

    def test_multiple_valid_drafts(self):
        result = approve_drafts([_VALID_DRAFT, _MISSING_FIELD_DRAFT, _NAV_DRAFT])
        assert len(result) == 3

    def test_all_approved_have_status_active(self):
        result = approve_drafts([_VALID_DRAFT, _SEARCH_DRAFT, _NAV_DRAFT])
        assert all(r["status"] == "active" for r in result)


# ══════════════════════════════════════════════════════════════
# 7. approve_drafts — deduplication
# ══════════════════════════════════════════════════════════════

class TestDeduplication:

    def test_duplicate_test_names_deduplicated(self):
        result = approve_drafts([_VALID_DRAFT, _VALID_DRAFT, _VALID_DRAFT])
        assert len(result) == 1

    def test_first_occurrence_wins(self):
        draft_a = {**_VALID_DRAFT, "priority": "high"}
        draft_b = {**_VALID_DRAFT, "priority": "low"}
        result = approve_drafts([draft_a, draft_b])
        assert result[0]["priority"] == "high"

    def test_different_test_names_not_deduplicated(self):
        result = approve_drafts([_VALID_DRAFT, _MISSING_FIELD_DRAFT])
        assert len(result) == 2
        ids = {r["test_id"] for r in result}
        assert "login_form_valid_submission" in ids
        assert "login_form_missing_password" in ids

    def test_non_dict_items_skipped(self):
        result = approve_drafts([None, "bad", 42, _VALID_DRAFT])
        assert len(result) == 1

    def test_invalid_drafts_excluded_from_batch(self):
        no_name  = {**_VALID_DRAFT, "test_name": ""}
        no_steps = {**_VALID_DRAFT, "steps": []}
        result   = approve_drafts([no_name, no_steps, _NAV_DRAFT])
        assert len(result) == 1
        assert result[0]["test_id"] == "navigation_smoke"


# ══════════════════════════════════════════════════════════════
# 8. Full batch round-trip
# ══════════════════════════════════════════════════════════════

class TestFullBatch:

    _ALL = [_VALID_DRAFT, _MISSING_FIELD_DRAFT, _SEARCH_DRAFT, _NAV_DRAFT]

    def setup_method(self):
        self.result  = approve_drafts(self._ALL)
        self.by_id   = {r["test_id"]: r for r in self.result}

    def test_four_drafts_approved(self):
        assert len(self.result) == 4

    def test_all_active(self):
        assert all(r["status"] == "active" for r in self.result)

    def test_types_correctly_mapped(self):
        assert self.by_id["login_form_valid_submission"]["type"]  == "functional"
        assert self.by_id["login_form_missing_password"]["type"]  == "negative"
        assert self.by_id["search_empty_search"]["type"]          == "functional"
        assert self.by_id["navigation_smoke"]["type"]             == "smoke"

    def test_all_have_meta_draft_true(self):
        assert all(r["meta"]["draft"] is True for r in self.result)

    def test_no_draft_status_in_output(self):
        assert not any(r["status"] == "draft" for r in self.result)

    def test_steps_non_empty_for_all(self):
        assert all(len(r["steps"]) > 0 for r in self.result)
