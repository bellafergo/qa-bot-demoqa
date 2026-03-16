# tests/test_application_explorer.py
"""
Tests for services/application_explorer.py  (analyze_html — pure function).

No browser is required; all tests feed static HTML strings to analyze_html().

Run: .venv/bin/python -m pytest tests/test_application_explorer.py -v
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.application_explorer import analyze_html


# ══════════════════════════════════════════════════════════════
# 1. Output contract
# ══════════════════════════════════════════════════════════════

class TestContract:

    def test_returns_dict(self):
        assert isinstance(analyze_html(""), dict)

    def test_has_required_keys(self):
        result = analyze_html("")
        for key in ("url", "title", "inputs", "buttons", "links", "forms"):
            assert key in result, f"missing key: {key}"

    def test_empty_html_returns_empty_collections(self):
        r = analyze_html("")
        assert r["inputs"]  == []
        assert r["buttons"] == []
        assert r["links"]   == []
        assert r["forms"]   == []

    def test_url_passthrough(self):
        r = analyze_html("", url="https://example.com")
        assert r["url"] == "https://example.com"

    def test_title_fallback_when_no_title_tag(self):
        r = analyze_html("<html></html>", title="Fallback Title")
        assert r["title"] == "Fallback Title"

    def test_title_from_html_overrides_fallback(self):
        r = analyze_html("<title>Real Title</title>", title="Fallback")
        assert r["title"] == "Real Title"

    def test_none_html_safe(self):
        r = analyze_html(None)
        assert isinstance(r, dict)


# ══════════════════════════════════════════════════════════════
# 2. Title
# ══════════════════════════════════════════════════════════════

class TestTitle:

    def test_extracts_title(self):
        r = analyze_html("<html><head><title>Swag Labs</title></head></html>")
        assert r["title"] == "Swag Labs"

    def test_title_whitespace_stripped(self):
        r = analyze_html("<title>  Padded Title  </title>")
        assert r["title"] == "Padded Title"

    def test_empty_title_tag(self):
        r = analyze_html("<title></title>", title="Fallback")
        assert r["title"] == "Fallback"


# ══════════════════════════════════════════════════════════════
# 3. Inputs
# ══════════════════════════════════════════════════════════════

class TestInputs:

    def test_text_input_by_id(self):
        html = '<input type="text" id="user-name" name="username">'
        r = analyze_html(html)
        assert len(r["inputs"]) == 1
        assert r["inputs"][0]["name"] == "user-name"
        assert r["inputs"][0]["selector"] == "#user-name"

    def test_input_selector_uses_name_when_no_id(self):
        html = '<input type="text" name="username">'
        r = analyze_html(html)
        assert r["inputs"][0]["selector"] == 'input[name="username"]'

    def test_input_name_falls_back_to_placeholder(self):
        html = '<input type="text" placeholder="Enter email">'
        r = analyze_html(html)
        assert r["inputs"][0]["name"] == "Enter email"

    def test_input_name_falls_back_to_aria_label(self):
        html = '<input type="text" aria-label="Search box">'
        r = analyze_html(html)
        assert r["inputs"][0]["name"] == "Search box"

    def test_password_input_included(self):
        html = '<input type="password" id="password">'
        r = analyze_html(html)
        assert len(r["inputs"]) == 1
        assert r["inputs"][0]["selector"] == "#password"

    def test_hidden_input_excluded(self):
        html = '<input type="hidden" name="csrf_token"><input type="text" id="q">'
        r = analyze_html(html)
        assert len(r["inputs"]) == 1
        assert r["inputs"][0]["selector"] == "#q"

    def test_multiple_inputs(self):
        html = '''
            <input type="text"     id="user-name">
            <input type="password" id="password">
        '''
        r = analyze_html(html)
        assert len(r["inputs"]) == 2
        ids = [inp["selector"] for inp in r["inputs"]]
        assert "#user-name" in ids
        assert "#password"  in ids

    def test_input_without_id_name_placeholder_gets_fallback(self):
        html = '<input type="text">'
        r = analyze_html(html)
        assert r["inputs"][0]["name"] != ""


# ══════════════════════════════════════════════════════════════
# 4. Buttons
# ══════════════════════════════════════════════════════════════

class TestButtons:

    def test_button_element_by_id(self):
        html = '<button id="login-button">Login</button>'
        r = analyze_html(html)
        assert len(r["buttons"]) == 1
        assert r["buttons"][0]["name"]     == "Login"
        assert r["buttons"][0]["selector"] == "#login-button"

    def test_button_selector_uses_name_when_no_id(self):
        html = '<button name="submit-btn">Submit</button>'
        r = analyze_html(html)
        assert r["buttons"][0]["selector"] == 'button[name="submit-btn"]'

    def test_button_text_is_name(self):
        html = '<button>Sign In</button>'
        r = analyze_html(html)
        assert r["buttons"][0]["name"] == "Sign In"

    def test_input_submit_treated_as_button(self):
        html = '<input type="submit" value="Login" id="login-button">'
        r = analyze_html(html)
        assert len(r["buttons"]) == 1
        assert r["buttons"][0]["name"]     == "Login"
        assert r["buttons"][0]["selector"] == "#login-button"

    def test_input_button_type_treated_as_button(self):
        html = '<input type="button" value="Click me">'
        r = analyze_html(html)
        assert len(r["buttons"]) == 1
        assert r["buttons"][0]["name"] == "Click me"

    def test_button_aria_label_used_when_no_text(self):
        html = '<button aria-label="Close dialog"></button>'
        r = analyze_html(html)
        assert r["buttons"][0]["name"] == "Close dialog"

    def test_button_fallback_when_no_text_no_attrs(self):
        html = '<button></button>'
        r = analyze_html(html)
        assert r["buttons"][0]["name"] != ""

    def test_hidden_input_not_treated_as_button(self):
        html = '<input type="hidden" name="csrf"><input type="submit" value="Go">'
        r = analyze_html(html)
        assert len(r["buttons"]) == 1


# ══════════════════════════════════════════════════════════════
# 5. Links
# ══════════════════════════════════════════════════════════════

class TestLinks:

    def test_link_text_and_selector(self):
        html = '<a href="/forgot-password">Forgot password?</a>'
        r = analyze_html(html)
        assert len(r["links"]) == 1
        assert r["links"][0]["text"]     == "Forgot password?"
        assert r["links"][0]["selector"] == 'a[href="/forgot-password"]'

    def test_link_with_id_uses_id_selector(self):
        html = '<a href="/register" id="register-link">Register</a>'
        r = analyze_html(html)
        assert r["links"][0]["selector"] == "#register-link"

    def test_javascript_link_excluded(self):
        html = '<a href="javascript:void(0)">Click</a><a href="/real">Real</a>'
        r = analyze_html(html)
        assert len(r["links"]) == 1
        assert r["links"][0]["text"] == "Real"

    def test_mailto_link_excluded(self):
        html = '<a href="mailto:help@example.com">Email us</a>'
        r = analyze_html(html)
        assert r["links"] == []

    def test_anchor_only_href_excluded(self):
        # pure #anchor links have no navigational value
        html = '<a href="#top">Back to top</a>'
        r = analyze_html(html)
        assert r["links"] == []

    def test_empty_href_excluded(self):
        html = '<a href="">Nothing</a>'
        r = analyze_html(html)
        assert r["links"] == []

    def test_multiple_links(self):
        html = '''
            <a href="/home">Home</a>
            <a href="/about">About</a>
            <a href="/contact">Contact</a>
        '''
        r = analyze_html(html)
        assert len(r["links"]) == 3

    def test_link_text_whitespace_collapsed(self):
        html = '<a href="/page">  Spaced   Text  </a>'
        r = analyze_html(html)
        assert r["links"][0]["text"] == "Spaced Text"


# ══════════════════════════════════════════════════════════════
# 6. Forms
# ══════════════════════════════════════════════════════════════

class TestForms:

    _LOGIN_FORM = '''
        <form id="login-form">
            <input type="text"     id="user-name">
            <input type="password" id="password">
            <input type="submit"   value="Login" id="login-button">
        </form>
    '''

    def test_form_detected(self):
        r = analyze_html(self._LOGIN_FORM)
        assert len(r["forms"]) == 1

    def test_form_name_from_id(self):
        r = analyze_html(self._LOGIN_FORM)
        assert r["forms"][0]["name"] == "login-form"

    def test_form_fields_contains_input_names(self):
        r = analyze_html(self._LOGIN_FORM)
        fields = r["forms"][0]["fields"]
        assert "user-name" in fields
        assert "password"  in fields

    def test_form_buttons_contains_button_names(self):
        r = analyze_html(self._LOGIN_FORM)
        assert "Login" in r["forms"][0]["buttons"]

    def test_form_name_falls_back_to_name_attr(self):
        html = '<form name="signup"><input type="text" id="email"></form>'
        r = analyze_html(html)
        assert r["forms"][0]["name"] == "signup"

    def test_form_name_falls_back_to_action_segment(self):
        html = '<form action="/auth/login"><input type="text" id="q"></form>'
        r = analyze_html(html)
        assert r["forms"][0]["name"] == "login"

    def test_form_fallback_name_when_no_attrs(self):
        html = '<form><input type="text" id="q"></form>'
        r = analyze_html(html)
        assert r["forms"][0]["name"].startswith("form_")

    def test_inputs_also_appear_in_top_level_inputs(self):
        r = analyze_html(self._LOGIN_FORM)
        selectors = [i["selector"] for i in r["inputs"]]
        assert "#user-name" in selectors
        assert "#password"  in selectors

    def test_page_without_forms(self):
        html = '''
            <input type="text" id="q">
            <button>Search</button>
        '''
        r = analyze_html(html)
        assert r["forms"] == []

    def test_multiple_forms(self):
        html = '''
            <form id="form-login"><input type="text" id="u"></form>
            <form id="form-register"><input type="text" id="e"></form>
        '''
        r = analyze_html(html)
        assert len(r["forms"]) == 2
        names = {f["name"] for f in r["forms"]}
        assert names == {"form-login", "form-register"}


# ══════════════════════════════════════════════════════════════
# 7. Full-page integration (SauceDemo-like HTML)
# ══════════════════════════════════════════════════════════════

class TestFullPage:

    _SAUCEDEMO = '''
        <html>
        <head><title>Swag Labs</title></head>
        <body>
            <form id="login_form" action="/login">
                <input type="text"     id="user-name"     name="user-name"  placeholder="Username">
                <input type="password" id="password"      name="password"   placeholder="Password">
                <input type="submit"   id="login-button"  value="Login">
            </form>
            <a href="/forgot-password" class="forgot-password">Forgot password</a>
        </body>
        </html>
    '''

    def test_title_extracted(self):
        r = analyze_html(self._SAUCEDEMO)
        assert r["title"] == "Swag Labs"

    def test_two_inputs_found(self):
        r = analyze_html(self._SAUCEDEMO)
        assert len(r["inputs"]) == 2

    def test_login_button_found(self):
        r = analyze_html(self._SAUCEDEMO)
        assert any(b["name"] == "Login" for b in r["buttons"])

    def test_forgot_password_link_found(self):
        r = analyze_html(self._SAUCEDEMO)
        assert any("Forgot" in lnk["text"] for lnk in r["links"])

    def test_form_links_inputs_and_buttons(self):
        r = analyze_html(self._SAUCEDEMO)
        f = r["forms"][0]
        assert "user-name" in f["fields"]
        assert "password"  in f["fields"]
        assert "Login"     in f["buttons"]


# ══════════════════════════════════════════════════════════════
# 8. Edge cases / robustness
# ══════════════════════════════════════════════════════════════

class TestEdgeCases:

    def test_malformed_html_does_not_raise(self):
        r = analyze_html("<div><input unclosed</div><<>")
        assert isinstance(r, dict)

    def test_deeply_nested_inputs_detected(self):
        html = '<div><section><form id="f"><div><input type="text" id="q"></div></form></section></div>'
        r = analyze_html(html)
        assert len(r["inputs"]) == 1
        assert r["inputs"][0]["selector"] == "#q"

    def test_image_input_excluded(self):
        html = '<input type="image" src="submit.png">'
        r = analyze_html(html)
        assert r["inputs"]  == []
        assert r["buttons"] == []

    def test_link_aria_label_used_when_no_text(self):
        html = '<a href="/home" aria-label="Go home"></a>'
        r = analyze_html(html)
        assert r["links"][0]["text"] == "Go home"

    def test_select_and_textarea_not_in_buttons(self):
        html = '<select id="s"><option>A</option></select><textarea id="t"></textarea>'
        r = analyze_html(html)
        assert r["buttons"] == []
