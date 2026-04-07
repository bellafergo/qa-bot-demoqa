# tests/test_email_domain_policy.py
"""ALLOWED_EMAIL_DOMAINS parsing and check_email_domain_policy behavior."""
from __future__ import annotations

import pytest

from core.vanya_auth import (
    allowed_email_domains,
    check_email_domain_policy,
    extract_user_email_from_claims,
)


def test_allowed_domains_empty_env_no_restriction(monkeypatch):
    monkeypatch.delenv("ALLOWED_EMAIL_DOMAINS", raising=False)
    assert allowed_email_domains() == frozenset()
    ok, msg = check_email_domain_policy("anyone@gmail.com", frozenset())
    assert ok is True
    assert msg == ""


def test_allowed_domains_parsing_trim_and_case_insensitive(monkeypatch):
    monkeypatch.setenv(
        "ALLOWED_EMAIL_DOMAINS",
        " fahorro.com.mx , Zuperio.COM.MX ,  ",
    )
    d = allowed_email_domains()
    assert d == frozenset({"fahorro.com.mx", "zuperio.com.mx"})


def test_check_policy_allows_matching_domain(monkeypatch):
    monkeypatch.setenv("ALLOWED_EMAIL_DOMAINS", "fahorro.com.mx,zuperio.com.mx")
    domains = allowed_email_domains()
    ok, msg = check_email_domain_policy("Jesus.Martinez@FAHORRO.COM.MX", domains)
    assert ok is True
    assert msg == ""


def test_check_policy_denies_gmail(monkeypatch):
    monkeypatch.setenv("ALLOWED_EMAIL_DOMAINS", "fahorro.com.mx,zuperio.com.mx")
    domains = allowed_email_domains()
    ok, msg = check_email_domain_policy("alguien@gmail.com", domains)
    assert ok is False
    assert "not authorized" in msg.lower()
    assert "gmail.com" in msg


def test_check_policy_denies_missing_email(monkeypatch):
    monkeypatch.setenv("ALLOWED_EMAIL_DOMAINS", "zuperio.com.mx")
    domains = allowed_email_domains()
    ok, msg = check_email_domain_policy(None, domains)
    assert ok is False
    assert "email" in msg.lower()


def test_check_policy_denies_malformed_email(monkeypatch):
    monkeypatch.setenv("ALLOWED_EMAIL_DOMAINS", "zuperio.com.mx")
    domains = allowed_email_domains()
    ok, msg = check_email_domain_policy("not-an-email", domains)
    assert ok is False
    assert "valid email" in msg.lower()


def test_extract_email_from_claims_root_then_metadata():
    assert extract_user_email_from_claims({"email": "  a@b.co  "}) == "a@b.co"
    assert (
        extract_user_email_from_claims(
            {"user_metadata": {"email": "meta@zuperio.com.mx"}},
        )
        == "meta@zuperio.com.mx"
    )
    assert (
        extract_user_email_from_claims(
            {
                "email": "root@zuperio.com.mx",
                "user_metadata": {"email": "ignored@other.com"},
            },
        )
        == "root@zuperio.com.mx"
    )
