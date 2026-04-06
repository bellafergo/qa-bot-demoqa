# tests/test_target_url_validation.py
"""Unit tests for SSRF-safe target URL validation (mocked DNS where needed)."""
from __future__ import annotations

import os
from unittest.mock import patch

import pytest

from core.target_url_validation import TargetURLNotAllowed, validate_target_url


def _public_gai(*_args, **_kwargs):
    return [(None, None, None, None, ("93.184.216.34", 0))]


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
@patch("socket.getaddrinfo", side_effect=_public_gai)
def test_allows_https_public_when_dns_ok(_mock_gai):
    assert validate_target_url("https://example.com/path").startswith("https://example.com")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
def test_blocks_localhost_literal():
    with pytest.raises(TargetURLNotAllowed, match="Target URL not allowed"):
        validate_target_url("http://localhost:3000/")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
def test_blocks_private_ipv4_literal():
    with pytest.raises(TargetURLNotAllowed, match="Target URL not allowed"):
        validate_target_url("http://192.168.1.10/")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
def test_blocks_loopback_ipv4_literal():
    with pytest.raises(TargetURLNotAllowed, match="Target URL not allowed"):
        validate_target_url("http://127.0.0.1:8080/x")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
def test_blocks_link_local_literal():
    with pytest.raises(TargetURLNotAllowed, match="Target URL not allowed"):
        validate_target_url("http://169.254.169.254/latest/meta-data/")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
def test_blocks_non_http_scheme():
    with pytest.raises(TargetURLNotAllowed, match="Target URL not allowed"):
        validate_target_url("file:///etc/passwd")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": "example.com"}, clear=False)
@patch("socket.getaddrinfo", side_effect=_public_gai)
def test_allowlist_permits_suffix(_mock_gai):
    validate_target_url("https://sub.example.com/foo")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": "example.com"}, clear=False)
def test_allowlist_blocks_other_host():
    with pytest.raises(TargetURLNotAllowed, match="Target URL not allowed"):
        validate_target_url("https://evil.com/")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
@patch("socket.getaddrinfo", return_value=[(None, None, None, None, ("10.0.0.1", 0))])
def test_blocks_when_dns_resolves_private(_mock_gai):
    with pytest.raises(TargetURLNotAllowed, match="Target URL not allowed"):
        validate_target_url("https://phishing-example.test/")


@patch.dict(os.environ, {"ALLOWED_TARGET_HOSTS": ""}, clear=False)
@patch("socket.getaddrinfo", side_effect=_public_gai)
def test_relative_resolved_with_base(_mock_gai):
    u = validate_target_url("/login", base_url="https://demoqa.com/")
    assert "demoqa.com" in u
    assert "/login" in u
