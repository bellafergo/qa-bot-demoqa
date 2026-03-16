# tests/test_execute_guardrails.py
"""
Tests for api/routes/execute.py request validation guardrails:
  max steps, base_url format/length, timeout_s cap.
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


class TestExecuteStepsRequestValidation:
    def _make_req(self, **kwargs):
        from api.routes.execute import ExecuteStepsRequest
        return ExecuteStepsRequest(**kwargs)

    def test_valid_request_passes(self):
        req = self._make_req(
            steps=[{"action": "goto", "url": "https://example.com"}],
            base_url="https://example.com",
            timeout_s=60,
        )
        assert len(req.steps) == 1

    def test_too_many_steps_rejected(self):
        from pydantic import ValidationError
        with pytest.raises(ValidationError, match="maximum length"):
            self._make_req(steps=[{"action": "wait_ms"} for _ in range(201)])

    def test_exactly_max_steps_allowed(self):
        req = self._make_req(steps=[{"action": "wait_ms"} for _ in range(200)])
        assert len(req.steps) == 200

    def test_bad_url_scheme_rejected(self):
        from pydantic import ValidationError
        with pytest.raises(ValidationError, match="http"):
            self._make_req(
                steps=[{"action": "goto"}],
                base_url="ftp://evil.com/path",
            )

    def test_javascript_url_rejected(self):
        from pydantic import ValidationError
        with pytest.raises(ValidationError, match="http"):
            self._make_req(
                steps=[{"action": "goto"}],
                base_url="javascript:alert(1)",
            )

    def test_url_too_long_rejected(self):
        from pydantic import ValidationError
        with pytest.raises(ValidationError, match="maximum length"):
            self._make_req(
                steps=[{"action": "goto"}],
                base_url="https://example.com/" + "a" * 2100,
            )

    def test_timeout_too_high_rejected(self):
        from pydantic import ValidationError
        with pytest.raises(ValidationError, match="maximum"):
            self._make_req(
                steps=[{"action": "goto"}],
                timeout_s=301,
            )

    def test_timeout_at_max_allowed(self):
        req = self._make_req(steps=[{"action": "goto"}], timeout_s=300)
        assert req.timeout_s == 300

    def test_none_url_allowed(self):
        req = self._make_req(steps=[{"action": "goto"}], base_url=None)
        assert req.base_url is None

    def test_none_timeout_allowed(self):
        req = self._make_req(steps=[{"action": "goto"}], timeout_s=None)
        assert req.timeout_s is None
