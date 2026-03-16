# tests/test_queue_guardrails.py
"""
Tests for services/queue.py production guard:
  RuntimeError when REDIS_URL is absent in prod environment.
"""
import os
import sys

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


class TestQueueProductionGuard:
    def test_local_queue_allowed_in_dev(self):
        """Importing queue in non-prod without REDIS_URL must not raise."""
        env_backup = os.environ.copy()
        try:
            os.environ.pop("REDIS_URL", None)
            os.environ["ENV"] = "dev"
            sys.modules.pop("services.queue", None)
            import services.queue  # must not raise
        finally:
            os.environ.clear()
            os.environ.update(env_backup)
            sys.modules.pop("services.queue", None)

    def test_local_queue_raises_in_prod(self):
        """Importing queue in prod without REDIS_URL must raise RuntimeError."""
        env_backup = os.environ.copy()
        try:
            os.environ.pop("REDIS_URL", None)
            os.environ["ENV"] = "prod"
            sys.modules.pop("services.queue", None)
            with pytest.raises(RuntimeError, match="REDIS_URL"):
                import services.queue
        finally:
            os.environ.clear()
            os.environ.update(env_backup)
            sys.modules.pop("services.queue", None)

    def test_redis_url_set_does_not_raise_in_prod(self):
        """In prod with REDIS_URL set, no RuntimeError (connection is lazy)."""
        env_backup = os.environ.copy()
        try:
            os.environ["REDIS_URL"] = "redis://localhost:6379"
            os.environ["ENV"] = "prod"
            sys.modules.pop("services.queue", None)
            try:
                import services.queue
            except Exception as e:
                assert "REDIS_URL" not in str(e), f"Unexpected RuntimeError: {e}"
        finally:
            os.environ.clear()
            os.environ.update(env_backup)
            sys.modules.pop("services.queue", None)
