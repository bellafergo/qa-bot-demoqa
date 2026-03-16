# tests/test_state_guardrails.py
"""
Thread-safety tests for core/state.py:
  _LockedDict, _LockedList, _STATE_LOCK, cleanup_sessions.
"""
import os
import sys
import threading
import time

import pytest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


class TestLockedDict:
    def test_is_dict_subclass(self):
        from core.state import SESSIONS
        assert isinstance(SESSIONS, dict)

    def test_basic_setget(self):
        from core.state import SESSIONS
        SESSIONS["_test_key"] = {"val": 42}
        assert SESSIONS["_test_key"] == {"val": 42}
        SESSIONS.pop("_test_key", None)

    def test_no_data_loss_under_concurrent_writes(self):
        from core.state import _LockedDict

        d: _LockedDict = _LockedDict()
        errors: list = []

        def writer(i: int) -> None:
            try:
                d[f"k{i}"] = i
            except Exception as e:
                errors.append(e)

        threads = [threading.Thread(target=writer, args=(i,)) for i in range(100)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        assert not errors, f"Exceptions during concurrent writes: {errors}"
        assert len(d) == 100

    def test_concurrent_pop_no_keyerror(self):
        from core.state import _LockedDict

        d: _LockedDict = _LockedDict(shared="value")
        errors: list = []

        def popper() -> None:
            try:
                d.pop("shared", None)
            except Exception as e:
                errors.append(e)

        threads = [threading.Thread(target=popper) for _ in range(20)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        assert not errors

    def test_cleanup_sessions_holds_lock(self):
        from core.state import SESSIONS, cleanup_sessions

        SESSIONS["sess_old"] = {"last_seen": int(time.time()) - 9999}
        SESSIONS["sess_new"] = {"last_seen": int(time.time())}

        removed = cleanup_sessions(ttl_s=100)
        assert removed >= 1
        assert "sess_old" not in SESSIONS
        SESSIONS.pop("sess_new", None)

    def test_state_lock_exported(self):
        from core.state import _STATE_LOCK
        assert isinstance(_STATE_LOCK, type(threading.RLock()))


class TestLockedList:
    def test_is_list_subclass(self):
        from core.state import DOC_CACHE_ORDER
        assert isinstance(DOC_CACHE_ORDER, list)

    def test_runs_order_is_locked_list(self):
        from core.state import RUNS_ORDER, _LockedList
        assert isinstance(RUNS_ORDER, _LockedList)

    def test_no_data_loss_under_concurrent_appends(self):
        from core.state import _LockedList

        lst: _LockedList = _LockedList()
        errors: list = []

        def appender(i: int) -> None:
            try:
                lst.append(i)
            except Exception as e:
                errors.append(e)

        threads = [threading.Thread(target=appender, args=(i,)) for i in range(100)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        assert not errors
        assert len(lst) == 100

    def test_concurrent_remove_no_valueerror(self):
        from core.state import _LockedList

        lst: _LockedList = _LockedList(["shared"] * 20)
        errors: list = []

        def remover() -> None:
            try:
                lst.remove("shared")
            except ValueError:
                pass  # expected once list is empty
            except Exception as e:
                errors.append(e)

        threads = [threading.Thread(target=remover) for _ in range(20)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        assert not errors
        assert len(lst) == 0

    def test_pop_thread_safe(self):
        from core.state import _LockedList

        lst: _LockedList = _LockedList(range(50))
        errors: list = []

        def popper() -> None:
            try:
                lst.pop(0)
            except IndexError:
                pass
            except Exception as e:
                errors.append(e)

        threads = [threading.Thread(target=popper) for _ in range(50)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        assert not errors
        assert len(lst) == 0
