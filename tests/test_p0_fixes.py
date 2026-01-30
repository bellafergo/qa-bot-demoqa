# tests/test_p0_fixes.py
"""
Sanity checks for P0-2 and P0-3 fixes.
Run with: python -m pytest tests/test_p0_fixes.py -v
Or standalone: python tests/test_p0_fixes.py
"""
import os
import sys

# Add parent to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def test_p0_2_create_thread_returns_correct_shape():
    """
    P0-2: create_thread should return {"id": str, "title": str, "updated_at": ...}
    NOT {"id": {"id": ..., "title": ...}, ...}
    """
    from services.store import create_thread

    result = create_thread("Test Thread P0-2")

    # Verify shape
    assert isinstance(result, dict), "create_thread must return dict"
    assert "id" in result, "result must have 'id' key"
    assert "title" in result, "result must have 'title' key"

    # P0-2 FIX: id must be a string, NOT a nested dict
    assert isinstance(result["id"], str), f"id must be str, got {type(result['id'])}"
    assert result["id"] != "", "id must not be empty"

    # Title should match or be normalized
    assert isinstance(result["title"], str), "title must be str"

    print(f"[PASS] P0-2: create_thread returned valid shape: id={result['id'][:8]}...")


def test_p0_3_sb_get_thread_exists():
    """
    P0-3: sb_get_thread should be importable from supabase_store.
    """
    from services.supabase_store import sb_get_thread

    assert callable(sb_get_thread), "sb_get_thread must be callable"
    print("[PASS] P0-3: sb_get_thread is importable and callable")


def test_p0_3_sb_get_thread_returns_none_for_missing():
    """
    P0-3: sb_get_thread should return None for non-existent thread.
    """
    from services.supabase_store import sb_get_thread

    result = sb_get_thread("nonexistent-thread-id-12345")

    # Should return None (not raise), whether Supabase is configured or not
    assert result is None, f"Expected None for missing thread, got {result}"
    print("[PASS] P0-3: sb_get_thread returns None for missing thread")


def test_p0_3_get_thread_roundtrip():
    """
    P0-3: get_thread should work for created threads.
    This test creates a thread and retrieves it.
    """
    from services.store import create_thread, get_thread

    # Create a thread first
    created = create_thread("Test Thread P0-3 Roundtrip")
    thread_id = created["id"]

    try:
        # Retrieve it
        fetched = get_thread(thread_id)

        assert fetched is not None, "get_thread should return the created thread"
        assert fetched["id"] == thread_id, "fetched id must match"
        assert "messages" in fetched, "get_thread must return messages array"
        assert isinstance(fetched["messages"], list), "messages must be a list"

        print(f"[PASS] P0-3: get_thread roundtrip works (Postgres via SQLAlchemy)")

    except KeyError:
        print(f"[FAIL] P0-3: get_thread raised KeyError - thread not found after create")
        raise


def test_response_backward_compatible():
    """
    Verify API response shape matches UI contract:
    {"id": str, "title": str, "updated_at": str|None}
    """
    from services.store import create_thread

    result = create_thread("Backward Compat Test")

    required_keys = {"id", "title", "updated_at"}
    actual_keys = set(result.keys())

    # Must have at least these keys (may have more)
    missing = required_keys - actual_keys
    assert not missing, f"Missing required keys: {missing}"

    # Types
    assert isinstance(result["id"], str)
    assert isinstance(result["title"], str)
    assert result["updated_at"] is None or isinstance(result["updated_at"], str)

    print("[PASS] Response shape is backward compatible")


if __name__ == "__main__":
    print("=" * 60)
    print("Running P0-2 and P0-3 sanity checks...")
    print("=" * 60)

    tests = [
        test_p0_2_create_thread_returns_correct_shape,
        test_p0_3_sb_get_thread_exists,
        test_p0_3_sb_get_thread_returns_none_for_missing,
        test_p0_3_get_thread_roundtrip,
        test_response_backward_compatible,
    ]

    passed = 0
    failed = 0

    for test_fn in tests:
        try:
            test_fn()
            passed += 1
        except Exception as e:
            failed += 1
            print(f"[FAIL] {test_fn.__name__}: {e}")

    print("=" * 60)
    print(f"Results: {passed} passed, {failed} failed")
    print("=" * 60)

    sys.exit(0 if failed == 0 else 1)
