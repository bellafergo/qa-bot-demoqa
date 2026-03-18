#!/usr/bin/env python
"""
Ensure POS desktop test cases exist in the Vanya catalog and validate them.

Usage (from project root):
    .venv/bin/python scripts/add_pos_tests.py [--dry-run] [--update]

Options:
    --dry-run   List existing catalog without inserting anything.
    --update    Force-update existing POS test cases with current config values.
                Use this after editing config/pos_desktop.py.
"""
import sys
import os

# Allow running from project root
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

POS_IDS      = ["TC-POS-001", "TC-POS-002", "TC-POS-003"]
DRY_RUN      = "--dry-run" in sys.argv
FORCE_UPDATE = "--update"  in sys.argv


def main() -> None:
    from services.test_catalog_seed import ensure_pos_seed
    from services.test_catalog_service import catalog_service

    print("=" * 60)
    print("Vanya — POS Desktop Test Case Loader")
    print("=" * 60)

    # ── Insert (idempotent) ────────────────────────────────────────────────────
    if not DRY_RUN:
        result = ensure_pos_seed(force_update=FORCE_UPDATE)
        print(
            f"\n[{'update' if FORCE_UPDATE else 'insert'}] "
            f"created={result['created']}  updated={result.get('updated', 0)}  skipped={result['skipped']}"
        )
    else:
        print("\n[dry-run] skipping insert")

    # ── List all test cases ────────────────────────────────────────────────────
    print("\n── All test cases in catalog ─────────────────────────────────")
    all_tests = catalog_service.list_test_cases()
    if not all_tests:
        print("  (catalog is empty)")
    else:
        fmt = "  {id:<18} {type_:<12} {tt:<10} {prio:<10} {mod}"
        print(fmt.format(id="test_case_id", type_="type", tt="test_type",
                         prio="priority", mod="module"))
        print("  " + "-" * 56)
        for tc in all_tests:
            print(fmt.format(
                id=tc.test_case_id,
                type_=tc.type,
                tt=getattr(tc, "test_type", "ui"),
                prio=tc.priority,
                mod=tc.module,
            ))

    # ── Confirm POS test cases ─────────────────────────────────────────────────
    print("\n── POS test cases verification ───────────────────────────────")
    all_found = True
    for tc_id in POS_IDS:
        tc = catalog_service.get_test_case(tc_id)
        if tc:
            print(f"  ✓ {tc_id}  «{tc.name}»  [{getattr(tc, 'test_type', 'ui')}]")
        else:
            print(f"  ✗ {tc_id}  NOT FOUND")
            all_found = False

    if not all_found:
        print("\n[WARN] Some POS test cases are missing.")
        sys.exit(1)

    # ── Dry-run execution example for TC-POS-001 ──────────────────────────────
    print("\n── Execution example (TC-POS-001, mock) ──────────────────────")
    os.environ.setdefault("DESKTOP_RUNNER_MOCK", "1")
    from services.test_catalog_service import TestCatalogService
    from services.test_catalog_service import _build_desktop_steps        # noqa: PLC2701
    from runners.desktop_runner import run_desktop_test

    tc = catalog_service.get_test_case("TC-POS-001")
    steps = _build_desktop_steps(tc)
    print(f"  steps to execute ({len(steps)}):")
    for i, s in enumerate(steps, 1):
        tgt = s.get("target", "")
        val = s.get("value", "")
        print(f"    {i}. {s['action']:<24} target={tgt!r:<30} value={val!r}")

    print("\n  running in mock mode …")
    result = run_desktop_test(steps=steps, timeout_s=10)
    status = result.get("status", "?")
    ok = result.get("ok", False)
    shots = len(result.get("evidence", {}).get("screenshots", []))
    print(f"  result: status={status}  ok={ok}  screenshots={shots}")

    print("\n[OK] POS test cases ready.\n")


if __name__ == "__main__":
    main()
