#!/usr/bin/env python
# scripts/backfill_test_versions.py
"""
Backfill v1 snapshots in test_versions for every test_case that has no history yet.

Safe to run multiple times — skips any test that already has at least one version.
Never modifies test_cases, never overwrites existing version rows.

Usage:
    .venv/bin/python scripts/backfill_test_versions.py
"""
from __future__ import annotations

import sys
import os

# Allow running from the repo root without installing the package
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.db.init_db import init_catalog_db
from services.db.catalog_repository import catalog_repo
from services.db.test_version_repository import test_version_repo


def run() -> None:
    print("[BACKFILL] initializing database …")
    init_catalog_db()

    # Fetch all test cases regardless of status
    all_tests = catalog_repo.list_test_cases(status=None, limit=10_000)
    total   = len(all_tests)
    created = 0
    skipped = 0
    failed  = 0

    print(f"[BACKFILL] found {total} test case(s) to inspect")

    for tc in all_tests:
        try:
            existing = test_version_repo.list_versions(tc.test_case_id)
            if existing:
                print(f"[BACKFILL] skipped  {tc.test_case_id}  (already has {len(existing)} version(s))")
                skipped += 1
                continue

            test_version_repo.snapshot(
                tc,
                version_number = 1,
                source         = "backfill",
                change_note    = "Backfilled initial version",
            )
            print(f"[BACKFILL] created v1 for {tc.test_case_id}  ({tc.name})")
            created += 1

        except Exception as exc:
            print(f"[BACKFILL] ERROR   {tc.test_case_id}: {exc}", file=sys.stderr)
            failed += 1

    print()
    print(f"[BACKFILL] done: {created} created, {skipped} skipped, {failed} failed  (total inspected: {total})")


if __name__ == "__main__":
    run()
