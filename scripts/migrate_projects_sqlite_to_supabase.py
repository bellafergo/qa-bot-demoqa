#!/usr/bin/env python3
"""
Copy rows from the local SQLite `projects` table (Vanya catalog DB) into
Supabase `public.projects` via the Supabase REST client.

Prerequisites:
  - SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY in the environment
  - Table `public.projects` exists in Supabase (see supabase/migration.sql)
  - Source DB path in VANYA_SQLITE_PATH (default ./data/vanya.db)

Usage (from repo root):
  VANYA_SQLITE_PATH=./data/vanya.db python scripts/migrate_projects_sqlite_to_supabase.py

By default, existing Supabase rows with the same `id` are skipped (ignore_duplicates).
Use --overwrite to upsert and replace name/description/color/base_url/settings_json/timestamps.
"""
from __future__ import annotations

import argparse
import json
import os
import sqlite3
import sys
from pathlib import Path

# Repo root on sys.path
_ROOT = Path(__file__).resolve().parents[1]
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))


def _load_rows(sqlite_path: str) -> list[dict]:
    conn = sqlite3.connect(sqlite_path)
    try:
        cur = conn.cursor()
        cur.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='projects'")
        if cur.fetchone() is None:
            return []
        cur.execute("SELECT * FROM projects ORDER BY id")
        col_names = [d[0] for d in cur.description]
        rows = []
        for tup in cur.fetchall():
            rows.append(dict(zip(col_names, tup)))
        return rows
    finally:
        conn.close()


def main() -> int:
    parser = argparse.ArgumentParser(description="Migrate projects SQLite → Supabase")
    parser.add_argument(
        "--sqlite",
        default=os.getenv("VANYA_SQLITE_PATH", "./data/vanya.db"),
        help="Path to SQLite file containing projects table",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Upsert on id (replace fields in Supabase with SQLite values)",
    )
    args = parser.parse_args()

    os.chdir(_ROOT)
    from services.supabase_store import is_supabase_configured, supabase_client

    if not is_supabase_configured():
        print("ERROR: SUPABASE_URL and SUPABASE_SERVICE_ROLE_KEY must be set.", file=sys.stderr)
        return 1

    sb = supabase_client()
    if sb is None:
        print("ERROR: Could not create Supabase client.", file=sys.stderr)
        return 1

    sp = Path(args.sqlite).expanduser().resolve()
    if not sp.is_file():
        print(f"ERROR: SQLite file not found: {sp}", file=sys.stderr)
        return 1

    raw_rows = _load_rows(str(sp))
    if not raw_rows:
        print(f"No rows in projects table ({sp}). Nothing to do.")
        return 0

    out: list[dict] = []
    for r in raw_rows:
        sid = (r.get("id") or "").strip()
        if not sid:
            continue
        settings = r.get("settings_json")
        if settings is None or settings == "":
            settings = "{}"
        elif not isinstance(settings, str):
            settings = json.dumps(settings, ensure_ascii=False)
        row = {
            "id": sid,
            "name": r.get("name") or "",
            "description": r.get("description") or "",
            "color": r.get("color") or "#6366f1",
            "base_url": r.get("base_url"),
            "settings_json": settings,
            "created_at": r.get("created_at") or "",
            "updated_at": r.get("updated_at") or "",
        }
        out.append(row)

    if args.overwrite:
        sb.table("projects").upsert(out, on_conflict="id").execute()
        print(f"Upserted {len(out)} project(s) into Supabase.")
    else:
        sb.table("projects").upsert(
            out, on_conflict="id", ignore_duplicates=True
        ).execute()
        print(
            f"Sent {len(out)} project(s) to Supabase "
            f"(ignore_duplicates=True — existing ids unchanged)."
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
