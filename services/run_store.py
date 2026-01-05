# services/run_store.py
from __future__ import annotations

import os
import time
from typing import Any, Dict, Optional

_RUNS: Dict[str, Dict[str, Any]] = {}
_RUNS_TTL_S = int(os.getenv("RUNS_TTL_S", "86400"))  # 24h


def _runs_cleanup() -> None:
    now = time.time()
    kill = []
    for evid, item in list(_RUNS.items()):
        ts = float(item.get("ts", 0) or 0)
        if now - ts > _RUNS_TTL_S:
            kill.append(evid)
    for evid in kill:
        _RUNS.pop(evid, None)


def save_run(run_payload: Dict[str, Any]) -> None:
    evid = (run_payload.get("evidence_id") or "").strip()
    if not evid:
        return
    _runs_cleanup()
    _RUNS[evid] = {"ts": time.time(), "data": run_payload}


def get_run(evidence_id: str) -> Optional[Dict[str, Any]]:
    evid = (evidence_id or "").strip()
    if not evid:
        return None
    _runs_cleanup()
    item = _RUNS.get(evid)
    return item.get("data") if item else None