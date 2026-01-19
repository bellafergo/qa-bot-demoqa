from __future__ import annotations

import os
from typing import Any, Dict, Optional, List

from supabase import create_client, Client


def _safe_str(x: Any) -> str:
    try:
        return (str(x) if x is not None else "").strip()
    except Exception:
        return ""


def _normalize_tags(x: Any) -> List[str]:
    if not x:
        return []
    if isinstance(x, str):
        t = x.strip()
        return [t] if t else []
    if isinstance(x, list):
        out: List[str] = []
        for it in x:
            s = _safe_str(it)
            if s:
                out.append(s)
        # unique preserving order
        seen = set()
        uniq: List[str] = []
        for t in out:
            if t in seen:
                continue
            seen.add(t)
            uniq.append(t)
        return uniq
    return []


def _get_supabase() -> Optional[Client]:
    url = os.getenv("SUPABASE_URL", "").strip()
    key = os.getenv("SUPABASE_SERVICE_ROLE_KEY", "").strip() or os.getenv("SUPABASE_ANON_KEY", "").strip()
    if not url or not key:
        return None
    try:
        return create_client(url, key)
    except Exception:
        return None


def persist_run_supabase(run_payload: Dict[str, Any]) -> bool:
    """
    Upsert por evidence_id.
    No debe lanzar excepción (robustez).
    Retorna True si persistió, False si no.
    """
    if not isinstance(run_payload, dict):
        return False

    evid = _safe_str(run_payload.get("evidence_id"))
    if not evid:
        return False

    sb = _get_supabase()
    if sb is None:
        return False

    meta = run_payload.get("meta") if isinstance(run_payload.get("meta"), dict) else {}
    meta = meta if isinstance(meta, dict) else {}

    # tags: soporta meta.tags/meta.suites/run_payload.tags
    tags = _normalize_tags(meta.get("tags") or meta.get("suites") or run_payload.get("tags"))

    # intenta leer campos comunes de tu contrato
    status = _safe_str(run_payload.get("status")) or _safe_str(run_payload.get("final_status")) or "failed"
    mode = _safe_str(run_payload.get("mode") or meta.get("mode"))
    persona = _safe_str(run_payload.get("persona") or meta.get("persona"))
    domain = _safe_str(meta.get("domain") or run_payload.get("domain"))
    flow = _safe_str(meta.get("flow") or meta.get("suite") or run_payload.get("flow"))

    duration_ms = run_payload.get("duration_ms")
    try:
        duration_ms = int(duration_ms) if duration_ms is not None else None
    except Exception:
        duration_ms = None

    # steps
    steps = run_payload.get("steps") if isinstance(run_payload.get("steps"), list) else []
    steps_count = len(steps) if steps else None

    failed_step_index = run_payload.get("failed_step_index")
    try:
        failed_step_index = int(failed_step_index) if failed_step_index is not None else None
    except Exception:
        failed_step_index = None

    error_summary = _safe_str(run_payload.get("error") or run_payload.get("error_summary") or meta.get("error_summary"))

    evidence_url = _safe_str(run_payload.get("evidence_url") or meta.get("evidence_url"))
    report_url = _safe_str(run_payload.get("report_url") or meta.get("report_url"))

    git_sha = _safe_str(meta.get("git_sha") or run_payload.get("git_sha"))
    runner_version = _safe_str(meta.get("runner_version") or run_payload.get("runner_version"))

    row = {
        "evidence_id": evid,
        "status": status,
        "duration_ms": duration_ms,
        "mode": mode or None,
        "persona": persona or None,
        "domain": domain or None,
        "flow": flow or None,
        "tags": tags,
        "error_summary": error_summary or None,
        "failed_step_index": failed_step_index,
        "steps_count": steps_count,
        "evidence_url": evidence_url or None,
        "report_url": report_url or None,
        "meta": meta,
        "result": run_payload,          # guardas TODO para auditoría
        "git_sha": git_sha or None,
        "runner_version": runner_version or None,
    }

    try:
        # upsert por evidence_id
        sb.table("qa_runs").upsert(row, on_conflict="evidence_id").execute()
        return True
    except Exception:
        return False
