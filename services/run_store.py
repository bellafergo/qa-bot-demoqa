# services/run_store.py
from __future__ import annotations

import os
import time
import threading
from typing import Any, Dict, List, Optional, Tuple

# ============================================================
# In-memory Run Store (TTL + indexes)
# - Guarda runs por evidence_id (compat)
# - Índices opcionales por PR y por tag (para PR Agent)
# - Thread-safe (Render / Uvicorn puede manejar concurrencia)
# ============================================================

_RUNS_TTL_S = int((os.getenv("RUNS_TTL_S") or "86400").strip() or "86400")  # 24h

_lock = threading.RLock()

# evidence_id -> {"ts": float, "data": Dict}
_RUNS: Dict[str, Dict[str, Any]] = {}

# run_id -> evidence_id (por si algún día usas otro id)
_RUN_ID_TO_EVID: Dict[str, str] = {}

# pr_key -> [evidence_id, ...]
# pr_key ejemplo: "owner/repo#123@<sha7>"
_PR_INDEX: Dict[str, List[str]] = {}

# tag -> [evidence_id, ...]
_TAG_INDEX: Dict[str, List[str]] = {}


def _now() -> float:
    return time.time()


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


def _compute_pr_key(meta: Dict[str, Any]) -> Optional[str]:
    """
    Espera (opcional) algo tipo:
      meta["pr"] = {"owner": "...", "repo": "...", "number": 123, "sha": "..."}
    """
    pr = meta.get("pr") if isinstance(meta, dict) else None
    if not isinstance(pr, dict):
        return None

    owner = _safe_str(pr.get("owner"))
    repo = _safe_str(pr.get("repo"))
    number = pr.get("number")
    sha = _safe_str(pr.get("sha"))

    if not owner or not repo or not number:
        return None

    sha7 = sha[:7] if sha else ""
    base = f"{owner}/{repo}#{int(number)}"
    return f"{base}@{sha7}" if sha7 else base


def _runs_cleanup_locked() -> None:
    """
    Limpia runs expirados. Debe llamarse dentro del lock.
    """
    now = _now()
    kill: List[str] = []
    for evid, item in list(_RUNS.items()):
        ts = float(item.get("ts", 0) or 0)
        if now - ts > _RUNS_TTL_S:
            kill.append(evid)

    if not kill:
        return

    # borrar runs
    for evid in kill:
        data = (_RUNS.get(evid) or {}).get("data") or {}
        meta = data.get("meta") if isinstance(data, dict) else {}
        meta = meta if isinstance(meta, dict) else {}

        # limpiar indices por tag
        tags = _normalize_tags(meta.get("tags") or meta.get("suites") or data.get("tags"))
        for t in tags:
            lst = _TAG_INDEX.get(t) or []
            _TAG_INDEX[t] = [x for x in lst if x != evid]
            if not _TAG_INDEX[t]:
                _TAG_INDEX.pop(t, None)

        # limpiar indices por PR
        pr_key = _compute_pr_key(meta)
        if pr_key:
            lst = _PR_INDEX.get(pr_key) or []
            _PR_INDEX[pr_key] = [x for x in lst if x != evid]
            if not _PR_INDEX[pr_key]:
                _PR_INDEX.pop(pr_key, None)

        # limpiar run_id map (si aplica)
        run_id = _safe_str(data.get("run_id"))
        if run_id:
            _RUN_ID_TO_EVID.pop(run_id, None)

        _RUNS.pop(evid, None)


def save_run(run_payload: Dict[str, Any]) -> Optional[str]:
    """
    Guarda un run en memoria.
    Requiere evidence_id (compat con tu runner).
    Opcional:
      run_payload["run_id"]
      run_payload["meta"]["tags"] = ["ui_smoke", ...]
      run_payload["meta"]["pr"] = {"owner","repo","number","sha"}
    Retorna evidence_id si guardó.
    """
    if not isinstance(run_payload, dict):
        return None

    evid = _safe_str(run_payload.get("evidence_id"))
    if not evid:
        return None

    with _lock:
        _runs_cleanup_locked()

        # asegura meta dict
        meta = run_payload.get("meta")
        if meta is None or not isinstance(meta, dict):
            meta = {}
            run_payload["meta"] = meta

        # timestamp
        _RUNS[evid] = {"ts": _now(), "data": run_payload}

        # index opcional por run_id
        run_id = _safe_str(run_payload.get("run_id"))
        if run_id:
            _RUN_ID_TO_EVID[run_id] = evid

        # index opcional por PR
        pr_key = _compute_pr_key(meta)
        if pr_key:
            lst = _PR_INDEX.get(pr_key) or []
            if evid not in lst:
                lst.append(evid)
            _PR_INDEX[pr_key] = lst

        # index opcional por tags
        tags = _normalize_tags(meta.get("tags") or meta.get("suites") or run_payload.get("tags"))
        if tags:
            meta["tags"] = tags  # normaliza en payload
            for t in tags:
                lst = _TAG_INDEX.get(t) or []
                if evid not in lst:
                    lst.append(evid)
                _TAG_INDEX[t] = lst

                # Persistencia best-effort a Supabase (no rompe si falla)
        try:
            from services.run_store_supabase import persist_run_supabase
            persist_run_supabase(run_payload)
        except Exception:
            pass

        return evid
    
def save_run_by_id(run_id: str, run_payload: Dict[str, Any]) -> Optional[str]:
    payload = dict(run_payload or {})
    payload["evidence_id"] = run_id
    return save_run(payload)


def get_run(evidence_id: str) -> Optional[Dict[str, Any]]:
    evid = _safe_str(evidence_id)
    if not evid:
        return None
    with _lock:
        _runs_cleanup_locked()
        item = _RUNS.get(evid)
        return item.get("data") if item else None


def get_run_by_id(run_id: str) -> Optional[Dict[str, Any]]:
    """
    Lookup alterno si en el futuro usas run_id distinto a evidence_id.
    """
    rid = _safe_str(run_id)
    if not rid:
        return None
    with _lock:
        _runs_cleanup_locked()
        evid = _RUN_ID_TO_EVID.get(rid)
        if not evid:
            return None
        item = _RUNS.get(evid)
        return item.get("data") if item else None


def list_runs(limit: int = 50) -> List[Dict[str, Any]]:
    """
    Lista runs recientes (por ts desc).
    """
    limit = max(1, min(int(limit or 50), 200))
    with _lock:
        _runs_cleanup_locked()
        items: List[Tuple[str, float, Dict[str, Any]]] = []
        for evid, it in _RUNS.items():
            ts = float(it.get("ts", 0) or 0)
            data = it.get("data") if isinstance(it.get("data"), dict) else {}
            items.append((evid, ts, data))
        items.sort(key=lambda x: x[1], reverse=True)
        return [x[2] for x in items[:limit]]


def list_runs_for_pr(owner: str, repo: str, pr_number: int, sha: Optional[str] = None, limit: int = 50) -> List[Dict[str, Any]]:
    """
    Regresa runs asociados a un PR (si guardaste meta.pr).
    sha es opcional; si viene, usamos sha7 para el pr_key.
    """
    owner_s = _safe_str(owner)
    repo_s = _safe_str(repo)
    if not owner_s or not repo_s or not pr_number:
        return []

    sha7 = _safe_str(sha)[:7]
    base = f"{owner_s}/{repo_s}#{int(pr_number)}"
    pr_key = f"{base}@{sha7}" if sha7 else base

    limit = max(1, min(int(limit or 50), 200))

    with _lock:
        _runs_cleanup_locked()
        evids = list(_PR_INDEX.get(pr_key) or [])
        # orden por ts desc
        scored: List[Tuple[float, Dict[str, Any]]] = []
        for evid in evids:
            it = _RUNS.get(evid)
            if not it:
                continue
            ts = float(it.get("ts", 0) or 0)
            data = it.get("data") if isinstance(it.get("data"), dict) else {}
            scored.append((ts, data))
        scored.sort(key=lambda x: x[0], reverse=True)
        return [d for _, d in scored[:limit]]


def list_runs_for_tag(tag: str, limit: int = 50) -> List[Dict[str, Any]]:
    """
    Regresa runs asociados a una tag (si guardaste meta.tags).
    """
    t = _safe_str(tag)
    if not t:
        return []
    limit = max(1, min(int(limit or 50), 200))

    with _lock:
        _runs_cleanup_locked()
        evids = list(_TAG_INDEX.get(t) or [])
        scored: List[Tuple[float, Dict[str, Any]]] = []
        for evid in evids:
            it = _RUNS.get(evid)
            if not it:
                continue
            ts = float(it.get("ts", 0) or 0)
            data = it.get("data") if isinstance(it.get("data"), dict) else {}
            scored.append((ts, data))
        scored.sort(key=lambda x: x[0], reverse=True)
        return [d for _, d in scored[:limit]]
