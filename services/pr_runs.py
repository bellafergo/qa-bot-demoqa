# services/pr_runs.py
from __future__ import annotations

import logging
import threading
import time
import uuid
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

from services.pr_agent import PRContext, update_pr_comment
from services.run_store import (
    save_run,
    list_runs_for_pr,
)

logger = logging.getLogger("vanya.pr_runs")


# ============================================================
# Model
# ============================================================

@dataclass
class RunResult:
    evidence_id: str
    run_id: str
    tag: str
    status: str  # "running" | "passed" | "failed" | "error"
    duration_ms: int = 0
    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    message: Optional[str] = None


# ============================================================
# Suite/tag -> steps mapping (MVP)
# AJUSTA ESTO A TU PROYECTO REAL
# ============================================================

def _steps_for_tag(tag: str, *, base_url: Optional[str] = None) -> List[Dict[str, Any]]:
    """
    Genera steps para tu runner.execute_test(steps=[...]).
    IMPORTANTÍSIMO: tu execute_test actual recibe steps: List[Dict[str,Any]]
    """
    tag = (tag or "").strip()

    # Si no pasas base_url aquí, puedes definir uno default en ENV
    base_url = (base_url or "").strip() or (  # puedes poner uno de demo
        ""  # por defecto vacío: si no hay goto, tu runner fallará (bien)
    )

    # EJEMPLOS MÍNIMOS (cámbialos a tus flujos reales):
    if tag in ("smoke", "ui_smoke"):
        if not base_url:
            # sin URL, esto no puede correr; mejor fallar explícito
            raise ValueError("PR_RUN_BASE_URL no configurado para tag smoke/ui_smoke")
        return [
            {"action": "goto", "url": base_url},
            {"action": "assert_visible", "selector": "body"},
        ]

    if tag == "api_smoke":
        # placeholder: normalmente aquí ejecutarías pytest/postman/newman
        # por ahora lo marcamos como un run "noop" con wait
        return [{"action": "wait_ms", "ms": 250}]

    if tag == "login":
        if not base_url:
            raise ValueError("PR_RUN_BASE_URL no configurado para tag login")
        # placeholder
        return [
            {"action": "goto", "url": base_url},
            {"action": "assert_visible", "selector": "body"},
        ]

    if tag in ("checkout", "visual_regression"):
        if not base_url:
            raise ValueError(f"PR_RUN_BASE_URL no configurado para tag {tag}")
        return [
            {"action": "goto", "url": base_url},
            {"action": "assert_visible", "selector": "body"},
        ]

    # Default mínimo
    if not base_url:
        raise ValueError(f"Tag no soportada aún y no hay base_url: {tag}")
    return [
        {"action": "goto", "url": base_url},
        {"action": "assert_visible", "selector": "body"},
    ]


# ============================================================
# Runner adapter
# ============================================================

def _run_tag(tag: str, *, pr: Optional[PRContext] = None) -> Dict[str, Any]:
    """
    Ejecuta una suite/tag usando tu runner existente.
    Corre dentro de un thread (no bloquea el webhook).
    """
    from runner import execute_test  # lazy import

    base_url = ""
    # Si quieres, puedes setear un env PR_RUN_BASE_URL en Render
    # y/o enriquecer por repo/domain
    try:
        import os
        base_url = (os.getenv("PR_RUN_BASE_URL") or "").strip()
    except Exception:
        base_url = ""

    steps = _steps_for_tag(tag, base_url=base_url)

    # Tu execute_test firma: execute_test(steps, base_url=None, headless=True, viewport=None, timeout_s=None, expected=None)
    return execute_test(
        steps=steps,
        base_url=base_url or None,
        headless=True,
        timeout_s=90,
        expected="pass",
    )


# ============================================================
# Public API
# ============================================================

def trigger_runs_for_tags(
    ctx: PRContext,
    tags: List[str],
    *,
    comment_id: int,
    comment_body_template: str,
) -> List[str]:
    """
    Crea runs "running" y dispara ejecución en background.
    - comment_id: comentario a actualizar live
    - comment_body_template: body base (de pr_agent) que vamos a re-render con resultados
    """
    tags = [t.strip() for t in (tags or []) if str(t or "").strip()]
    run_ids: List[str] = []

    for tag in tags:
        run_id = f"PRRUN-{uuid.uuid4().hex[:10]}"
        evidence_id = run_id  # IMPORTANT: run_store requiere evidence_id; usamos run_id como evidence_id

        run_ids.append(run_id)

        # Guardar estado inicial
        _save_run_safe(
            evidence_id=evidence_id,
            run_payload={
                "evidence_id": evidence_id,
                "run_id": run_id,
                "status": "running",
                "expected": "pass",
                "outcome": "pass",
                "reason": "RUNNING",
                "meta": {
                    "tags": [tag],
                    "pr": {
                        "owner": ctx.owner,
                        "repo": ctx.repo,
                        "number": ctx.pr_number,
                        "sha": ctx.sha,
                        "title": ctx.title,
                        "html_url": ctx.html_url,
                    },
                },
            },
        )

        t = threading.Thread(
            target=_execute_and_update,
            kwargs=dict(
                ctx=ctx,
                tag=tag,
                run_id=run_id,
                evidence_id=evidence_id,
                comment_id=comment_id,
                comment_body_template=comment_body_template,
            ),
            daemon=True,
        )
        t.start()

    # Primer update “live” inmediato (muestra running)
    try:
        new_body = _render_live_comment(comment_body_template, ctx=ctx)
        update_pr_comment(ctx, comment_id, new_body)
    except Exception:
        logger.exception("Failed to update PR comment (initial running state)")

    return run_ids


# ============================================================
# Internals
# ============================================================

def _execute_and_update(
    *,
    ctx: PRContext,
    tag: str,
    run_id: str,
    evidence_id: str,
    comment_id: int,
    comment_body_template: str,
) -> None:
    start = time.time()
    result = RunResult(evidence_id=evidence_id, run_id=run_id, tag=tag, status="running")

    try:
        raw = _run_tag(tag, pr=ctx) or {}

        # Normaliza resultado del runner
        status_raw = str(raw.get("status") or "").lower().strip()
        outcome_raw = str(raw.get("outcome") or raw.get("result") or "").lower().strip()

        if status_raw in ("passed", "pass") or outcome_raw in ("pass", "passed", "ok", "success"):
            result.status = "passed"
        elif status_raw in ("failed", "fail") or outcome_raw in ("fail", "failed", "error"):
            result.status = "failed"
        else:
            # fallback: si runner no reportó claro, asumimos passed si no hubo exception
            result.status = "passed"

        # Links (si tu runner los llena)
        result.evidence_url = raw.get("evidence_url") or raw.get("evidence") or raw.get("evidenceLink")
        result.report_url = raw.get("report_url") or raw.get("report") or raw.get("reportLink")
        result.message = raw.get("reason") or raw.get("message") or raw.get("summary")

    except Exception as e:
        logger.exception("Run failed for tag=%s run_id=%s", tag, run_id)
        result.status = "error"
        result.message = f"{type(e).__name__}: {e}"

    finally:
        result.duration_ms = int((time.time() - start) * 1000)

        # Guardar resultado final (actualiza el mismo evidence_id)
        _save_run_safe(
            evidence_id=evidence_id,
            run_payload={
                "evidence_id": evidence_id,
                "run_id": result.run_id,
                "status": result.status,
                "duration_ms": result.duration_ms,
                "evidence_url": result.evidence_url,
                "report_url": result.report_url,
                "message": result.message,
                "expected": "pass",
                "outcome": "pass" if result.status == "passed" else "fail",
                "reason": result.message or result.status,
                "meta": {
                    "tags": [tag],
                    "pr": {
                        "owner": ctx.owner,
                        "repo": ctx.repo,
                        "number": ctx.pr_number,
                        "sha": ctx.sha,
                        "title": ctx.title,
                        "html_url": ctx.html_url,
                    },
                },
            },
        )

        # Update comentario “live” con estado más reciente
        try:
            new_body = _render_live_comment(comment_body_template, ctx=ctx)
            update_pr_comment(ctx, comment_id, new_body)
        except Exception:
            logger.exception("Failed to update PR comment live")


def _render_live_comment(template: str, *, ctx: PRContext) -> str:
    """
    Inserta/actualiza un bloque de runs en el comentario.
    Usa list_runs_for_pr() del run_store.
    Recomendación: tu template debe incluir marcador:
      <!-- VANYA_RUNS -->
    Si no lo trae, lo agregamos al final.
    """
    runs = list_runs_for_pr(ctx.owner, ctx.repo, ctx.pr_number, sha=ctx.sha, limit=50)

    runs_block = _format_runs_block(runs)

    marker = "<!-- VANYA_RUNS -->"
    if marker in (template or ""):
        before, _, after = template.partition(marker)
        # Reemplazamos todo lo que venga después del marker por el bloque
        # para evitar “duplicados”
        return before + marker + "\n\n" + runs_block + "\n"

    # si no hay marker, lo anexamos
    base = (template or "").rstrip()
    return base + "\n\n" + marker + "\n\n" + runs_block + "\n"


def _format_runs_block(runs: List[Dict[str, Any]]) -> str:
    """
    Formato markdown simple (GitHub).
    """
    if not runs:
        return "### Ejecuciones (live)\n_(pendiente)_\n"

    # Deduplicar por tag conservando el más reciente
    latest_by_tag: Dict[str, Dict[str, Any]] = {}
    for r in runs:
        meta = r.get("meta") if isinstance(r.get("meta"), dict) else {}
        tags = meta.get("tags") if isinstance(meta, dict) else None
        tag = ""
        if isinstance(tags, list) and tags:
            tag = str(tags[0] or "").strip()
        tag = tag or str(r.get("tag") or "").strip() or "unknown"
        if tag not in latest_by_tag:
            latest_by_tag[tag] = r

    # Orden estable por tag
    items = [(t, latest_by_tag[t]) for t in sorted(latest_by_tag.keys())]

    lines: List[str] = []
    lines.append("### Ejecuciones (live)")
    lines.append("")
    lines.append("| Tag | Estado | Duración | Evidence | Reporte |")
    lines.append("|---|---|---:|---|---|")

    def fmt_link(url: Optional[str], label: str) -> str:
        u = (url or "").strip()
        return f"[{label}]({u})" if u else "—"

    def fmt_status(s: str) -> str:
        s = (s or "").lower().strip()
        if s == "passed":
            return "✅ passed"
        if s == "failed":
            return "❌ failed"
        if s == "error":
            return "⚠️ error"
        if s == "running":
            return "⏳ running"
        return s or "unknown"

    for tag, r in items:
        status = fmt_status(str(r.get("status") or "unknown"))
        dur = r.get("duration_ms")
        dur_s = f"{int(dur)} ms" if isinstance(dur, int) else "—"

        ev_url = r.get("evidence_url")
        rep_url = r.get("report_url")

        lines.append(
            f"| `{tag}` | {status} | {dur_s} | {fmt_link(ev_url, 'evidence')} | {fmt_link(rep_url, 'reporte')} |"
        )

    lines.append("")
    lines.append("> _Este bloque se actualiza automáticamente conforme terminan las suites._")
    lines.append("")
    return "\n".join(lines)


def _save_run_safe(*, evidence_id: str, run_payload: Dict[str, Any]) -> None:
    """
    run_store.save_run() requiere evidence_id dentro del payload.
    """
    try:
        if not isinstance(run_payload, dict):
            return
        run_payload["evidence_id"] = (run_payload.get("evidence_id") or evidence_id)
        save_run(run_payload)
    except Exception:
        logger.exception("save_run failed for evidence_id=%s", evidence_id)
