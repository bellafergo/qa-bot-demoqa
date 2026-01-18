# services/pr_runs.py
from __future__ import annotations

import os
import time
import logging
from dataclasses import dataclass
from typing import Any, Dict, List, Optional

# Importa tu runner real (ajusta el import si tu runner está en otra ruta)
from runner import execute_test  # type: ignore

logger = logging.getLogger("vanya.pr_runs")

# Base URL del sitio que vas a probar (MVP). Para tu repo demo, puedes usar DemoQA o SauceDemo.
# Para tu producto, lo ideal es que cada cliente tenga su base_url configurada por proyecto.
SUITE_BASE_URL = (os.getenv("SUITE_BASE_URL") or "").strip()

# Si tu app vive en Render, úsalo para construir links visibles en PR
PUBLIC_BASE_URL = (os.getenv("PUBLIC_BASE_URL") or "").strip().rstrip("/")

# Timeout defaults
DEFAULT_TIMEOUT_S = int((os.getenv("SUITE_TIMEOUT_S") or "60").strip() or "60")
DEFAULT_HEADLESS = (os.getenv("SUITE_HEADLESS") or "1").strip() != "0"


@dataclass
class RunSummary:
    tag: str
    ok: bool
    status: str
    evidence_id: str
    duration_ms: int
    reason: str
    run_url: str
    report_url: Optional[str] = None
    evidence_url: Optional[str] = None


# ============================================================
# Suite registry (MVP)
# ============================================================

def _require_base_url() -> str:
    if not SUITE_BASE_URL:
        raise RuntimeError("Missing SUITE_BASE_URL env. Set it to a site you want to test.")
    return SUITE_BASE_URL


def _suite_smoke() -> List[Dict[str, Any]]:
    base = _require_base_url()
    return [
        {"action": "goto", "url": base},
        {"action": "wait_ms", "ms": 800},
        {"action": "assert_visible", "selector": "body"},
    ]


def _suite_ui_smoke() -> List[Dict[str, Any]]:
    # MVP: igual que smoke, pero aquí después pondrás asserts de header/nav/cta.
    return _suite_smoke()


def _suite_api_smoke() -> List[Dict[str, Any]]:
    # MVP: si tu app no tiene endpoints, déjalo igual.
    # En producto, aquí harías requests a /health, /api, etc. (con requests), o tests de API.
    return _suite_smoke()


def _suite_login() -> List[Dict[str, Any]]:
    # MVP placeholder: depende del sitio del cliente.
    # Para producto: definimos login por proyecto/cliente, no hardcode aquí.
    return _suite_smoke()


def _suite_checkout() -> List[Dict[str, Any]]:
    # MVP placeholder: depende del sitio del cliente.
    return _suite_smoke()


TAG_TO_SUITE = {
    "smoke": _suite_smoke,
    "ui_smoke": _suite_ui_smoke,
    "api_smoke": _suite_api_smoke,
    "login": _suite_login,
    "checkout": _suite_checkout,
    # "visual_regression": (todavía no lo corremos aquí; eso será en services/visual_diff.py)
}


# ============================================================
# Runner launcher
# ============================================================

def run_suite_by_tag(tag: str) -> RunSummary:
    tag = (tag or "").strip()
    if not tag:
        raise ValueError("tag required")

    builder = TAG_TO_SUITE.get(tag)
    if not builder:
        # fallback
        builder = TAG_TO_SUITE["smoke"]

    steps = builder()

    # OJO: expected="pass" porque en PR checks normalmente buscamos que pase
    r = execute_test(
        steps=steps,
        base_url=SUITE_BASE_URL or None,
        headless=DEFAULT_HEADLESS,
        timeout_s=DEFAULT_TIMEOUT_S,
        expected="pass",
    )

    evidence_id = str(r.get("evidence_id") or "")
    ok = bool(r.get("ok") is True)
    status = str(r.get("status") or "unknown")
    reason = str(r.get("reason") or "")

    run_url = ""
    if PUBLIC_BASE_URL and evidence_id:
        run_url = f"{PUBLIC_BASE_URL}/runs/{evidence_id}"

    # Si tu runner guarda report_url/evidence_url en meta (como ya lo normalizaste),
    # intentamos leerlos de ahí.
    meta = r.get("meta") or {}
    report_url = meta.get("report_url") or r.get("report_url")
    evidence_url = meta.get("evidence_url") or r.get("evidence_url")

    return RunSummary(
        tag=tag,
        ok=ok,
        status=status,
        evidence_id=evidence_id,
        duration_ms=int(r.get("duration_ms") or 0),
        reason=reason,
        run_url=run_url,
        report_url=report_url,
        evidence_url=evidence_url,
    )


def trigger_runs(tags: List[str]) -> List[RunSummary]:
    """
    Corre tags secuencial (MVP).
    En la versión Pro: paralelizas con Celery/RQ/Threads o Kubernetes jobs.
    """
    out: List[RunSummary] = []
    tags = [t.strip() for t in (tags or []) if str(t).strip()]
    if not tags:
        tags = ["smoke"]

    t0 = time.time()
    for tag in tags[:8]:  # limit MVP
        try:
            out.append(run_suite_by_tag(tag))
        except Exception as e:
            logger.exception("Suite failed: %s", tag)
            out.append(
                RunSummary(
                    tag=tag,
                    ok=False,
                    status="failed",
                    evidence_id="",
                    duration_ms=int((time.time() - t0) * 1000),
                    reason=f"{type(e).__name__}: {e}",
                    run_url="",
                )
            )

    return out
