# services/evidence_pipeline.py
"""
Evidence Pipeline — centraliza captura, report y upload de evidencia.

Responsabilidades:
- Normalizar screenshot desde resultado del runner
- Generar PDF (report_service) con fallback
- Subir screenshot y PDF a Cloudinary
- Devolver evidence_url, report_url, pdf_error, metadata

Reutilizable por:
- execute_engine (chat/execute path)
- test_catalog_service (catalog runs con HAS_CLOUDINARY)
"""
from __future__ import annotations

import logging
import os
from typing import Any, Dict, List, Optional

from core.settings import settings
from core.redaction import redact_secrets, redact_steps

logger = logging.getLogger("vanya.evidence_pipeline")


def _make_png_data_url(b64_or_data_url: Optional[str]) -> Optional[str]:
    """Normaliza base64 o data URL a data:image/png;base64,..."""
    if not b64_or_data_url:
        return None
    s = str(b64_or_data_url).strip()
    if not s or s == "None":
        return None
    if s.startswith("data:image"):
        return s
    return f"data:image/png;base64,{s}"


def _pull_evidence_from_runner(runner: Dict[str, Any]) -> tuple[str, Optional[str]]:
    """Extrae evidence_id y screenshot_b64 del resultado del runner."""
    raw = runner.get("raw") if isinstance(runner.get("raw"), dict) else {}
    evidence_id = str(
        runner.get("evidence_id")
        or raw.get("evidence_id")
        or ""
    ).strip()
    b64 = (
        runner.get("screenshot_b64")
        or runner.get("screenshotBase64")
        or runner.get("screenshotB64")
        or raw.get("screenshot_b64")
        or raw.get("screenshotBase64")
        or raw.get("screenshotB64")
    )
    return evidence_id, (str(b64) if b64 else None)


def _build_fallback_pdf(
    *,
    prompt: str,
    base_url: str,
    status: str,
    duration_ms: int,
    evidence_id: str,
    evidence_url: Optional[str],
    steps: List[Dict[str, Any]],
    runner: Dict[str, Any],
) -> bytes:
    """PDF mínimo cuando report_service falla."""
    from reportlab.pdfgen import canvas
    from reportlab.lib.pagesizes import letter
    import io

    buf = io.BytesIO()
    c = canvas.Canvas(buf, pagesize=letter)
    w, h = letter

    y = h - 50
    c.setFont("Helvetica-Bold", 14)
    c.drawString(50, y, "Vanya QA Run Report")
    y -= 24

    c.setFont("Helvetica", 10)
    c.drawString(50, y, f"Base URL: {base_url}")
    y -= 14
    c.drawString(50, y, f"Status: {status}   Duration: {duration_ms} ms   Evidence ID: {evidence_id}")
    y -= 14
    c.drawString(50, y, (prompt or "")[:1200])
    y -= 18

    if evidence_url:
        c.drawString(50, y, f"Evidence URL: {evidence_url}")
        y -= 18

    c.setFont("Helvetica-Bold", 11)
    c.drawString(50, y, "Steps")
    y -= 14
    c.setFont("Helvetica", 9)
    for i, s in enumerate((steps or [])[:80], start=1):
        line = f"{i}. {s.get('action')} " + (
            " ".join(
                f"{k}={s.get(k)}"
                for k in ("url", "selector", "text", "key", "ms")
                if s.get(k) is not None
            )
        )
        c.drawString(55, y, line[:120])
        y -= 11
        if y < 80:
            c.showPage()
            y = h - 50
            c.setFont("Helvetica", 9)

    c.showPage()
    c.save()
    return buf.getvalue()


def process_evidence(
    *,
    runner: Dict[str, Any],
    prompt: str = "",
    base_url: str = "",
    steps: Optional[List[Dict[str, Any]]] = None,
    evidence_id: Optional[str] = None,
    status: Optional[str] = None,
    duration_ms: Optional[int] = None,
    meta: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Procesa evidencia del runner: normaliza, genera PDF, sube a Cloudinary.

    Args:
        runner: Resultado de execute_test (o compatible).
        prompt: Texto de la solicitud (para PDF). Redactado internamente.
        base_url: URL base.
        steps: Pasos ejecutados (para PDF). Redactados internamente.
        evidence_id: Override; si no se pasa, se extrae del runner.
        status: Override; si no se pasa, se infiere del runner.
        duration_ms: Override; si no se pasa, se toma del runner.
        meta: Metadata adicional para report (thread_id, session_id, etc.).

    Returns:
        {
            "evidence_url": str | None,
            "report_url": str | None,
            "pdf_error": str | None,
            "screenshot_data_url": str | None,
            "evidence_id": str,
        }
    """
    evid, b64 = _pull_evidence_from_runner(runner)
    if evidence_id:
        evid = str(evidence_id).strip()
    if not evid:
        import uuid
        evid = f"EV-{uuid.uuid4().hex[:10]}"

    _status = status or str(runner.get("status") or "").strip().lower()
    if not _status:
        _status = "passed" if runner.get("ok", True) else "failed"
    # Preserve technical runner errors as "error" so reports/UI can
    # differentiate between assertion failures and runtime issues.

    _duration = duration_ms
    if _duration is None:
        _duration = runner.get("duration_ms")
    if _duration is None:
        _duration = 0

    screenshot_data_url = _make_png_data_url(b64) if b64 else None

    evidence_url: Optional[str] = None
    report_url: Optional[str] = None
    pdf_error: Optional[str] = None
    pdf_bytes: Optional[bytes] = None

    has_cloudinary = getattr(settings, "HAS_CLOUDINARY", False)

    # 1) Upload screenshot
    try:
        if screenshot_data_url and has_cloudinary:
            from services.cloudinary_service import upload_screenshot_b64
            res = upload_screenshot_b64(str(screenshot_data_url), evidence_id=evid)
            if isinstance(res, dict):
                evidence_url = (res.get("secure_url") or res.get("url") or "").strip() or None
            else:
                evidence_url = str(res).strip() or None
    except Exception as e:
        logger.exception("evidence_pipeline: screenshot upload failed")

    if not evidence_url:
        raw0 = runner.get("raw", {}) if isinstance(runner.get("raw"), dict) else {}
        evidence_url = (
            runner.get("evidence_url")
            or runner.get("screenshot_url")
            or raw0.get("evidence_url")
            or raw0.get("screenshot_url")
        )

    # 2) Generate PDF
    runner_for_report = dict(runner)
    if screenshot_data_url:
        runner_for_report["screenshot_data_url"] = screenshot_data_url

    try:
        from services.report_service import generate_pdf_report
        rep = generate_pdf_report(
            prompt=redact_secrets(prompt),
            base_url=base_url,
            runner=runner_for_report,
            steps=redact_steps(steps or []),
            evidence_id=evid,
            meta=meta or {},
        )
        if isinstance(rep, dict) and rep.get("report_path"):
            rp = str(rep.get("report_path") or "").strip()
            if rp and os.path.exists(rp):
                with open(rp, "rb") as f:
                    pdf_bytes = f.read()
    except Exception as e:
        pdf_error = f"report_service failed: {type(e).__name__}: {e}"
        logger.exception("evidence_pipeline: generate_pdf_report failed")

    # 3) Fallback PDF
    if not pdf_bytes:
        try:
            pdf_bytes = _build_fallback_pdf(
                prompt=redact_secrets(prompt),
                base_url=base_url,
                status=_status,
                duration_ms=int(_duration),
                evidence_id=evid,
                evidence_url=evidence_url,
                steps=redact_steps(steps or []),
                runner=runner,
            )
        except Exception as e:
            pdf_error = (pdf_error or "") + f" | fallback_pdf failed: {type(e).__name__}: {e}"
            logger.exception("evidence_pipeline: fallback PDF failed")

    # 4) Upload PDF
    if has_cloudinary and pdf_bytes and len(pdf_bytes) > 800:
        try:
            from services.cloudinary_service import upload_pdf_bytes
            res_pdf = upload_pdf_bytes(pdf_bytes, evidence_id=evid)
            if isinstance(res_pdf, dict):
                report_url = (res_pdf.get("secure_url") or res_pdf.get("url") or "").strip() or None
            else:
                report_url = str(res_pdf).strip() or None
            if not report_url:
                pdf_error = (pdf_error or "") + " | Cloudinary upload returned no URL"
        except Exception as e:
            pdf_error = (pdf_error or "") + f" | cloudinary upload failed: {type(e).__name__}: {e}"
            logger.exception("evidence_pipeline: PDF upload failed")
    elif not has_cloudinary:
        pdf_error = pdf_error or "HAS_CLOUDINARY is false"
    elif not pdf_bytes:
        pdf_error = pdf_error or "pdf_bytes empty"
    elif len(pdf_bytes) <= 800:
        pdf_error = pdf_error or f"pdf_bytes too small: {len(pdf_bytes)}"

    return {
        "evidence_url": evidence_url,
        "report_url": report_url,
        "pdf_error": pdf_error,
        "screenshot_data_url": screenshot_data_url,
        "evidence_id": evid,
    }
