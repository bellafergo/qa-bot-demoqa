# services/report_service.py
from __future__ import annotations

import os
import base64
import datetime as dt
from typing import Any, Dict, List, Optional

from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch
from reportlab.lib import colors
from reportlab.platypus import (
    SimpleDocTemplate,
    Paragraph,
    Spacer,
    Table,
    TableStyle,
    Image,
)
from reportlab.lib.styles import getSampleStyleSheet


REPORTS_DIR = os.path.join("evidence", "reports")


def _now_iso_utc() -> str:
    return dt.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"


def _ensure_dir(path: str) -> None:
    os.makedirs(path, exist_ok=True)


def _decode_data_url_to_png(data_url_or_b64: str) -> Optional[bytes]:
    """
    Acepta:
      - data:image/png;base64,....
      - base64 puro (fallback)
    """
    if not data_url_or_b64:
        return None
    s = str(data_url_or_b64).strip()
    if not s:
        return None

    if s.startswith("data:image"):
        try:
            _, b64 = s.split(",", 1)
            return base64.b64decode(b64)
        except Exception:
            return None

    # base64 puro
    try:
        return base64.b64decode(s)
    except Exception:
        return None


def _risk_label(status: str) -> str:
    s = (status or "").lower().strip()
    if s in ("fail", "failed", "error"):
        return "ALTO"
    return "BAJO"


def _public_base_url() -> str:
    """
    Usa PUBLIC_BASE_URL para devolver links absolutos (ideal para Vercel).
    Si no existe, regresa vacío.
    """
    return (os.getenv("PUBLIC_BASE_URL", "") or "").strip().rstrip("/")


def _build_report_url(filename: str) -> str:
    """
    Devuelve URL absoluta si hay PUBLIC_BASE_URL, si no devuelve relativa.
    """
    base = _public_base_url()
    if base:
        return f"{base}/reports/{filename}"
    return f"/reports/{filename}"


def generate_pdf_report(
    *,
    prompt: str,
    base_url: str,
    runner: Dict[str, Any],
    steps: Optional[List[Dict[str, Any]]] = None,
    evidence_id: Optional[str] = None,
    meta: Optional[Dict[str, Any]] = None,
) -> Dict[str, str]:
    """
    Genera un PDF en evidence/reports/ y devuelve:
      { "report_path": "...", "report_url": "https://.../reports/....pdf" }
    """
    _ensure_dir(REPORTS_DIR)

    meta = meta or {}
    evidence_id = (evidence_id or runner.get("evidence_id") or "EV-unknown").strip()

    status = (runner.get("status") or "").strip() or ("passed" if runner.get("ok") else "fail")
    error_msg = runner.get("error")
    duration_ms = runner.get("duration_ms")
    generated_at = _now_iso_utc()

    filename = f"{evidence_id}.pdf"
    report_path = os.path.join(REPORTS_DIR, filename)
    report_url = _build_report_url(filename)

    styles = getSampleStyleSheet()
    story = []

    # ---------- Header ----------
    story.append(Paragraph("Reporte de Ejecución de Pruebas Automatizadas", styles["Title"]))
    story.append(Spacer(1, 0.15 * inch))
    story.append(Paragraph("<b>Agente:</b> Vanya", styles["Normal"]))
    story.append(Paragraph(f"<b>Fecha (UTC):</b> {generated_at}", styles["Normal"]))
    story.append(Paragraph(f"<b>Evidencia ID:</b> {evidence_id}", styles["Normal"]))
    story.append(Spacer(1, 0.2 * inch))

    # ---------- Executive summary ----------
    story.append(Paragraph("Resumen ejecutivo", styles["Heading2"]))
    risk = _risk_label(status)
    story.append(Paragraph(f"<b>Resultado:</b> {status.upper()}", styles["Normal"]))
    story.append(Paragraph(f"<b>URL:</b> {base_url}", styles["Normal"]))
    if isinstance(duration_ms, int):
        story.append(Paragraph(f"<b>Duración:</b> {duration_ms} ms", styles["Normal"]))
    story.append(Paragraph(f"<b>Riesgo:</b> {risk}", styles["Normal"]))
    if error_msg:
        story.append(Paragraph(f"<b>Error:</b> {str(error_msg)}", styles["Normal"]))
    story.append(Spacer(1, 0.2 * inch))

    # ---------- Prompt ----------
    story.append(Paragraph("Solicitud", styles["Heading2"]))
    story.append(Paragraph(f"<b>Prompt:</b> {prompt}", styles["Normal"]))
    story.append(Spacer(1, 0.2 * inch))

    # ---------- Steps table ----------
    story.append(Paragraph("Pasos ejecutados", styles["Heading2"]))

    report_steps = runner.get("steps") if isinstance(runner.get("steps"), list) else None
    rows_src = report_steps or steps or []

    table_data = [["#", "Acción", "Resultado", "Duración (ms)", "Error"]]
    for idx, s in enumerate(rows_src, start=1):
        action = str(s.get("action") or s.get("name") or "").strip()
        st = str(s.get("status") or "").strip()
        dur = s.get("duration_ms")
        err = s.get("error")
        table_data.append(
            [
                str(s.get("i") or idx),
                action,
                st,
                str(dur) if dur is not None else "",
                str(err) if err else "",
            ]
        )

    tbl = Table(
        table_data,
        colWidths=[0.5 * inch, 2.2 * inch, 0.9 * inch, 1.1 * inch, 2.8 * inch],
    )
    tbl.setStyle(
        TableStyle(
            [
                ("BACKGROUND", (0, 0), (-1, 0), colors.HexColor("#222222")),
                ("TEXTCOLOR", (0, 0), (-1, 0), colors.white),
                ("FONTNAME", (0, 0), (-1, 0), "Helvetica-Bold"),
                ("FONTSIZE", (0, 0), (-1, 0), 10),
                ("GRID", (0, 0), (-1, -1), 0.25, colors.grey),
                ("FONTSIZE", (0, 1), (-1, -1), 9),
                ("VALIGN", (0, 0), (-1, -1), "TOP"),
            ]
        )
    )
    story.append(tbl)
    story.append(Spacer(1, 0.25 * inch))

    # ---------- Evidence image ----------
    story.append(Paragraph("Evidencia visual", styles["Heading2"]))

    # Preferimos screenshot_data_url; si no, screenshot_b64
    s_data = runner.get("screenshot_data_url") or runner.get("screenshotDataUrl")
    if not s_data:
        b64 = (
            runner.get("screenshot_b64")
            or runner.get("screenshotBase64")
            or runner.get("screenshotB64")
            or runner.get("screenshot_base64")
        )
        if b64:
            s_data = "data:image/png;base64," + str(b64).strip()

    img_bytes = _decode_data_url_to_png(s_data) if s_data else None
    if img_bytes:
        # guardamos png junto al pdf (útil para debug)
        png_path = os.path.join(REPORTS_DIR, f"{evidence_id}.png")
        try:
            with open(png_path, "wb") as f:
                f.write(img_bytes)
            story.append(Paragraph("Estado final del sistema al concluir la prueba.", styles["Normal"]))
            story.append(Spacer(1, 0.10 * inch))
            story.append(Image(png_path, width=7.0 * inch, height=3.9 * inch))
        except Exception:
            story.append(Paragraph("No se pudo incrustar la imagen en el PDF.", styles["Normal"]))
    else:
        story.append(Paragraph("No se recibió screenshot para incrustar en el PDF.", styles["Normal"]))

    story.append(Spacer(1, 0.15 * inch))
    story.append(Paragraph("Metadatos", styles["Heading2"]))
    story.append(Paragraph(f"<b>Thread:</b> {meta.get('thread_id','')}", styles["Normal"]))
    story.append(Paragraph(f"<b>Session:</b> {meta.get('session_id','')}", styles["Normal"]))
    story.append(Paragraph(f"<b>Headless:</b> {meta.get('headless','')}", styles["Normal"]))

    doc = SimpleDocTemplate(
        report_path,
        pagesize=letter,
        rightMargin=36,
        leftMargin=36,
        topMargin=36,
        bottomMargin=36,
        title=f"Vanya Report {evidence_id}",
    )
    doc.build(story)

    return {"report_path": report_path, "report_url": report_url}