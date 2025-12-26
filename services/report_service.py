# services/report_service.py
from __future__ import annotations

import os
import base64
import datetime as dt
from typing import Any, Dict, List, Optional

from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch
from reportlab.lib import colors
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, Image
from reportlab.lib.styles import getSampleStyleSheet

REPORTS_DIR = os.path.join("evidence", "reports")


def _utc_iso() -> str:
    return dt.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"


def _ensure_dir(path: str) -> None:
    os.makedirs(path, exist_ok=True)


def _decode_data_url_to_png(data_url: str) -> Optional[bytes]:
    if not data_url:
        return None
    s = str(data_url).strip()
    if not s:
        return None

    if s.startswith("data:image"):
        try:
            _, b64 = s.split(",", 1)
            return base64.b64decode(b64)
        except Exception:
            return None

    try:
        return base64.b64decode(s)
    except Exception:
        return None


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
    Genera un PDF local (temporal) para luego subirlo a Cloudinary.
    Retorna:
      {"report_path": "...", "report_filename": "..."}
    """
    _ensure_dir(REPORTS_DIR)

    meta = meta or {}
    evidence_id = (evidence_id or runner.get("evidence_id") or "EV-unknown").strip()

    status = (runner.get("status") or "").strip() or ("passed" if runner.get("ok") else "fail")
    error_msg = runner.get("error")
    duration_ms = runner.get("duration_ms")

    filename = f"{evidence_id}.pdf"
    report_path = os.path.join(REPORTS_DIR, filename)

    styles = getSampleStyleSheet()
    story = []

    # Header
    story.append(Paragraph("Vanya - Reporte de Ejecución", styles["Title"]))
    story.append(Spacer(1, 0.15 * inch))
    story.append(Paragraph(f"<b>Fecha (UTC):</b> {_utc_iso()}", styles["Normal"]))
    story.append(Paragraph(f"<b>Evidencia ID:</b> {evidence_id}", styles["Normal"]))
    story.append(Paragraph(f"<b>URL:</b> {base_url}", styles["Normal"]))
    story.append(Spacer(1, 0.2 * inch))

    # Resumen
    story.append(Paragraph("Resumen", styles["Heading2"]))
    story.append(Paragraph(f"<b>Resultado:</b> {status.upper()}", styles["Normal"]))
    if isinstance(duration_ms, int):
        story.append(Paragraph(f"<b>Duración:</b> {duration_ms} ms", styles["Normal"]))
    if error_msg:
        story.append(Paragraph(f"<b>Error:</b> {str(error_msg)}", styles["Normal"]))
    story.append(Spacer(1, 0.2 * inch))

    # Prompt
    story.append(Paragraph("Solicitud", styles["Heading2"]))
    story.append(Paragraph(f"{prompt}", styles["Normal"]))
    story.append(Spacer(1, 0.2 * inch))

    # Steps
    story.append(Paragraph("Pasos ejecutados", styles["Heading2"]))
    rows_src = runner.get("steps") if isinstance(runner.get("steps"), list) else (steps or [])

    table_data = [["#", "Acción", "Resultado", "Duración (ms)", "Error"]]
    for idx, s in enumerate(rows_src, start=1):
        action = str(s.get("action") or s.get("name") or "").strip()
        st = str(s.get("status") or "").strip()
        dur = s.get("duration_ms")
        err = s.get("error")
        table_data.append([
            str(s.get("i") or idx),
            action,
            st,
            str(dur) if dur is not None else "",
            str(err) if err else "",
        ])

    tbl = Table(table_data, colWidths=[0.5 * inch, 2.3 * inch, 1.0 * inch, 1.1 * inch, 2.6 * inch])
    tbl.setStyle(TableStyle([
        ("BACKGROUND", (0, 0), (-1, 0), colors.HexColor("#222222")),
        ("TEXTCOLOR", (0, 0), (-1, 0), colors.white),
        ("FONTNAME", (0, 0), (-1, 0), "Helvetica-Bold"),
        ("FONTSIZE", (0, 0), (-1, 0), 10),
        ("GRID", (0, 0), (-1, -1), 0.25, colors.grey),
        ("FONTSIZE", (0, 1), (-1, -1), 9),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
    ]))
    story.append(tbl)
    story.append(Spacer(1, 0.25 * inch))

    # Evidencia
    story.append(Paragraph("Evidencia visual", styles["Heading2"]))

    s_data = runner.get("screenshot_data_url") or runner.get("screenshotDataUrl")
    if not s_data:
        b64 = runner.get("screenshot_b64") or runner.get("screenshotBase64") or runner.get("screenshotB64")
        if b64:
            s_data = "data:image/png;base64," + str(b64).strip()

    img_bytes = _decode_data_url_to_png(s_data) if s_data else None
    if img_bytes:
        png_path = os.path.join(REPORTS_DIR, f"{evidence_id}.png")
        try:
            with open(png_path, "wb") as f:
                f.write(img_bytes)
            story.append(Paragraph("Estado final capturado.", styles["Normal"]))
            story.append(Spacer(1, 0.10 * inch))
            story.append(Image(png_path, width=7.0 * inch, height=3.9 * inch))
        except Exception:
            story.append(Paragraph("No se pudo incrustar la imagen en el PDF.", styles["Normal"]))
    else:
        story.append(Paragraph("No se recibió screenshot para incrustar.", styles["Normal"]))

    story.append(Spacer(1, 0.2 * inch))

    # Meta
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

    return {"report_path": report_path, "report_filename": filename}
✅ 2) En services/chat_service.py: pega ESTE bloque en EXECUTE
Esto es lo que te faltaba: poner report_error y asignar report_url sí o sí.

Busca tu sección donde generas reporte y reemplázala por:

python
Copiar código
# ============================================================
# REPORT (PDF -> Cloudinary)
# ============================================================
report_url: Optional[str] = None
report_error: Optional[str] = None

try:
    from services.report_service import generate_pdf_report
    from services.cloudinary_service import upload_pdf_bytes

    rep = generate_pdf_report(
        prompt=prompt,
        base_url=base_url,
        runner=result,
        steps=steps,
        evidence_id=evidence_id,
        meta={
            "thread_id": thread_id,
            "session_id": session_id,
            "headless": bool(getattr(req, "headless", True)),
        },
    )

    pdf_path = rep.get("report_path")
    if not pdf_path or not os.path.exists(pdf_path):
        raise RuntimeError(f"PDF no se generó en disco: {pdf_path}")

    with open(pdf_path, "rb") as f:
        pdf_bytes = f.read()

    uploaded_pdf = upload_pdf_bytes(
        pdf_bytes,
        evidence_id=evidence_id,
        folder="vanya/reports",
    )

    report_url = uploaded_pdf.get("secure_url") or uploaded_pdf.get("url")
    if not report_url:
        raise RuntimeError(f"Cloudinary no regresó URL: {uploaded_pdf}")

    # ✅ guarda para UI
    result["report_url"] = report_url

except Exception as e:
    report_error = str(e)
    result["report_error"] = report_error
    logger.warning(f"PDF report failed: {report_error}", exc_info=True)
Y ASEGÚRATE de guardar report_url y report_error en assistant_meta:

python
Copiar código
"report_url": report_url,
"report_error": report_error,
"runner": {
   ...
   "report_url": report_url,
   "report_error": report_error,
}