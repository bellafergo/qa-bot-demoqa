import time
import traceback
from services.run_store import save_run
from runners.heb_checkout import execute_heb_full_purchase  # o tu runner principal
# o from runners.generic_steps import execute_test

def run_execute_job(evidence_id: str, payload: dict) -> dict:
    t0 = time.time()
    save_run(evidence_id, {
        "status": "running",
        "started_at": int(t0 * 1000),
        "meta": payload,
    })

    try:
        # Ejecuta tu runner (ajústalo a lo que ya tengas)
        result = execute_heb_full_purchase(**payload)

        # Aquí idealmente subes evidencia a Cloudinary/GCS y generas URLs
        # Si ya tienes evidence_service, llama ese aquí.
        status = result.get("status") or "failed"
        t1 = time.time()

        out = {
            "status": status,
            "finished_at": int(t1 * 1000),
            "duration_ms": int((t1 - t0) * 1000),
            "report_url": result.get("report_url"),
            "evidence_url": result.get("evidence_url"),
        }
        save_run(evidence_id, out)
        return out

    except Exception as e:
        t1 = time.time()
        err = f"{e}\n{traceback.format_exc()}"
        out = {
            "status": "failed",
            "finished_at": int(t1 * 1000),
            "duration_ms": int((t1 - t0) * 1000),
            "error_message": err[:5000],
        }
        save_run(evidence_id, out)
        return out
