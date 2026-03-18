#!/usr/bin/env python
"""
Run TC-POS-001 (POS Login exitoso) against a real desktop application.

Prerequisites (Windows only):
  1. pip install pywinauto pillow
  2. La aplicación POS debe estar en C:\POS\pos.exe  (o ajusta APP_PATH abajo)
  3. La ventana de login debe tener el título exacto definido en WINDOW_TITLE
  4. Los controles deben ser accesibles por pywinauto (Win32 / WinForms / VB6)

Usage (desde raíz del proyecto en Windows):
  .venv\Scripts\python scripts\run_pos_001_real.py

Para forzar mock en cualquier OS (útil para CI o depuración):
  set DESKTOP_RUNNER_MOCK=1 && .venv\Scripts\python scripts\run_pos_001_real.py

Control de target names:
  Los nombres de target ("usuario", "password", "Entrar", etc.) deben coincidir
  con el título del control (window_text) según pywinauto.
  Para inspeccionarlos: python -m pywinauto.findwindows --title "POS Login"
  O usa: app.window(title="POS Login").print_control_identifiers()

Ajuste de targets para VB6:
  Si los nombres no coinciden, usa el formato dict con fallbacks:
    {
        "primary": "txtUsuario",
        "primary_strategy": "automation_id",
        "fallbacks": [
            {"type": "class_name",  "value": "ThunderRT6TextBox"},
            {"type": "text_label",  "value": "Usuario"},
        ],
    }
  Asegúrate de actualizar también el seed en services/test_catalog_seed.py.
"""
import logging
import os
import sys

# Allow running from project root
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# ── Configuration — adjust these for your environment ─────────────────────────

APP_PATH     = r"C:\POS\pos.exe"
WINDOW_TITLE = "POS Login"
USER         = "tester"
PASSWORD     = "1234"
EXPECTED_CTL = "pantalla_principal"   # control to assert after login

# ── Logging setup ──────────────────────────────────────────────────────────────

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s  %(levelname)-8s  %(name)s — %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger("pos_001_real")

# ── Steps for TC-POS-001 ───────────────────────────────────────────────────────

STEPS = [
    {"action": "launch_app",    "value":  APP_PATH},
    {"action": "attach_window", "target": WINDOW_TITLE},
    {"action": "input",         "target": "usuario",           "value": USER},
    {"action": "input",         "target": "password",          "value": PASSWORD},
    {"action": "click",         "target": "Entrar"},
    {"action": "wait_for",      "target": EXPECTED_CTL,        "ms": 5000},
    {"action": "assert_exists", "target": EXPECTED_CTL},
    {"action": "screenshot"},
]


def main() -> None:
    from runners.desktop_runner import run_desktop_test

    # DESKTOP_RUNNER_MOCK must NOT be set (or be "0") for real execution.
    # _should_use_mock() will log the reason before the run starts.
    mock_env = os.getenv("DESKTOP_RUNNER_MOCK", "0")
    if mock_env.lower() in ("1", "true", "yes"):
        logger.warning("DESKTOP_RUNNER_MOCK=%s — running in mock mode (not real)", mock_env)
    else:
        logger.info("DESKTOP_RUNNER_MOCK not set — attempting real backend")

    logger.info("Running TC-POS-001 with %d steps", len(STEPS))
    result = run_desktop_test(steps=STEPS, timeout_s=30)

    # ── Summary ──────────────────────────────────────────────────────────────
    print("\n" + "=" * 60)
    print(f"  status       : {result['status']}")
    print(f"  ok           : {result['ok']}")
    print(f"  backend      : {'mock' if result['meta']['is_mock'] else 'pywinauto/real'}")
    print(f"  duration     : {result['duration_ms']} ms")
    print(f"  steps run    : {result['meta']['steps_count']}")
    print(f"  screenshots  : {len(result['evidence']['screenshots'])}")
    print(f"  evidence_id  : {result['evidence_id']}")
    if not result["ok"]:
        print(f"  reason       : {result['reason']}")
    print("=" * 60)

    # ── Per-step detail ───────────────────────────────────────────────────────
    print("\nStep detail:")
    for s in result["steps"]:
        icon = "✓" if s["status"] == "passed" else "✗"
        err  = f"  → {s['error']}" if s.get("error") else ""
        print(f"  {icon}  [{s['index']}] {s['action']:<22} target={s['target']!r}{err}")

    # ── Logs ─────────────────────────────────────────────────────────────────
    print("\nRunner logs:")
    for line in result["logs"]:
        print(f"  {line}")

    sys.exit(0 if result["ok"] else 1)


if __name__ == "__main__":
    main()
