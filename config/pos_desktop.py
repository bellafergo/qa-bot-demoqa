# config/pos_desktop.py
"""
POS Desktop Test Configuration — edita este archivo cuando tengas acceso al POS real.

Dos formas de sobrescribir valores:
  1. Editar este archivo directamente (recomendado para config persistente)
  2. Variables de entorno (útil para CI o sesiones temporales — ver lista abajo)

Para descubrir los nombres reales de controles en Windows:
    from pywinauto.application import Application
    app = Application(backend="win32").connect(title="POS Login")
    app.window(title="POS Login").print_control_identifiers()

Para targets frágiles en VB6, usa dict con fallbacks en vez de string:
    TARGET_USUARIO = {
        "primary":  "txtUsuario",
        "primary_strategy": "automation_id",
        "fallbacks": [
            {"type": "class_name",  "value": "ThunderRT6TextBox"},
            {"type": "text_label",  "value": "Usuario"},
        ],
    }

Variables de entorno disponibles
─────────────────────────────────────────────────────────────────────────────
  POS_APP_PATH             ruta al ejecutable
  POS_WIN_LOGIN            título de la ventana de login
  POS_WIN_MAIN             título de la ventana principal
  POS_USER_VALID           usuario válido para TC-POS-001
  POS_PASS_VALID           password válido
  POS_USER_INVALID         usuario inválido para TC-POS-002
  POS_PASS_INVALID         password inválido
  POS_ERROR_TEXT           texto esperado en el label de error (TC-POS-002)
  POS_TARGET_USUARIO       nombre del control de usuario
  POS_TARGET_PASSWORD      nombre del control de password
  POS_TARGET_BTN_ENTRAR    nombre del botón Entrar
  POS_TARGET_MAIN_SCREEN   nombre del control de pantalla principal
  POS_TARGET_ERROR         nombre del control de error
  POS_TARGET_BTN_VENTAS    nombre del botón de ventas (TC-POS-003)
  POS_TARGET_PANEL_VENTAS  nombre del panel de ventas (TC-POS-003)
  POS_TIMEOUT_LOGIN_MS     timeout para esperar pantalla principal (ms)
  POS_TIMEOUT_ERROR_MS     timeout para esperar mensaje de error (ms)
  POS_TIMEOUT_VENTAS_MS    timeout para esperar panel de ventas (ms)
─────────────────────────────────────────────────────────────────────────────
"""
import os

# ── Aplicación ────────────────────────────────────────────────────────────────

APP_PATH = os.getenv("POS_APP_PATH", r"C:\POS\pos.exe")

# ── Títulos de ventana ────────────────────────────────────────────────────────

WIN_LOGIN = os.getenv("POS_WIN_LOGIN", "POS Login")
WIN_MAIN  = os.getenv("POS_WIN_MAIN",  "POS Principal")

# ── Credenciales ──────────────────────────────────────────────────────────────

USER_VALID   = os.getenv("POS_USER_VALID",   "tester")
PASS_VALID   = os.getenv("POS_PASS_VALID",   "1234")
USER_INVALID = os.getenv("POS_USER_INVALID", "bad_user")
PASS_INVALID = os.getenv("POS_PASS_INVALID", "bad_pass")

# ── Texto esperado en error (TC-POS-002) ──────────────────────────────────────
#
# Ajusta si el mensaje de error del POS real dice otra cosa, p. ej. "Credenciales incorrectas".

ERROR_TEXT_EXPECTED = os.getenv("POS_ERROR_TEXT", "invalid")

# ── Targets de controles ──────────────────────────────────────────────────────
#
# Valor str  → pywinauto busca por control_name (window_text exacto)
# Valor dict → usa estrategias de fallback (ver cabecera del archivo)
#
# EDITA ESTOS VALORES con los resultados de print_control_identifiers().

TARGET_USUARIO = os.getenv("POS_TARGET_USUARIO", "usuario")

TARGET_PASSWORD = os.getenv("POS_TARGET_PASSWORD", "password")

TARGET_BTN_ENTRAR = os.getenv("POS_TARGET_BTN_ENTRAR", "Entrar")

TARGET_PANTALLA_PRINCIPAL = os.getenv("POS_TARGET_MAIN_SCREEN", "pantalla_principal")

TARGET_ERROR_LABEL = os.getenv("POS_TARGET_ERROR", "error_label")

TARGET_BTN_VENTAS = os.getenv("POS_TARGET_BTN_VENTAS", "Ventas")

TARGET_PANEL_VENTAS = os.getenv("POS_TARGET_PANEL_VENTAS", "panel_ventas")

# ── Timeouts ──────────────────────────────────────────────────────────────────

TIMEOUT_LOGIN_MS  = int(os.getenv("POS_TIMEOUT_LOGIN_MS",  "5000"))
TIMEOUT_ERROR_MS  = int(os.getenv("POS_TIMEOUT_ERROR_MS",  "3000"))
TIMEOUT_VENTAS_MS = int(os.getenv("POS_TIMEOUT_VENTAS_MS", "3000"))
