# core/settings.py
import os
from pathlib import Path
from dataclasses import dataclass
from typing import List

from dotenv import load_dotenv

# Carga .env una sola vez, temprano
load_dotenv()


def _csv_list(val: str) -> List[str]:
    """
    Convierte "a,b,c" -> ["a","b","c"] limpiando espacios y vacíos.
    """
    if not val:
        return []
    return [x.strip().rstrip("/") for x in val.split(",") if x.strip()]


@dataclass(frozen=True)
class Settings:
    # ----------------------------
    # OpenAI
    # ----------------------------
    OPENAI_API_KEY: str = (os.getenv("OPENAI_API_KEY") or "").strip()
    OPENAI_MODEL: str = (os.getenv("OPENAI_MODEL") or "gpt-4o-mini").strip()

    # ----------------------------
    # Sesión / memoria
    # ----------------------------
    SESSION_TTL_S: int = int(os.getenv("SESSION_TTL_S", "3600"))
    MAX_HISTORY_MSGS: int = int(os.getenv("MAX_HISTORY_MSGS", "16"))
    MEMORY_MAX_MESSAGES: int = int(os.getenv("MEMORY_MAX_MESSAGES", "12"))
    MAX_SESSIONS_IN_MEMORY: int = int(os.getenv("MAX_SESSIONS_IN_MEMORY", "200"))

    # ----------------------------
    # DOC (artefactos)
    # ----------------------------
    DOC_DEFAULT_DOMAIN: str = (os.getenv("DOC_DEFAULT_DOMAIN") or "retail").strip()
    DOC_MAX_TEST_CASES: int = int(os.getenv("DOC_MAX_TEST_CASES", "14"))
    DOC_MAX_GHERKIN: int = int(os.getenv("DOC_MAX_GHERKIN", "8"))
    DOC_MAX_FILES: int = int(os.getenv("DOC_MAX_FILES", "3"))
    DOC_MAX_CODE_CHARS: int = int(os.getenv("DOC_MAX_CODE_CHARS", "3500"))
    DOC_HISTORY_MSGS: int = int(os.getenv("DOC_HISTORY_MSGS", "4"))
    DOC_CACHE_MAX: int = int(os.getenv("DOC_CACHE_MAX", "80"))

    DOC_TEMPERATURE: float = float(os.getenv("DOC_TEMPERATURE", "0.2"))
    ADV_TEMPERATURE: float = float(os.getenv("ADV_TEMPERATURE", "0.4"))
    EXEC_TEMPERATURE: float = float(os.getenv("EXEC_TEMPERATURE", "0.2"))

    DOC_MAX_TOKENS: int = int(os.getenv("DOC_MAX_TOKENS", "1100"))
    ADV_MAX_TOKENS: int = int(os.getenv("ADV_MAX_TOKENS", "700"))
    EXEC_MAX_TOKENS: int = int(os.getenv("EXEC_MAX_TOKENS", "700"))

    # ----------------------------
    # Runner (si lo usas)
    # ----------------------------
    RUNNER_URL: str = (os.getenv("RUNNER_URL") or "").strip()
    RUNNER_TOKEN: str = (os.getenv("RUNNER_TOKEN") or "").strip()
    RUNNER_TIMEOUT_S: int = int(os.getenv("RUNNER_TIMEOUT_S", "120"))

    # ----------------------------
    # DB
    # ----------------------------
    DATABASE_URL: str = (os.getenv("DATABASE_URL") or "").strip()

    @property
    def HAS_DB(self) -> bool:
        return bool(self.DATABASE_URL)

    # ----------------------------
    # Cloudinary
    # ----------------------------
    CLOUDINARY_URL: str = (os.getenv("CLOUDINARY_URL") or "").strip()

    # acepta variantes
    CLOUDINARY_CLOUD_NAME: str = (
        os.getenv("CLOUDINARY_CLOUD_NAME") or os.getenv("CLOUDINARY_CLOUD") or ""
    ).strip()
    CLOUDINARY_API_KEY: str = (
        os.getenv("CLOUDINARY_API_KEY") or os.getenv("CLOUDINARY_KEY") or ""
    ).strip()
    CLOUDINARY_API_SECRET: str = (
        os.getenv("CLOUDINARY_API_SECRET") or os.getenv("CLOUDINARY_SECRET") or ""
    ).strip()

    @property
    def HAS_CLOUDINARY(self) -> bool:
        return bool(self.CLOUDINARY_CLOUD_NAME and self.CLOUDINARY_API_KEY and self.CLOUDINARY_API_SECRET)

    # ----------------------------
    # Paths
    # ----------------------------
    @property
    def BASE_DIR(self) -> Path:
        # core/ está un nivel abajo de la raíz del proyecto
        return Path(__file__).resolve().parents[1]

    @property
    def EVIDENCE_DIR(self) -> Path:
        p = Path(os.getenv("EVIDENCE_DIR", str(self.BASE_DIR / "evidence")))
        # asegura que exista para StaticFiles / guardado
        try:
            p.mkdir(parents=True, exist_ok=True)
        except Exception:
            pass
        return p

    # ----------------------------
    # CORS
    # ----------------------------
    @property
    def CORS_ORIGINS(self) -> List[str]:
        """
        Permite override via env:
          CORS_ORIGINS="http://localhost:5173,https://valtre-vanya.vercel.app"
        """
        env_val = (os.getenv("CORS_ORIGINS") or "").strip()
        if env_val:
            return _csv_list(env_val)

        # defaults seguros
        defaults = [
            "http://localhost:5173",
            "http://localhost:5174",
            "https://valtre-vanya.vercel.app",
        ]
        # normaliza (sin slash final)
        return [x.rstrip("/") for x in defaults]

    # Backwards compat con tu código viejo
    @property
    def CORS_ALLOW_ORIGINS(self) -> List[str]:
        return self.CORS_ORIGINS


# Singleton
settings = Settings()