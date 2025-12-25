# core/settings.py
import os
from pathlib import Path
from dataclasses import dataclass

from dotenv import load_dotenv

# Carga .env una sola vez, temprano
load_dotenv()


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
    MAX_HISTORY_MSGS: int = int(os.getenv("MAX_HISTORY_MSGS", "10"))

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
    # Cloudinary
    # ----------------------------
    CLOUDINARY_URL: str = (os.getenv("CLOUDINARY_URL") or "").strip()

    # acepta variantes (como ya lo tienes)
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
    # Evidence dir + CORS
    # ----------------------------
    @property
    def BASE_DIR(self) -> Path:
        # core/ está un nivel abajo de app.py (raíz del proyecto)
        return Path(__file__).resolve().parents[1]

    @property
    def EVIDENCE_DIR(self) -> Path:
        # respeta tu env actual EVIDENCE_DIR o default <project>/evidence
        return Path(os.getenv("EVIDENCE_DIR", str(self.BASE_DIR / "evidence")))

    @property
    def CORS_ALLOW_ORIGINS(self) -> list[str]:
        # lo que ya usas en app.py
        return [
            "http://localhost:5173",
            "http://localhost:5174",
            "https://valtre-vanya.vercel.app",
            "https://valtre-vanya.vercel.app/".rstrip("/"),
            "https://valtre-vanya.vercel.app".rstrip("/"),
        ]


# Singleton para importar en toda la app
settings = Settings()