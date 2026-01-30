# api/routes/meta.py
import os
from fastapi import APIRouter

from core.settings import settings
from core.state import cleanup_sessions, SESSIONS, DOC_CACHE
from services.supabase_store import check_supabase_health, SUPABASE_STRICT

router = APIRouter()

@router.get("/meta")
def meta():
    cleanup_sessions()

    # Check Supabase health
    sb_health = check_supabase_health()

    return {
        "ok": True,
        "render_git_commit": os.getenv("RENDER_GIT_COMMIT"),
        "model": settings.OPENAI_MODEL,
        "has_openai_key": bool(settings.OPENAI_API_KEY),
        "session_ttl_s": settings.SESSION_TTL_S,
        "sessions_in_memory": len(SESSIONS),
        "doc_cache_items": len(DOC_CACHE),
        "has_db": bool(os.getenv("DATABASE_URL")),
        "has_cloudinary": bool(settings.CLOUDINARY_URL),
        # Supabase status
        "supabase_ok": sb_health["ok"],
        "supabase_configured": sb_health["configured"],
        "supabase_strict": SUPABASE_STRICT,
    }