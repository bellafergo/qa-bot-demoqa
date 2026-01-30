from __future__ import annotations

import hashlib
import math
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Tuple

from core.settings import settings

@dataclass
class Chunk:
    chunk_index: int
    text: str
    char_start: int
    char_end: int
    tokens_est: int

def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()

def chunk_text(text: str, max_chars: int = 1200, overlap: int = 150) -> List[Chunk]:
    text = (text or "").strip()
    if not text:
        return []
    chunks: List[Chunk] = []
    n = len(text)
    start = 0
    idx = 0
    while start < n:
        end = min(n, start + max_chars)
        piece = text[start:end].strip()
        if piece:
            tokens_est = max(1, math.ceil(len(piece) / 4))  # rough
            chunks.append(Chunk(idx, piece, start, end, tokens_est))
            idx += 1
        start = max(end - overlap, end)
    return chunks

def embeddings_enabled() -> bool:
    # si tienes OPENAI_API_KEY y habilitas setting (o simplemente key)
    return bool((settings.OPENAI_API_KEY or "").strip())

def embed_texts(texts: List[str]) -> Optional[List[List[float]]]:
    """
    Devuelve embeddings para textos.
    Si no hay API key, regresa None.
    Ajusta modelo/dimensión según tu elección.
    """
    if not embeddings_enabled() or not texts:
        return None

    try:
        from openai import OpenAI
        client = OpenAI(api_key=settings.OPENAI_API_KEY)

        # OJO: el modelo/dim varía; ajusta si usas otro
        resp = client.embeddings.create(
            model="text-embedding-3-small",
            input=texts,
        )
        return [d.embedding for d in resp.data]
    except Exception:
        return None
