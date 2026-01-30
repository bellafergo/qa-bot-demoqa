from __future__ import annotations

import time
from typing import Any, Dict, List, Optional

from services.supabase_store import supabase_client
from services.document_extractors import extract_text
from services.document_rag import sha256_bytes, chunk_text, embed_texts

DOCUMENT_BUCKET = "documents"

def upload_document(
    *,
    filename: str,
    content_type: Optional[str],
    data: bytes,
    thread_id: Optional[str] = None,
    workspace_id: Optional[str] = None,
    tags: Optional[List[str]] = None,
    source: Optional[str] = None,
) -> Dict[str, Any]:
    sb = supabase_client()
    if not sb:
        return {"ok": False, "error": "Supabase not configured"}

    tags = tags or []
    digest = sha256_bytes(data)
    ts = int(time.time())
    safe_name = (filename or f"document_{ts}").replace("..", ".").replace("/", "_")
    storage_path = f"{workspace_id or 'default'}/{thread_id or 'no-thread'}/{ts}_{safe_name}"

    # 1) Upload to storage
    sb.storage.from_(DOCUMENT_BUCKET).upload(
        path=storage_path,
        file=data,
        file_options={"content-type": content_type or "application/octet-stream", "upsert": "true"},
    )

    # 2) Extract
    raw_text = extract_text(filename, content_type, data)
    extracted_at = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())

    # 3) Insert documents row
    doc_res = (
        sb.table("documents")
        .insert({
            "workspace_id": workspace_id,
            "thread_id": thread_id,
            "filename": filename,
            "content_type": content_type,
            "size_bytes": len(data),
            "storage_bucket": DOCUMENT_BUCKET,
            "storage_path": storage_path,
            "sha256": digest,
            "tags": tags,
            "source": source,
            "raw_text": raw_text,
            "extracted_at": extracted_at,
        })
        .execute()
    )

    doc_row = (getattr(doc_res, "data", None) or [None])[0]
    if not doc_row or not doc_row.get("id"):
        return {"ok": False, "error": "Failed to insert documents row"}

    doc_id = doc_row["id"]

    # 4) Chunks + embeddings (best effort)
    chunks = chunk_text(raw_text)
    embeddings = embed_texts([c.text for c in chunks])  # may be None

    rows = []
    for i, c in enumerate(chunks):
        row = {
            "doc_id": doc_id,
            "chunk_index": c.chunk_index,
            "text": c.text,
            "char_start": c.char_start,
            "char_end": c.char_end,
            "tokens_est": c.tokens_est,
        }
        if embeddings and i < len(embeddings):
            row["embedding"] = embeddings[i]
        rows.append(row)

    if rows:
        # Insert in batches
        for j in range(0, len(rows), 100):
            sb.table("document_chunks").insert(rows[j:j+100]).execute()

    return {
        "ok": True,
        "doc": {
            "id": doc_id,
            "filename": filename,
            "content_type": content_type,
            "size_bytes": len(data),
            "storage_path": storage_path,
            "thread_id": thread_id,
            "workspace_id": workspace_id,
            "tags": tags,
            "source": source,
            "extracted": bool(raw_text),
            "chunks": len(chunks),
            "has_embeddings": bool(embeddings),
        }
    }


def query_documents(
    *,
    query: str,
    thread_id: Optional[str] = None,
    doc_ids: Optional[List[str]] = None,
    top_k: int = 5,
) -> Dict[str, Any]:
    sb = supabase_client()
    if not sb:
        return {"ok": False, "error": "Supabase not configured"}

    query = (query or "").strip()
    if not query:
        return {"ok": False, "error": "query is required"}

    # Strategy:
    # A) If embeddings exist: use a SQL RPC later (recommended) OR fallback to ILIKE now.
    # For MVP: do ILIKE text search (works immediately), then you upgrade to vector search RPC.
    q = sb.table("document_chunks").select("id,doc_id,chunk_index,text")

    if doc_ids:
        q = q.in_("doc_id", doc_ids)

    if thread_id and not doc_ids:
        # constrain to thread docs
        docs = sb.table("documents").select("id").eq("thread_id", thread_id).execute()
        ids = [r["id"] for r in (getattr(docs, "data", None) or []) if r.get("id")]
        if ids:
            q = q.in_("doc_id", ids)

    # naive search
    q = q.ilike("text", f"%{query}%").limit(top_k)
    res = q.execute()
    rows = getattr(res, "data", None) or []

    # add filename for citations
    doc_map = {}
    if rows:
        unique_docs = list({r["doc_id"] for r in rows if r.get("doc_id")})
        docs_res = sb.table("documents").select("id,filename,storage_path").in_("id", unique_docs).execute()
        for d in (getattr(docs_res, "data", None) or []):
            doc_map[d["id"]] = d

    snippets = []
    for r in rows:
        d = doc_map.get(r["doc_id"], {})
        snippets.append({
            "doc_id": r["doc_id"],
            "filename": d.get("filename"),
            "chunk_index": r.get("chunk_index"),
            "text": (r.get("text") or "")[:1200],
        })

    return {"ok": True, "snippets": snippets}
