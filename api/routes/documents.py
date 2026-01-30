from __future__ import annotations

from typing import Any, Dict, List, Optional

from fastapi import APIRouter, File, Form, UploadFile
from pydantic import BaseModel

from services.document_store import upload_document, query_documents

router = APIRouter()

class DocQueryRequest(BaseModel):
    query: str
    thread_id: Optional[str] = None
    doc_ids: Optional[List[str]] = None
    top_k: int = 5

@router.post("/documents/upload")
async def documents_upload(
    file: UploadFile = File(...),
    thread_id: Optional[str] = Form(None),
    workspace_id: Optional[str] = Form(None),
    source: Optional[str] = Form(None),
    tags: Optional[str] = Form(None),  # comma separated
) -> Dict[str, Any]:
    data = await file.read()
    tag_list = [t.strip() for t in (tags or "").split(",") if t.strip()]
    return upload_document(
        filename=file.filename,
        content_type=file.content_type,
        data=data,
        thread_id=thread_id,
        workspace_id=workspace_id,
        tags=tag_list,
        source=source,
    )

@router.post("/documents/query")
def documents_query(req: DocQueryRequest) -> Dict[str, Any]:
    return query_documents(
        query=req.query,
        thread_id=req.thread_id,
        doc_ids=req.doc_ids,
        top_k=req.top_k,
    )
